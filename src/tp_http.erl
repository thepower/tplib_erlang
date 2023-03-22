-module(tp_http).

-export([parse_url/1]).
-export([submit_tx/2, wait_tx/3, get/2, post_stream/5]).


parse_url(Node) when is_binary(Node) ->
  parse_url(binary_to_list(Node));

parse_url(Node) when is_list(Node) ->
  CaCerts = try
              certifi:cacerts()
            catch _:_ ->
                    []
            end,

  case uri_string:parse(Node) of
    {error,invalid_uri,":"} ->
      throw({invalid_uri,Node});
    #{scheme:=Sch,
      path:=Path,
      host:=Host}=P ->
      Inet=case inet:parse_address(Host) of
             {ok,{_,_,_,_,_,_,_,_}} ->
               [inet6];
             {ok,{_,_,_,_}} ->
               [inet];
             _ -> []
           end,

      {Opts,Port}=case Sch of
                    "https" when CaCerts==[] ->
                      {
                       #{ transport=>tls, transport_opts=>Inet },
                       maps:get(port,P,443)
                      };
                    "https" ->
                      CHC=[
                           {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                          ],
                      {
                       #{ transport=>tls,
                          transport_opts => [{verify, verify_peer},
                                             {customize_hostname_check, CHC},
                                             {depth, 5},
                                             {versions,['tlsv1.2']},
                                             %{versions,['tlsv1.3']},
                                             {cacerts, CaCerts}
                                             |Inet
                                            ]
                        },
                       maps:get(port,P,443)
                      };
                    "http" ->
                      {
                       #{ transport=>tcp, transport_opts=>Inet },
                       maps:get(port,P,80)
                      }
                  end,
      {Host,
       Port,
       Opts,
       #{path=>Path}
      }
  end.

get(Address, Endpoint) ->
  {Host, Port, Opts,_} = tp_http:parse_url(Address),
  {ok, ConnPid} = gun:open(Host,Port,Opts),
      Get=fun(Endpoint1) ->
              StreamRef = gun:get(ConnPid, Endpoint1, []),
              case gun:await(ConnPid, StreamRef) of
                {response, Fin, Code, Headers} ->
                  Body=case Fin of
                         fin -> <<>>;
                         nofin ->
                           {ok, Body2} = gun:await_body(ConnPid, StreamRef),
                           Body2
                       end,
                  {Code, Headers, Body};
                {error, {closed, Msg}} ->
                  {500,[], Msg};
                {error, Reason} ->
                  {500,[], Reason}
              end
          end,
      try
      case gun:await_up(ConnPid) of
        {ok, _} ->
          Get(Endpoint);
        {error,timeout} ->
          {500,[],<<"timeout">>}
      end
  after
      gun:close(ConnPid)
  end.

submit_tx(Node, Tx) ->
  {Host, Port, Opts,_} = tp_http:parse_url(Node),
  {ok, ConnPid} = gun:open(Host,Port,Opts),
  {ok, _} = gun:await_up(ConnPid),
  Post=fun(Endpoint, Bin) ->
          StreamRef = gun:post(ConnPid, Endpoint, [], Bin, #{}),
          {response, Fin, Code, _Headers} = gun:await(ConnPid, StreamRef),
          Body=case Fin of
                 fin -> <<>>;
                 nofin ->
                   {ok, Body2} = gun:await_body(ConnPid, StreamRef),
                   Body2
               end,
          {Code, Body}
      end,

  %{200, Bytecode} = Get(<<"/api/address/0x",(hex:encode(Address))/binary,"/code">>),
  case Post("/api/tx/new.bin",tx:pack(Tx)) of
    {200,JSON} ->
      Res=case jsx:decode(JSON, [return_maps]) of
            #{<<"ok">>:=true,
              <<"result">>:= <<"ok">>,
              <<"txid">> := TxID
             } ->
              wait_tx(ConnPid, TxID, erlang:system_time(second)+30);
            _ ->
              gun:close(ConnPid),
              throw('bad_result')
          end,
      gun:close(ConnPid),
      Res;
    {500, JSON} ->
      Res=jsx:decode(JSON, [return_maps]),
      gun:close(ConnPid),
      {error,Res};
    Other ->
      {error, Other}
  end.

wait_tx(ConnPid, TxID, Timeout) ->
  Now=erlang:system_time(second),
  if (Now>Timeout) ->
       {error, timeout};
     true ->
       Endpoint = "/api/tx/status/"++binary_to_list(TxID),
       StreamRef = gun:get(ConnPid, Endpoint, []),
       {response, Fin, Code, _Headers} = gun:await(ConnPid, StreamRef),
       if(Code == 200) ->
           Body=case Fin of
                  fin -> <<>>;
                  nofin ->
                    {ok, Body2} = gun:await_body(ConnPid, StreamRef),
                    Body2
                end,
           case jsx:decode(Body, [return_maps]) of
             #{<<"res">> := null, <<"ok">> := true} ->
               timer:sleep(1000),
               wait_tx(ConnPid, TxID, Timeout);
             #{<<"res">>:= Result, <<"ok">> := true} ->
               {ok, Result#{<<"txid">> => TxID}};
             Other ->
               {error, Other}
           end;
         true ->
           {error, bad_res}
       end
  end.


post_stream(Address, Endpoint, Body, Fun1, A) when is_function(Fun1,3) ->
  {Host, Port, Opts,_} = tp_http:parse_url(Address),
  {ok, ConnPid} = gun:open(Host,Port,Opts#{retry=>0}),
  case gun:await_up(ConnPid) of
    {ok, _Proto} ->
      StreamRef = gun:post(ConnPid, Endpoint, [], Body, #{}),
      case gun:await(ConnPid, StreamRef) of
        {response, Fin, Code, Headers} ->
          case Fin of
            fin ->
              gun:close(ConnPid),
              {Code, Headers, <<>>};
            nofin ->
              MRef = monitor(process, ConnPid),
              Str=cont_stream(ConnPid, StreamRef, Fun1, A, 10000,MRef),
              demonitor(MRef, [flush]),
              case Str of
                {ok, D} ->
                  gun:close(ConnPid),
                  {Code, Headers, D};
                {ok, D, Trailer} ->
                  gun:close(ConnPid),
                  {Code, [Trailer|Headers], D};
                {error, Reason} ->
                  gun:close(ConnPid),
                  {500, Headers, Reason}
              end
          end;
        {error, {closed, Msg}} ->
          {500,[], Msg};
        {error, Reason} ->
          {500,[], Reason}
      end;
    {error,timeout} ->
      {500,[],<<"timeout">>}
  end.

cont_stream(ConnPid, StreamRef, Fun1, A, Timeout, MRef) ->
  receive
    {gun_data, ConnPid, StreamRef, nofin, Data} ->
      A1=Fun1(Data,A,false),
      cont_stream(ConnPid, StreamRef, Fun1, A1, Timeout, MRef);
    {gun_data, ConnPid, StreamRef, fin, Data} ->
      A1=Fun1(Data,A,true),
      {ok, A1};
    %% It's OK to return trailers here because the client
    %% specifically requested them.
    {gun_trailers, ConnPid, StreamRef, Trailers} ->
      {ok, A, Trailers};
    {gun_error, ConnPid, StreamRef, Reason} ->
      {error, Reason};
    {gun_error, ConnPid, Reason} ->
      {error, Reason};
    {'DOWN', MRef, process, ConnPid, Reason} ->
      {error, Reason}
  after Timeout ->
          {error, timeout}
  end.

