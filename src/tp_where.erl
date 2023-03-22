%%%-------------------------------------------------------------------
%% @doc tp_where gen_server
%% @end
%%%-------------------------------------------------------------------
-module(tp_where).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2023-03-03").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,start_link/1,lookup/1,chain/1,ask_where/2,ask_where/3,ask_where/5]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

lookup(<<_:64>>=Address) ->
  gen_server:call(?SERVER, {lookup, Address}).

chain(<<_:64>>=Address) ->
  gen_server:call(?SERVER, {getchain, Address}).

start_link(C) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [C], []).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  io:format("Ar ~p~n", [Args]),
  BS=case Args of
       [#{bootstrap:=L}] when is_list(L) ->
         L;
       #{app:=App} ->
         case application:get_env(App,bootstrap) of
           {ok, L} when is_list(L) -> L;
           _ -> []
         end;
       _ ->
         case application:get_env(bootstrap) of
           {ok, L} when is_list(L) -> L;
           _ -> []
         end
     end,
  self() ! ready,
  {ok, #{bootstrap=>BS, cache=>#{}}}.

handle_call({getchain, Addr}, _From, State=#{cache:=C, bootstrap:=BSN}) ->
  try
    {Code,Res,C1}=ask_where(BSN,
                           naddress:encode(Addr),
                           get,
                           fun tp_http:get/2,
                           C
                          ),
     {reply, {Code,Res}, State#{cache=>C1}}
  catch Ec:Ee:S ->
          {reply, {Ec, Ee, S}, State}
  end;

handle_call({lookup, Addr}, _From, State=#{cache:=C, bootstrap:=BSN}) ->
  try
    {Code,Res,C1}=ask_where(BSN,
                           naddress:encode(Addr),
                           undefined,
                           fun tp_http:get/2,
                           C
                          ),
     {reply, {Code,Res}, State#{cache=>C1}}
  catch Ec:Ee:S ->
          {reply, {Ec, Ee, S}, State}
  end;

handle_call(_Request, _From, State) ->
    logger:notice("Unknown call ~p",[_Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    logger:notice("Unknown cast ~p",[_Msg]),
    {noreply, State}.

handle_info(ready, State) ->
  try 
    {?MODULE,{CModule, CFun, CArgs}}=lists:keyfind(?MODULE,1,application:get_env(on_ready)),
    erlang:spawn(CModule,CFun,CArgs),
    {noreply,State}
  catch
    _Ec:_Ee ->
      {noreply,State}
  end;

handle_info(_Info, State) ->
    logger:notice("Unknown info  ~p",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ask_where(Nodes, <<_:64>>=Address) ->
  ask_where(Nodes, naddress:encode(Address), fun tp_http:get/2);

ask_where(Nodes, Address) ->
  ask_where(Nodes,Address, fun tp_http:get/2).

ask_where([Node|Rest],Address, GetF) ->
ask_where([Node|Rest],Address, undefined, GetF).

ask_where([],_Address,_,_) ->
  {error,not_found};

ask_where([{_,#{<<"host">>:=_}}|_]=All,Address,Chain,GetF) ->
  All1=lists:merge(
         lists:map(
         fun({_,#{<<"host">>:=L}}) ->
             L;
            (Other) when is_binary(Other) ->
             [Other];
            ([N|_]=Other) when is_integer(N) ->
             [Other]
         end, All)
        ),
  ask_where(
    lists:sort(
      fun(A,B) -> A>B end,
      All1
     ),
    Address,
    Chain,
    GetF
   );

ask_where([Node|Rest],Address, Chain, GetF) ->
  EP=["/api/where/",Address],
  logger:info("Ask ~s @ ~p~n",[EP,Node]),
  case GetF(Node, EP) of
    {200, _, JSON} ->
      case jsx:decode(JSON,[return_maps]) of
        #{<<"ok">>:=true,
          <<"result">>:=<<"found">>,
          <<"chain_nodes">> := Nodes} ->
          {ok,
           lists:sort(
                  fun(_,_) -> rand:uniform()>0.5 end,
                  maps:to_list(Nodes)
               )
          };
        #{<<"ok">>:=true,
          <<"result">>:=<<"other_chain">>,
          <<"chain">>:=Chain1,
          <<"chain_nodes">> := Nodes} ->
          S=maps:size(Nodes),
          if(S>0 orelse Rest==[]) ->
              Lst=lists:sort(
                  fun(_,_) -> rand:uniform()>0.5 end,
                  maps:to_list(Nodes)
                 ),
              logger:info("Address ~s found ~w nodes on other chain ~w~n",
                          [Address, length(Lst),Chain1]),
              ask_where(Lst, Address, Chain1, GetF);
            true ->
              ask_where(Rest, Address, Chain, GetF)
          end
      end;
    {404, _, _} when Rest==[] ->
      {error,not_exists};
    {404, _, _} ->
      ask_where(Rest, Address, Chain, GetF);
    _Other ->
      io:format("~p~n",[_Other]),
      ask_where(Rest, Address, Chain, GetF)
  end.

%% --- [ Cache version ] ---

ask_where([{_,#{<<"host">>:=_}}|_]=All,Address,Chain,GetF,Cache) ->
  All1=lists:merge(
         lists:map(
           fun({_,#{<<"host">>:=L}}) ->
               L;
              (Other) when is_binary(Other) ->
               [Other];
              ([N|_]=Other) when is_integer(N) ->
               [Other]
           end, All)
        ),
  ask_where(
    lists:sort(
      fun(A,B) -> A>B end,
      All1
     ),
    Address, Chain, GetF, Cache
   );

ask_where(Nodes,Address, Chain, GetF, Cache) ->
  if Chain==undefined ->
       case maps:get(Address,Cache,undefined) of
         {chain, No} ->
           ask_where(Nodes, Address, No, GetF, Cache);
         undefined ->
           ask_where_p(Nodes,Address, Chain, GetF, Cache)
       end;
     Chain==get ->
       case maps:get(Address,Cache,undefined) of
         {chain, No} ->
           {ok, No, Cache};
         undefined ->
           ask_where_p(Nodes,Address, Chain, GetF, Cache)
       end;
     is_integer(Chain) ->
       case maps:get({chain,Chain},Cache,undefined) of
         {Data, true} ->
           {ok, lists:sort(
                  fun(_,_) -> rand:uniform()>0.5 end,
                  Data
                 ), Cache};
         {Data, false} ->
           {ok, lists:sort(
                  fun(_,_) -> rand:uniform()>0.5 end,
                  Data
                 ), Cache};
         undefined ->
           ask_where_p(Nodes,Address, Chain, GetF, Cache)
       end
  end.

ask_where_p([Node|Rest],Address, Chain, GetF, Cache) ->
  EP=["/api/where/",Address],
  logger:info("Ask ~s @ ~p~n",[EP,Node]),
  case GetF(Node, EP) of
    {200, _, JSON} ->
      case jsx:decode(JSON,[return_maps]) of
        #{<<"ok">>:=true,
          <<"chain">>:=Chain1,
          <<"result">>:=<<"found">>,
          <<"chain_nodes">> := Nodes} ->
          Lst=lists:sort(
                fun(_,_) -> rand:uniform()>0.5 end,
                maps:to_list(Nodes)
               ),
          Acc1=maps:put(Address,{chain,Chain1},Cache),
          Acc2=maps:put({chain,Chain1},{Lst,true},Acc1),
          if Chain==get ->
               {ok, Chain1, Acc2};
             true ->
               {ok, Lst, Acc2}
          end;
        #{<<"ok">>:=true,
          <<"result">>:=<<"other_chain">>,
          <<"chain">>:=Chain1,
          <<"chain_nodes">> := Nodes} ->
          S=maps:size(Nodes),
          if(S>0 orelse Rest==[]) ->
              Lst=lists:sort(
                    fun(_,_) -> rand:uniform()>0.5 end,
                    maps:to_list(Nodes)
                   ),
              logger:info("Address ~s found ~w nodes on other chain ~w~n",
                          [Address, length(Lst),Chain1]),
              Acc1=maps:put(Address,{chain,Chain1},Cache),
              Acc2=if Lst==[] ->
                        Acc1;
                      true ->
                        maps:put({chain,Chain1},{Lst,false},Acc1)
                   end,
              if Chain==get ->
                   {ok, Chain1, Acc2};
                 true ->
                   case maps:is_key({chain,Chain},Cache) of
                     true ->
                       {N1,_} = maps:get({chain,Chain},Cache),
                       {ok, N1, Acc2};
                     false ->
                       ask_where(Lst, Address, Chain1, GetF, Acc2)
                   end
              end;
            true ->
              ask_where(Rest, Address, Chain, GetF, Cache)
          end
      end;
    {404, _, _} when Rest==[] ->
      {error,not_exists,Cache};
    {404, _, _} ->
      ask_where(Rest, Address, Chain, GetF, Cache);
    _Other ->
      ask_where(Rest, Address, Chain, GetF, Cache)
  end.

