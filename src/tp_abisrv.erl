%%%-------------------------------------------------------------------
%% @doc tp_abisrv gen_server
%% @end
%%%-------------------------------------------------------------------
-module(tp_abisrv).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2023-03-03").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,find_all/1,get/1,known/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Path) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Path], []).

get(Addr) ->
  gen_server:call(?SERVER, {get, Addr}).

known() ->
  gen_server:call(?SERVER, known).

find_all(Path) ->
  ABIs= filelib:wildcard(filename:join(Path,"*.abi")),
  lists:filtermap(
    fun(N) ->
        case string:split((filename:basename(N)),".") of
          [Addr,"abi"] ->
            try
              {true,
               { naddress:decode(Addr), tp_abi:parse_abifile(N) }
              }
            catch throw:bad_addr ->
                    false
            end;
          _ -> false
        end
    end,
    ABIs).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Path) ->
  self() ! load_abi,
  {ok, #{abi=>#{},path=>Path}}.

handle_call({get,Addr}, _From, #{abi:=ABIs}=State) ->
  case maps:get(Addr,ABIs,undefined) of
    undefined ->
      try
        Name=filename:join(code:priv_dir(tpns),[binary_to_list(naddress:encode(Addr)),".abi"]),
        logger:info("Loading ABI from ~p",[Name]),
        New=tp_abi:parse_abifile(Name),
        {reply, New, State#{abi=>maps:put(Addr,New,ABIs)}}
      catch error:Ee:S ->
              Ec=error,
              logger:notice("Failed to load ABI for ~p: ~p:~p @~p",[Addr,Ec,Ee,hd(S)]),
              {reply, {error, not_found}, State}
      end;
    Cached ->
      {reply, Cached, State}
  end;

handle_call(known, _From, #{abi:=ABIs}=State) ->
  {reply, maps:keys(ABIs), State};

handle_call(_Request, _From, State) ->
  logger:notice("Unknown call ~p",[_Request]),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  logger:notice("Unknown cast ~p",[_Msg]),
  {noreply, State}.

handle_info(load_abi, #{path:=Path}=State) ->
  {noreply, State#{abi=>maps:from_list(tp_abisrv:find_all(Path))}};

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

