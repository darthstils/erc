%%%-------------------------------------------------------------------
%%% @author ilyastepanov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2017 11:01 AM
%%%-------------------------------------------------------------------
-module(erc_client).

-behaviour(gen_server).

-define(HEADER, "*~s\r\n").
-define(WRAPP,  "$~s\r\n~s\r\n").

-define(DELEMITER, <<"\r\n">>).

-define(STRING,   16#2b).
-define(ARRAY,    16#2a).
-define(ERROR,    16#2d).
-define(BULK,     16#24).
-define(INT,      16#3a).
-define(NEGATIVE, 16#2d).
-define(ONE,      16#31).

%% API
-export([
  start_link/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([
  state/0,
  connect/0,
  execute/1
]).

-record(state, {
  config = #{
     host     => "localhost",
     port     => 6379,
     database => 0,
     password => "",
     name     => client
  },
  connected = false,
  sock = null,
  timeout = 5000,
  queue = []
}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
execute(Data) -> gen_server:call(?MODULE,{execute,Data}).
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
state() -> gen_server:call(?MODULE,state).
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
connect() -> ?MODULE ! connect.
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) -> {ok, #state{}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({execute,Data},From,State) ->
  gen_tcp:send(State#state.sock,raw_package(length(Data),Data)),
  {noreply,State#state{ queue = [From] ++ State#state.queue}};
handle_call(state,_From, State) -> {reply,State,State};
handle_call(_Request, _From, State) -> {reply, ok, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(connect,#state{config = Config, connected = false, sock = null} = State) ->
  {noreply,connect(gen_tcp:connect(
    maps:get(host,Config),
    maps:get(port,Config),
    [binary,{packet,0}]),
  State)};
handle_info({tcp_closed,Sock},#state{sock = Sock,connected = true,timeout = Timeout} = State) when Timeout > 0 ->
  timer:apply_after(Timeout,?MODULE,connect,[]),
  {noreply,State#state{sock = null, connected = false}};
handle_info({tcp,Sock,Message},#state{sock = Sock,connected = true, queue = [Last|Rest]} = State) ->
  gen_server:reply(Last,parse_package(Message)),
  {noreply,State#state{queue = Rest}};
handle_info({tcp_closed,Sock},#state{sock = Sock,connected = true,timeout = Timeout} = State) ->
  timer:apply_after(Timeout,?MODULE,connect,[]),
  {noreply,State#state{sock = null, connected = false}};
handle_info(_Info, State) -> {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) -> ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
connect({ok,Sock},State) ->
  gen_tcp:controlling_process(Sock,erlang:whereis(?MODULE)),
  State#state{connected = true, sock = Sock};
connect(_,#state{timeout = Timeout} = State) when Timeout > 0 ->
  timer:apply_after(Timeout,?MODULE,connect,[]),
  State#state{connected = false, sock = null};
connect(_,State) -> State#state{connected = false, sock = null}.
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
raw_package(Length,Data) ->
  lists:foldl(fun(X,Raw) ->
    Wrap = iolist_to_binary(io_lib:format(?WRAPP,[to_bin(byte_size(to_bin(X))),to_bin(X)])),
    <<Raw/binary,Wrap/binary>>
  end,to_bin(io_lib:format(?HEADER,[to_bin(Length)])),Data).
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
parse_package(<<?BULK:8,?NEGATIVE:8,?ONE:8,_/binary>>)   -> {ok,undefined};
parse_package(<<?BULK:8,Rest/binary>>)                   -> {ok,binary:split(Rest,[?DELEMITER],[global,trim_all])};
parse_package(<<?STRING:8,Rest/binary>>)                 -> {ok,binary:split(Rest,[?DELEMITER],[global,trim_all])};
parse_package(<<?ERROR:8,Rest/binary>>)                  -> {error,binary:split(Rest,[?DELEMITER],[global,trim_all])}.
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
to_bin(X) when is_list(X)     -> list_to_binary(X);
to_bin(X) when is_atom(X)     -> atom_to_binary(X,latin1);
to_bin(X) when is_float(X)    -> io_lib:format("~.2f",[X]);
to_bin(X) when is_binary(X)   -> X;
to_bin(X) when is_integer(X)  -> integer_to_binary(X);
to_bin(_) -> <<>>.
