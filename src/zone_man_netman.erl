%%%-------------------------------------------------------------------
%%% @author philipcristiano
%%% @copyright 2018 philipcristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(zone_man_netman).
-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
  check_config/0,
  ensure_vnic/3]).

-record(state, {nicconfig}).

%%%===================================================================
%%% API
%%%===================================================================


check_config() ->
    NicConfig = application:get_env(zone_man, nic_groups),
    case NicConfig of
        undefined -> throw("`nic_groups` not defined");
        _ -> ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(NicConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [NicConfig], []).

ensure_vnic(Type, Name, Opts) ->
    gen_server:call(?MODULE, {vnic, Type, Name, Opts}).


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
init([NicConfig]) ->
    {ok, #state{nicconfig = NicConfig}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({vnic, Type, Name, Opts}, _From,
            State = #state{nicconfig=NicConfig}) ->
    lager:info("Ensure vnic ~p", [{Type, Name, Opts}]),
    LinkName = proplists:get_value(Type, NicConfig),
    LName = binary:bin_to_list(Name),

    SystemVnic = zone_man_cmd:get_vnic(LName),
    case SystemVnic of
        undefined -> zone_man_cmd:create_vnic(LinkName, LName);
        _ -> lager:debug("VNIC ~p already exists", [LName])
    end,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
