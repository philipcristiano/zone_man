%%%-------------------------------------------------------------------
%%% @author philipcristiano
%%% @copyright 2018 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(zone_man_master).
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

-export([list/0,
         stop/0,
         create/1]).

-record(state, {}).

-define(TABLE, zone_registry).
-record(zone_record, {name, spec, desired_state}).

%%%===================================================================
%%% API
%%%===================================================================

list() ->
    gen_server:call(?MODULE, {list}).

stop() ->
    gen_server:call(?MODULE, {stop}).

create(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {create, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FileBase) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [FileBase], []).

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
init([FileBase]) ->
    Filename = filename:join([FileBase, "dets", "zone_registry"]),
    ok = lager:info("Zone registry file ~p", [Filename]),
    ok = filelib:ensure_dir(Filename),
    {ok, _Name} = dets:open_file(?TABLE, [{type, set},
                                          {keypos, #zone_record.name},
                                          {file, Filename}]),
    {ok, #state{}}.

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
handle_call({list}, _From, State) ->
    % Zones = zone_man_cmd:list_zones(),
    Zones = all_zone_names(),
    {reply, {ok, Zones}, State};
handle_call({stop}, _From, State) ->
    {stop, normal, State};
handle_call({create, Name}, _From, State) ->
    Spec = #{name => Name},
    Record =  #zone_record{name = Name, spec=#{}, desired_state=running},
    ok = dets:insert(?TABLE, Record),
    {ok, _Child} = zone_man_manager_sup:start(Spec),
    {reply, {ok}, State};
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

all_zone_names() ->
    Key = dets:first(?TABLE),
    all_zone_names(Key, []).

all_zone_names('$end_of_table', L) ->
    L;
all_zone_names(Key, L) ->
    lager:info("test: ~p", [Key]),
    NewL = [Key] ++ L,
    Next = dets:next(?TABLE, Key),
    all_zone_names(Next, NewL).
