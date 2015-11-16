%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(zlogin_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, start/2, send/2, stop/1,
         %%initialize/2, initialize/3,
         stopped/2, stopped/3,
         connected/2, connected/3,
         kvm/2, kvm/3,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-ignore_xref([
              stopped/2, stopped/3,
              connected/2, connected/3,
              kvm/2, kvm/3,
              stop/1, send/2, start/2, start_link/2
             ]).

-define(SERVER, ?MODULE).
-define(TICK, 1000).

-record(state, {uuid, console, type = undefined,
                listeners = []}).

-define(VMADM, "/usr/sbin/vmadm").
-define(ZLOGIN, "/usr/sbin/zlogin").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(UUID, Type) ->
    gen_fsm:start_link({global, {zlogin, UUID}}, ?MODULE, [UUID, Type], []).

start(UUID, Type) ->
    fifo_zlogin_sup:start_child(UUID, Type).

send(UUID, Data) ->
    gen_fsm:send_event({global, {zlogin, UUID}}, {send, Data}).

stop(UUID) ->
    gen_fsm:send_all_state_event({global, {zlogin, UUID}}, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

tick() ->
    gen_fsm:send_event_after(1000, tick).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([UUID, kvm]) ->
    {ok, kvm, #state{uuid = UUID}, 0};

init([UUID, docker]) ->
    tick(),
    process_flag(trap_exit, true),
    {ok, stopped, #state{uuid = UUID, type = docker}};

init([UUID, Type]) ->
    tick(),
    process_flag(trap_exit, true),
    {ok, stopped, #state{uuid = UUID, type = Type}}.

kvm(_, State) ->
    {ok, kvm, State}.

kvm(_, _From, State) ->
    {ok, {error, kvm}, kvm, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
stopped(tick, State = #state{uuid = UUID}) ->
    lager:info("[~s] tick", [UUID]),
    ConsolePort = open_zlogin(State),
    receive
        {'EXIT', ConsolePort, _PosixCode} ->
            tick(),
            {next_state, stopped, State}
    after
        200 ->
            remove_lock(UUID),
            State1  = State#state{console = ConsolePort},
            {next_state, connected, State1}
    end;

stopped({send, Data}, State = #state{uuid = UUID}) ->
    lager:info("[~s] ! < ~s", [UUID, Data]),
    {next_state, stopped, State}.


connected({send, Data}, State = #state{uuid = UUID, console = C}) ->
    lager:info("[~s] < ~s", [UUID, Data]),
    port_command(C, Data),
    {next_state, connected, State};

connected(_, State) ->
    {next_state, connected, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
stopped(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, stopped, State}.

connected(_, _From, State) ->
    {next_state, connected, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

handle_event({subscribe, Pid}, StateName, State = #state{listeners = Ls}) ->
    Ref = erlang:monitor(process, Pid),
    {next_state, StateName, State#state{listeners = [{Ref, Pid} | Ls]}};

handle_event(stop, _StateName, State) ->
    {stop, normal, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

handle_info({_C,{exit_status, PosixCode}}, connected,
            State = #state{console = _C}) ->
    lager:warning("[console:~s] Exited with status ~p but vm in stopped.",
                  [State#state.uuid, PosixCode]),
    {next_state, stopped, State#state{console = undefined}};

handle_info({'EXIT', _C, PosixCode}, connected,
            State = #state{console = _C}) ->
    lager:warning("[console:~s] Exited with status ~p but vm in stopped.",
                  [State#state.uuid, PosixCode]),
    {next_state, stopped, State#state{console = undefined}};

handle_info({C, {data, Data}}, connected,
            State = #state{uuid = UUID, console = C}) ->
    lager:info("[~s] ~p", [UUID, Data]),
    relay(Data, State),
    {next_state, connected, State};

handle_info({'DOWN', Ref, _, _, _}, StateName,
            State = #state{listeners = Ls}) ->
    Ls1 = lists:keydelete(Ref, 1, Ls),
    {next_state, StateName, State#state{listeners = Ls1}};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, connected, #state{console = C}) ->
    incinerate(C),
    ok;

terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

remove_lock(UUID) ->
    File = mktemp(),
    Update = [{<<"remove_internal_metadata">>, [<<"docker:wait_for_attach">>]}],
    file:write_file(File, jsx:encode(Update)),
    case fifo_cmd:run(?VMADM, ["update", UUID, {f, File}]) of
        {ok, _} ->
            ok = file:delete(File),
            ok;
        E ->
            file:delete(File),
            E
    end.

mktemp() ->
    lib:nonl(os:cmd("mktemp")).

open_zlogin(S) ->
    Args = zlogin_args(S),
    open_port({spawn_executable, runpty()},
              [use_stdio, binary, stderr_to_stdout, {args, Args}, exit_status]).

zlogin_args(#state{uuid = UUID, type = docker}) ->
    [?ZLOGIN, "-Q", "-I", UUID];
zlogin_args(#state{uuid = UUID}) ->
    [?ZLOGIN, "-Q", UUID].


runpty() ->
    code:priv_dir(fifo_zlogin) ++ "/runpty".


incinerate(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            port_close(Port),
            lager:warning("Killing ~p with -9", [OsPid]),
            os:cmd(io_lib:format("/usr/bin/kill -9 ~p", [OsPid]));
        _ ->
            ok
    end.

%% init_console(State = #state{uuid = UUID, zone_type = docker}) ->
%%     case State#state.console of
%%         undefined ->
%%             [{_, Name, _, _, _, _}] = chunter_zone:get_raw(State#state.uuid),
%%             %%Console = code:priv_dir(chunter) ++ "/runpty /usr/sbin/zlogin -I " ++ binary_to_list(Name),
%%             Console = "/usr/sbin/zlogin -Q -I " ++ binary_to_list(Name),
%%             %% This is a bit of a hack
%%             %% https://github.com/joyent/sdc-cn-agent/blob/4efd72f2dda2a6daceb51a0cb84d0b06d0bc011d/lib/update-wait-flag.js
%%             %% explains why we need it
%%             ConsolePort = open_port({spawn, Console},
%%                                     [use_stdio, binary, stderr_to_stdout]),
%%             %% We make sure that the console actually survuves for 200 ms to
%%             %% prevent the attach to 'succeed' but not succeed
%%             receive
%%                 {'EXIT', ConsolePort, _PosixCode} ->
%%                     State
%%             after
%%                 200 ->
%%                     chunter_vmadm:update(UUID, [{<<"remove_internal_metadata">>, [<<"docker:wait_for_attach">>]}]),
%%                     State#state{console = ConsolePort}
%%             end;
%%         _ ->
%%             State
%%     end;

%% init_console(State) ->
%%     case State#state.console of
%%         undefined ->
%%             [{_, Name, _, _, _, _}] = chunter_zone:get_raw(State#state.uuid),
%%             Console = code:priv_dir(chunter) ++ "/runpty /usr/sbin/zlogin -Q " ++ binary_to_list(Name),
%%             ConsolePort = open_port({spawn, Console}, [binary]),
%%             State#state{console = ConsolePort};
%%         _ ->
%%             State
%%     end.


relay(Msg, #state{listeners = Ls}) ->
    [L ! {data, Msg} || {_, L} <- Ls].
