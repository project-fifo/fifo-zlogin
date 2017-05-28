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

check() ->
    gen_fsm:send_event_after(60000, check).

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
    {ok, kvm, #state{uuid = UUID}};

init([UUID, docker]) ->
    lager:info("[zlogin:~s] Starting docker zlogin.", [UUID]),
    tick(),
    check(),
    process_flag(trap_exit, true),
    {ok, stopped, #state{uuid = UUID, type = docker}};

init([UUID, jail]) ->
    lager:info("[zlogin:~s] Starting jail login.", [UUID]),
    tick(),
    check(),
    process_flag(trap_exit, true),
    {ok, stopped, #state{uuid = UUID, type = jail}};

init([UUID, Type]) ->
    tick(),
    check(),
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

stopped(tick, State = #state{uuid = UUID, type = jail}) ->
    case check_jail(UUID) of
        running ->
            ConsolePort = open_jail(State),
            receive
                {'EXIT', ConsolePort, _PosixCode} ->
                    tick(),
                    {next_state, stopped, State};
                {_C, {exit_status, _PosixCode}} ->
                    tick(),
                    {next_state, stopped, State}
            after
                500 ->
                    lager:info("[zlogin:~s] connected.", [UUID]),
                    State1  = State#state{console = ConsolePort},
                    {next_state, connected, State1}
            end;
        _ ->
            tick(),
            {next_state, stopped, State}
    end;

stopped(tick, State = #state{uuid = UUID}) ->
    case check_state(UUID) of
        running ->
            ConsolePort = open_zlogin(State),
            receive
                {'EXIT', ConsolePort, _PosixCode} ->
                    tick(),
                    {next_state, stopped, State};
                {_C, {exit_status, _PosixCode}} ->
                    tick(),
                    {next_state, stopped, State}
            after
                500 ->
                    lager:info("[zlogin:~s] connected.", [UUID]),
                    remove_lock(UUID),
                    State1  = State#state{console = ConsolePort},
                    {next_state, connected, State1}
            end;
        _ ->
            tick(),
            {next_state, stopped, State}
    end;

stopped(check, State = #state{uuid = UUID, type = jail}) ->
    eval_check(check_jail(UUID), State);

stopped(check, State = #state{uuid = UUID}) ->
    eval_check(check_state(UUID), State);

stopped({send, Data}, State = #state{uuid = UUID}) ->
    lager:info("[~s] ! < ~s", [UUID, Data]),
    {next_state, stopped, State}.


connected({send, Data}, State = #state{uuid = UUID, console = C}) ->
    lager:info("[zlogin:~s] < ~s", [UUID, Data]),
    port_command(C, Data),
    {next_state, connected, State};

connected(_, State) ->
    {next_state, connected, State}.

eval_check(not_found, State = #state{ uuid = UUID}) ->
    lager:info("[zlogin:~s] shutting down since check failed.", [UUID]),
    {stop, normal, State};
eval_check(_, State) ->
    check(),
    {next_state, stopped, State}.

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

handle_info({_C, {exit_status, PosixCode}}, connected,
            State = #state{console = _C}) ->
    lager:warning("[console:~s] Exited with status ~p but vm in stopped.",
                  [State#state.uuid, PosixCode]),
    tick(),
    check(),
    {next_state, stopped, State#state{console = undefined}};

handle_info({'EXIT', _C, PosixCode}, connected,
            State = #state{console = _C}) ->
    lager:warning("[console:~s] Exited with status ~p but vm in stopped.",
                  [State#state.uuid, PosixCode]),
    tick(),
    check(),
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
    lager:info("[zlogin:~s] removing docker attach lock.", [UUID]),
    File = mktemp(),
    Update = [{<<"remove_internal_metadata">>, [<<"docker:wait_for_attach">>]}],
    file:write_file(File, jsx:encode(Update)),
    case fifo_cmd:run(?VMADM, ["update", UUID, {f, File}]) of
        {ok, _} ->
            lager:info("[zlogin:~s] docker attach lock removed.", [UUID]),
            ok = file:delete(File),
            ok;
        E ->
            lager:info("[zlogin:~s] failed to delete docker attach lock: ~p.",
                       [UUID, E]),
            file:delete(File),
            E
    end.

mktemp() ->
    lib:nonl(os:cmd("mktemp")).

open_jail(#state{uuid = UUID, type = jail}) ->
    pty(["/usr/local/bin/iocage", "console", UUID]).

open_zlogin(S) ->
    pty(zlogin_args(S)).

pty(Args) ->
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

relay(Msg, #state{listeners = Ls}) ->
    [L ! {data, Msg} || {_, L} <- Ls].

check_state(UUID) ->
    case fifo_cmd:run("/usr/sbin/zoneadm", [{z, UUID}, "list", p]) of
        {ok, Data} ->
            [_, _, State | _] = re:split(Data, ":"),
            case State of
                <<"running">> ->   running;
                <<"installed">> -> stopped;
                _ -> other
            end;
        _ ->
            not_found
    end.
