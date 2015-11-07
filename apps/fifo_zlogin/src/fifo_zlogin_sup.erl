%%%-------------------------------------------------------------------
%% @doc fifo_zlogin top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('fifo_zlogin_sup').

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(UUID, Type) ->
    supervisor:start_child(?SERVER, [UUID, Type]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Element = {zlogin_fsm, {zlogin_fsm, start_link, []},
               transient, infinity, worker, [zlogin_fsm]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
