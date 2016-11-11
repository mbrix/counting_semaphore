%%%-------------------------------------------------------------------
%% @doc counting_semaphore public API
%% @end
%%%-------------------------------------------------------------------

-module(counting_semaphore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	{ok, Pid} = counting_semaphore_sup:start_link(),
	{ok, Groups} = application:get_env(groups),
	counting_semaphore_sup:add_semaphores(Groups),
	{ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


