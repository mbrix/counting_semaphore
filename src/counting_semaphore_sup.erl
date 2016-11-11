-module(counting_semaphore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% External
-export([add_semaphores/1,
		 add_semaphore/2]).

-define(CHILD(Id, Mod, Type, Args),
		#{id => Id,
		  start => {Mod, start_link, Args},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => [Mod]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 1, 2}, []} }.

%%====================================================================
%% External functions
%%====================================================================
%%

add_semaphore(Name, Count) ->
	supervisor:start_child(?MODULE, new_semaphore(Name, Count)).

add_semaphores([]) -> ok;
add_semaphores([{N,Count}|T]) ->
	add_semaphore(N, Count),
	add_semaphores(T).

%%====================================================================
%% Internal functions
%%====================================================================
%%

new_semaphore(Name, Count) ->
	?CHILD(list_to_atom(atom_to_list(Name) ++ "_id"),
		   semaphore, worker, [Name, Count]).
