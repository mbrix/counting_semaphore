-module(semaphore).

%% Simple semaphore module
%% Each Pid is entitled to 1 semaphore of each type spawned
%% Pids release semaphores automatically on exit
%% Will return true on spurious acquire, i.e if the pid already holds the semaphore

-behaviour(gen_server).

%% API functions
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([acquire/1,
		 release/1]).

-record(state, {count, monitors=#{}}).

start_link(Name, Count) ->
    gen_server:start_link({local, Name}, ?MODULE, Count, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Count) -> 
	{ok, #state{count = Count}}.

handle_call(acquire, _From, #state{count=0}=S) ->
	{reply, false, S};

handle_call(acquire, From, #state{count=C,
								  monitors=M}=S) ->
	{Pid, _Ref} = From,
	case maps:find(Pid, M) of
		{ok, _MRef} ->
			%% This process already has acquired this semaphore
			{reply, true, S};
		error ->
			MRef = erlang:monitor(process, Pid), 
	{reply, true, S#state{count=C-1,
						  monitors=maps:put(Pid, MRef, M)}}
	end;

handle_call(release, From, #state{count=C,
								   monitors=M}=S) ->
	{Pid, _Ref} = From,
	case maps:find(Pid, M) of
		{ok, MRef} ->
			%% This process previously acquired this semaphore
			erlang:demonitor(MRef),
			{reply, true, S#state{count=C+1,
								  monitors=maps:remove(Pid, M)}};
		error ->
			%% This process can't release something they haven't acquired
			{reply, false, S}
	end;

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'DOWN', _Ref, process, Pid, _Reason},
			#state{count=C,
				   monitors=M}=S) ->
	%% Remove this from the monitor map and increment the semaphore
	{noreply, S#state{count=C+1,
					  monitors=maps:remove(Pid, M)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Exernal functions
%%%===================================================================

acquire(Name) -> gen_server:call(Name, acquire).
release(Name) -> gen_server:call(Name, release).

%%%===================================================================
%%% Internal functions
%%%===================================================================



