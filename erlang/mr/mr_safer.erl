%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <kflarsen@diku.dk>
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------
-module(mr_safer).
-export([start/2, stop/1, job/5, status/1]).
-compile(debug_all).

-define(otherwise, true). % Nice little hack.
%%%% Interface

start(N, Name) ->
    This = self(),
    spawn(fun() -> process_flag(trap_exit,true),
		   Red = spawn_link(fun reducer_loop/0),
		   Mps = [spawn_link(fun() -> mapper_loop(Red, id()) end) || _ <- lists:seq(1,N) ],
		   Super = self(),
		   Crd = spawn_link(fun() -> coordinator_loop(Super, Red, Mps) end),
		   This ! {ready, Crd},
		   supervise(Red, Mps, Crd, Name, idle)
	  end),
    receive
	{ready, Crd} ->
	    register(Name, Crd)
    end,
    ok.

status(Name) ->
    rpc(whereis(Name), status).

stop(Name) ->
    rpc(whereis(Name), stop).

job(Name, MapFun, RedFun, RedInit, Data) -> 
    rpc(whereis(Name), {job, MapFun, RedFun, RedInit, Data}).

%%%% Internal implementation


%% init(N) ->
%%     Red = spawn(fun reducer_loop/0),
%%     {Red, [spawn(fun() -> mapper_loop(Red, id()) end) || _ <- lists:seq(1,N) ]}.

%% Function that generates the id function.
id() ->
    fun(X) -> X end.


%% synchronous communication

rpc(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
	{Ref, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).


%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    info(Pid, stop).

data_async(Pid, D) ->
    info(Pid, {data, D}).

setup_async(Pid, Fun) ->
    info(Pid, {setup, Fun}).

reducer_async(Pid, Red) ->
    info(Pid, {reducer, Red}).

%%% Coordinator
supervise(Reducer, Mappers, Cord, CordName, Idle) ->
    receive
	{'EXIT', _, normal } ->
	    supervise(Reducer, Mappers, Cord, CordName, Idle);
	{'EXIT', Pid, Reason } ->
	    if Idle == idle ->
		    if Pid =:= Reducer ->
			    %% Reducer died
			    NewRed = spawn_link(fun reducer_loop/0),
			    lists:foreach(fun(M) -> reducer_async(M, NewRed) end, Mappers),
			    info(Cord, {new_reducer, NewRed}),
			    io:format("Reducer ~p exited because of ~p. Restarted as ~p~n", [Pid, Reason, NewRed]),
			    supervise(NewRed, Mappers, Cord, CordName, Idle);
		       Pid =:= Cord ->
			    NewCord = spawn_link(fun() -> coordinator_loop(self(), Reducer, Mappers) end),
			    register(CordName, NewCord),
			    supervise(Reducer, Mappers, NewCord, CordName, Idle);
		       ?otherwise ->
			    %% Mapper died
			    NewMapper = spawn_link(fun() -> mapper_loop(Reducer, id()) end),
			    NewMappers = [NewMapper|lists:delete(Pid, Mappers)],
			    info(Cord, {new_mappers, NewMappers}),
			    io:format("Mapper ~p exited because of ~p. Restarted as ~p~n", [Pid, Reason, NewMapper]),
			    supervise(Reducer, NewMappers, Cord, CordName, Idle)
		    end;
	       ?otherwise ->
		    exit("Died in a job")
	    end;
	active ->
	    supervise(Reducer, Mappers, Cord, CordName, active);
	idle ->
	    supervise(Reducer, Mappers, Cord, CordName, idle);
	stop ->
	    ok
    end.

coordinator_loop(Supervisor, Reducer, Mappers) ->
    receive
	{From, Ref, stop} ->
	    io:format("~p stopping~n", [self()]),
	    lists:foreach(fun stop_async/1, Mappers),
	    stop_async(Reducer),
	    io:format("Stopping supervisor ~p~n", [Supervisor]),
	    stop_async(Supervisor),
	    From ! {Ref, ok};
	{From, Ref, status} ->
	    io:format("This is MR coordinator ~p managing ~B mappers.~n", [self(), length(Mappers)]),
	    From ! {Ref, ok},
	    coordinator_loop(Supervisor,Reducer, Mappers);
	{From, Ref, {job, MapFun, RedFun, RedInit, Data}} ->
	    info(Supervisor, active),
	    lists:foreach(fun(M) -> setup_async(M, MapFun) end, Mappers),

	    send_data(Mappers, Data),

	    %% Wait for the reducer to return something
	    {ok, Result} = rpc(Reducer, {job, {RedFun, RedInit, length(Data)}}),

	    From ! {Ref, {ok, Result}},				  
	    
	    info(Supervisor, idle),
	    coordinator_loop(Supervisor, Reducer, Mappers);
	{new_reducer, NewR} ->
	    coordinator_loop(Supervisor,NewR, Mappers);
	{new_mappers, NewMs} ->
	    coordinator_loop(Supervisor,Reducer, NewMs)
    end.

send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).


%%% Reducer

%% The idle reducer state
reducer_loop() ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
	{From, Ref, {job, {Fun, Acc, Missing}}} ->
	    io:format("Reducer received new job~n"),
	    From ! {Ref, {ok, gather_data_from_mappers(Fun, Acc, Missing)}},
	    reducer_loop()
    end.

%% Active reducer.
%% Missing is assumed to be the length of the input. This is simple and crude but it works.
gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
	{data, Data} ->
	    NAcc = Fun(Data, Acc),
	    if
		Missing > 1 ->
		    gather_data_from_mappers(Fun, NAcc, Missing-1);
		?otherwise ->
		    io:format("Finished~n"),
		    NAcc
	    end
    after 5000 ->
	    io:format("Timed out - state is now: Acc: ~p, Missing: ~p~n", [Acc, Missing]),
	    Acc
    end.

%%% Mapper

mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
	{reducer, RedFun} ->
	    mapper_loop(RedFun, Fun);
	{data, D} ->
	    data_async(Reducer, Fun(D)),
	    mapper_loop(Reducer, Fun);
	{setup, NewFun} ->
	    io:format("Mapper ~p received new mapper function~n", [self()]),
	    mapper_loop(Reducer, NewFun);
	Unknown ->
	    io:format("unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun)
    end.

