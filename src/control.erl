-module(control).

%% API
-export([go/2]).

% N is the number of ring processes, N >= 1
% M is the range of targets
% flush the mailbox to erase obsolete info,
% creates the worker ring and starts the game
%
go(N, M) -> flush_mailbox(),
  TargetList = generate(M),
  io:format("Targets ~p~n", [TargetList]),
  Workers = worker_ring(N, self()),
  [FirstWorker|_] = Workers,
  io:format("Workers ~p~n", [Workers]),
  FirstWorker ! token,
  ResultList = controlgame(TargetList, []),
  io:format("~w~n", [ResultList]),
  FirstWorker ! stop.

%generates a list of M random numbers in rtrange 1..M
generate(0) -> [];
generate(M) -> [rand:uniform(M)|generate(M-1)].

controlgame([], ResultList) -> ResultList;
controlgame(TargetList, ResultList) ->
  receive
    {Pid, eat} ->
      io:format("~p eats~n", [Pid]),
      [Target|Rest] = TargetList,
      controlgame(Rest, [{Pid, Target}|ResultList]);
    token ->
      [{Pid, _}|_] = lists:reverse(ResultList),
      Pid ! token,
      controlgame(TargetList, ResultList)
  end.

flush_mailbox() ->
  receive
    _Any -> flush_mailbox()
  after 0 -> ok
end.

% sets up a ring of N workers, each one running worke_node function from worker module
worker_ring(0, _PidControl) -> [];
worker_ring(1, PidControl) ->
  Pid = spawn(worker, worker_node, [PidControl]),
  Pid ! PidControl,
  [Pid];
worker_ring(N, PidControl) ->
  Workers = worker_ring(N-1, PidControl),
  [NextWorker|_] = Workers,
  WorkerPid = spawn(worker, worker_node, [PidControl]),
  WorkerPid ! NextWorker,
  [WorkerPid|Workers].