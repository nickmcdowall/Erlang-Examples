-module(links).
-export([start/1, print_status/0]).
-export([manager/1, worker/1, cleanup/0]).

%
% To trap an exit add: process_flag(trap_exit, true) to the process
% and match on {'EXIT',FromPid,Reason}
%
% alternatively use monitor(process, Pid) to trap {'DOWN', Ref, process, Pid, Reason}
%
start(LinkOption) ->
	cleanup(),
	register(owner, self()),
	create(manager, LinkOption).

create(Func, LinkOption) ->
	case LinkOption of
		with_link -> spawn_link(links, Func, [LinkOption]), ok;
		no_link   -> spawn(links, Func, [LinkOption]), ok
	end.
	
manager(LinkOption) ->
	register(manager, self()),
	create(worker, LinkOption),
	manager().
		
manager() ->
	receive
		report -> 
			io:format("[manager] I had better delegate to worker..~n"),
			worker ! do_work,
			manager();
		look_busy ->
			io:format("[manager] Let's pretend we are busy!~n"),
			worker ! do_filing,
			manager();
		{worker_reply, Report} -> 
			io:format("[manager] I can send now send the report up to the big cheese.~n"),
			owner ! {report, Report},
			manager();
		_ ->
			exit(stress_related)
	end.
	
worker(_LinkOption) ->
	register(worker, self()),
	print_status(),
	worker().

worker() ->
	receive
		do_work -> 
			io:format("[worker] I love working hard!~n"),
			manager ! {worker_reply, some_report},
			worker();
		do_filing ->
			io:format("[worker] This is not what I signed up for, I quit!~n"),
			exit(boredom)
	end.

	
print_status() -> 
	io:format("~p~n", [{{worker, whereis(worker)}, {manager, whereis(manager)}, {owner, whereis(owner)}}]).

cleanup() -> 
	[ unregister(Name) || Name <- [worker, manager, owner], whereis(Name) =/= undefined ].
	
	

