-module(distributed).

% Public API
-export([start/0, status/0]).

-export([incoming/1, supervise/1, supervise/2]).
-define(BOB_NODE, 'bob@localhost').

%% Sets up the connection to remote node and spawns the listeners.
%%
%% send message to bob with: 'global:send(bob, Msg).'
%%
start() ->
	connect_to(?BOB_NODE),
	spawn(?MODULE, supervise, [?BOB_NODE, bob]),
	spawn(?MODULE, supervise, [nick]),
	ok.

supervise(Name) ->
	supervise(node(), Name).
	
supervise(Node, Name) ->
	monitor(process, spawn(Node, ?MODULE, incoming, [Name])),
	log_new_life(Name, Node),
	receive
		{'DOWN', _Ref, _Type, Object, Info} ->
			case Info of
				noconnection ->
					log_node_down();
				_Other ->
					log_man_down(Object, Info),
					supervise(Node, Name)
			end
	end.
			
incoming(Name) ->
	yes = global:re_register_name(Name, self()),
	incoming().
	
incoming() ->
	receive
		stop ->
			erlang:display({stopped, self()}),
			exit(old_age);
		Msg -> 
			erlang:display(Msg),
			incoming()
	end.
	
connect_to(Node) -> 
	pong = net_adm:ping(Node),
	io:format("Successful connection to ~p~n", [Node]).
	
status() ->
	[{Name, global:whereis_name(Name)} || Name <- [nick, bob]].
	
log_new_life(Name, Node) -> 
	io:format("Supervisor: Gave life to ~p on ~p.~n",[Name, Node]).

log_node_down() -> 
	io:format("Supervisor: Node down! Bye.~n").

log_man_down(Object, Info) -> 
	io:format("Supervisor: Man DOWN: [~p,~p]~n",[Object, Info]).
