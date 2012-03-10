-module(pricing_service).
-export([register/1, message_receiver/0, handle/1]).
-define(VERSION, '1.10').

register(ClientPid) ->
	register(client, ClientPid),
	spawn_link(?MODULE, message_receiver, []).

message_receiver() ->
	receive
		Request 	-> ?MODULE:handle(Request)
	end,
	?MODULE:message_receiver().

handle(Request) -> 
	case Request of 
		version 		-> reply_with_version();
		{price, Item} 	-> reply_with_price_of(Item)
	end.

reply_with_version() ->
	client ! io:format("Service Version:[~p]~n",[?VERSION]).

reply_with_price_of(Item) ->
	client ! io:format("The price of ~p is: $~p ~n", [Item, price(Item)]).

price(Item) -> 
	case Item of
		tea 	-> 2.05;
		coffee 	-> 2.10;
		milk 	-> 0.97;
		bread 	-> 0.50
	end.