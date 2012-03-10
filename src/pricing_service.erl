-module(pricing_service).
-export([register/1, message_receiver/0, handle/1]).

register(ClientPid) ->
	register(client, ClientPid),
	spawn_link(?MODULE, message_receiver, []).

message_receiver() ->
	receive
		Request -> ?MODULE:handle(Request),
		message_receiver()
	end.

handle({price, Item}) -> reply_with_price_of(Item).	

reply_with_price_of(Item) ->
	client ! io:format("The price of ~p is: $~p~n", [Item, price(Item)]).

price(Item) -> 
	case Item of
		tea 	-> 2.39;
		coffee 	-> 2.63;
		milk 	-> 0.97;
		bread 	-> 0.50
	end.