
#Erlang - Hot Code Swapping#
Wouldn't it be great to be able to upgrade your code or fix bugs without having to bring the server or system down?

Well Erlang let's you do this by hot swapping your code - which is just one of the ways that the language lets you build highly available systems with minimal down-time.  

So how does it work?  Good question, I had the same question so I started experimenting and came up with a simple idea:- 
I would create a process that I could send simple messages to; then make a couple small changes and compile the code then see if the updates are loaded the next time I communicate with the process.

Below is the first version of the code

### A Service is born ###

<script src="https://gist.github.com/2031699.js?file=message_receiver.erl"></script>

Notice that the pricing_service module allows a client to register a process id.  This is basically like giving the service an email or postal address so that it knows where to reply to.

So to start the process I run the following commands in the Erlang shell after navigating to the directory containing the pricing_service.erl file:

<pre>
	<code>
		C:\Dev\Erlang-Examples\src>erl
		 Eshell V5.9  (abort with ^G)
		1> c(pricing_service).
		 {ok,pricing_service}
		2> Service = pricing_service:register(self()).
		 <0.37.0>
		3> Service ! {price, milk}.
		 The price of milk is: $0.99
		 {price,milk}
	</code>
</pre>

So far so good - the _Service_ variable holds the service process id for me so that I can start sending messages to it and when I register I pass in the shell process id (_self()_) so that I get the responses in the shell.  
On line 3 I send the service a message in the format it understands to ask for the price of milk.

Next up I update the price of milk to $1.03 and recompile the file before asking for the price of milk for a second time:

<pre>
	<code>
		4> c(pricing_service).
		 {ok,pricing_service}
		5> Service ! {price, milk}.
		 The price of milk is: $0.99
		 {price,milk}
	</code>
</pre>

Mmm a bit odd... I get the same price as before.. not what I was expecting but after a bit of research I found out that if I specify the module name before a function call the VM will use the latest version available for that module:

So I add the module name as a prefix when looping back to the _message\_receiver_ function and in front of the _reply\_with\_price\_of_ function [now I have to export the function too since it is being treated like a public function by including the module name prefix]:

<script src="https://gist.github.com/2031997.js?file=gistfile1.erl"></script>

Now when I update the price and compile the code I get the change hot swapped in for me.. but what if I want to handle more request types to the service, 
for example a request of the version the service is running on.  
Then I would need to add the module name as a prefix to all new function calls [and export the functions too even though I don't really want to expose all my internal functions!] - 
things could soon start getting messy so lets refactor the code and add the ability to return the ?VERSION macro that is defined at the top of the file:

<script src="https://gist.github.com/2011970.js?file=message_receiver.erl"></script>

Now only the _handle_ and _message\_receiver_ functions need to be prefixed with the module name since all message handling is delegated to the handle method so the new code will be triggered by the time any further functions are invoked. 

*Note*: that Erlang has a built in macro _?MODULE_ which can be used instead of the module name - this would save you from having to update the prefixes if you ever change the module name.

*Note*: that the Erlang VM will only allow two versions of the module to be loaded at any one time - so if you try compiling the module twice while the process is running you will find that the process gets killed unless it gets updated between the compilations since it would be running on an old version which gets replaced by the two new versions you have since compiled and loaded..  I plan to explore this further in the near future including looking at using timeouts to refresh the code automatically to avoid this scenario.

 
