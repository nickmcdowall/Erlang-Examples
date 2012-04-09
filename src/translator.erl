-module(translator).
-export([translate/2]).


translate(Word, Language) ->
	case Language of
		'Spanish' -> spanish(Word)
	end.
	
spanish(Word) ->
	case Word of
		'friend' -> 'amigo'
	end.