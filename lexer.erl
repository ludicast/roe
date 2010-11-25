-module(lexer).

-export([get_token_list/1, token_for_char/1, get_tokens/1, error_at/1]).

-include_lib("eunit/include/eunit.hrl").

error_at(Msg) ->
	io:format("error of ~p.~n", [Msg]),
	throw({ error, unparsable_unit, Msg}).
	
token_for_char(Char) ->
	case Char of 
		45 -> {binop, minus};
		43 -> {binop, plus};
		42 -> {binop, times};
		41 -> close_paren;
		40 -> open_paren;
		126 -> unary_minus;
		61 -> equals;
		_ -> { nomatch, Char }
	end.

get_string_token(String) ->
	case String of
		"if" -> if_token;
		"else" -> else_token;
		"then" -> then_token;
		"let" -> let_token;
		"in" -> in_token;
		_ -> {string, String}											
	end.

pull_string([], String) ->
	[get_string_token(String)];
pull_string([Token|Rest], String) when Token > 96, Token < 123 -> 
	pull_string(Rest, String ++ [Token]);
pull_string(Rest, String) ->
	[ get_string_token(String) | get_tokens(Rest) ].

pull_number([], Accum) ->
	[{number, Accum}];
pull_number([Token|Rest], Accum) when Token > 47, Token < 58 ->
	pull_number(Rest, (Accum * 10) + (Token - 48));
pull_number(List, Accum) ->
	[{number, Accum} | get_tokens(List)].

get_tokens([]) ->
	[];
get_tokens([Token|Rest]) when Token > 47, Token < 58 -> 
	pull_number(Rest, Token - 48);
get_tokens([Token|Rest]) when Token > 96, Token < 123 -> 
	pull_string(Rest, [Token]);	
get_tokens([Token|Rest]) when Token =:= 32 -> 
	get_tokens(Rest);
get_tokens([Token|Rest]) -> 
	[token_for_char(Token) | get_tokens(Rest)].

get_token_list(String) ->
	get_tokens(String).
	
get_token_list_test_() ->
	[
		?_assert(get_token_list("-") =:= [{binop, minus}]),
		?_assert(get_token_list("+") =:= [{binop, plus}]),
		?_assert(get_token_list("*") =:= [{binop, times}]),
		?_assert(get_token_list(")") =:= [close_paren]),
		?_assert(get_token_list("(") =:= [open_paren]),
		?_assert(get_token_list("\~") =:= [unary_minus]),
		?_assert(get_token_list("3") =:= [{number, 3}]),
		?_assert(get_token_list("3+3") =:= [{number, 3},{binop, plus},{number, 3}]),				
		?_assert(get_token_list(" 3  +   3 ") =:= [{number, 3},{binop, plus},{number, 3}]),
		?_assert(get_token_list("33") =:= [{number, 33}]),
		?_assert(get_token_list("foobar") =:= [{string, "foobar"}]),	
		?_assert(get_token_list("if") =:= [if_token]),
		?_assert(get_token_list("else") =:= [else_token]),
		?_assert(get_token_list("then") =:= [then_token]),
		?_assert(get_token_list("let") =:= [let_token]),
		?_assert(get_token_list("in") =:= [in_token]),
		?_assert(get_token_list("=") =:= [equals])			
	].