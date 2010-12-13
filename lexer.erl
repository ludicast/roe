-module(lexer).

-export([get_tokens/1]).

-include_lib("eunit/include/eunit.hrl").

token_for_char(Char) ->
	case Char of 
		45 -> {binop, minus};
		43 -> {binop, plus};
		42 -> {binop, times};
		41 -> close_paren;
		40 -> open_paren;
		10 -> newline;
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


	
get_token_list_test_() ->
	[
		?_assert(get_tokens("-") =:= [{binop, minus}]),
		?_assert(get_tokens("+") =:= [{binop, plus}]),
		?_assert(get_tokens("*") =:= [{binop, times}]),
		?_assert(get_tokens(")") =:= [close_paren]),
		?_assert(get_tokens("(") =:= [open_paren]),
		?_assert(get_tokens("\~") =:= [unary_minus]),
		?_assert(get_tokens("3") =:= [{number, 3}]),
		?_assert(get_tokens("3+3") =:= [{number, 3},{binop, plus},{number, 3}]),				
		?_assert(get_tokens(" 3  +   3 ") =:= [{number, 3},{binop, plus},{number, 3}]),
		?_assert(get_tokens("33") =:= [{number, 33}]),
		?_assert(get_tokens("3\n3") =:= [{number, 3},newline,{number, 3}]),
		?_assert(get_tokens("foobar") =:= [{string, "foobar"}]),	
		?_assert(get_tokens("if") =:= [if_token]),
		?_assert(get_tokens("else") =:= [else_token]),
		?_assert(get_tokens("then") =:= [then_token]),
		?_assert(get_tokens("let") =:= [let_token]),
		?_assert(get_tokens("in") =:= [in_token]),
		?_assert(get_tokens("=") =:= [equals])		
	].
	
get_token_list_from_lexer_test_() ->
	[
		?_assert(get_tokens_from_lexer("-") =:= [{binop, minus}]),
		?_assert(get_tokens_from_lexer("+") =:= [{binop, plus}]),
		?_assert(get_tokens_from_lexer("*") =:= [{binop, times}]),
		?_assert(get_tokens_from_lexer(")") =:= [close_paren]),
		?_assert(get_tokens_from_lexer("(") =:= [open_paren]),
		?_assert(get_tokens_from_lexer("\~") =:= [unary_minus]),
		?_assert(get_tokens_from_lexer("3") =:= [{number, 3}]),
		?_assert(get_tokens_from_lexer("3+3") =:= [{number, 3},{binop, plus},{number, 3}]),				
		?_assert(get_tokens_from_lexer(" 3  +   3 ") =:= [{number, 3},{binop, plus},{number, 3}]),
		?_assert(get_tokens_from_lexer("33") =:= [{number, 33}]),
		?_assert(get_tokens_from_lexer("3\n3") =:= [{number, 3},newline,{number, 3}]),
		?_assert(get_tokens_from_lexer("foobar") =:= [{string, "foobar"}]),	
		?_assert(get_tokens_from_lexer("if") =:= [if_token]),
		?_assert(get_tokens_from_lexer("else") =:= [else_token]),
		?_assert(get_tokens_from_lexer("then") =:= [then_token]),
		?_assert(get_tokens_from_lexer("let") =:= [let_token]),
		?_assert(get_tokens_from_lexer("in") =:= [in_token]),
		?_assert(get_tokens_from_lexer("=") =:= [equals])		
	].	