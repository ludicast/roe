Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]
NewLine = [\n]

Rules.

{Digit}+\.{Digit}+ : build_float(TokenChars, TokenLine).
{Digit}+ : build_integer(TokenChars, TokenLine).
({LowerCase}|_)({UpperCase}|{LowerCase}|{Digit}|_)* : build_identifier(TokenChars, TokenLine).
{NewLine} : {token,{eol,TokenLine}}.
{Whitespace}+ : skip_token.

\( : {token,{'(',TokenLine}}.
\) : {token,{')',TokenLine}}.
-  : {token,{'-',TokenLine}}.
\+ : {token,{'+',TokenLine}}.
=  : {token,{'=',TokenLine}}.
~  : {token,{'~',TokenLine}}.
\* : {token,{'*',TokenLine}}.

Erlang code.  

-record(integer, {line, value}).
-record(float, {line, value}).

build_integer(Chars, Line) ->
  {token, #integer{line = Line, value = list_to_integer(Chars)}}.

build_float(Chars, Line) ->
  {token, #float{line = Line, value = list_to_float(Chars)}}.

build_identifier(Chars, Line) ->
    Atom = list_to_atom(Chars),
    case reserved_word(Atom) of
        true -> {token, {Atom, Line}};
        false -> {token, {identifier, Line, Atom}}
    end.

reserved_word('if')      -> true;
reserved_word('then')    -> true;
reserved_word('else')    -> true;
reserved_word('let')    -> true;
reserved_word('begin')    -> true;
reserved_word('end')    -> true;
reserved_word('in')    -> true;
reserved_word(_)    -> false.