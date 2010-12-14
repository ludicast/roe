Nonterminals
  expr_list
  expr
  if_then_else
  grammar
  number
  variable
  assignment
  let_in
  .

Terminals
  '(' ')' eol integer 'if' 'else' 'then' 'let' 'in' '+' '-' '*' '=' '~' identifier
  .
	
Rootsymbol grammar.

Right 100 '='. 
Left 300 '+'. 
Left 300 '-'. 
Left 400 '*'. 
Unary 500 '~'.

grammar -> expr_list : '$1'.
grammar -> '$empty' : [].

%% Expression lists (eol delimited)
expr_list -> eol : [].
expr_list -> expr : ['$1'].
expr_list -> expr eol : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr eol expr_list : ['$1'|'$3'].

expr -> '(' expr ')' : '$2'.
expr -> number : '$1'.
expr -> variable : '$1'.
expr -> assignment : '$1'.
expr -> '~' expr : {unary_minus, '$2'}.
expr -> expr '*' expr : {times, '$1', '$3'}.
expr -> expr '+' expr : {plus, '$1', '$3'}.
expr -> expr '-' expr : {minus, '$1', '$3'}.
expr -> if_then_else : '$1'.
expr -> let_in : '$1'.

number -> integer : {num, unwrap('$1')}.
variable -> identifier : {var_name, atom_to_list(unwrap('$1'))}.


let_in -> 'let' variable '=' expr 'in' expr : {let_clause, {'$2', '$4', '$6'}}.
if_then_else -> 'if' expr 'then' expr 'else' expr : {if_operation, '$2', '$4', '$6'}.
assignment -> identifier '=' expr : { assignment, {{var_name, atom_to_list(unwrap('$1'))}, '$3'}}.

Erlang code.

unwrap({_,_,V}) -> V.
