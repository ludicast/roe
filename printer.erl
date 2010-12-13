deeper(DepthString) ->
	string:concat("  ", DepthString).
	
print_binop(OpName, Lhs, Rhs, DepthString) ->
	io:format(string:concat(string:concat(DepthString, OpName), ":~n")),
	print(Lhs, deeper(DepthString)),	
	print(Rhs, deeper(DepthString)).
		
print(Expression, DepthString) ->
	case Expression of 
		{num, Val} -> 
			ValString = string:concat(DepthString, "~p~n"),
			io:format(ValString, [Val]);
		{var_name, Var} ->
			VarString = string:concat(Var, "~p~n"),
			io:format(VarString, [Var]);			 
		{plus, Lhs, Rhs} -> print_binop("Plus", Lhs, Rhs, DepthString);	
		{minus, Lhs, Rhs} -> print_binop("Minus", Lhs, Rhs, DepthString);		
		{times, Lhs, Rhs} -> print_binop("Times", Lhs, Rhs, DepthString);							
		{unary_minus, Negated} -> 
			NegatedString = string:concat(DepthString,"Negated:~n"),
			io:format(NegatedString),
			print(Negated, deeper(DepthString));
		{if_operation, Condition, ThenClause, ElseClause} ->
			io:format(string:concat(DepthString,"If:~n")),
			print(Condition, deeper(DepthString)),
			io:format(string:concat(DepthString,"Then:~n")),
			print(ThenClause, deeper(DepthString)),						
			io:format(string:concat(DepthString,"Else:~n")),
			print(ElseClause, deeper(DepthString));					
		{let_clause, {{var_name, VarName}, VarValue, InnerExpr}} ->
			io:format(string:concat(DepthString,"Let ~p=~n"), VarName),	
			print(VarValue, deeper(DepthString)),
			io:format(string:concat(DepthString,"In:~n")),
			print(InnerExpr, deeper(DepthString));						
		{ assignment, {{var_name, VarName}, AssignmentValue}} ->
			io:format(string:concat(DepthString,"~p=~n"), VarName),	
			print(AssignmentValue, deeper(DepthString))
	end.
			

pretty_print(String) ->
	Expression = create_syntax_tree(String),
	print(Expression, "").			