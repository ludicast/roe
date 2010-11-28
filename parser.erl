-module(parser).
-export([create_syntax_tree/1, interpret/1,pretty_print/1,run/1,compile/1, simplifier/1]).
-include_lib("eunit/include/eunit.hrl").

expression_from_tokens([First|List]) ->
	{Value,Rest} = case First of
		{number, Val} -> {{num, Val}, List};
		{string, Val} -> {{var_name, Val}, List};
		open_paren -> 
			{InnerValue, [close_paren|PostParens]} = expression_from_tokens(List),
			{InnerValue, PostParens};
		unary_minus -> 
			{FlippedValue, PostValue} = expression_from_tokens(List),
			{{unary_minus, FlippedValue},PostValue};
		if_token ->
			{ FirstExpression, [then_token|AfterThen] } = expression_from_tokens(List),
			{ SecondExpression, [ else_token | AfterElse] } =  expression_from_tokens(AfterThen),
			{ ElseResult, EndTokens } = expression_from_tokens(AfterElse),
			{{if_operation, FirstExpression, SecondExpression, ElseResult}, EndTokens};
		let_token ->
			[{string, VarName} | [ equals |  AfterEquals  ] ]  = List,
			{Rhs, [in_token | InExpression] } = expression_from_tokens(AfterEquals),
			{TranslatedInExpression, InExpressionRemainder} = expression_from_tokens(InExpression),
			{{let_clause, {{var_name, VarName}, Rhs, TranslatedInExpression}}, InExpressionRemainder}
	end,
	case Rest of
		[{binop, Operation}|RightHandSide] ->
			{ RightHandSideTree, AfterTree } = expression_from_tokens(RightHandSide),
			{{Operation, Value, RightHandSideTree}, AfterTree};
		[equals | Assignment] ->
			{AssignmentValue, ParseRamainder} = expression_from_tokens(Assignment),
			{{ assignment, {Value, AssignmentValue}}, ParseRamainder};	
		_ -> {Value, Rest}
	end.
		
create_syntax_tree(String) ->
	[Tokens | _ ] = lexer:get_token_list(String),
	{Result, []} = expression_from_tokens(Tokens),
	simplifier(Result).


evaluate_list([{ Expression, []} | []], Context) ->
	evaluate(Expression, Context);
evaluate_list([{ FinalExpression, []} | Rest], Context) ->
	case evaluate(FinalExpression, Context) of
		{assignment_statement, {{var_name, VariableName}, Value}} ->
			evaluate_list(Rest, dict:store(VariableName, Value, Context));
		_Evaled -> 
			evaluate_list(Rest, Context)
	end.


evaluate(Expression, Context) ->
	case Expression of 
		{num, Val} -> Val;
		{var_name, VarName} -> 
			{ok, Value} = dict:find(VarName, Context),
			Value;
		{plus, Lhs, Rhs} -> evaluate(Lhs, Context) + evaluate(Rhs, Context);		
		{minus, Lhs, Rhs} -> evaluate(Lhs, Context) - evaluate(Rhs, Context);		
		{times, Lhs, Rhs} -> evaluate(Lhs, Context) * evaluate(Rhs, Context);							
		{unary_minus, Negated} -> -evaluate(Negated, Context);
		{if_operation, Condition, ThenClause, ElseClause} ->
			case evaluate(Condition, Context) of
				0 -> evaluate(ElseClause, Context);
				_ -> evaluate(ThenClause, Context)
			end;
		{let_clause, {{var_name, VarName}, VarValue, InnerExpr}}	->
			EvaluatedVarValue = evaluate(VarValue, Context),
			evaluate(InnerExpr, dict:store(VarName, EvaluatedVarValue, Context));
		{ assignment, {Variable, AssignmentValue}} ->
			{assignment_statement, {Variable, evaluate(AssignmentValue, Context)}}
	end.
	
deeper(DepthString) ->
	string:concat("  ", DepthString).
	
print_binop(OpName, Lhs, Rhs, DepthString) ->
	io:format(string:concat(string:concat(DepthString, OpName), ":~n")),
	print(Lhs, deeper(DepthString)),	
	print(Rhs, deeper(DepthString)).
		
simplifier({unary_minus, Negated}) ->
	case simplifier(Negated) of
		{num, 0} -> {num, 0};
		Simplified -> {unary_minus, Simplified}
	end;	
simplifier({times, Lhs , Rhs}) ->
	SimpleLhs = simplifier(Lhs),
	SimpleRhs = simplifier(Rhs),
	case { SimpleLhs, SimpleRhs } of 
		{{num, 0}, _} -> {num, 0};
		{_,{num, 0}} -> {num, 0};
		{{num, 1}, SimpleRhs} -> SimpleRhs;
		{SimpleLhs,{num, 1}} -> SimpleLhs;	
		_ -> {times, SimpleLhs, SimpleRhs }
	end;	
simplifier({if_operation, Condition, ThenClause, ElseClause}) ->
	case simplifier(Condition) of
		{num, 0} -> simplifier(ElseClause);
		{num, _} -> simplifier(ThenClause);
		SimplifiedCondition -> { if_operation, SimplifiedCondition, simplifier(ThenClause), simplifier(ElseClause)}
	end;						 
	
simplifier({plus, Lhs , Rhs}) ->
	SimpleLhs = simplifier(Lhs),
	SimpleRhs = simplifier(Rhs),
	case SimpleLhs of
		{num, 0} -> SimpleRhs;
		_ ->
			case SimpleRhs of
				{num, 0} -> 
					SimpleLhs;
				_ ->
					{plus, SimpleLhs, SimpleRhs}
			end		
	end;
simplifier({minus, Lhs , Rhs}) ->
	SimpleLhs = simplifier(Lhs),
	SimpleRhs = simplifier(Rhs),
	case SimpleRhs of 
		{num, 0} -> SimpleLhs;
		_ -> {minus, SimpleLhs, SimpleRhs}
	end;
simplifier({let_clause, {VarName, VarValue, InnerExpr}})	->
	{let_clause, {VarName, simplifier(VarValue), simplifier(InnerExpr)}};
simplifier(Expression) ->
	Expression.		
		
print(Expression, DepthString) ->
	case Expression of 
		{num, Val} -> 
			ValString = string:concat(DepthString, "~p~n"),
			io:format(ValString, [Val]);
		{plus, Lhs, Rhs} -> print_binop("Plus", Lhs, Rhs, DepthString);	
		{minus, Lhs, Rhs} -> print_binop("Minus", Lhs, Rhs, DepthString);		
		{times, Lhs, Rhs} -> print_binop("Times", Lhs, Rhs, DepthString);							
		{unary_minus, Negated} -> 
			NegatedString = string:concat(DepthString,"Negated:~n"),
			io:format(NegatedString),
			print(Negated, deeper(DepthString))
	end.

ast_to_machine(Expression) ->
	case Expression of
		{num, Num} -> 
			[Num];
		{unary_minus, Negated} -> 
			ast_to_machine(Negated) ++ [unary_minus];
	 	{plus, Lhs, Rhs} -> 
			ast_to_machine(Lhs) ++ ast_to_machine(Rhs) ++ [plus];
		{minus, Lhs, Rhs} -> 
			ast_to_machine(Lhs) ++ ast_to_machine(Rhs) ++ [minus];
	 	{times, Lhs, Rhs} -> 
			ast_to_machine(Lhs) ++ ast_to_machine(Rhs) ++ [times]					
	end.	

run_simulator([Value| []]) ->	
	Value;
run_simulator([ Negatable | [ unary_minus | Rest ] ]) ->
	run_simulator( [-Negatable | Rest] );
run_simulator([ Lhs | [Rhs | [ plus | Rest ] ] ]) ->
	run_simulator( [Lhs + Rhs | Rest] );
run_simulator([ Lhs | [Rhs | [ minus | Rest ] ] ]) ->
	run_simulator( [Lhs - Rhs | Rest] );
run_simulator([ Lhs | [Rhs | [ times | Rest ] ] ]) ->
	run_simulator( [Lhs * Rhs | Rest] ).				
	
compile(String) ->
	Expression = create_syntax_tree(String),
	ast_to_machine(Expression).
				
run(String) ->
	MachineCode = compile(String),
	run_simulator(MachineCode).
	
interpret(String) ->
	Strings =  lexer:get_token_list(String),
	ExpressionList = lists:map(fun expression_from_tokens/1, Strings),
	evaluate_list(ExpressionList, dict:new()).
	
pretty_print(String) ->
	Expression = create_syntax_tree(String),
	print(Expression, "").
	
get_syntax_tree_test_() ->
	[
		?_assert(create_syntax_tree("4") =:= {num, 4}),
		?_assert(create_syntax_tree("(4)") =:= {num, 4}),
		?_assert(create_syntax_tree("((4))") =:= {num, 4}),
		?_assert(create_syntax_tree("\~(4)") =:= {unary_minus, {num, 4}}),
		?_assert(create_syntax_tree("4+2") =:= {plus,{num, 4},{num,2}}),		
		?_assert(create_syntax_tree("4-2") =:= {minus,{num, 4},{num,2}}),	
		?_assert(create_syntax_tree("4*2") =:= {times,{num, 4},{num,2}}),
		?_assert(create_syntax_tree("\~4") =:= {unary_minus, {num, 4}}),
		?_assert(create_syntax_tree("if \~1 then 2 else 3") =:= { if_operation, {unary_minus, {num, 1}}, {num, 2}, {num, 3} }),
		?_assert(create_syntax_tree("((2+3)-4)") =:= {minus, {plus, {num, 2}, {num,3}}, {num, 4}}),
		?_assert(create_syntax_tree("let x = 1 in x") =:= {let_clause, {{var_name, "x"}, {num, 1}, {var_name, "x"}}} ),
		?_assert(create_syntax_tree("x = 1") =:= { assignment, {{var_name, "x"}, {num, 1}}})
	].
	
interpretor_test_() ->
	[
		?_assert(interpret("4") =:= 4),
		?_assert(interpret("(4)") =:= 4),
		?_assert(interpret("((4))") =:= 4),
		?_assert(interpret("\~(4)") =:= -4),
		?_assert(interpret("4+2") =:= 6),		
		?_assert(interpret("4-2") =:= 2),	
		?_assert(interpret("4*2") =:= 8),
		?_assert(interpret("((2+3)-4)") =:= 1),
		?_assert(interpret("if 1 then 2 else 3") =:= 2),
		?_assert(interpret("if (9 * 0) then 2 else 3") =:= 3),
		?_assert(interpret("let x = 1 in x") =:= 1),				
		?_assert(interpret("let x = 1 in let y = x in y") =:= 1),	
		?_assert(interpret("x = 1\nx") =:= 1)
	].	
	
simplifier_test_() ->
	[	
		?_assert(simplifier({plus,{num, 4},{num,2}}) =:= {plus,{num, 4},{num,2}}),
		?_assert(simplifier({plus,{num, 0},{num,2}}) =:= {num,2}),
		?_assert(simplifier({plus,{num, 4},{num,0}}) =:= {num,4}),
		?_assert(simplifier({unary_minus, {plus,{num, 0},{num,0}}}) =:= {num,0}),
		?_assert(simplifier({ if_operation, {num, 1}, {num, 2}, {num, 3} }) =:= {num,2}),
		?_assert(simplifier({ if_operation, {num, 0}, {num, 2}, {num, 3} }) =:= {num,3})
	].
compiler_test_() ->
	[
		?_assert(compile("4") =:= [4]),
		?_assert(compile("((4))") =:= [4]),
		?_assert(compile("2+4") =:= [ 2 | [ 4 | [ plus ] ] ]),
		?_assert(compile("2-4") =:= [ 2 | [ 4 | [ minus ] ] ]),
		?_assert(compile("2*4") =:= [ 2 | [ 4 | [ times ] ] ]),		
		?_assert(compile("\~4") =:= [ 4 | [ unary_minus ] ]),
		?_assert(compile("\~\~4") =:= [ 4 | [ unary_minus | [ unary_minus ] ] ])
	].
	
simulator_test_() ->
	[
		?_assert(run("4") =:= 4),
		?_assert(run("((4))") =:= 4),
		?_assert(run("\~4") =:= -4),
		?_assert(run("\~\~4") =:= 4),
		?_assert(run("2+4") =:= 6),
		?_assert(run("2-4") =:= -2),
		?_assert(run("2*4") =:= 8)			
	].	