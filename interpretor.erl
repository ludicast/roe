-module(interpretor).
-export([interpret/1]).
-include_lib("eunit/include/eunit.hrl").
	
create_syntax_tree(String) ->
	{ok, Tokens, _Lines} = ruby_scan:string(String),
	%TokenGroups = split_groups(Tokens),
	%Result =   lists:map(fun clean_expression_from_tokens/1, TokenGroups),
	{ok, Result} = ruby_parse:parse(Tokens),
	{block, lists:map(fun simplifier:simplify/1, Result) }.

evaluate_block([Last | []], Context) ->
	{Val, _Cxt} = evaluate(Last, Context),
	Val;
evaluate_block([Head | Rest], Context) ->
	{_Val, NewContext} = evaluate(Head, Context),
	evaluate_block(Rest, NewContext).

evaluate_without_returning_context(Expression, Context) ->
	{Result, _NewContext} = evaluate(Expression, Context),
	Result.

evaluate({ assignment, {{var_name, VarName}, AssignmentValue}}, Context) ->
	{ EvaluatedVarValue, AssignedContext } = evaluate(AssignmentValue, Context),
	{ EvaluatedVarValue, dict:store(VarName, EvaluatedVarValue, AssignedContext)};
evaluate(Expression, Context) ->
	RetVal = case Expression of 
		{num, Val} -> Val;
		{block, BlockList} -> evaluate_block(BlockList, Context);
		{var_name, VarName} -> 
			{ok, Value} = dict:find(VarName, Context),
			Value;
		{plus, Lhs, Rhs} -> 
			evaluate_without_returning_context(Lhs, Context) + evaluate_without_returning_context(Rhs, Context);		
		{minus, Lhs, Rhs} -> 
			evaluate_without_returning_context(Lhs, Context) - evaluate_without_returning_context(Rhs, Context);		
		{times, Lhs, Rhs} -> 
			evaluate_without_returning_context(Lhs, Context) * evaluate_without_returning_context(Rhs, Context);							
		{unary_minus, Negated} -> -evaluate_without_returning_context(Negated, Context);
		{if_operation, Condition, ThenClause, ElseClause} ->
			case evaluate_without_returning_context(Condition, Context) of
				0 -> evaluate_without_returning_context(ElseClause, Context);
				_ -> evaluate_without_returning_context(ThenClause, Context)
			end;
		{let_clause, {{var_name, VarName}, VarValue, InnerExpr}}	->
			{ EvaluatedVarValue, Context } = evaluate(VarValue, Context),
			evaluate_without_returning_context(InnerExpr, dict:store(VarName, EvaluatedVarValue, Context))			
	end,
	{RetVal, Context}.
	

interpret(String) ->
	RootExpression = create_syntax_tree(String),
	{Result, _Context} = evaluate(RootExpression, dict:new()),
	Result.
	
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