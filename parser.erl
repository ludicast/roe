-module(parser).
-export([create_syntax_tree/1, interpret/1, split_groups/1, clean_expression_from_tokens/1, expression_from_tokens/1]).
-include_lib("eunit/include/eunit.hrl").

expression_from_tokens([First|List]) ->
	{Value,Rest} = case First of
		{integer, _Line, Val} -> {{num, Val}, List};
		{identifier, _Line, Val} -> {{var_name, atom_to_list(Val)}, List};
		{'(', _OpenLine} -> 
			{InnerValue, [{')', _CloseLine}|PostParens]} = expression_from_tokens(List),
			{InnerValue, PostParens};
		{'~', _Line} -> 
			{FlippedValue, PostValue} = expression_from_tokens(List),
			{{unary_minus, FlippedValue},PostValue};
		{'if', _IfLine} ->
			{ FirstExpression, [{'then', _ThenLine} | AfterThen] } = expression_from_tokens(List),
			{ SecondExpression, [ {'else', _ElseLine} | AfterElse] } =  expression_from_tokens(AfterThen),
			{ ElseResult, EndTokens } = expression_from_tokens(AfterElse),
			{{if_operation, FirstExpression, SecondExpression, ElseResult}, EndTokens};
		{'let', _LetLine} ->
			[{identifier, _VarLine, VarName} | [{'=', _EqualLine} |  AfterEquals  ] ]  = List,
			{Rhs, [{ in, _InLine }| InExpression] } = expression_from_tokens(AfterEquals),
			{TranslatedInExpression, InExpressionRemainder} = expression_from_tokens(InExpression),
			{{let_clause, {{var_name, atom_to_list(VarName)}, Rhs, TranslatedInExpression}}, InExpressionRemainder}
	end,
	case Rest of
		[{'+', _OpLine}|RightHandSide] ->
			{ RightHandSideTree, AfterTree } = expression_from_tokens(RightHandSide),
			{{plus, Value, RightHandSideTree}, AfterTree};
		[{'-', _OpLine}|RightHandSide] ->
			{ RightHandSideTree, AfterTree } = expression_from_tokens(RightHandSide),
			{{minus, Value, RightHandSideTree}, AfterTree};		
		[{'*', _OpLine}|RightHandSide] ->
			{ RightHandSideTree, AfterTree } = expression_from_tokens(RightHandSide),
			{{times, Value, RightHandSideTree}, AfterTree};				
		[{'=', _OpLine} | Assignment] ->
			{AssignmentValue, ParseRamainder} = expression_from_tokens(Assignment),
			{{ assignment, {Value, AssignmentValue}}, ParseRamainder};	
		_ -> {Value, Rest}
	end.
		
clean_expression_from_tokens(Tokens) ->	
	{Result, []} = expression_from_tokens(Tokens),
	Result.
		
split_groups([{eol,_Line} | Rest], Remainder, Grouped) ->
	split_groups(Rest, [],Grouped ++ [Remainder]);
split_groups([Other | Rest], Remainder, Grouped) ->
	split_groups(Rest, Remainder ++ [Other],Grouped);	
split_groups([], Remainder, Grouped) ->
	Grouped ++ [Remainder].
		
split_groups(Tokens) ->		
	split_groups(Tokens, [], []).
	
create_syntax_tree(String) ->
	{ok, Tokens, _Lines} = ruby_scan:string(String),
	TokenGroups = split_groups(Tokens),
	Result = lists:map(fun clean_expression_from_tokens/1, TokenGroups),
	{block, lists:map(fun simplifier:simplify/1, Result) }.

evaluate_block([Last | []], Context) ->
	{Val, Cxt} = evaluate(Last, Context),
	Val;
evaluate_block([Head | Rest], Context) ->
	{Val, NewContext} = evaluate(Head, Context),
	io:format("evalling 2nd ~p ~n", [Rest]),	
	evaluate_block(Rest, NewContext).

evaluate_without_returning_context(Expression, Context) ->
	{Result, NewContext} = evaluate(Expression, Context),
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
	{Result, Context} = evaluate(RootExpression, dict:new()),
	Result.
	
pull_from_block(String)	->
	{block, ExpressionList} = create_syntax_tree(String),
	ExpressionList.
	
pull_single_from_block(String) ->
	[Head | []] = pull_from_block(String),
	Head.	
	
	
get_syntax_tree_test_() ->
	[
		?_assert(pull_single_from_block("4") =:= {num, 4}),
		?_assert(pull_single_from_block("(4)") =:= {num, 4}),
		?_assert(pull_single_from_block("((4))") =:= {num, 4}),
		?_assert(pull_single_from_block("\~(4)") =:= {unary_minus, {num, 4}}),
		?_assert(pull_single_from_block("4+2") =:= {plus,{num, 4},{num,2}}),		
		?_assert(pull_single_from_block("4-2") =:= {minus,{num, 4},{num,2}}),	
		?_assert(pull_single_from_block("4*2") =:= {times,{num, 4},{num,2}}),
		?_assert(pull_single_from_block("\~4") =:= {unary_minus, {num, 4}}),
		?_assert(pull_single_from_block("if \~1 then 2 else 3") =:= { if_operation, {unary_minus, {num, 1}}, {num, 2}, {num, 3} }),
		?_assert(pull_single_from_block("((2+3)-4)") =:= {minus, {plus, {num, 2}, {num,3}}, {num, 4}}),
		?_assert(pull_single_from_block("let x = 1 in x") =:= {let_clause, {{var_name, "x"}, {num, 1}, {var_name, "x"}}} ),
		?_assert(pull_single_from_block("x = 1") =:= { assignment, {{var_name, "x"}, {num, 1}}}),
		?_assert(pull_from_block("x = 1\nx") =:= [{ assignment, {{var_name, "x"}, {num, 1}}}, {var_name, "x"}])
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