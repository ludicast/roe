-module(simplifier).
-export([simplify/1]).
-include_lib("eunit/include/eunit.hrl").
 
simplify({unary_minus, Negated}) ->
	case simplify(Negated) of
		{num, 0} -> {num, 0};
		Simplified -> {unary_minus, Simplified}
	end;	
simplify({times, Lhs , Rhs}) ->
	SimpleLhs = simplify(Lhs),
	SimpleRhs = simplify(Rhs),
	case { SimpleLhs, SimpleRhs } of 
		{{num, 0}, _} -> {num, 0};
		{_,{num, 0}} -> {num, 0};
		{{num, 1}, SimpleRhs} -> SimpleRhs;
		{SimpleLhs,{num, 1}} -> SimpleLhs;	
		_ -> {times, SimpleLhs, SimpleRhs }
	end;	
simplify({if_operation, Condition, ThenClause, ElseClause}) ->
	case simplify(Condition) of
		{num, 0} -> simplify(ElseClause);
		{num, _} -> simplify(ThenClause);
		SimplifiedCondition -> { if_operation, SimplifiedCondition, simplify(ThenClause), simplify(ElseClause)}
	end;						 
simplify({plus, Lhs , Rhs}) ->
	SimpleLhs = simplify(Lhs),
	SimpleRhs = simplify(Rhs),
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
simplify({minus, Lhs , Rhs}) ->
	SimpleLhs = simplify(Lhs),
	SimpleRhs = simplify(Rhs),
	case SimpleRhs of 
		{num, 0} -> SimpleLhs;
		_ -> {minus, SimpleLhs, SimpleRhs}
	end;
simplify({let_clause, {VarName, VarValue, InnerExpr}}) ->
	{let_clause, {VarName, simplify(VarValue), simplify(InnerExpr)}};
simplify({ assignment, {VarName, VarValue}}) ->
	{ assignment, {VarName, simplify(VarValue)}};
simplify(Expression) ->
	Expression.
	
simplify_test_() ->
	[	
		?_assert(simplify({plus,{num, 4},{num,2}}) =:= {plus,{num, 4},{num,2}}),
		?_assert(simplify({plus,{num, 0},{num,2}}) =:= {num,2}),
		?_assert(simplify({plus,{num, 4},{num,0}}) =:= {num,4}),
		?_assert(simplify({unary_minus, {plus,{num, 0},{num,0}}}) =:= {num,0}),
		?_assert(simplify({ if_operation, {num, 1}, {num, 2}, {num, 3} }) =:= {num,2}),
		?_assert(simplify({ if_operation, {num, 0}, {num, 2}, {num, 3} }) =:= {num,3}),
		?_assert(simplify({ assignment, {{var_name, "x"}, {plus,{num, 0},{num,0}}}}) =:= { assignment, {{var_name, "x"}, {num, 0}}})
	].	