-module(compiler).
-export([compile/1]).
-include_lib("eunit/include/eunit.hrl").

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