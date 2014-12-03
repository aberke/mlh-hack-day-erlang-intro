%%%-------------------------------------------------------------------
%%% @doc
%%% Demo module for MLH Hack Day: Intro to Erlang
%%%
%%% @end
%%% @contributors:
%%%     Alexandra Berke (github: aberke)
%%%-------------------------------------------------------------------

-module(demo).


-export([
		get_favorite_number/0,
		pattern_matching_unpack_variables/0,
		pattern_matching_function_clause/1
	]).

-include("demo.hrl").


% use definition in .hrl file
get_favorite_number() ->
	?DEMO_FAVORITE_NUMBER.


%%%-------------------------------------------------------------------
%%% Pattern matching is powerful
%%% - Can be used to unpack variables
%%% - Often leveraged at function definitions (see demo.erl)
%%% - Erlang attempts to pattern match each definition, going from top to bottom
%%% - Error if no available function clause definition
%%%-------------------------------------------------------------------

% This function will return tuple {1, 2}.
pattern_matching_unpack_variables() ->
	PackedTuple = {some_atom, 1},
	PackedList = [1,2,3],
	% assigning variable starting with "_" ignores value
	{_, SomeInteger} = PackedTuple, 
	[_Value_a, Value_b, _Value_c] = PackedList,
	{SomeInteger, Value_b}.

% only matches when 2-item tuple as argument
pattern_matching_function_clause({_, SecondValue}) ->
	SecondValue; % note semicolon - this function definition isn't done
% only matches 3-item tuple
pattern_matching_function_clause({Item1, _Item2, _Item3}=SomeTuple) ->
	[Item1, SomeTuple].


% about lists


% recursion not iteration








