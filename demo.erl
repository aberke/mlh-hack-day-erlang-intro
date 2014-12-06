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
		pattern_matching_function_clause/1,

		integers_list/1,
		double/1,
		filter_non_integers/1,

		double_with_otp/1,
		filter_non_integers_with_opt/1
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


%% about lists
%--------------
% a list is the concatenation of the first node and another list 
% [] is the empty list
% a list is [Head | Tail] where Tail is another list

% can represent the same list in the following ways
% [2, 1, 0]
% [2 | [1, 0]]
% [2 | [1 | [0]]]


% Make a list of positive integers up to Digit that was passed as argument
% eg, [0,1,2] = integer_list(2)
-spec integers_list(integer()) -> list().
integers_list(0) -> [];
integers_list(Digit) ->
	[Digit | integers_list(Digit - 1)].


% think in recursion not iteration!

% Double each of the values in a list of integers
-spec double([integer()]) -> [integer()].
double([]) -> [];
double([Value | Tail]) ->
	[Value*2 | double(Tail)].

% filter all the non integers out of a list
% [0, 2] = demo:filter_non_integers([0, "erlang", 2, {5}]).
-spec filter_non_integers(list()) -> list().
filter_non_integers([]) -> [];
filter_non_integers([Value | Tail]) when is_integer(Value) ->
	[Value | filter_non_integers(Tail)];
filter_non_integers([_ | Tail]) ->
	filter_non_integers(Tail). % skip that non integer!


% in practice we use OTP!

% double with the commonly used lists:map/2 function to make code more readable
double_with_otp(SomeList) ->
	lists:map(fun(Value) -> 2*Value end, SomeList).

% filter with the commonly used lists:filter/2 to make code more readable
filter_non_integers_with_opt(SomeList) ->
	lists:filter(fun is_integer/1, SomeList).












