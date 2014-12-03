%%%-------------------------------------------------------------------
%%% @doc
%%% Demo module for MLH Hack Day: Intro to Erlang
%%% Let's talk about syntax
%%% @end
%%% @contributors:
%%%     Alexandra Berke (github: aberke)
%%%-------------------------------------------------------------------

% Comments are preceded by '%' symbols

% variable names start with uppercase letters
% atoms start with lowercase letters
% booleans are just the atoms 'true' and 'false'
% tuples are in brackets, eg {1, 2} or {ok, 1, false}
% lists are nested -- more later, eg [] is empty list
% strings are syntactic sugar for lists of characters
% binaries are nice...


% module name always matches the name of the file (this is demo_syntax.erl)
% each module (a .erl file) compiled to individual .beam file
%	> c(demo_syntax.erl) --> demo_syntax.beam
-module(demo_syntax).

% Any "public" function you want to call from outside the module 
%  must be exported
-export([
		test_export/0, % exporting function with arity 0 (takes no parameters)
		
		public_function/0,

		arity_one/1, % this function has arity 1

		% the same function name can have multiple arities
		% all, none, or some of the arities can be exported
		arity_multiple/0,
		% note arity_multiple/1, is defined but not exported
		arity_multiple/2,

		function_with_spec/1 % exporting function with spec
	]).

% this is the definition for a function 
% function simply returns the value 1
% function is named test_export
% function takes no arguments (has arity 0)
% function invoked by calling "demo_syntax:demo_export()."
test_export() ->
	Value0 = 0, % statements end in a comma
	Value1 = Value0 + 1,
	Value1. % function definitions end with a period

% note this function isn't exported, so CANNOT be called from outside module
% Try invoking this function...
% 	> demo_syntax:private_function().
% 	** exception error: undefined function demo_syntax:private_function/0
private_function() ->
	1.

% note this function is exported, so CAN by called from outside module
% invoke from outside module with "demo_syntax:public_function()"
public_function() ->
	private_function(). % simply a public interface to private_function


%%%-------------------------------------------------------------------
%%% About Function Arity
%%%  - arity refers to the number of arguments a function accepts
%%%  - the same function name can have multiple arities
%%%  - all, none, or some of the arities can be exported
%%%-------------------------------------------------------------------


% this function has arity 1 and is the identity function
arity_one(SomeValue) ->
	SomeValue.

% The following are each a separate function definition
arity_multiple() ->
	"I am arity_multiple/0". % returns string
arity_multiple(SomeValue) ->
	SomeValue. 
arity_multiple(Value1, Value2) ->
	arity_multiple([Value1, Value2]). % calls arity_multiple/1


%%%-------------------------------------------------------------------
%%% Function specs
%%%  - typically go directly above a function
%%%  - they provide documentation for argument types and return types
%%%  - specs can be overloaded
%%%-------------------------------------------------------------------

-spec function_with_spec(integer()) -> list().
function_with_spec(SomeNumber) when is_integer(SomeNumber) ->
	[SomeNumber]. % returns list with 1 item










