%%%---------------------------------------------------------------------
%%% Copyright Lang Thomas 2014
%%%
%%% You can use this code in your implementation as often as you want.
%%% You are free to copy and distribute it, if you want to change it,
%%% please contact me.
%%%---------------------------------------------------------------------
%%% Description module main 
%%%---------------------------------------------------------------------
%%% This module test the QuickSort algorithm.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% main/1
%%% Sorts the passed list with the non-in-situ quickSort algorithm.
%%%---------------------------------------------------------------------

-module( main ).
-created( 'Date: 2014/07/21 19:32' ).
-created_by( 'Lang Thomas' ).
-export( [main/1] ).

% importing the function "quickSort" with arity 1
% from the module "quickSort"
-import( quickSort, [quickSort/1] ).


%%%-------------------------------------------------------------------------
%%% Function: main
%%% Purpose : Testing function for the QuickSort algorithm
%%% Args    : The List to Sort.
%%%-------------------------------------------------------------------------
main(_Args) -> ListToSort = _Args,
               SortedList = quickSort:quickSort( ListToSort ),
               io:format( "Sorted list: " ),
               lists:map( fun(X) -> io:format("~p ", [X]) end, SortedList ),
               io:format( "~n" ).

