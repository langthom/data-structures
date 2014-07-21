%%%---------------------------------------------------------------------
%%% Copyright Lang Thomas 2014
%%%
%%% You can use this code in your implementation as often as you want.
%%% You are free to copy and distribute it, if you want to change it,
%%% please contact me.
%%%---------------------------------------------------------------------
%%% Description module quickSort
%%%---------------------------------------------------------------------
%%% This module implements the QuickSort sorting algorithm (non-in-situ)
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% quickSort/1
%%% Sorts a list using the quickSort algorithm.
%%% This sorting algorithm gets the very first element of the passed
%%% list as Pivot element and then sorts recursively the list of all
%%% elements that are smaller and the list of all elements that are
%%% greater than the Pivot element.
%%%---------------------------------------------------------------------

-module( quickSort ).
-created( 'Date: 2014/07/21 19:29' ).
-created_by( 'Lang Thomas' ).
-export( [quickSort/1] ).



%%-----------------------------------------------------------------------
%% Function: quickSort/1
%% Purpose : Sorting a passed list with the QuickSort algorithm.
%% Args    : The list to sort.
%% Returns : The sorted list.
%%-----------------------------------------------------------------------
quickSort( [] ) -> [];
quickSort( [Pivot|Rest] ) ->
    quickSort( [ Y || Y <- Rest, Y < Pivot ] )
    ++ [Pivot] ++
    quickSort( [ Z || Z <- Rest, Z > Pivot ] ).


