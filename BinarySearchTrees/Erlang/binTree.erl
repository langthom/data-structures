%%%---------------------------------------------------------------------
%%% Copyright Lang Thomas 2014
%%%
%%% You can use this code in your implementation as often as you want.
%%% You are free to copy and distribute it, if you want to change it,
%%% please contact me.
%%%---------------------------------------------------------------------
%%% Description module BinTree
%%%---------------------------------------------------------------------
%%% This module implements a typical binary search tree. Such a tree
%%% follows the ordering rule that the left child of a node is smaller
%%% than its right child at any time.
%%% Nodes itself contains this value and the references to its children.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% newNode( Value )
%%%     returns a new node containing the value "Value"
%%%
%%% isEmpty( Tree )
%%%     returns true if Tree is empty or false otherwise
%%%---------------------------------------------------------------------

-module( binTree ).
-created( 'Date: 2014/07/20 12:47' ).
-created_by( 'Lang Thomas' ).

-export( [newNode/1,  %% creating a new node
          isEmpty/1]   %% is tree empty?
       ).



%%-----------------------------------------------------------------------
%% Data Type: node
%% where:
%%      value: An integer ( default is undefined )
%%      left : A node     ( default is undefined )
%%      right: A node     ( default is undefined )
%%-----------------------------------------------------------------------
-record( node, {value,
                left,
                right }).

%%-----------------------------------------------------------------------
%% Data Type: bintree
%% where:
%%      size: An integer ( default is 0         )
%%      root: A node     ( default is undefined )
%%-----------------------------------------------------------------------
-record( bintree, {size = 0,
                   root     }).


%%-----------------------------------------------------------------------
%% Function: newNode/1
%% Purpose : Creates a new Node containing the passed value
%% Args    : The Value to insert
%% Returns : A new node containing the passed value
%%-----------------------------------------------------------------------
newNode( Value ) -> #node{ value = Value }.


%%-----------------------------------------------------------------------
%% Function: isEmpty/1
%% Purpose : Checks whether the passed tree is empty or not.
%% Args    : The tree to check
%% Returns : Either true if the tree is empty or false otherwise
%%-----------------------------------------------------------------------
isEmpty( Tree ) when Tree==#bintree{ size = 0, root = undefined } -> true;
isEmpty( Tree ) -> false.


