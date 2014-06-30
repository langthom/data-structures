// node.cpp - implementation of a single node for the binary search tree

#include "node.h"

Node::Node( int val )
{
    value  = val;
    left   = NULL;
    right  = NULL;
    parent = NULL;
}

