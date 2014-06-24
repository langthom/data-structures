// BinarySearchTree.java - Implementation of a binary search tree

public class BinarySearchTree<T extends Comparable<T>>{

    private class Node<T extends Comparable<T>>{
        private T value;
        private Node<T> left;
        private Node<T> right;

        private Node( T value ){
            this.value = value;
            left = null;
            right = null;
        }

        public String toString(){
            return ( "{" + value + "}" );
        }
    }


    // root node of the whole tree, we only have one
    // root node in a BINARY search tree
    Node<T> root;

    public BinarySearchTree(){ root = null; }

    public void add( T value ){
        if( this.isEmpty() ){
            root = new Node<T>( value );
        }else{
            insertInTree( root, value );
        }
    }

    private void insertInTree( Node<T> node, T value ){
        if( value.compareTo( node.value ) < 0 ){
            if( node.left != null ){
                insertInTree( node.left, value );
            }else{
                node.left = new Node<T>( value );
            }
        }else if( value.compareTo( node.value ) > 0 ){
            if( node.right != null )
                insertInTree( node.right, value );
            else
                node.right = new Node<T>( value );
        }else{
            return;
        }
    }

    public void traversePreorder(){
        if( root != null )
            traversePreorderRec( root );
    }

    private void traversePreorderRec( Node<T> node ){
        System.out.println( node );
        if( node.left != null )
            traversePreorderRec( node.left );
        if( node.right != null )
            traversePreorderRec( node.right );
        System.out.println();
    }

    public void traverseInorder(){
        if( root != null )
            traverseInorderRec( root );
    }

    private void traverseInorderRec( Node<T> node ){
        if( node.left != null )
            traverseInorderRec( node.left );
        System.out.println( node );
        if( node.right != null )
            traverseInorderRec( node.right );
        System.out.println();
    }

    public void traversePostorder(){
        if( root != null )
            traversePostorderRec( root );
    }

    private void traversePostorderRec( Node<T> node ){
        if( node.left != null )
            traversePostorderRec( node.left );
        if( node.right != null )
            traversePostorderRec( node.right );
        System.out.println( node );
        System.out.println();
    }





    public boolean isEmpty(){ return ( root == null ); }

    public void printTree(){
        if( isEmpty() )
            System.out.println( "( empty tree )" );
        else{
            System.out.println( "Root: " + root );
            printTreeRec( root );
        }
    }

    private void printTreeRec( Node<T> node ){
        if( node.left != null )
            System.out.println( "Left: " + node.left );
        if( node.right != null )
            System.out.println( "Right: " + node.right );
        if( node.left != null )
            printTreeRec( node.left );
        if( node.right != null )
            printTreeRec( node.right );
    }
}
