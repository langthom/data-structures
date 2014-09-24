
using System;

namespace BinTreeApplication
{
    public class Node<T>
    {
        private T value_;
        private Node<T> left;
        private Node<T> right;
        private Node<T> parent;

        public Node( T _value )
        {
            value_ = _value;
        }

        public T Value
        {
            get
            {
                return value_;
            }
            set
            {
                value_ = value;
            }
        }

        public Node<T> Left
        {
            get
            {
                return left;
            }
            set
            {
                left = value;
            }
        }

        public Node<T> Right
        {
            get
            {
                return right;
            }
            set
            {
                right = value;
            }
        }

        public Node<T> Parent
        {
            get
            {
                return parent;
            }
            set
            {
                parent = value;
            }
        }

        public override string ToString()
        {
            return "" + value_;
        }
    }

    public class BinTree<T> where T : IComparable
    {
        private Node<T> root;
        private int size;

        public BinTree()
        {
            root = null;
            size = 0;
        }

        /**************************************************************/

        public bool isEmpty()
        {
            return root == null;
        }

        public int getSize()
        {
            return size;
        }

        /**************************************************************/

        public void insert( T value_ )
        {
            Node<T> node = new Node<T>( value_ );
            insert( node );
        }

        public void insert( Node<T> node )
        {
            if( isEmpty() )
            {
                root = node;
            }
            else
            {
                insert( root, node );
            }
            size++;
        }

        private void insert( Node<T> cur, Node<T> node )
        {
            if( node.Value.CompareTo( cur.Value ) < 0 )
            {
                if( cur.Left == null )
                {
                    cur.Left = node;
                    node.Parent = cur;
                }
                else
                {
                    insert( cur.Left, node );
                }
            }
            else if( node.Value.CompareTo( cur.Value ) > 0 )
            {
                if( cur.Right == null )
                {
                    cur.Right = node;
                    node.Parent = cur;
                }
                else
                {
                    insert( cur.Right, node );
                }
            }
        }

        /**************************************************************/

        public void delete( T element )
        {
            if( isEmpty() )
            {
                throw new Exception(
                        "Error! Cannot delete from empty tree.");
            }
            else
            {
                Node<T> node_to_delete_ = findNode( element );
                if( node_to_delete_ != null )
                {
                    if( node_to_delete_.Value.CompareTo( root.Value ) == 0 )
                    {
                        deleteInternalNode( root );
                    }
                    else if( node_to_delete_.Left != null &&
                             node_to_delete_.Right != null )
                    {
                        deleteInternalNode( node_to_delete_ );
                    }
                    else
                    {
                        deleteNode( node_to_delete_ );
                    }
                }
                else
                {
                    throw new Exception("Error! Element not found.");
                }
            }
        }

        private void deleteInternalNode( Node<T> node )
        {
            Node<T> max_of_min_ = findMaxOfMinNode( node );
            node.Value = max_of_min_.Value;
            deleteNode( max_of_min_ );
        }

        private void deleteNode( Node<T> node )
        {
            if( node.Left != null && node.Right != null )
            {
                if( node.Parent.Right.Value.CompareTo( node.Value ) == 0 )
                {
                    node.Parent.Right = null;
                }
                else
                {
                    node.Parent.Left = null;
                }
                node.Parent = null;
            }
            else if( node.Left != null )
            {
                if( node.Parent.Right.Value.CompareTo( node.Value ) == 0 )
                {
                    node.Parent.Right = node.Left;
                }
                else
                {
                    node.Parent.Left = node.Left;
                }
                node.Left.Parent = node.Parent;
                node.Parent = null;
            }
            else
            {
                if( node.Parent.Right.Value.CompareTo( node.Value ) == 0 )
                {
                    node.Parent.Left = node.Right;
                }
                else
                {
                    node.Parent.Right = node.Right;
                }
                node.Right.Parent = node.Parent;
                node.Parent = null;
            }
        }

        private Node<T> findNode( T element )
        {
            if( isEmpty() )
            {
                throw new Exception("Error! No nodes in emtpy tree");
            }
            else
            {
                return findNode( root, element );
            }
            return null;
        }

        private Node<T> findNode( Node<T> cur, T element )
        {
            if( cur.Value.CompareTo( element ) == 0 )
            {
                return cur;
            }
            else if( element.CompareTo( cur.Value ) < 0 )
            {
                findNode( cur.Left, element );
            }
            else
            {
                findNode( cur.Right, element );
            }
            return null;
        }

        private Node<T> findMaxOfMinNode( Node<T> node )
        {
            Node<T> leftChild = null;
            if( node.Left != null )
            {
                leftChild = node.Left;
                while( leftChild.Right != null )
                {
                    leftChild = leftChild.Right;
                }
            }
            return leftChild;
        }

        /**************************************************************/

        public void preorder()
        {
            if( !isEmpty() )
            {
                preorder( root );
            }
            Console.WriteLine();
        }

        private void preorder( Node<T> node )
        {
            Console.WriteLine( node );
            if( node.Left != null )
            {
                preorder( node.Left );
            }
            if( node.Right != null )
            {
                preorder( node.Right );
            }
        }

        /**************************************************************/

        public void inorder()
        {
            if( !isEmpty() )
            {
                inorder( root );
            }
            Console.WriteLine();
        }

        private void inorder( Node<T> node )
        {
            if( node.Left != null )
            {
                inorder( node.Left );
            }
            Console.WriteLine( node );
            if( node.Right != null )
            {
                inorder( node.Right );
            }
        }

        /**************************************************************/

        public void postorder()
        {
            if( !isEmpty() )
            {
                postorder( root );
            }
            Console.WriteLine();
        }

        private void postorder( Node<T> node )
        {
            if( node.Left != null )
            {
                postorder( node.Left );
            }
            if( node.Right != null )
            {
                postorder( node.Right );
            }
            Console.WriteLine( node );
        }

        /**************************************************************/

        public void print()
        {
            if( isEmpty() )
            {
                Console.WriteLine("(empty tree)");
            }
            else
            {
                Console.WriteLine("Root: {0}", root );
                print( root );
            }
        }

        private void print( Node<T> cur )
        {
            if( cur.Left != null )
            {
                Console.WriteLine("Left: {0}", cur.Left );
            }
            if( cur.Right != null )
            {
                Console.WriteLine("Right: {0}", cur.Right );
            }
            if( cur.Left != null )
            {
                print( cur.Left );
            }
            if( cur.Right != null )
            {
                print( cur.Right );
            }
        }
    }

    class Program
    {
        public static void Main( string[] args )
        {
            BinTree<int> test = new BinTree<int>();
            Console.WriteLine("*** Initializing new tree ... done.");

            test.insert( 5  );
            test.insert( 7  );
            test.insert( 9  );
            test.insert( 1  );
            test.insert( 4  );
            test.insert( 0  );
            test.insert( 6  );
            test.insert( 2  );
            test.insert( 42 );
            Console.WriteLine("*** Adding values ... done.");

            Console.WriteLine("   ### Current tree:");
            test.print();

            Console.WriteLine("   ### Preorder traversing:");
            test.preorder();

            Console.WriteLine("   ### Inorder traversing:");
            test.inorder();

            Console.WriteLine("   ### Postorder traversing:");
            test.postorder();

            test.delete( 5 );
            Console.WriteLine("*** Deleting element 5 ... done.");

            Console.WriteLine("   ### Current tree:");
            test.print();
        }
    }
}

