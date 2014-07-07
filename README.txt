    
            Data structures
            ---------------

--- General things ---
This repository contains implementations of several data structures,
like linked lists, trees (binary search trees, AVL trees, ...) and
also basic graphs.


--- Compiling ---
You can either use the listed structures by including the modules 
in your programs, or you can test them with. For this, every 
implementation has a  test/  directory that includes a main method
which does some testing and output.
If you want to test like this, compile them just as usual for the
individual language:

( Replace the things in [ ] with the appropiate name ):

Note: For C and c++ I will add Makefiles, so just go into the appropiate 
      directory and type in the shell  "make".
      Then all will compile automatically, because there are some 
      dependencies on other files.

      The compiler used for C is  gcc  while the compiler used for
      C++ is   g++.


* Java   ->  $>  javac Main.java
             $>  java Main


--- Python ---
As Python is an interpreted language, just typ in on your console:

$> python [MODULE_NAME].py


If you want to execute that file without explicitly starting the python
command, you have to do the following:

$> chmod u+x [MODULE_NAME].py
$> ./[MODULE_NAME].py



... others may follow


---- What do you need? ---

--- C ---
To compile the programs written in C, you need a C-Compiler (which comes
with most operating systems), e.g. the gcc under Linux.

--- C++ ---
To compile the programs written in C++, you need a C++ - Compiler.
Under Linux, I recommend the g++ (may come with some distributions).

--- Java ---
To compile the Java programs, you need - well - Java. It should be already
come with most operating systems.

--- Python ---
To execute Python code, you'll need that programming language. Under Linux
type:

$> sudo apt-get install python

... or emerge, pacman -S or what else your distributions supports.
