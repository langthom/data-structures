    
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


--- Haskell ---
All Haskell structures include a "Main.hs" file. Note that some of these
include a main function with a signature similar to

            main :: IO()

If this is existant, just type into your console:

$> runghc Main.hs


--- Python ---
As Python is an interpreted language, just typ in on your console:

$> python [MODULE_NAME].py


If you want to execute that file without explicitly starting the python
command, you have to do the following:

$> chmod u+x [MODULE_NAME].py
$> ./[MODULE_NAME].py



--- Erlang ---
To test the Erlang programs, you need to habe the erlang programming 
language installed.
If installed, you can compile the code via

$> erl -compile [MODULE_NAME].erl

This will create a .beam file.
After that, start the erlang-shell and execute the code:

$> erl

Now load the module ( without the  .erl extension ).

$> l( MODULE_NAME )

For more shell commands, please look at the  
Official documentatino at: 
        http://www.erlang.org/


--- C# ---
To compile and execute C# code, you will have to have installed
a C# compiler (gmcs recommended), and an executer (mono recommended).

$> gmcs MODULE_NAME.cs
$> mono MODULE_NAME.exe


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


--- Erlang ---
To compile Erlang code, you'll need the erlang programming language installed.


--- MIPS Assembly ---
In order to execute the files wrote in MIPS Assembly code, I recommend the 
MARS emulator. There are some other emulators available, the code should work
on every of these, but the most handsome for a User is the MARS emulator.
The version I use is 4.4, the file is therefore named "Mars4_4.jar".

You can start it (of course when you have downloaded it) with

$> java -jar Mars4_4.jar

Then simple click "Open", load the file and then type "F3". This will assemble
the typed-in code. After that there is a "Run"-button in the symbol bar, that
execute the code with the speed determined by the bar at its right.

