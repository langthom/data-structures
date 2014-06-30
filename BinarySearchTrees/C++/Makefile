CC     = g++
CFLAGS = -c -Wall
LFLAGS = -o
EXEC   = treeExample
OBJS   = node.o \
		 bintree.o \
		 main.o

treeExample: $(OBJS)
	$(CC) $(LFLAGS) $(EXEC) $(OBJS)

node.o: node.cpp \
		node.h
	$(CC) $(CFLAGS) node.cpp

bintree.o: bintree.cpp \
		   bintree.h
	$(CC) $(CFLAGS) bintree.cpp

main.o: main.cpp node.h bintree.h
	$(CC) $(CFLAGS) main.cpp

all: $(EXEC)

clean:
	rm $(EXEC) $(OBJS)
