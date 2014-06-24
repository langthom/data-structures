CC = gcc
CFLAGS = -c -Wall
LFLAGS = -o
EXEC   = treeExample
OBJS   = binary_search_tree.o \
		 main.o

treeExample: $(OBJS)
	$(CC) $(LFLAGS) $(EXEC) $(OBJS)

binary_search_tree.o: binary_search_tree.c \
					  binary_search_tree.h
	$(CC) $(CFLAGS) binary_search_tree.c

main.o: main.c  binary_search_tree.h
	$(CC) $(CFLAGS) main.c

all: $(EXEC)

clean:
	rm $(EXEC) $(OBJS)
