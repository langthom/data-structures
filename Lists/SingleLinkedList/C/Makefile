CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o
EXEC   = listTest
OBJS   = main.o \
		 single_linked_list.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(EXEC) $(OBJS)

main.o: main.c
	$(CC) $(CFLAGS) main.c

single_linked_list.o: single_linked_list.c
	$(CC) $(CFLAGS) single_linked_list.c

clean:
	rm $(EXEC) $(OBJS)
