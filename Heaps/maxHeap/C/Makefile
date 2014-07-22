CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = heapTest
OBJS   = main.o \
		 max_heap.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(OBJS)

main.o: main.c
	$(CC) $(CFLAGS) main.c

max_heap.o: max_heap.c
	$(CC) $(CFLAGS) max_heap.c

clean:
	rm $(EXEC) $(OBJS)

