CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = heapTest
OBJS   = main.o \
		 min_heap.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(OBJS)

main.o: main.c
	$(CC) $(CFLAGS) main.c

min_heap.o: min_heap.c
	$(CC) $(CFLAGS) min_heap.c

clean:
	rm $(EXEC) $(OBJS)

