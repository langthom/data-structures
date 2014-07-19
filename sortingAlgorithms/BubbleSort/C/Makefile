CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = sortingTest
OBJS   = main.o \
		 bubble_sort.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(OBJS)

main.o: main.c
	$(CC) $(CFLAGS) main.c

bubble_sort.o: bubble_sort.c
	$(CC) $(CFLAGS) bubble_sort.c

clean:
	rm $(EXEC) $(OBJS)

