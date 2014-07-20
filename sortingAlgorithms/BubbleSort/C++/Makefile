CC     = g++
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = sortingTest
OBJS   = main.o \
		 bubble_sort.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(OBJS)

main.o: main.cpp
	$(CC) $(CFLAGS) main.cpp

bubble_sort.o: bubble_sort.cpp
	$(CC) $(CFLAGS) bubble_sort.cpp

clean:
	rm $(EXEC) $(OBJS)

