CC     = g++
CFLAGS = -c -std=c++11 -Wall
LFLAGS = -o $(EXEC)
EXEC   = heapTest
OBJS   = main.o 

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(OBJS)

main.o: main.cpp \
		max_heap.h
	$(CC) $(CFLAGS) main.cpp

clean:
	@echo "Cleaning up:"
	@echo "Deleting executable ... done."
	@echo "Deleting object files ... done."
	@rm $(EXEC) $(OBJS)

