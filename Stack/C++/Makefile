CC     = g++
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = stackTest
OBJS   = main.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(OBJS) 

main.o: main.cpp stack.h
	$(CC) $(CFLAGS) main.cpp

clean:
	rm $(EXEC) $(OBJS)
