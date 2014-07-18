CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o
EXEC   = stackTest
OBJS   = main.o \
		 stack.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(LFLAGS) $(EXEC) $(OBJS)

main.o: main.c
	$(CC) $(CFLAGS) main.c

stack.o: stack.c
	$(CC) $(CFLAGS) stack.c

clean:
	rm $(EXEC) $(OBJS)
