CC     = g++
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = queueTest
OBJS   = main.o

all: $(EXEC)
	@echo "Making executable ..."

$(EXEC): $(OBJS)
	@echo "Linking object files together ... done."
	@$(CC) $(LFLAGS) $(OBJS)

main.o: main.cpp queue.h
	@echo "Compiling main.cpp ... done."
	@$(CC) $(CFLAGS) main.cpp

clean:
	@echo "Cleaning up ... done."
	@echo "Deleting executable ... done."
	@echo "Deleting object files ... done."
	@rm $(EXEC) $(OBJS)
