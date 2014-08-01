CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = queueTest
OBJS   = main.o \
		 queue.o \
		 double_linked_list.o

all: $(EXEC)
	@echo "Created executable successfully."

$(EXEC): $(OBJS)
	@echo "Linking object files together ... done."
	@$(CC) $(LFLAGS) $(OBJS)

main.o: main.c
	@echo "Compiling 'main.c' ... done."
	@$(CC) $(CFLAGS) main.c

queue.o: queue.c 
	@echo "Compiling 'queue.c' ... done."
	@$(CC) $(CFLAGS) queue.c

double_linked_list.o: double_linked_list.c
	@echo "Compiling 'double_linked_list.c' ... done."
	@$(CC) $(CFLAGS) double_linked_list.c

clean:
	@echo "Cleaning up ..."
	@echo "Deleting executable   ... done."
	@echo "Deleting object files ... done."
	@rm $(EXEC) $(OBJS)
	@echo "Cleaned up successfully."
