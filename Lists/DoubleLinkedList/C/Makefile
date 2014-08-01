CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o
EXEC   = listTest
OBJS   = main.o \
		 double_linked_list.o

all: $(EXEC)

$(EXEC): $(OBJS)
	@echo "Linking object files together ... done."
	@$(CC) $(LFLAGS) $(EXEC) $(OBJS)

main.o: main.c
	@echo "Compiling 'main.c' ... done."
	@$(CC) $(CFLAGS) main.c

double_linked_list.o: double_linked_list.c
	@echo "Compiling 'double_linked_list.c' ... done."
	@$(CC) $(CFLAGS) double_linked_list.c

clean:
	@echo "Cleaning up ..."
	@echo "Removing executable     ... done."
	@echo "Removing object files   ... done."
	@rm $(EXEC) $(OBJS)
	@echo "Cleaned up successfully ... done."
