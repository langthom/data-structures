CC     = g++
LD     = g++
CFLAGS = -c -Wall
EXEC   = listTest
LFLAGS = -o $(EXEC)
OBJS   = main.o 


listTest: $(OBJS)
	$(LD) $(LFLAGS) $(OBJS) 

main.o: main.cpp \
		single_linked_list.h
	$(CC) $(CFLAGS) main.cpp

all: $(EXEC)

clean:
	rm $(EXEC) $(OBJS)
