#  Copyright (c) 2015, Thomas Lang. All rights reserved.
#  
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
# 
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>

CC     = gcc
CFLAGS = -c -Wall
LFLAGS = -o $(EXEC)
EXEC   = avltest
SRCS   = node.c avl.c main.c
OBJS   = $(SRCS:.c=.o)

$(EXEC): $(OBJS)
	@printf "Linking everything together ... "
	@$(CC) $(OBJS) $(LFLAGS) 
	@printf "done.\n"

node.o: node.c
	@printf "Compiling 'node.c' ... "
	@$(CC) $(CFLAGS) node.c
	@printf "done.\n"

avl.o: avl.c
	@printf "Compiling 'avl.c' ... "
	@$(CC) $(CFLAGS) avl.c
	@printf "done.\n"

main.o: main.c
	@printf "Compiling 'main.c' ... "
	@$(CC) $(CFLAGS) main.c
	@printf "done.\n"

all: $(EXEC)

.PHONY: clean

clean:
	@printf "Removing executable ... "
	@rm $(EXEC)
	@printf "done.\n"
	@printf "Removing object files ... "
	@rm $(OBJS)
	@printf "done.\n"
