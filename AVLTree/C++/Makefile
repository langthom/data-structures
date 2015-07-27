# Copyright (c) 2015, Thomas Lang. All rights reserved.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>

CC     = g++
CFLAGS = -c -Wall
LFLAGS = -o
EXEC   = avltest
OBJS   = main.o

$(EXEC): $(OBJS)
	@printf "Linking everything together ... "
	@$(CC) $(OBJS) $(LFLAGS) $(EXEC)
	@printf "done.\n"

main.o: main.cpp
	@printf "Compiling 'main' ... "
	@$(CC) $(CFLAGS) main.cpp
	@printf "done.\n"

all: $(EXEC)

.PHONY: clean

clean:
	@printf "Removing object files ... "
	@rm $(OBJS)
	@printf "done.\n"
	@printf "Removing executable ... "
	@rm $(EXEC)
	@printf "done.\n"

