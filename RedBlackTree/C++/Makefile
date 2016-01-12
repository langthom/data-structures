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

CC       = g++
LD       = g++
CFLAGS   = -c -g -std=c++11 $(WARNINGS) 
LFLAGS   = -o $(EXEC)
WARNINGS = -Wall -pedantic -Wextra
EXEC     = rbtree_test
OBJS     = Main.o

all: $(EXEC)

$(EXEC): $(OBJS)
	$(LD) $(OBJS) $(LFLAGS)

Main.o: RedBlackTree.h Main.cpp
	$(CC) $(CFLAGS) Main.cpp

clean:
	rm $(EXEC)
	rm *.o

.PHONY: clean


