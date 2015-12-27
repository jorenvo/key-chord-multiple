#!/usr/bin/env python2

# Copyright 2015 (C) Joren Van Onder
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import sys
from itertools import permutations

regex_string = ""

if len(sys.argv) == 1:
    print 'example usage: grep -rn "$(./generate_permutations_regex.py ,hn ,k)" /usr/share/dict/american-english ~/code/github/*'
    exit(1)

for pattern in sys.argv[1:]:
    pattern_permutations = ["".join(permutation) for permutation in permutations(pattern)]
    regex_string += "\|".join(pattern_permutations) + "\|"

regex_string = regex_string[:-2]

print regex_string
