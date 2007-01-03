# debug aid. the daemon will write a textual representation of trees to
# /tmp/lvrouted.tree*. this will take such a file and convert it to Tree.make
# statements, which can then be compiled and debugged with.
import re
import string
import sys

linere = re.compile("^(\t*)([0-9.]+) @ ([0-9]+)$")

def do_line(line):
	match = linere.match(line)
	if match.group(1) != None:
		numtabs = len(match.group(1))
	else:
		numtabs = 0
	addr = match.group(2)
	bandwidth = int(match.group(3))
	return addr, bandwidth, numtabs

def do_node(lines):
	addr, bw, numtabs = do_line(lines[0][:-1])
	children = []
	lines = lines[1:]

	if lines != []:
		n_numtabs = do_line(lines[0][:-1])[2]
		while lines != [] and n_numtabs > numtabs:
			child, lines = do_node(lines)
			children.append(child)
			if lines != []:
				n_numtabs = do_line(lines[0][:-1])[2]
	node = addr, bw, children
	return node, lines

def serialize(node):
	return "(Tree.make (Unix.inet_addr_of_string \"%s\") %d [%s])" % \
			(node[0], node[1], "; ".join(map(lambda n: serialize(n), node[2])))

node, lines = do_node(open(sys.argv[1]).readlines())
print serialize(node)
