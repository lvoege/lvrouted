# debug aid. the daemon will write a textual representation of trees to
# /tmp/lvrouted.tree*. this will take such a file and convert it to Tree.make
# statements, which can then be compiled and debugged with.
import re
import string
import sys

linere = re.compile("^(\t*)([0-9.]+)$")

def do_line(line):
	match = linere.match(line)
	if match.group(1) != None:
		numtabs = len(match.group(1))
	else:
		numtabs = 0
	addr = match.group(2)
	return addr, numtabs

def do_node(lines):
	addr, numtabs = do_line(lines[0][:-1])
	children = []
	lines = lines[1:]

	if lines != []:
		n_numtabs = do_line(lines[0][:-1])[1]
		while lines != [] and n_numtabs > numtabs:
			child, lines = do_node(lines)
			children.append(child)
			if lines != []:
				n_numtabs = do_line(lines[0][:-1])[1]
	node = addr, children
	return node, lines

def serialize(node):
	return "(Tree.make (Unix.inet_addr_of_string \"%s\") [%s])" % \
			(node[0], "; ".join(map(lambda n: serialize(n), node[1])))

node, lines = do_node(open(sys.argv[1]).readlines())
print serialize(node)
