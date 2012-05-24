# turn a textual dump of a routing tree into GML, which some graph utilities
# use.
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

node, lines = do_node(open(sys.argv[1]).readlines())

def addr2id(addr):
	a = addr.split('.')
	return (int(a[0]) << 24) + (int(a[1]) << 16) + (int(a[2]) << 8) + (int(a[3]))

nodes = {} # id -> addr
edges = set() # (id, id)

def do_node(node):
	i = addr2id(node[0])
	if i not in nodes:
		nodes[i] = node
		for c in node[1]:
			cid = addr2id(c[0])
			edges.add( (i, cid) )
			do_node(c)

do_node(node)

print "graph ["
print "\tdirected 1"

keymap = {}
node_keys = sorted(nodes.keys())

i = 1
for k in sorted(nodes.keys()):
	keymap[k] = i
	print "\tnode ["
	print "\t\tid", i
	print "\t\tlabel \"%s\"" % nodes[k][0]
	print "\t]"

	i = i + 1

for e in edges:
	print "\tedge ["
	print "\t\tsource", keymap[e[0]]
	print "\t\ttarget", keymap[e[1]]
	print "\t]"
print "]"
