# tooltje om de adressen in de door lvrouted gedumpte tree's naar
# namen te resolven
import re
import socket
import sys

def f(match):
	try:
		return socket.gethostbyaddr(match.group(1))[0].replace('.wleiden.net', '')
	except:
		return match.group(1)

for l in open(sys.argv[1]).readlines():
	print re.sub("(172.[0-9\.]+)", f, l[:-1])
