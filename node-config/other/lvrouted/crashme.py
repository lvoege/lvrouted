# send random stuff to the routing daemon
import random
import socket
import sys

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
fd = open("/dev/urandom")

for i in range(int(sys.argv[1])):
	len = random.randint(0, 8096)
	print len
	blorp = fd.read(len)
	s.sendto(blorp, ("172.16.0.14", 12345))
