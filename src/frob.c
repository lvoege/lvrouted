#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>


// 2001:610:6e2:1::/48

static u_int8_t prefix[] = { 0x20, 0x01, 0x06, 0x10, 0x06, 0xe2, 0x00, 0x00,
			     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
static u_int8_t proxy[] = { 172, 31, 255, 1 };

static int interlink_netmask = 28;

static void ip4_to_ip6(struct in_addr *a4, int masklen, struct in6_addr *a6, int *masklen6) {
	u_int32_t a4addr_int;
	u_int8_t a4addr[4];

	/* Make a copy, and the MSB is completely irrelevant to us */
	//a4addr_int = ntohl(a4->s_addr);
	a4addr_int = a4->s_addr;
	memcpy(a4addr, &a4addr_int, 4);
	a4addr[0] = 0;

	memcpy(a6->s6_addr, prefix, sizeof(*a6));
	if (masklen >= interlink_netmask) {
		*masklen6 = 128 - (32 - masklen);
	} else {
		*masklen6 = 64 - (24 - masklen);
	}
	if (masklen < interlink_netmask) {
		a4addr_int <<= 1;
		memcpy(a4addr, &a4addr_int, 4);
		// Keep .1's ending in ::1
		a4addr[3] >>= 1;
		a6->s6_addr[6] = 1 + a4addr[1];
		a6->s6_addr[7] = a4addr[2];
		a6->s6_addr[15] = a4addr[3];
	} else {
		memcpy(a6->s6_addr + 12, a4addr, 4);
	}
}


int main(int argc, char *argv[]) {
	struct in_addr a4;
	struct in6_addr a6;
	int masklen6;
	char buf[64];
	inet_aton(argv[1], &a4);
	ip4_to_ip6(&a4, atoi(argv[2]), &a6, &masklen6);
	printf("%s/%d\n", inet_ntop(AF_INET6, &a6, buf, 64), masklen6);
	return 0;
}
