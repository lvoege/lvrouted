/* implementation of the definitions in LowLevel.ml */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/ether.h>
#include <ifaddrs.h>
#ifndef __linux__
#include <net/if.h>
#include <net/if_dl.h>
#include <net/if_media.h>
#include <net/if_types.h>
#endif
#include <net/ethernet.h>
#include <arpa/inet.h>   
#include <net/route.h>
#include <netinet/in.h>
#include <stdio.h>
#include <unistd.h>
#include <unistd.h>

#include <bzlib.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static int sockfd = -1;

/* stolen from ocaml-3.08.1/byterun/weak.c */
#define None_val (Val_int(0))
#define Some_tag 0

static inline int bitcount(unsigned char i) {
	int c;
	for (c = 0; i; i >>= 1)
	  c += i & 1;
	return c;
}

CAMLprim value int_of_file_descr(value file_descr) {
	return file_descr;
}

/* mostly stolen from /usr/src/sbin/ifconfig/ifmedia.c */
static inline int iface_is_associated(const char *iface) {
#ifdef __linux__
	return 1;
#else
	struct ifmediareq ifmr;
	int *media_list;

	if (sockfd == -1)
	  sockfd = socket(PF_INET, SOCK_DGRAM, 0);
	memset(&ifmr, 0, sizeof(ifmr));
	strncpy(ifmr.ifm_name, iface, sizeof(ifmr.ifm_name));

	if (ioctl(sockfd, SIOCGIFMEDIA, (caddr_t)&ifmr) < 0 ||
	    ifmr.ifm_count == 0) {
		/* huh? interface can't report media state */
		fprintf(stderr, "warning: %s can't report media state\n", iface);
		return 1;
	}
	media_list = malloc(ifmr.ifm_count * sizeof(int));
	ifmr.ifm_ulist = media_list;

	if (ioctl(sockfd, SIOCGIFMEDIA, (caddr_t)&ifmr) < 0) {
		fprintf(stderr, "error ioctl()ing iface %s\n", iface);
		return 1;
	}
	free(media_list);
#if 0
	if ((ifmr.ifm_status & IFM_AVALID) &&
	    (ifmr.ifm_status & IFM_ACTIVE))
	  printf("iface %s i up!\n", iface);
#endif
	return (ifmr.ifm_status & IFM_AVALID) &&
	       (ifmr.ifm_status & IFM_ACTIVE);
#endif
}

CAMLprim value caml_iface_is_associated(value iface) {
	return Val_bool(iface_is_associated(String_val(iface)));
}

static inline in_addr_t get_addr(value addr) {
	if (string_length(addr) != 4) {
	  printf("ouch, length is %d!\n\n", (int)string_length(addr));
	  printf("\t%s\n", inet_ntoa(*(struct in_addr *)addr));
	  fflush(stdout);
	}
	assert(string_length(addr) == 4);
	return ntohl(((struct in_addr *)addr)->s_addr);
}

static inline in_addr_t mask_addr_impl(in_addr_t addr, int mask) {
	unsigned int bitmask;

	bitmask = -1 - ((1 << (32 - mask)) - 1);
	return addr & bitmask;
}

CAMLprim value mask_addr(value addr, value mask) {
	CAMLparam2(addr, mask);
	CAMLlocal1(result);
	in_addr_t res_addr;
	
	res_addr = htonl(mask_addr_impl(get_addr(addr),
			 Long_val(mask)));
	result = alloc_string(4);
	memcpy(String_val(result), &res_addr, 4);
	CAMLreturn(result);
}

CAMLprim value caml_daemon(value nochdir, value noclose) {
	daemon(Long_val(nochdir), Long_val(noclose));
	return Val_unit;
}

CAMLprim value caml_valaddr(value addr) {
	assert(string_length(addr) == 4);
	return Val_unit;
}

CAMLprim value string_compress(value s) {
	CAMLparam1(s);
	CAMLlocal1(result);
	int code, buflen;
	char *buffer;

	buffer = 0;
	buflen = string_length(s);
	do {
		buffer = realloc(buffer, buflen);
		code = BZ2_bzBuffToBuffCompress(
				buffer,
				&buflen,
				String_val(s),
				string_length(s),
				1,
				0,
				0);
		buflen *= 2;
	} while (code == BZ_OUTBUFF_FULL);	
	if (code == BZ_OK) {
		result = alloc_string(buflen);
		memcpy(String_val(result), buffer, buflen);	
		free(buffer);
		CAMLreturn(result);
	} else {
		failwith("Cannot handle error in string_compress");
	}
}

CAMLprim value string_decompress(value s) {
	CAMLparam1(s);
	CAMLlocal1(result);
	int code, buflen;
	char *buffer;

	buffer = 0;
	buflen = string_length(s) * 2;
	do {
		buffer = realloc(buffer, buflen);
		code = BZ2_bzBuffToBuffDecompress(
				buffer,
				&buflen,
				String_val(s),
				string_length(s),
				0,
				0);
		buflen *= 2;
	} while (code == BZ_OUTBUFF_FULL);	
	if (code == BZ_OK) {
		result = alloc_string(buflen);
		memcpy(String_val(result), buffer, buflen);	
		free(buffer);
		CAMLreturn(result);
	} else {
		failwith("Cannot handle error in string_decompress");
	}
}

int route_add(in_addr_t dest, int masklen, in_addr_t gw) {
#ifndef __linux__
	unsigned char buffer[sizeof(struct rt_msghdr) + 3 * sizeof(struct sockaddr_in)];
	int sockfd;
	struct rt_msghdr *msghdr;
	struct sockaddr_in *addr;

	assert(sockfd != -1);
	sockfd = socket(PF_ROUTE, SOCK_RAW, AF_INET);
	memset(buffer, 0, sizeof(buffer));
	msghdr = (struct rt_msghdr *)buffer;	
	msghdr->rtm_version = RTM_VERSION;
	msghdr->rtm_type = RTM_ADD;
	msghdr->rtm_addrs = RTA_DST | RTA_GATEWAY | RTA_NETMASK;
	msghdr->rtm_pid = getpid();
	msghdr->rtm_flags = RTF_UP | RTF_GATEWAY | RTF_STATIC;

	addr = (struct sockaddr_in *)(msghdr + 1);
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = dest;
	addr++;
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = -1 - ((1 << (32 - masklen)) - 1);
	addr++;
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = gw;

	write(sockfd, buffer, sizeof(buffer));
	close(sockfd);
#else
	assert(0);
#endif
	return 0;
}

CAMLprim value caml_route_add(value dest, value masklen, value gw) {
	return Val_long(route_add(get_addr(dest), Long_val(masklen), get_addr(gw)));
}

static int routemsg_add(unsigned char *buffer, int type,
			value dest, value masklen, value gw) {
#ifdef __linux__
	assert(0);
	return 0;
#else
	struct rt_msghdr *msghdr;
	struct sockaddr_in *addr;

	msghdr = (struct rt_msghdr *)buffer;	
	msghdr->rtm_version = RTM_VERSION;
	msghdr->rtm_type = type;
	msghdr->rtm_addrs = RTA_DST | RTA_GATEWAY | RTA_NETMASK;
	msghdr->rtm_pid = getpid();
	msghdr->rtm_flags = RTF_UP | RTF_GATEWAY | RTF_STATIC;

	addr = (struct sockaddr_in *)(msghdr + 1);
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = dest;
	addr++;
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = -1 - ((1 << (32 - masklen)) - 1);
	addr++;
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = gw;

	msghdr->rtm_msglen = ((unsigned char *)addr) - buffer;
	return msghdr->rtm_msglen;
#endif
}

CAMLprim value routes_commit(value deletes, value numdeletes,
			     value adds, value numadds) {
#ifdef __linux__
	assert(0);
#else
	int i, sockfd, buflen;
	unsigned char *buffer, *p;

	sockfd = socket(PF_ROUTE, SOCK_RAW, AF_INET);
	buflen = (numdeletes + numadds) * (sizeof(struct rt_msghdr) + 3 * sizeof(struct sockaddr_in));
	buffer = malloc(buflen);
	p = buffer;
	for (i = 0; i < Long_val(numdeletes); i++) {
		value v = Field(deletes, i);
		p += routemsg_add(p, RTM_DELETE, Field(v, 0), Field(v, 1), Field(v, 2));
	}
	for (i = 0; i < Long_val(numadds); i++) {
		value v = Field(adds, i);
		p += routemsg_add(p, RTM_ADD, Field(v, 0), Field(v, 1), Field(v, 2));
	}
	if (write(sockfd, buffer, buflen) < 0) {
		perror("oops");
	}
	free(buffer);
	close(sockfd);
#endif
	return Val_long(1);
}

CAMLprim value caml_ether_aton(value s, value mac) {
	struct ether_addr *ea;
	ea = ether_aton(String_val(s));
	if (ea == 0)
	  return Val_int(0);
	memcpy(String_val(mac), ea, ETHER_ADDR_LEN);
	return Val_int(1);
}

CAMLprim value caml_ether_ntoa(value s, value res) {
	char *p;

	p = ether_ntoa((struct ether_addr *)String_val(s));
	if (p == 0 || strlen(p) > 17)
	  return Val_int(0);
	memcpy(String_val(res), p, strlen(p));
	return Val_int(1);
}

CAMLprim value caml_getifaddrs(value unit) {
	CAMLparam1(unit);
	struct ifaddrs *ifap, *ifp;
	int i, num_ifaces;
	in_addr_t a;
	CAMLlocal4(result, tuple, addr, option);

	getifaddrs(&ifap);

	num_ifaces = 0;
	for (ifp = ifap; ifp; ifp = ifp->ifa_next)
	  num_ifaces += ifp->ifa_addr->sa_family == AF_INET;

	result = alloc(num_ifaces, 0);
	i = 0;
	for (ifp = ifap; ifp; ifp = ifp->ifa_next) {
		if (ifp->ifa_addr->sa_family != AF_INET)
		  continue;

		tuple = alloc_tuple(6);
		Store_field(tuple, 0, copy_string(ifp->ifa_name));
		Store_field(tuple, 1, copy_nativeint(ifp->ifa_flags));

		addr = alloc_string(sizeof(in_addr_t));
		a = ((struct sockaddr_in *)ifp->ifa_addr)->sin_addr.s_addr;
		memcpy(String_val(addr), &a, sizeof(in_addr_t));
		Store_field(tuple, 2, addr);
/* I'm not sure how the GC preprocessor magic would work with an inline
 * function here, so I'll use an ugly macro instead */
#define STORE_OPTIONAL_ADDR(x, idx) \
		if (ifp->x) { \
			addr = alloc_string(sizeof(in_addr_t)); \
			a = ((struct sockaddr_in *)ifp->x)->sin_addr.s_addr; \
			memcpy(String_val(addr), &a, sizeof(in_addr_t)); \
			option = alloc_small(1, Some_tag); \
			Store_field(option, 0, addr); \
		} else option = None_val; \
		Store_field(tuple, idx, option);
		STORE_OPTIONAL_ADDR(ifa_netmask, 3);
		STORE_OPTIONAL_ADDR(ifa_broadaddr, 4);
		STORE_OPTIONAL_ADDR(ifa_dstaddr, 5);

		Store_field(result, i, tuple);
		i++;
	}

	freeifaddrs(ifap);
	CAMLreturn(result);
}

CAMLprim value bits_in_inet_addr(value addr) {
	CAMLparam1(addr);
	CAMLlocal1(result);
	int c, len, i;
	char *s;

	len = string_length(addr);
	s = String_val(addr);
	for (i = c = 0; i < len; i++)
	  c += bitcount(s[i]);
	
	result = Val_int(c);
	CAMLreturn(result);
}

CAMLprim value caml_strstr(value big, value little) {
	char *p;

	p = strstr(String_val(big), String_val(little));
	return Val_int(p ? p - String_val(big) : -1);
}

CAMLprim value inet_addr_in_range(value addr) {
	CAMLparam1(addr);
	CAMLlocal1(result);
	in_addr_t a;

	a = get_addr(addr);
	result = Val_bool(mask_addr_impl(a, 12) == 0xac100000 &&
			  a < 0xac1fff00);
	CAMLreturn(result);
}

CAMLprim value get_addrs_in_block(value addr, value mask) {
	CAMLparam2(addr, mask);
	CAMLlocal2(result, em);
	in_addr_t a;
	int i, numaddrs;

	a = mask_addr_impl(get_addr(addr), Long_val(mask)) + 1;
	numaddrs = (1 << (32 - Long_val(mask))) - 2;
	result = alloc(numaddrs, 0);
	for (i = 0; i < numaddrs; i++) {
		em = alloc_string(sizeof(in_addr_t));
		*(in_addr_t *)(String_val(em)) = htonl(a);
		a++;
		Store_field(result, i, em);
	}
	CAMLreturn(result);
}
