/* implementation of the definitions in LowLevel.ml */

/* There's plenty of code from FreeBSD's /usr/src here. For the BSD
 * license blurbs see the end of the file. */
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/sysctl.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <net/if.h>
#ifdef __FreeBSD__
#include <net/if_dl.h>
#include <net/if_media.h>
#include <net/if_types.h>
#include <netinet/if_ether.h>
#include <sys/param.h>
#if __FreeBSD_version < 502000
#include <net/if_ieee80211.h>
#include <dev/wi/wi_hostap.h>
#else
#include <net80211/ieee80211.h>
#include <net80211/ieee80211_ioctl.h>
#endif
#include <dev/wi/if_wavelan_ieee.h>
#include <dev/wi/if_wireg.h>
#elif defined(__linux__)
#include <netinet/ether.h>
#include <asm/types.h>
#include <linux/rtnetlink.h>
#endif
#include <net/ethernet.h>
#include <arpa/inet.h>   
#include <net/route.h>

#include <openssl/sha.h>

#include <bzlib.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

//#define DUMP_ROUTEPACKET

#define ROUNDUP(a) \
	((a) > 0 ? (1 + (((a) - 1) | (sizeof(long) - 1))) : sizeof(long))

/* stolen from ocaml-3.08.1/byterun/weak.c */
#define None_val (Val_int(0))
#define Some_tag 0

/* prepend element e before list l and return the new list. the empty
   list is to be passed as Val_int(0) */
static inline value prepend_listelement(value e, value l) {
	CAMLparam2(e, l);
	CAMLlocal1(cell);

	cell = alloc_small(2, 0);
	Field(cell, 0) = e;
	Field(cell, 1) = l;

	CAMLreturn(cell);
}

/* how many bits set in int i? */
static inline int bitcount(unsigned int i) {
	int c;
	for (c = 0; i; i >>= 1)
	  c += i & 1;
	return c;
}

CAMLprim value int_of_file_descr(value file_descr) {
	return file_descr;
}

/* mostly stolen from /usr/src/sbin/wicontrol/wicontrol.c */
static inline int iface_is_associated(const char *iface) {
#ifndef __FreeBSD__
	return 1;
#else
	struct ifreq ifr;
	int sockfd;
	struct wi_req wir;

	sockfd = socket(AF_INET, SOCK_DGRAM, 0);
	if (sockfd == -1)
	  failwith("socket for get_associated_stations");
	memset(&ifr, 0, sizeof(ifr));
	strncpy(ifr.ifr_name, String_val(iface), sizeof(ifr.ifr_name));
	ifr.ifr_data = (caddr_t)&wir;

	memset(&wir, 0, sizeof(wir));
	wir.wi_len = WI_MAX_DATALEN;
	wir.wi_type = WI_RID_CURRENT_SSID;

	if (ioctl(sockfd, SIOCGWAVELAN, &ifr) == -1) {
		close(sockfd);
		failwith("SIOCGWAVELAN");
	}
	close(sockfd);
	return wir.wi_val[0];
#endif
}

CAMLprim value caml_iface_is_associated(value iface) {
	CAMLparam1(iface);
	CAMLreturn(Val_bool(iface_is_associated(String_val(iface))));
}

static inline in_addr_t get_addr(value addr) {
	if (string_length(addr) != 4)
	  failwith("I only support IPv4 for now");
	return ntohl(((struct in_addr *)(String_val(addr)))->s_addr);
}

static inline unsigned int bitmask(int masklen) {
	return masklen ? 0xffffffff << (32 - masklen) : 0;
}

static inline in_addr_t mask_addr_impl(in_addr_t addr, int masklen) {
	return addr & bitmask(masklen);
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
	CAMLparam2(nochdir, noclose);
	if (daemon(Long_val(nochdir), Long_val(noclose)) < 0)
	  failwith(strerror(errno));
	CAMLreturn(Val_unit);
}

CAMLprim value string_compress(value s) {
	assert(0); /* TESTME first */
	return Val_unit;
#if 0
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
		free(buffer);
		failwith("Cannot handle error in string_compress");
	}
#endif
}

CAMLprim value string_decompress(value s) {
	assert(0); /* TESTME first */
	return Val_unit;
#if 0
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
		free(buffer);
		failwith("Cannot handle error in string_decompress");
	}
#endif
}

#ifdef __FreeBSD__
static int routemsg_add(unsigned char *buffer, int type,
			value dest, value masklen, value gw) {
	struct rt_msghdr *msghdr;
	struct sockaddr_in *addr;
	static int seq = 1;
	unsigned char *p;
	CAMLparam3(dest, masklen, gw);

	msghdr = (struct rt_msghdr *)buffer;	
	memset(msghdr, 0, sizeof(struct rt_msghdr));
	msghdr->rtm_version = RTM_VERSION;
	msghdr->rtm_type = type;
	msghdr->rtm_addrs = RTA_DST | RTA_GATEWAY | RTA_NETMASK;
	msghdr->rtm_pid = 0;
	msghdr->rtm_flags = RTF_UP | RTF_GATEWAY | RTF_DYNAMIC;
	msghdr->rtm_seq = seq++;

	addr = (struct sockaddr_in *)(msghdr + 1);
#define ADD(x) \
	memset(addr, 0, sizeof(struct sockaddr_in));	\
	addr->sin_len = sizeof(struct sockaddr_in);	\
	addr->sin_family = AF_INET;			\
	addr->sin_addr.s_addr = htonl(x);		\
	addr++;

	ADD(mask_addr_impl(get_addr(dest), Long_val(masklen)));
	ADD(get_addr(gw));
	ADD(bitmask(Long_val(masklen)));

	/*
	 * for some reason, the sin_len for the netmask's sockaddr_in should
	 * not be the length of the sockaddr_in at all, but the offset of
	 * the sockaddr_in's last non-zero byte. I don't know why. From
	 * the last byte of the sockaddr_in, step backwards until there's a
	 * non-zero byte under the cursor, then set the length.
	 */
	addr--;
	for (p = (unsigned char *)(addr + 1) - 1; p > (unsigned char *)addr; p--)
	  if (*p) {
		addr->sin_len = p - (unsigned char *)addr + 1;
		break;
	  }
	addr->sin_family = 0; /* just to be totally in sync with /usr/sbin/route */

	msghdr->rtm_msglen = (unsigned char *)addr +
				ROUNDUP(addr->sin_len)
				- buffer;
	
	CAMLreturn(msghdr->rtm_msglen);
}
#endif

CAMLprim value routes_commit(value rtsock,
			value deletes, value adds, value changes) {
	CAMLparam4(rtsock, deletes, adds, changes);
	CAMLlocal5(result, adderrs, delerrs, cherrs, tuple);
	CAMLlocal1(v);
#ifndef __FreeBSD__
	assert(0);
#else
	int sockfd, buflen, len;
	unsigned char *buffer;
#ifdef DUMP_ROUTEPACKET
	FILE *debug;
#endif

	sockfd = Long_val(rtsock);
	buflen = sizeof(struct rt_msghdr) + 3 * sizeof(struct sockaddr_in);
	buffer = malloc(buflen);
	if (buffer == 0)
	  failwith("malloc");

	for (adderrs = Val_int(0); adds != Val_int(0); adds = Field(adds, 1)) {
		v = Field(adds, 0);
		len = routemsg_add(buffer, RTM_ADD, Field(v, 0), Field(v, 1), Field(v, 2));
#ifdef DUMP_ROUTEPACKET
		debug = fopen("/tmp/packet.lvrouted", "w");
		fwrite(buffer, 1, len, debug);
		fclose(debug);
#endif
		if (write(sockfd, buffer, len) < 0) {
			tuple = alloc_tuple(2);
			Store_field(tuple, 0, v);
			Store_field(tuple, 1, copy_string(strerror(errno)));
			adderrs = prepend_listelement(tuple, adderrs);
		}
	}

	for (delerrs = Val_int(0); deletes != Val_int(0); deletes = Field(deletes, 1)) {
		v = Field(deletes, 0);
		len = routemsg_add(buffer, RTM_DELETE, Field(v, 0), Field(v, 1), Field(v, 2));
		if (write(sockfd, buffer, len) < 0) {
			tuple = alloc_tuple(2);
			Store_field(tuple, 0, v);
			Store_field(tuple, 1, copy_string(strerror(errno)));
			delerrs = prepend_listelement(tuple, delerrs);
		}
	}

	for (cherrs = Val_int(0); changes != Val_int(0); changes = Field(changes, 1)) {
		v = Field(changes, 0);
		len = routemsg_add(buffer, RTM_CHANGE, Field(v, 0), Field(v, 1), Field(v, 2));
		if (write(sockfd, buffer, len) < 0) {
			tuple = alloc_tuple(2);
			Store_field(tuple, 0, v);
			Store_field(tuple, 1, copy_string(strerror(errno)));
			cherrs = prepend_listelement(tuple, cherrs);
		}
	}

	free(buffer);
	result = alloc_tuple(3);
	Store_field(result, 0, delerrs);
	Store_field(result, 1, adderrs);
	Store_field(result, 2, cherrs);
#endif
	CAMLreturn(result);
}

CAMLprim value caml_ether_aton(value s, value mac) {
	CAMLparam2(s, mac);
	CAMLlocal1(res);
	struct ether_addr *ea;

	ea = ether_aton(String_val(s));
	if (ea == 0)
	  res = Val_int(0);
	else {
		memcpy(String_val(mac), ea, ETHER_ADDR_LEN);
		res = Val_int(1);
	}
	CAMLreturn(res);
}

CAMLprim value caml_ether_ntoa(value s, value res) {
	CAMLparam2(s, res);
	CAMLlocal1(rescode);
	char *p;

	p = ether_ntoa((struct ether_addr *)String_val(s));
	if (p == 0 || strlen(p) > 17)
	  rescode = Val_int(0);
	else {
		memcpy(String_val(res), p, strlen(p));
		rescode = Val_int(1);
	}
	CAMLreturn(rescode);
}

CAMLprim value caml_getifaddrs(value unit) {
	CAMLparam1(unit);
	struct ifaddrs *ifap, *ifp;
	in_addr_t a;
	CAMLlocal4(result, tuple, addr, option);

	if (getifaddrs(&ifap) == -1)
	  failwith("getifaddrs");

	result = Val_int(0);
	for (ifp = ifap; ifp; ifp = ifp->ifa_next) {
		if (ifp->ifa_addr->sa_family != AF_INET)
		  continue;	/* not interested */

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

		result = prepend_listelement(tuple, result);
	}

	freeifaddrs(ifap);
	CAMLreturn(result);
}

CAMLprim value bits_in_inet_addr(value addr) {
	CAMLparam1(addr);

	CAMLreturn(Val_int(bitcount(get_addr(addr))));
}

CAMLprim value caml_strstr(value big, value little) {
	CAMLparam2(big, little);
	char *p;

	p = strstr(String_val(big), String_val(little));
	CAMLreturn(Val_int(p ? p - String_val(big) : -1));
}

CAMLprim value get_addrs_in_block(value addr, value mask) {
	CAMLparam2(addr, mask);
	CAMLlocal2(result, em);
	in_addr_t a;
	int i, numaddrs;

	a = mask_addr_impl(get_addr(addr), Long_val(mask)) + 1;
	numaddrs = (1 << (32 - Long_val(mask))) - 2;
	if (numaddrs < 0) /* happens if the netmask was 32 to begin with */
	  numaddrs = 0;
	result = Val_int(0);
	for (i = 0; i < numaddrs; i++) {
		em = alloc_string(sizeof(in_addr_t));
		*(in_addr_t *)(String_val(em)) = htonl(a);
		a++;
		result = prepend_listelement(em, result);
	}
	CAMLreturn(result);
}

/* stolen from /usr/src/usr.sbin/arp/arp.c */
CAMLprim value get_arp_entries(value unit) {
	CAMLparam1(unit);
	CAMLlocal4(result, tuple, ipaddr, macaddr);

	result = Val_int(0);
#ifdef __FreeBSD__
	int mib[6], numentries;
	size_t needed;
	char *lim, *buf, *next;
	struct rt_msghdr *rtm;
	struct sockaddr_inarp *sin2;
	struct sockaddr_dl *sdl;
	char ifname[IF_NAMESIZE];

	mib[0] = CTL_NET;
	mib[1] = PF_ROUTE;
	mib[2] = 0;
	mib[3] = AF_INET;
	mib[4] = NET_RT_FLAGS;
	mib[5] = RTF_LLINFO;
	if (sysctl(mib, 6, NULL, &needed, 0, 0) < 0)
	  failwith("fetch of arp table size");
	if (needed) {
		buf = malloc(needed);
		if (buf == 0)
		  failwith("malloc");
		if (sysctl(mib, 6, buf, &needed, 0, 0) < 0) {
			free(buf);
			failwith("fetch of arp table");
		}
		numentries = 0;
		lim = buf + needed;
		for (next = buf; next < lim; next += rtm->rtm_msglen) {
			rtm = (struct rt_msghdr *)next;
			sin2 = (struct sockaddr_inarp *)(rtm + 1);
			sdl = (struct sockaddr_dl *)((char *)sin2 +
				ROUNDUP(sin2->sin_len)
			);
			if (sdl->sdl_alen == 0)
			  continue; /* incomplete entry */
			if (sdl->sdl_type != IFT_ETHER ||
			    sdl->sdl_alen != ETHER_ADDR_LEN)
			  continue; /* huh? */
			if (if_indextoname(sdl->sdl_index, ifname) == 0)
			  continue; /* entry without interface? shouldn't happen */
			macaddr = alloc_string(ETHER_ADDR_LEN);
			memcpy(String_val(macaddr), ((struct ether_addr *)LLADDR(sdl))->octet, ETHER_ADDR_LEN);

			ipaddr = alloc_string(sizeof(in_addr_t));
			memcpy(String_val(ipaddr), &sin2->sin_addr.s_addr, sizeof(in_addr_t));

			tuple = alloc_tuple(3);
			Store_field(tuple, 0, copy_string(ifname));
			Store_field(tuple, 1, ipaddr);
			Store_field(tuple, 2, macaddr);

			result = prepend_listelement(tuple, result);
		}
		free(buf);
	}
#endif

	CAMLreturn(result);
}

CAMLprim value get_associated_stations(value iface) {
	CAMLparam1(iface);	
	CAMLlocal2(result, mac);
#ifdef __FreeBSD__
	struct ifreq ifr;
	int sockfd, i;
#if __FreeBSD_version >= 502000
	int n;
	struct wi_req wir;
	struct wi_apinfo *s;

	sockfd = socket(AF_INET, SOCK_DGRAM, 0);
	if (sockfd == -1)
	  failwith("socket for get_associated_stations");
	memset(&ifr, 0, sizeof(ifr));
	strncpy(ifr.ifr_name, String_val(iface), sizeof(ifr.ifr_name));
	ifr.ifr_data = (caddr_t)&wir;

	memset(&wir, 0, sizeof(wir));
	wir.wi_len = WI_MAX_DATALEN;
	wir.wi_type = WI_RID_READ_APS;

	if (ioctl(sockfd, SIOCGWAVELAN, &ifr) == -1) {
		close(sockfd);
		failwith("SIOCGWAVELAN");
	}

	n = *(int *)(wir.wi_val);
	result = alloc_tuple(n);
	s = (struct wi_apinfo *)(wir.wi_val + sizeof(int));
	for (i = 0; i < n; i++) {
		mac = alloc_string(6);
		memcpy(String_val(mac), s->bssid, ETHER_ADDR_LEN);
		s++;
		Store_field(result, i, mac);
	}
	close(sockfd);
#else
	struct hostap_getall    reqall;
	struct hostap_sta       stas[WIHAP_MAX_STATIONS];

	sockfd = socket(AF_INET, SOCK_DGRAM, 0);
	if (sockfd == -1)
	  failwith("socket for get_associated_stations");
	memset(&ifr, 0, sizeof(ifr));
	strncpy(ifr.ifr_name, String_val(iface), sizeof(ifr.ifr_name));
	ifr.ifr_data = (caddr_t) &reqall;

	memset(&reqall, 0, sizeof(reqall));
	reqall.size = sizeof(stas);
	reqall.addr = stas;

	memset(&stas, 0, sizeof(stas));

	if (ioctl(sockfd, SIOCHOSTAP_GETALL, &ifr) < 0) {
		close(sockfd);
		failwith("SIOCHOSTAP_GETALL");
	}
	result = alloc_tuple(reqall.nstations);
	for (i = 0; i < reqall.nstations; i++) {
		mac = alloc_string(ETHER_ADDR_LEN);
		memcpy(String_val(mac), stas[i].addr, ETHER_ADDR_LEN);
		Store_field(result, i, mac);
	}
	close(sockfd);
#endif
#else
	result = alloc_tuple(0);
#endif
	CAMLreturn(result);
}

CAMLprim value routes_fetch(value unit) {
	CAMLparam1(unit);
	CAMLlocal3(result, tuple, addr);
#ifdef __linux__
	assert(0);
	// parse /proc/net/route
#elif defined(__FreeBSD__)
	int sockfd, count;
	int mib[6] = { CTL_NET, PF_ROUTE, 0, 0, NET_RT_DUMP, 0 };
	size_t needed;
	unsigned char *buf, *p, *lim, *p2, *lim2;
	struct rt_msghdr *rtm;
	struct sockaddr_in *sin;

	sockfd = socket(AF_INET, SOCK_DGRAM, 0);
	if (sockfd == -1)
	  failwith("socket for routes_fetch");
	shutdown(sockfd, SHUT_RD);
	if (sysctl(mib, 6, 0, &needed, 0, 0) == -1) {
		close(sockfd);
		failwith("fetch of route table size");
	}
	buf = malloc(needed);
	if (buf == 0) {
		close(sockfd);
		failwith("malloc in routes_fetch");
	}
	if (sysctl(mib, 6, buf, &needed, 0, 0) == -1) {
		close(sockfd);
		free(buf);
		failwith("route retrieval in routes_fetch");
	}

	lim = buf + needed;
	result = Val_int(0);
	for (p = buf; p < lim; p += rtm->rtm_msglen) {
		rtm = (struct rt_msghdr *)p;
		if ((rtm->rtm_flags & RTF_GATEWAY) == 0 ||
		    (rtm->rtm_addrs & RTA_NETMASK) == 0)
		  continue;
		sin = (struct sockaddr_in *)(rtm + 1);
		if (sin->sin_family != AF_INET)
		  continue;

		tuple = alloc_tuple(3);

		/* fill in destination */
		addr = alloc_string(sizeof(in_addr_t));
		*(in_addr_t *)(String_val(addr)) = sin->sin_addr.s_addr;
		Store_field(tuple, 0, addr);
		sin++;
	
		/* gateway */
		addr = alloc_string(sizeof(in_addr_t));
		*(in_addr_t *)(String_val(addr)) = sin->sin_addr.s_addr;
		Store_field(tuple, 2, addr);
		sin++;

		/* netmask. bwurk, why the fsck all this fudging with
		   ->sin_len?! */
		count = 0;
		lim2 = (unsigned char *)sin + sin->sin_len;
		p2 = (unsigned char *)&sin->sin_addr.s_addr;
		for (; p2 < lim2; p2++)
		  count += bitcount(*p2);
		Store_field(tuple, 1, Val_long(count));

		result = prepend_listelement(tuple, result);
	}
	free(buf);
	close(sockfd);
#else
	result = Val_int(0);
#endif
	CAMLreturn(result);
}

CAMLprim value sha_string(value string) {
	CAMLparam1(string);
	CAMLlocal1(result);

	result = alloc_string(SHA_DIGEST_LENGTH);
	SHA1(String_val(string), string_length(string), String_val(result));
	CAMLreturn(result);
}

CAMLprim value hexdump_string(value s) {
	CAMLparam1(s);
	CAMLlocal1(result);
	int i, len;
	unsigned char *sp, *rp;

	len = string_length(s);
	result = alloc_string(2 * len);
	sp = String_val(s);
	rp = String_val(result);
	for (i = 0; i < len; i++) {
#define DIGIT(x) ((x) + ((x) < 10 ? '0' : 'a' - 10))
		*rp++ = DIGIT(*sp >> 4);
		*rp++ = DIGIT(*sp & 15);
		sp++;
	}
	CAMLreturn(result);
}

CAMLprim value caml_syslog(value pri, value s) {
	CAMLparam2(pri, s);
	int tmp[5] = {
		LOG_CRIT,
		LOG_ERR,
		LOG_WARNING,
		LOG_INFO,
		LOG_DEBUG
	};
	if (Long_val(pri) >= 5)
	  failwith("Invalid priority for syslog()");
	syslog(tmp[Long_val(pri)], String_val(s));
	CAMLreturn(Val_unit);
}

CAMLprim value caml_pack_int(value i) {
	CAMLparam1(i);
	CAMLlocal1(res);

	res = alloc_string(sizeof(int));
	*(int *)(String_val(res)) = Long_val(i);
	CAMLreturn(res);
}

CAMLprim value caml_unpack_int(value s) {
	CAMLparam1(s);
	CAMLreturn(Val_int(*(int *)(String_val(s))));
}

/* Store a node into a buffer. It is enough to store the node contents
 * (the address in this case) plus the number of children and recurse.
 * Since the 172.16.0.0/12 range only uses 20 bits, the number of children
 * can be packed into the 12 fixed bits.
 * 
 * It is conceivable for our nodes to have more than 16 addresses to
 * propagate, so packing a node in 24 bits instead of 32 would probably
 * be pushing our luck.
 */
static unsigned char *tree_to_string_rec(value node, unsigned char *buffer) {
	CAMLparam1(node);
	CAMLlocal1(t);
	unsigned char *buffer_tmp;
	int i;

	buffer_tmp = buffer + sizeof(int);
	i = 0;
	for (t = Field(node, 1); t != Val_int(0); t = Field(t, 1)) {
		buffer_tmp = tree_to_string_rec(Field(t, 0), buffer_tmp);
		i++;
	}
	i <<= 20;
	i |= get_addr(Field(node, 0)) & ((1 << 20) - 1);
	*(int *)buffer = htonl(i);
	CAMLreturn(buffer_tmp);
}

CAMLprim value tree_to_string(value node) {
	CAMLparam1(node);
	CAMLlocal1(result);
	unsigned char *buffer, *t;

	buffer = malloc(65536);
	t = tree_to_string_rec(node, buffer);
	result = alloc_string(t - buffer);
	memcpy(String_val(result), buffer, t - buffer);
	free(buffer);
	CAMLreturn(result);
}

static value string_to_tree_rec(unsigned char **pp,
				 unsigned char *limit) {
	value a, node, child, chain;
	int i;

	if (*pp >= limit)
	  failwith("faulty packet");
	i = ntohl(*(int *)(*pp));
	*pp += sizeof(int);
	a = alloc_string(4);
	*(int *)(String_val(a)) = htonl(0xac100000 + (i & ((1 << 20) - 1)));
	node = alloc_small(2, 0);
	Field(node, 0) = a;
	Field(node, 1) = Val_int(0);

	/* new children get hooked on the second field of chain. by luck,
	 * the list itself is in the second field of node, so chain can be
	 * assigned to node. if the node struct changes so the list of
	 * children is no longer the second field, this must be changed */
	chain = node;
	for (i >>= 20; i > 0; i--) {
		child = alloc_small(2, 0);
		Field(child, 0) = Val_unit;
		Field(child, 1) = Val_int(0);
		modify(&Field(child, 0), string_to_tree_rec(pp, limit));
		modify(&Field(chain, 1), child);
		chain = child;
	}
	return node;
}

CAMLprim value string_to_tree(value s) {
	CAMLparam1(s);
	CAMLlocal2(res, a);
	unsigned char *p;

	p = String_val(s);
	CAMLreturn(string_to_tree_rec(&p, p + string_length(s)));
}

CAMLprim value open_rtsock(value unit) {
	CAMLparam1(unit);
	int sockfd;
	sockfd = socket(PF_ROUTE, SOCK_RAW, 0);
	if (sockfd == -1)
	  failwith("Routing socket");
	CAMLreturn(Val_int(sockfd));
}

#ifdef __FreeBSD__ 
static value get_routemsg(struct ifa_msghdr *ifa, int tag) {
	CAMLparam0();
	CAMLlocal2(res, addr);
	char *p, ifnam[IFNAMSIZ];
	int i, masklen, okay_to_add;
	struct sockaddr_in *sin;

	if (if_indextoname(ifa->ifam_index, ifnam) == 0)
	  failwith("Unknown interface in read_routemsg");
	p = (char *)(ifa + 1);
	okay_to_add = 1;
	for (i = 1; i && okay_to_add; i <<= 1) {
		if (ifa->ifam_addrs & i) {
			sin = (struct sockaddr_in *)p;
			switch (i) {
				case RTA_NETMASK:
					if (sin->sin_family != AF_INET)
					  okay_to_add = 0;
					else
					  masklen = bitcount(sin->sin_addr.s_addr);
					break;
				case RTA_IFA:
					if (sin->sin_family != AF_INET)
					  okay_to_add = 0;
					else {
						addr = alloc_string(4);
						memcpy(String_val(addr), &sin->sin_addr.s_addr, sizeof(in_addr_t));
					}
					break;
			}
			p += ROUNDUP(sin->sin_len);
		}
	}
	if (okay_to_add) {
		res = alloc_small(3, tag);
		Field(res, 0) = copy_string(ifnam);
		Field(res, 1) = addr;
		Field(res, 2) = Val_int(masklen);
	} else res = Val_int(0);
	CAMLreturn(res);
}
#endif

/* read a routing message from the given file descriptor and return what it
 * said. */
CAMLprim value read_routemsg(value fd) {
	CAMLparam1(fd);
#ifdef __FreeBSD__
	CAMLlocal2(res, addr);
	char *p, *buffer;
	int buflen, toread, numread;
	struct rt_msghdr *rtm;
	struct ifa_msghdr *ifa;

	buflen = toread = 1024;
	buffer = p = malloc(buflen);
	if (buffer == 0)
	  failwith("malloc in read_routemsg");
	while ((numread = read(Long_val(fd), p, toread)) == toread) {
		numread += p - buffer;
		buflen += 1024;
		buffer = realloc(buffer, buflen);
		p = buffer + numread;
	}
	
	rtm = (struct rt_msghdr *)buffer;
	ifa = (struct ifa_msghdr *)buffer;
	switch (rtm->rtm_type) {
		case RTM_NEWADDR:
			res = get_routemsg(ifa, 0);
			break;
		case RTM_DELADDR:
			res = get_routemsg(ifa, 1);
			break;
		default:
			res = Val_int(0);
	}
	free(buffer);
	CAMLreturn(res);
#else
	CAMLreturn(Val_int(0));
#endif
}

/* from wicontrol.c: */
/*
 * Copyright (c) 1997, 1998, 1999
 *	Bill Paul <wpaul@ctr.columbia.edu>.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Bill Paul.
 * 4. Neither the name of the author nor the names of any co-contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY Bill Paul AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL Bill Paul OR THE VOICES IN HIS HEAD
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

/* from arp.c: */
/*
 * Copyright (c) 1984, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sun Microsystems, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
