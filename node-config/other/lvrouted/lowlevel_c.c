/* implementation of the definitions in LowLevel.ml */
/* There's plenty of code from FreeBSD's /usr/src here. For the BSD
 * license blurbs see the end of the file. */
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/sysctl.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#ifndef __linux__
#include <net/if.h>
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
#else
#include <netinet/ether.h>
#endif
#include <net/ethernet.h>
#include <arpa/inet.h>   
#include <net/route.h>

#include <bzlib.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static int sockfd = -1;

#if __FreeBSD_version < 502000
#define ROUNDUP(a) \
	((a) > 0 ? (1 + (((a) - 1) | (sizeof(long) - 1))) : sizeof(long))
#endif

/* stolen from ocaml-3.08.1/byterun/weak.c */
#define None_val (Val_int(0))
#define Some_tag 0

static inline void ensure_sockfd() {
	if (sockfd == -1)
	  sockfd = socket(AF_INET, SOCK_DGRAM, 0);
}

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

	ensure_sockfd();
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

static int routemsg_add(unsigned char *buffer, int type,
			value dest, value masklen, value gw) {
#ifdef __linux__
	assert(0);
	return 0;
#else
	struct rt_msghdr *msghdr;
	struct sockaddr_in *addr;
	static int seq = 0;

	msghdr = (struct rt_msghdr *)buffer;	
	memset(msghdr, 0, sizeof(struct rt_msghdr));
	msghdr->rtm_version = RTM_VERSION;
	msghdr->rtm_type = type;
	msghdr->rtm_addrs = RTA_DST | RTA_GATEWAY | RTA_NETMASK;
	msghdr->rtm_pid = getpid();
	msghdr->rtm_flags = RTF_UP | RTF_GATEWAY | RTF_STATIC;
	msghdr->rtm_seq = seq++;

	addr = (struct sockaddr_in *)(msghdr + 1);
	memset(addr, 0, sizeof(struct sockaddr_in));
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = htonl(get_addr(dest));
	addr++;
	memset(addr, 0, sizeof(struct sockaddr_in));
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = -1 - ((1 << (32 - Long_val(masklen))) - 1);
	addr++;
	memset(addr, 0, sizeof(struct sockaddr_in));
	addr->sin_len = sizeof(struct sockaddr_in);
	addr->sin_family = AF_INET;
	addr->sin_addr.s_addr = htonl(get_addr(gw));
	addr++;

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
	unsigned char *buffer;

	sockfd = socket(PF_ROUTE, SOCK_RAW, AF_INET);
	if (sockfd == -1)
	  failwith("routing socket");
	buflen = sizeof(struct rt_msghdr) + 3 * sizeof(struct sockaddr_in);
	buffer = malloc(buflen);
	if (buffer == 0)
	  failwith("malloc");
	for (i = 0; i < Long_val(numdeletes); i++) {
		value v = Field(deletes, i);
		routemsg_add(buffer, RTM_DELETE, Field(v, 0), Field(v, 1), Field(v, 2));
		if (write(sockfd, buffer, buflen) < 0)
		  failwith(strerror(errno));
	}
	for (i = 0; i < Long_val(numadds); i++) {
		value v = Field(adds, i);
		routemsg_add(buffer, RTM_ADD, Field(v, 0), Field(v, 1), Field(v, 2));
		if (write(sockfd, buffer, buflen) < 0)
		  failwith(strerror(errno));
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

/* stolen from /usr/src/usr.sbin/arp/arp.c */
CAMLprim value get_arp_entries(value unit) {
	CAMLparam1(unit);
	CAMLlocal4(result, tuple, ipaddr, macaddr);

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
		if (sysctl(mib, 6, buf, &needed, 0, 0) < 0)
		  failwith("fetch of arp table");
		numentries = 0;
		lim = buf + needed;
		for (next = buf; next < lim; next += rtm->rtm_msglen) {
			rtm = (struct rt_msghdr *)next;
			sin2 = (struct sockaddr_inarp *)(rtm + 1);
			sdl = (struct sockaddr_dl *)((char *)sin2 +
#if __FreeBSD_version < 502000
				ROUNDUP(sin2->sin_len)
#else
				SA_SIZE(sin2)
#endif
			);
			if (sdl->sdl_alen == 0)
			  continue; /* incomplete entry */
			if (sdl->sdl_type != IFT_ETHER ||
			    sdl->sdl_alen != ETHER_ADDR_LEN)
			  continue; /* huh? */
			if (if_indextoname(sdl->sdl_index, ifname) == 0)
			  continue; /* entry without interface? shouldn't happen */
			numentries++;
		}
		result = alloc_tuple(numentries);
		numentries = 0;
		for (next = buf; next < lim; next += rtm->rtm_msglen) {
			rtm = (struct rt_msghdr *)next;
			sin2 = (struct sockaddr_inarp *)(rtm + 1);
			sdl = (struct sockaddr_dl *)((char *)sin2 +
#if __FreeBSD_version < 502000
				ROUNDUP(sin2->sin_len)
#else
				SA_SIZE(sin2)
#endif
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

			Store_field(result, numentries, tuple);
			numentries++;
		}
		free(buf);
	} else result = alloc_tuple(0);

	CAMLreturn(result);
}

CAMLprim value get_associated_stations(value iface) {
	CAMLparam1(iface);	
	CAMLlocal2(result, mac);
	struct ifreq ifr;
	int i;
#if __FreeBSD_version >= 502000
	int n;
	struct wi_req wir;
	struct wi_apinfo *s;

	ensure_sockfd();
	memset(&ifr, 0, sizeof(ifr));
	strncpy(ifr.ifr_name, String_val(iface), sizeof(ifr.ifr_name));
	ifr.ifr_data = (caddr_t)&wir;

	memset(&wir, 0, sizeof(wir));
	wir.wi_len = WI_MAX_DATALEN;
	wir.wi_type = WI_RID_READ_APS;

	if (ioctl(sockfd, SIOCGWAVELAN, &ifr) == -1)
	  failwith("SIOCGWAVELAN");

	n = *(int *)(wir.wi_val);
	result = alloc_tuple(n);
	s = (struct wi_apinfo *)(wir.wi_val + sizeof(int));
	for (i = 0; i < n; i++) {
		mac = alloc_string(6);
		memcpy(String_val(mac), s->bssid, ETHER_ADDR_LEN);
		s++;
		Store_field(result, i, mac);
	}
#else
	struct hostap_getall    reqall;
	struct hostap_sta       stas[WIHAP_MAX_STATIONS];

	ensure_sockfd();
	memset(&ifr, 0, sizeof(ifr));
	strncpy(ifr.ifr_name, String_val(iface), sizeof(ifr.ifr_name));
	ifr.ifr_data = (caddr_t) &reqall;

	memset(&reqall, 0, sizeof(reqall));
	reqall.size = sizeof(stas);
	reqall.addr = stas;

	memset(&stas, 0, sizeof(stas));

	if (ioctl(sockfd, SIOCHOSTAP_GETALL, &ifr) < 0)
	  failwith("SIOCHOSTAP_GETALL");
	result = alloc_tuple(reqall.nstations);
	for (i = 0; i < reqall.nstations; i++) {
		mac = alloc_string(ETHER_ADDR_LEN);
		memcpy(String_val(mac), stas[i].addr, ETHER_ADDR_LEN);
		Store_field(result, i, mac);
	}
#endif
	CAMLreturn(result);
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
