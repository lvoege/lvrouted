#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#ifndef __linux__
#include <net/if.h>
#include <net/if_dl.h>
#include <net/if_types.h>
#include <net/if_media.h>
#endif
#include <netinet/in.h>
#include <net/route.h>
#include <stdio.h>
#include <arpa/inet.h>   

#include <bzlib.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

CAMLprim value int_of_file_descr(value file_descr) {
	return file_descr;
}

/* mostly stolen from /usr/src/sbin/ifconfig/ifmedia.c */
static inline int check_iface(const char *iface) {
#ifdef __linux__
	return 1;
#else
	struct ifmediareq ifmr;
	int *media_list;
	static int sockfd = -1;

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

CAMLprim value caml_check_iface(value iface) {
	return Val_bool(check_iface(String_val(iface)));
}

static inline in_addr_t get_addr(value addr) {
	if (string_length(addr) != 4) {
	  printf("ouch, length is %d!\n\n", (int)string_length(addr));
	  printf("\t%s\n", inet_ntoa(*(struct in_addr *)addr));
	  fflush(stdout);
	}
	assert(string_length(addr) == 4);
	return ((struct in_addr *)addr)->s_addr;
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
	
	res_addr = htonl(mask_addr_impl(ntohl(get_addr(addr)),
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
