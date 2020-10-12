#include <sys/errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/timeb.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <ctype.h>
#include <pthread.h>

#include "zlogc.h"

void gen_timestamp(char *buf)
{
	time_t t = time(NULL);
	struct tm tm;
	struct timeval tv;
	localtime_r(&t, &tm);
	gettimeofday(&tv, NULL);
    sprintf(buf, "%02d/%02d/%04d %02d:%02d:%02d.%03d (%lu) ",
			tm.tm_mon + 1, tm.tm_mday, tm.tm_year + 1900,
            tm.tm_hour, tm.tm_min, tm.tm_sec, (int)tv.tv_usec/1000,
            (unsigned long)(pthread_self()));
}

void check_append_newline(char *buf)
{
	if (buf[strlen(buf)-1] != '\n')
		strcat(buf, "\n");
}

int zlog_file(const char *fmt, ...)
{
	char buf[256];
	va_list ap;
	va_start(ap, fmt);
	gen_timestamp(buf);
	vsnprintf(buf + strlen(buf), 200, fmt, ap);
	va_end(ap);
	check_append_newline(buf);

	int fd = open("/tmp/zlog", O_CREAT | O_RDWR | O_APPEND, 0644);
	CHECK_ERR(fd > 0);
	int rc = write(fd, buf, strlen(buf));
	CHECK_ERR(rc >=0);
	close(fd);
	return 0;
}

int zlog_udp(const char *fmt, ...)
{
	static int zlog_udp_initiated = 0;
	static int sockfd = -1;
	static struct sockaddr_in addr;

	if (!zlog_udp_initiated)
	{
		sockfd = socket(AF_INET, SOCK_DGRAM, 0);
		if(sockfd < 0)
		{
			printf("Socket Error: %s\n", strerror(errno));
			exit(1);
		}
		int optval = 1;
		setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST | SO_REUSEADDR, &optval, sizeof(int));

    	struct hostent* host;
//    	host = gethostbyname("selnpctool-071-02-001");
        host = gethostbyname("localhost");

		bzero(&addr, sizeof(struct sockaddr_in));
		addr.sin_family = AF_INET;
		addr.sin_port = htons(8888);
    	addr.sin_addr = *((struct in_addr*)host->h_addr);

		zlog_udp_initiated = 1;
	}

	char buf[2000];
	va_list ap;
	va_start(ap, fmt);
	gen_timestamp(buf);
	vsnprintf(buf + strlen(buf), 2000, fmt, ap);
	va_end(ap);
	check_append_newline(buf);


	int len = sendto(sockfd, buf, strlen(buf), 0, (struct sockaddr*)&addr, sizeof(addr));
	if (len <= 0) {
		printf("send failed: %s\n", strerror(errno));
	}

    //close(sockfd);
	return 0;
}

//0x1234567812345678 11111111 22222222 33333333 44444444 0123456789abcdef
#define DUMP_LEN ((2+16+9*4+1+16+1)*(16+1))
void zlog_dump(unsigned char * buf_ptr, int len, int (*print_func)(const char *format, ...)) {
	char wbuf[DUMP_LEN];
	char *wp = wbuf;
    wp += sprintf(wp, "buf %p, %d bytes\n", buf_ptr, len);
    if (len > 256)
        len = 256;
    unsigned long row1 = ((unsigned long)(buf_ptr)) / 16;
    unsigned long row2 = ((unsigned long)(buf_ptr + len - 1)) / 16;
    unsigned char* rp = (unsigned char *)((unsigned long)buf_ptr/16*16);
    int nbr_rows = row2 - row1 + 1;
    int i, j;
    for(i = 0; i < nbr_rows; i++) {
		unsigned char *rp_start = rp;

        wp += sprintf(wp, "%p\t", (unsigned char*)((unsigned long)rp/16*16));
        for(j = 0; j < 16; j++) {
            if(rp < buf_ptr || rp >= buf_ptr + len)
                wp += sprintf(wp, "  ");
            else
                wp += sprintf(wp, "%02x", *rp);
			rp++;

            if(!((j+1)%4))
                wp += sprintf(wp, " ");
        }

		rp = rp_start;
        wp += sprintf(wp, "\t");
        for(j = 0; j < 16; j++) {
            if(rp < buf_ptr || rp > buf_ptr + len - 1)
                wp += sprintf(wp, " ");
            else if(isgraph(*rp))
                wp += sprintf(wp, "%1c", *rp);
            else
            {
                /*if(j < 15 && *rp > 0x7f && *(rp+1) > 0x7f)*/
                /*{*/
                    /*char tmp[3] = {0, 0, 0};*/
                    /*tmp[0] = *rp;*/
                    /*tmp[1] = *(rp+1);*/
                    /*wp += sprintf(wp, "%.2s", tmp);*/
                    /*j++;*/
                /*}*/
                /*else*/
				{
                    wp += sprintf(wp, ".");
				}
            }
			rp++;
        }
        wp += sprintf(wp, "\n");
		/*rp = rp_start + 16;*/
    }
	print_func("%s", wbuf);
}

static struct timespec zlog_ts;
void zlog_timedelta_init()
{
	clock_gettime(CLOCK_REALTIME, &zlog_ts);
}

int zlog_timedelta_rec()
{
    struct timespec ts = zlog_ts;
	clock_gettime(CLOCK_REALTIME, &zlog_ts);
	return (zlog_ts.tv_sec - ts.tv_sec) * 1000000 + 
		(zlog_ts.tv_nsec - ts.tv_nsec)/1000;
}

int MAIN(int argc, char *argv[]) {
	char *msg = "hello zlog";
	if (argc > 1)
		msg = argv[1];
	zlog_file("c zlogc file: %s\n", msg); 
	zlog_udp("c zlogc udp: %s\n", msg); 
	zlog_dump((unsigned char*)msg, strlen(msg), zlog_udp);
	return 0;
}
