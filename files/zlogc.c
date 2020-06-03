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

#include "zlog.h"

void gen_timestamp(char *buf)
{
	time_t t = time(NULL);
	struct tm tm;
	struct timeval tv;
	localtime_r(&t, &tm);
	gettimeofday(&tv, NULL);
	sprintf(buf, "%02d/%02d/%04d %02d:%02d:%02d.%03d ", 
			tm.tm_mon + 1, tm.tm_mday, tm.tm_year + 1900,
			tm.tm_hour, tm.tm_min, tm.tm_sec, (int)tv.tv_usec/1000);
}

void check_append_newline(char *buf)
{
	if (buf[strlen(buf)-1] != '\n')
		strcat(buf, "\n");
}

void zlog_file(char *fmt, ...)
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
}

void zlog_udp(char *fmt, ...)
{
	char buf[256];
	va_list ap;
	va_start(ap, fmt);
	gen_timestamp(buf);
	vsnprintf(buf + strlen(buf), 200, fmt, ap);
	va_end(ap);
	check_append_newline(buf);

    struct sockaddr_in addr;
	int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if(sockfd<0)
    {
        printf("Socket Error: %s\n", strerror(errno));
        exit(1);
    }
    int optval = 1;
    setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST | SO_REUSEADDR, &optval, sizeof(int));

    bzero(&addr, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
	inet_aton("localhost", &(addr.sin_addr));
    addr.sin_port = htons(8888);

	int len = sendto(sockfd, buf, strlen(buf), 0, (struct sockaddr*)&addr, sizeof(addr));
	if (len <= 0) {
		printf("send failed: %s\n", strerror(errno));
	}

    close(sockfd);
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
	(void)argc;
	(void)argv;
	//zlog_timedelta_rec();
	//zlog_file("hello, %s\n", "zlog file"); 
	//ZLOGT();
	zlog_udp("hello, %s\n", "zlog udp..."); 
	//ZLOGT();
	return 0;
}
