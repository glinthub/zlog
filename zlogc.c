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

#include "zlog.h"

int zlog_udp(char *fmt, ...)
{
	char buf[256];
    int len;
    struct sockaddr_in addr;

	time_t t = time(NULL);
	struct tm tm;
	struct timeval tv;
	localtime_r(&t, &tm);
	gettimeofday(&tv, NULL);
	sprintf(buf, "%02d/%02d/%04d %02d:%02d:%02d.%03d ", 
			tm.tm_mon + 1, tm.tm_mday, tm.tm_year + 1900,
			tm.tm_hour, tm.tm_min, tm.tm_sec, tv.tv_usec/1000);

	va_list ap;
	va_start(ap, fmt);
	vsnprintf(buf + strlen(buf), 200, fmt, ap);
	va_end(ap);

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

	len = sendto(sockfd, buf, strlen(buf), 0, (struct sockaddr*)&addr, sizeof(addr));
	if (len <= 0) {
		printf("send failed: %s\n", strerror(errno));
	}

    close(sockfd);
}

int MAIN(int argc, char *argv[]) {
	zlog_udp("hello, %s\n", "zlog"); 
	return 0;
}
