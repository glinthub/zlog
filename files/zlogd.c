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
#include <stdarg.h>

#include "zlog.h"

#define RECV_BUF_LEN	(4096)
#define LOGD_BUF_THRESH	(512*RECV_BUF_LEN)
#define LOGD_BUF_LEN	(LOGD_BUF_THRESH + RECV_BUF_LEN)
char logd_buf[LOGD_BUF_LEN];
volatile int logd_fd;
volatile int logd_buflen = 0;


void logd_sighandler(int sig) {
	printf("signal %d received\n", sig);
	printf("last %d bytes in buffer\n", logd_buflen);
	if (logd_buflen > 0)
	{
		printf("saving last %d bytes log, fd %d...\n", logd_buflen, logd_fd);
        logd_fd = open("zlog.log", O_CREAT | O_WRONLY, 0644);
        CHECK_ERR(logd_fd != -1);
        int rc = write(logd_fd, logd_buf, logd_buflen);
		CHECK_ERR(rc > 0);
		logd_buflen = 0;
        close(logd_fd);
    }
	exit(0);
}

void *logd_task(void *p)
{
	char recv_buf[RECV_BUF_LEN];

    int len;
    int rc;
    int file_seq = 0;
    struct sockaddr_in addr;
    struct sockaddr_in srcAddr;
    int addrLen = sizeof(srcAddr);


    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    CHECK_ERR(sockfd > 0);
    
    bzero(&addr, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(8888);
    inet_aton("0.0.0.0", &addr.sin_addr);
    //inet_aton("192.168.1.222", &addr.sin_addr);

    rc = bind(sockfd, (struct sockaddr *)&addr, sizeof(addr));
	CHECK_ERR(rc != -1);

    while (1)
    {
        bzero(&srcAddr, sizeof(struct sockaddr_in));
        len = recvfrom(sockfd, recv_buf, RECV_BUF_LEN-1, 0, (struct sockaddr*)&srcAddr, &addrLen);
        if (len > 0)
        {
			if (recv_buf[len-1] != '\n')
			{
				recv_buf[len++] = '\n';
			}
			recv_buf[len+1] = '\0';
            //printf("received %d bytes from %s (len %d): %s", len, inet_ntoa(srcAddr.sin_addr), addrLen, recv_buf);
            printf("recv (%d): %s", len, recv_buf);
			strncpy(logd_buf + logd_buflen, recv_buf, len);
			logd_buflen += len;
			if (logd_buflen >= LOGD_BUF_THRESH)
			{
				printf("saving %d bytes ...\n", logd_buflen);
                logd_fd = open("zlog.log", O_CREAT | O_WRONLY, 0644);
                CHECK_ERR(logd_fd != -1);
                rc = write(logd_fd, logd_buf, logd_buflen);
				CHECK_ERR(rc > 0);
				logd_buflen = 0;
                close(logd_fd);
			}
        }
    }

    close(sockfd);
}


int main(int argc, char** argv)
{
	if (argc > 1)
	{
		char *msg = argv[1];
		printf("zlog client sending: %s\n", msg);
		//zlog_udp("%s %d: %s\n", __FILE__, __LINE__, msg);
	}
	else
	{
		printf("zlogd started.\n");
		struct sigaction sa;
		sa.sa_handler = logd_sighandler;
		sigaction(SIGINT, &sa, NULL);

		pthread_t rxTid;
        pthread_create(&rxTid, NULL, logd_task, NULL);
        pthread_join(rxTid, NULL);
	}
    return 0;
} 
