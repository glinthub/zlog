#include <stdarg.h>
#include <time.h>
#include <pthread.h>

#ifdef __cplusplus
extern "C" {
#endif

int zlog_file(const char *fmt, ...);
int zlog_udp(const char *fmt, ...);
void zlog_timedelta_init();
int zlog_timedelta_rec();
void zlog_dump(unsigned char * buf_ptr, int len, int (*print_func)(const char *format, ...));

#define HERE printf("%s %s %d\n", __FILE__, __FUNCTION__, __LINE__)
#define ZLOGF(fmt, ...) zlog_file("[%d, %x] %s %s %d: " fmt "\n", getpid(), pthread_self(), __FILE__, __FUNCTION__, __LINE__, ##__VA_ARGS__)
#define ZLOGU(fmt, ...) zlog_udp("%s %s %d: " fmt "\n", __FILE__, __FUNCTION__, __LINE__, ##__VA_ARGS__)
#define ZLOGT() do { \
	int us = zlog_timedelta_rec(); \
	zlog_udp("%s %s %d: delta %d\n", __FILE__, __FUNCTION__, __LINE__, us); \
	} while(0)

#define CHECK_ERR(x) do { \
	if(!(x)) { \
		printf("check failed at %s, ln %d: %s\n", \
				__FILE__, __LINE__, strerror(errno)); \
		exit(1); \
	} } while (0)

#ifdef __cplusplus
}
#endif

