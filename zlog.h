#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

int zlog_udp(char *fmt, ...);
#define HERE zlog_udp("%s %s %d\n", __FILE__, __FUNCTION__, __LINE__)
#define ZLOG(fmt, ...) zlog_udp("%s %s %d: " fmt "\n", __FILE__, __FUNCTION__, __LINE__, ##__VA_ARGS__)

#ifdef __cplusplus
}
#endif

