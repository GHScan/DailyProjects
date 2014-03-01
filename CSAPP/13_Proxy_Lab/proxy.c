/*
 * proxy.c - CS:APP Web proxy
 *
 * TEAM MEMBERS:
 *     Andrew Carnegie, ac00@cs.cmu.edu 
 *     Harry Q. Bovik, bovik@cs.cmu.edu
 * 
 * IMPORTANT: Give a high level description of your code here. You
 * must also provide a header comment at the beginning of each
 * function that describes what that function does.
 */ 

#include <stdarg.h>
#include "csapp.h"

/*
 * Function prototypes
 */
int parse_uri(char *uri, char *target_addr, char *path, int  *port);
void format_log_entry(char *logstring, struct sockaddr_in *sockaddr, char *uri, int size);

static void unix_warning(char *msg) {
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
}
static void dns_warning(char *msg) {
    fprintf(stderr, "%s: DNS error %d\n", msg, h_errno);
}

ssize_t Read_w(int fd, void *buf, size_t count) {
    ssize_t rc;
    while ((rc = read(fd, buf, count)) < 0) {
        if (errno == EINTR) continue;
        else {
            unix_warning("Read error");
            break;
        }
    }
    return rc;
}
static ssize_t Rio_writen_w(int fd, void *usrbuf, size_t n) {
    ssize_t wc;
    if ((wc = rio_writen(fd, usrbuf, n)) != n)
        unix_warning("Rio_writen_w error");
    return wc;
}
static ssize_t Rio_readlineb_w(rio_t *rp, void *usrbuf, size_t maxlen) {
    ssize_t rc;
    if ((rc = rio_readlineb(rp, usrbuf, maxlen)) < 0)
	unix_warning("Rio_readlineb_w error");
    return rc;
} 
static int Open_clientfd_w(char *hostname, int port) {
    int rc;
    if ((rc = open_clientfd(hostname, port)) < 0) {
	if (rc == -1)
	    unix_warning("Open_clientfd Unix error");
	else        
	    dns_warning("Open_clientfd DNS error");
    }
    return rc;
}

typedef struct {
    int threadIdx;
    int connfd;
    struct sockaddr_in addr;
    pthread_t tid;
} ThreadData;
static void* threadForClient(void *_data);

static sem_t g_locker;
static FILE* g_logFile;

static void fprintf_ts(FILE *f, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    P(&g_locker);
    vfprintf(f, fmt, args);
    fflush(f);
    V(&g_locker);
    va_end(args);
}
static int isHttpLineEmpty(char *buf, int size) {
    return strcmp(buf, "\n") == 0 || strcmp(buf, "\r\n") == 0;
}

/* 
 * main - Main routine for the proxy program 
 */
int main(int argc, char **argv)
{
    /* Check arguments */
    if (argc != 2) {
	fprintf(stderr, "Usage: %s <port number>\n", argv[0]);
	exit(0);
    }

    Signal(SIGPIPE, SIG_IGN);

    Sem_init(&g_locker, 0, 1);

    g_logFile = fopen("proxy.log", "a");
    if (g_logFile == NULL) {
        fprintf(stderr, "open log file failed!");
        exit(1);
    }

    int threadIdx = 0;
    int serverfd = Open_listenfd(atoi(argv[1]));
    for (;;) {
        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        int connfd = Accept(serverfd, (SA*)&addr, &addrlen);

        ThreadData *data = (ThreadData*)Malloc(sizeof(ThreadData));
        data->threadIdx = ++threadIdx;
        data->connfd = connfd;
        data->addr = addr;
        Pthread_create(&data->tid, NULL, &threadForClient, data);
    }

    fclose(g_logFile);

    exit(0);
}

static void* threadForClient(void *_data) {
    ThreadData data = *(ThreadData*)_data;
    Free(_data);
    Pthread_detach(data.tid);

    rio_t riobuf;
    Rio_readinitb(&riobuf, data.connfd);
    char buf[MAXLINE] = "";
    char uri[MAXLINE] = "";
    char hostname[MAXLINE] = "";
    char pathname[MAXLINE] = "";
    int port;
    int clientfd = -1;
    int requestSize = 0, responseSize = 0;

    // parse uri
    if ((requestSize = Rio_readlineb_w(&riobuf, buf, sizeof(buf))) <= 0) goto _cleanup;
    {
        char method[32] = "";
        char httpversion[32] = "";
        if (sscanf(buf, "%s %s %s", method, uri, httpversion) != 3) goto _cleanup;
    }
    if (parse_uri(uri, hostname, pathname, &port) == -1) goto _cleanup;

    // connect to real server
    P(&g_locker);
    clientfd = Open_clientfd_w(hostname, port);
    V(&g_locker);
    if (clientfd < 0) goto _cleanup;

    // forward request
    if (Rio_writen_w(clientfd, buf,  requestSize) != requestSize) goto _cleanup;
    for (;;) {
        ssize_t rc = Rio_readlineb_w(&riobuf, buf, sizeof(buf));
        if (rc < 0) goto _cleanup;
        else if (rc == 0) goto _cleanup;
        requestSize += rc;
        if (Rio_writen_w(clientfd, buf, rc) != rc) goto _cleanup;
        if (isHttpLineEmpty(buf, rc)) break;
    }

    // forward response
    for (;;) {
        int rc = Read_w(clientfd, buf, sizeof(buf));
        if (rc < 0) goto _cleanup;
        else if (rc == 0) break;
        responseSize += rc;
        if (Rio_writen_w(data.connfd, buf, rc) != rc) goto _cleanup;
    }

    format_log_entry(buf, &data.addr, uri, responseSize);
    fprintf_ts(g_logFile, "%s\n", buf);

_cleanup:
    Close(data.connfd);
    if (clientfd > 0) Close(clientfd);

    return NULL;
}


/*
 * parse_uri - URI parser
 * 
 * Given a URI from an HTTP proxy GET request (i.e., a URL), extract
 * the host name, path name, and port.  The memory for hostname and
 * pathname must already be allocated and should be at least MAXLINE
 * bytes. Return -1 if there are any problems.
 */
int parse_uri(char *uri, char *hostname, char *pathname, int *port)
{
    char *hostbegin;
    char *hostend;
    char *pathbegin;
    int len;

    if (strncasecmp(uri, "http://", 7) != 0) {
	hostname[0] = '\0';
	return -1;
    }
       
    /* Extract the host name */
    hostbegin = uri + 7;
    hostend = strpbrk(hostbegin, " :/\r\n\0");
    len = hostend - hostbegin;
    strncpy(hostname, hostbegin, len);
    hostname[len] = '\0';
    
    /* Extract the port number */
    *port = 80; /* default */
    if (*hostend == ':')   
	*port = atoi(hostend + 1);
    
    /* Extract the path */
    pathbegin = strchr(hostbegin, '/');
    if (pathbegin == NULL) {
	pathname[0] = '\0';
    }
    else {
	pathbegin++;	
	strcpy(pathname, pathbegin);
    }

    return 0;
}

/*
 * format_log_entry - Create a formatted log entry in logstring. 
 * 
 * The inputs are the socket address of the requesting client
 * (sockaddr), the URI from the request (uri), and the size in bytes
 * of the response from the server (size).
 */
void format_log_entry(char *logstring, struct sockaddr_in *sockaddr, 
		      char *uri, int size)
{
    time_t now;
    char time_str[MAXLINE];
    unsigned long host;
    unsigned char a, b, c, d;

    /* Get a formatted time string */
    now = time(NULL);
    strftime(time_str, MAXLINE, "%a %d %b %Y %H:%M:%S %Z", localtime(&now));

    /* 
     * Convert the IP address in network byte order to dotted decimal
     * form. Note that we could have used inet_ntoa, but chose not to
     * because inet_ntoa is a Class 3 thread unsafe function that
     * returns a pointer to a static variable (Ch 13, CS:APP).
     */
    host = ntohl(sockaddr->sin_addr.s_addr);
    a = host >> 24;
    b = (host >> 16) & 0xff;
    c = (host >> 8) & 0xff;
    d = host & 0xff;


    /* Return the formatted log entry string */
    sprintf(logstring, "%s: %d.%d.%d.%d %s %d", time_str, a, b, c, d, uri, size);
}


