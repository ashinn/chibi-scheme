/* Win32 shim for (chibi filesystem) */

#include <windows.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <direct.h>

static int mkdir_shim(const char* path, int ignored) {
    return mkdir(path);
}

#if !defined(__MINGW32__) && !defined(__MINGW64__)
/* Flags for _access() API */
#define R_OK 4
#define W_OK 2
#define X_OK 1 /* Follow MinGW */

#define SHIM_WIN32_STAT_IS(m, flg) ((m & _S_IFMT) == flg)
#define S_ISREG(m) SHIM_WIN32_STAT_IS(m, _S_IFREG)
#define S_ISDIR(m) SHIM_WIN32_STAT_IS(m, _S_IFDIR)
#define S_ISCHR(m) SHIM_WIN32_STAT_IS(m, _S_IFCHR)
#define S_ISFIFO(m) SHIM_WIN32_STAT_IS(m, _S_IFIFO)
#define S_ISBLK(m) 0
#endif

#define S_ISLNK(m) 0
#define S_ISSOCK(m) S_ISFIFO(m)

struct dirent {
    char d_name[MAX_PATH];
};

struct DIR_s {
    int want_next;
    HANDLE hFind;
    struct dirent result;
};

typedef struct DIR_s DIR;

static DIR* opendir(const char* path) {
    HANDLE hFind;
    WIN32_FIND_DATAA ffd;
    DIR* dp;
    char* query;
    query = malloc(MAX_PATH + 1);
    if(!query){
        errno = ENOMEM;
        return NULL;
    }
    query[0] = 0;
    strncat(query, path, MAX_PATH);
    strncat(query, "\\*", MAX_PATH);
    query[MAX_PATH] = 0;
    hFind = FindFirstFileA(query, &ffd);
    if(hFind == INVALID_HANDLE_VALUE){
        switch(GetLastError()){
            case ERROR_FILE_NOT_FOUND:
                errno = ENOENT;
                break;
            default:
                errno = EACCES;
                break;
        }
        return NULL;
    }
    free(query);
    dp = malloc(sizeof(DIR));
    if(!dp){
        errno = ENOMEM;
        return NULL;
    }
    dp->hFind = hFind;
    strncpy(dp->result.d_name, ffd.cFileName, MAX_PATH);
    dp->want_next = 0;
    return dp;
}

static struct dirent *readdir(DIR *dp) {
    BOOL b;
    WIN32_FIND_DATAA ffd;
    if(dp->want_next){
        /* Query the next file */
        b = FindNextFile(dp->hFind, &ffd);
        if(! b){
            return NULL;
        }
        strncpy(dp->result.d_name, ffd.cFileName, MAX_PATH);
    }
    dp->want_next = 1;
    return &dp->result;
}

static int closedir(DIR *dp) {
    BOOL b;
    b = FindClose(dp->hFind);
    if(! b){
        errno = EBADF;
        return -1;
    }
    free(dp);
    return 0;
}
