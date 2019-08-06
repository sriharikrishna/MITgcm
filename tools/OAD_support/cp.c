



#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <linux/limits.h>
#include <sys/stat.h>
#ifdef ALLOW_USE_MPI
#include <mpi.h>
#endif
#include <zlib.h>
#include <string.h>
#include <assert.h>

#define THRESHOLD 1024
#define MAXITR 1024
#define FSIZE (100 * 1024 * 1024)
#define MAXITR 1024

typedef struct cp_fd{
    FILE * fd;
    char *buf, *abuf, *cbuf;
} cp_fd;

static int cp_file_num = 0;
cp_fd *fd;

int cur_num;
int wr;
int dsize;

size_t bsize, bused;
void *buffer, *abuffer;

double topen, tclose;
unsigned long long fsize[MAXITR];
static int max_itr;
void *iobuf;
size_t iobsize;

void buffer_init(){
    int pagesize;

    pagesize = getpagesize();
    buffer = malloc(FSIZE + pagesize);
    assert(buffer!=NULL);
    abuffer=(void*)((((unsigned long long)buffer+(unsigned long long)pagesize-1)/(unsigned long long)pagesize)*(unsigned long long)pagesize);
    assert(abuffer!=NULL);
    bsize = FSIZE;
    buffer = (float*)malloc(bsize);
}

void buffer_free(){
    bsize = 0;
    free(buffer);
}

void buffer_resize(size_t size){
    if (size > bsize){
        while(bsize < size){
            bsize <<= 1;
        }
        buffer = (float*)realloc(buffer, bsize);
        assert(buffer!=NULL);
    }
}   


void compresswr(void *R, int size) {
    int err;
    double t1, t2, t3, t4, t5;
    off_t wsize;
    ssize_t ioret;

    // zlib struct
    z_stream defstream;
    defstream.zalloc = Z_NULL;
    defstream.zfree = Z_NULL;
    defstream.opaque = Z_NULL;
    defstream.avail_in = (uInt)(size); // input size
    defstream.next_in = (Bytef *)R; // input
    defstream.avail_out = (uInt)bsize; // output buffer size
    defstream.next_out = (Bytef *)((char*)buffer + sizeof(dsize)); // output buffer

    // the actual compression work.
    err = deflateInit(&defstream, Z_BEST_COMPRESSION);
    if (err < 0){
        printf("deflateInit fail: %d: %s\n", err, defstream.msg);
    }
    err = deflate(&defstream, Z_FINISH);
    if (err < 0){
        printf("deflate fail: %d: %s\n", err, defstream.msg);
    }
    err = deflateEnd(&defstream);
    if (err < 0){
        printf("deflateEnd fail: %d: %s\n", err, defstream.msg);
    }

#ifdef ALLOW_USE_MPI
    MPI_Barrier(MPI_COMM_WORLD);
#endif

    // Size of variable
    dsize = (int)defstream.total_out;
    *((int*)buffer) = dsize;

    // Write to file
    ioret = fwrite(buffer,defstream.total_out + sizeof(dsize), 1, fd->fd);
}

void compressrd(void *D) {
    int err;
    double t1, t2, t3;
    off_t rsize;
    ssize_t ioret;

    assert(fd->fd!=NULL);
    ioret=fread(&dsize, sizeof(dsize), 1, fd->fd);
        if (ioret <= 0){
            printf("read fail1: %ld %s\n", ioret);
            perror("read fail1");
        }
    ioret = fread(buffer, dsize, 1, fd->fd);
        if (ioret <= 0){
            printf("read fail: %ld %s\n", ioret);
            perror("read fail");
        }

    // zlib struct
    z_stream infstream;
    infstream.zalloc = Z_NULL;
    infstream.zfree = Z_NULL;
    infstream.opaque = Z_NULL;
    infstream.avail_in = (unsigned long) dsize; // input size
    infstream.next_in = (Bytef *)buffer; // input
    infstream.avail_out = (uInt)(FSIZE); // output buffer
    infstream.next_out = (Bytef *)D; // buffer size
     
    // the actual DE-compression work.
    err = inflateInit(&infstream);
    if (err < 0){
        printf("inflateInit fail: %d: %s\n", err, infstream.msg);
    }
    err = inflate(&infstream, Z_NO_FLUSH);
    if (err < 0){
        printf("inflate fail: %d: %s\n", err, infstream.msg);
    }
    err = inflateEnd(&infstream);
    if (err < 0){
        printf("inflateEnd fail: %d: %s\n", err, infstream.msg);
    }
}

int cp_wr_open(char* fname, size_t fsize){
    int pagesize;

    fd = malloc(sizeof(cp_fd));
    assert(fd!=NULL);
    pagesize = getpagesize();
    fd->buf  = (char*)malloc(FSIZE + pagesize);
    assert(fd!=NULL);
    fd->abuf = fd->cbuf = (char*)((((size_t)fd->buf + (size_t)pagesize - 1) / (size_t)pagesize) * (size_t)pagesize);
    fd->fd = fopen(fname, "w+");
#ifdef DEBUG_OPENAD_COMPRESS
    printf("cp_wr_open ::%s::\n",fname);
#endif
    return 0;
}

int cp_write(cp_fd *fd, void *data, size_t size){
    assert(fd!=NULL);
    assert(fd->cbuf!=NULL);
    assert(data!=NULL);
    memcpy(fd->cbuf, data, size);
    fd->cbuf += size;
}

int cp_wr_close(cp_fd *fd){
    int ret = 0;
    off_t wsize;
    ssize_t ioret;
    
    compresswr(fd->abuf, fd->cbuf - fd->abuf);

    fclose(fd->fd);
    free(fd->buf);
    free(fd);
    return ret;
}

int cp_rd_open(char* fname){
    int pagesize;
    double t1;

    fd = malloc(sizeof(cp_fd));
    assert(fd!=NULL);

    pagesize = getpagesize();
    fd->buf  = (char*)malloc(FSIZE + pagesize);
    assert(fd->buf!=NULL);
    fd->abuf = fd->cbuf = (char*)((((size_t)fd->buf + (size_t)pagesize - 1) / (size_t)pagesize) * (size_t)pagesize);
    assert(fd->abuf!=NULL);

#ifdef DEBUG_OPENAD_COMPRESS
    printf("cp_rd_open ::%s::\n",fname);
#endif
    fd->fd = fopen(fname, "r");

#ifdef ALLOW_USE_MPI
    MPI_Barrier(MPI_COMM_WORLD);
#endif

    compressrd(fd->abuf);

    return 0;
}

int cp_read(cp_fd *fd, void *data, size_t size){
    memcpy(data, fd->cbuf, size);
    fd->cbuf += size;
}

int cp_rd_close(cp_fd *fd){   
    fclose(fd->fd);
    free(fd->buf);
    free(fd);

    return 0;
}

void cp_wr_open_(int *num, char* prefix){
    int rank;
    char fname[PATH_MAX]; 
    char *envfname;

    if (*num >= 0){
        cp_file_num = *num;
    }

#ifdef ALLOW_USE_MPI
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#else
    rank = 0;
#endif

    buffer_init();
    sprintf(fname, "%s.%03d.%05d", prefix, rank, cp_file_num);
    cur_num = cp_file_num;

    if (*num <= 0){
        cp_file_num++;
    }
    wr = 1;

    cp_wr_open(fname, FSIZE);
}

void cp_rd_open_(int *num, char * prefix){
    int rank;
    char fname[PATH_MAX];
    char *envfname;

    if (*num >= 0){
        cp_file_num = *num;
    }
    else{
        cp_file_num--;
    }

#ifdef ALLOW_USE_MPI
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#else
    rank = 0;
#endif

    buffer_init();
    sprintf(fname, "%s.%03d.%05d", prefix, rank, cp_file_num);
    cur_num = cp_file_num;
    wr = 0;
    cp_rd_open(fname);
}

void cpc_close_(){
    int rank, np;
    char fname[PATH_MAX];
    double tio;
    unsigned long long size;
    
    if (wr){
        cp_wr_close(fd);
    }
    else{
        cp_rd_close(fd);
    }

    if (max_itr < cur_num){
        max_itr = cur_num;
    }

    buffer_free();
}

void compresswr_real_(double *R, int *size  ) {
#ifdef DEBUG_OPENAD_COMPRESS
    printf("Write %d bytes from %llx\n", *size, R);
#endif
    cp_write(fd, R, (size_t)(*size));
}

void compressrd_real_(double *D, int *size  ) {
#ifdef DEBUG_OPENAD_COMPRESS
    printf("Read %d bytes to %llx\n", *size, D);
#endif
    cp_read(fd, D, (size_t)(*size));
}


void compresswr_integer_(int *R, int *size  ) {
#ifdef DEBUG_OPENAD_COMPRESS
    printf("Write %d bytes from %llx\n", *size, R);
#endif
    cp_write(fd, R, (size_t)(*size));
}

void compressrd_integer_(int *D, int *size  ) {
#ifdef DEBUG_OPENAD_COMPRESS
    printf("Read %d bytes to %llx\n", *size, D);
#endif
    cp_read(fd, D, (size_t)(*size));
}


void compresswr_bool_(int *R, int *size  ) {
#ifdef DEBUG_OPENAD_COMPRESS
    printf("Write %d bytes from %llx\n", *size, R);
#endif
    cp_write(fd, R, (size_t)(*size));
}

void compressrd_bool_(int *D, int *size  ) {
#ifdef DEBUG_OPENAD_COMPRESS
    printf("Read %d bytes to %llx\n", *size, D);
#endif
    cp_read(fd, D, (size_t)(*size));
}


void compresswr_string_(char *R, int *size , long l ) {
#ifdef DEBUG_OPENAD_COMPRESS
    printf("Write %d bytes from %llx\n", *size, R);
#endif
    cp_write(fd, R, (size_t)(*size));
}

void compressrd_string_(char *D, int *size , long l ) {
#ifdef DEBUG_OPENAD_COMPRESS
    //printf("Read %d bytes to %llx\n", *size, D);
#endif
    cp_read(fd, D, (size_t)(*size));
}


