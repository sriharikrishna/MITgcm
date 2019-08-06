
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <linux/limits.h>
#include <mpi.h>

static int cp_file_num;
int fd;

void cp_wr_open(int num){
    int rank;
    char fname[PATH_MAX];

    if (num < 0){
        num = cp_file_num;
        cp_file_num++;
    }
    else{
        cp_file_num = num;
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    sprintf(fname, "oad_cp.%03d.%05d", rank, num);

    fd = open(fname, O_CREAT | O_WRONLY, 0644);
}

void cp_rd_open(int num){
    int rank;
    char fname[PATH_MAX];

    if (num < 0){
        cp_file_num--;
        num = cp_file_num;
    }
    else{
        cp_file_num = num;
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    sprintf(fname, "oad_cp.%03d.%05d", rank, num);

    fd = open(fname, O_CREAT | O_RDONLY, 0644);
}



void compresswrc_real_(double *R, size_t size) {
    write(fd, R, size);
}

void compressrdc_real_(double *D, size_t size) {
    read(fd, R, size);
}

void compresswrc_int_(int *R, size_t size) {
    write(fd, R, size);
}

void compressrdc_int_(int *D, size_t size) {
    read(fd, R, size);
}

void compresswrc_bool_(bool *R, size_t size) {
    write(fd, R, size);
}

void compressrdc_bool_(bool *D, size_t size) {
    read(fd, R, size);
}





