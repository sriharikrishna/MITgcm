cd ../build_diva
rm -rf *

# Switch compilers as  the Cray compiler gives an error.
module swap PrgEnv-intel PrgEnv-cray
module swap PrgEnv-gnu PrgEnv-cray
#module load cray-hdf5/1.8.13
#module load cray-netcdf/4.3.2
#module swap cray-netcdf netcdf
module load netcdf-nc-max-vars
module add gcc/4.9.3

ROOTDIRTEMP=/home/n02/n02/shkndiva/MITgcm_shk3/
OFTEMP=$ROOTDIRTEMP/verification/pig/build_options/linux_ia64_cray_archer_openad_diva

make CLEAN;
#$ROOTDIR/tools/genmake2 -ieee -mods='../code ../newcode' -of=../scripts/linux_amd64_archer_ifort -mpi
#$ROOTDIR/tools/genmake2 -ieee -mods='../code ../newcode' -of=$ROOTDIR/tools/build_options/linux_amd64_gfortran -mpi
$ROOTDIRTEMP/tools/genmake2 -oad -diva -oadc -mods='../code_pig' -of=$OFTEMP -mpi -rootdir=$ROOTDIRTEMP
make adAll
#make depend
#make -j 8

# Switch Programming Environment back
#module swap PrgEnv-intel PrgEnv-cray
#module swap netcdf cray-netcdf
