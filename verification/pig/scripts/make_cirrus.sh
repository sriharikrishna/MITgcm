cd ../build
rm -Rf *

# Switch compilers as  the Cray compiler gives an error.
#module swap PrgEnv-intel PrgEnv-cray
#module swap PrgEnv-gnu PrgEnv-cray
#module swap cray-netcdf netcdf
#module load netcdf-nc-max-vars
#module add gcc/4.9.3
module load intel-mpi-15/15

ROOTDIRTEMP=//lustre/home/sc030/snarayan/MITgcm/
OFTEMP=$ROOTDIRTEMP/verification/pig/build_options/linux_ia64_cray_cirrus_openad_diva

make CLEAN;
#$ROOTDIR/tools/genmake2 -ieee -mods='../code ../newcode' -of=../scripts/linux_amd64_archer_ifort -mpi
#$ROOTDIR/tools/genmake2 -ieee -mods='../code ../newcode' -of=$ROOTDIR/tools/build_options/linux_amd64_gfortran -mpi
$ROOTDIRTEMP/tools/genmake2 -oad -mods='../code_pig' -of=$OFTEMP -mpi -rootdir=$ROOTDIRTEMP -diva
make adAll
#make depend
#make -j 8

# Switch Programming Environment back
#module swap PrgEnv-intel PrgEnv-cray
#module swap netcdf cray-netcdf
