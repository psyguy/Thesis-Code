#!/bin/bash -l
#PBS -l nodes=5:ppn=36
#PBS -l walltime=02:59:59
#PBS -l pmem=5gb
#PBS -N December15_hhg-unstandardized-remaining-24q
#PBS -m b -M mhscientist@gmail.com
 
export OPENBLAS_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OMP_NUM_THREADS=1

 
cd $PBS_O_WORKDIR
 
 
module load R/3.6.0-foss-2018a-bare
 
./scripts/20191215_hpc_hhg.R $counter