#!/bin/bash -l
#PBS -l nodes=1:ppn=12
#PBS -l walltime=5:40:59
#PBS -l pmem=15gb
#PBS -N July08_lost4start_1450
#PBS -m b -M mhscientist@gmail.com
 
export OPENBLAS_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OMP_NUM_THREADS=1
 
 
cd $PBS_O_WORKDIR
 
 
module load R/3.5.0-iomkl-2018a-X11-20180131
 
./scripts/20190707_reincarnation_lost4.R $index