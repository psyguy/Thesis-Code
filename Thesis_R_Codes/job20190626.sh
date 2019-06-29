#!/bin/bash -l
#PBS -l nodes=2:ppn=25
#PBS -l walltime=10:00:00
#PBS -l pmem=5gb
#PBS -N June26_1340
#PBS -m b -M mhscientist@gmail.com
 
export OPENBLAS_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OMP_NUM_THREADS=1
 
 
cd $PBS_O_WORKDIR
 
 
module load R/3.5.0-iomkl-2018a-X11-20180131
 
./scripts/20190626_hpc.R $index