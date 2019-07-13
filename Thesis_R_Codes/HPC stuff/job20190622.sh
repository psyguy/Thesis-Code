#!/bin/bash -l
#PBS -l nodes=3:ppn=36
#PBS -l walltime=20:00:00
#PBS -N June22
#PBS -m b -M mhscientist@gmail.com
 
export OPENBLAS_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OMP_NUM_THREADS=1
 
 
cd $PBS_O_WORKDIR
 
 
module load R/3.5.0-iomkl-2018a-X11-20180131
 
./scripts/20190622_hpc.R $index