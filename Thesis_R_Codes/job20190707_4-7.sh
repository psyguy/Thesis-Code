#!/bin/bash -l
#PBS -l nodes=5:ppn=16
#PBS -l walltime=23:59:59
#PBS -l pmem=11gb
#PBS -N July07_1810
#PBS -m b -M mhscientist@gmail.com
 
export OPENBLAS_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OMP_NUM_THREADS=1
 
 
cd $PBS_O_WORKDIR
 
 
module load R/3.5.0-iomkl-2018a-X11-20180131
 
./scripts/20190707_reincarnation_4-7.R $index