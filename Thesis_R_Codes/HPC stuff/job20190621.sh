#!/bin/bash -l
#PBS -l nodes=3:ppn=36
#PBS -l walltime=20:00:00
#PBS -N pall
#PBS -m b -M mhscientist@gmail.com
 
export OPENBLAS_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OMP_NUM_THREADS=1
 
 
cd $PBS_O_WORKDIR
 
 
module load R/3.5.0-iomkl-2018a-X11-20180131
 
./scripts/20190621_hpc.R $round_alphabeta