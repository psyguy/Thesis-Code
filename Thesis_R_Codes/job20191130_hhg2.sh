#!/bin/bash -l
#PBS -l nodes=5:ppn=30
#PBS -l walltime=00:59:59
#PBS -l pmem=6gb
#PBS -N November30_hhg2
#PBS -m b -M mhscientist@gmail.com
 
export OPENBLAS_NUM_THREADS=1
export GOTO_NUM_THREADS=1
export OMP_NUM_THREADS=1
 
 
cd $PBS_O_WORKDIR
 
 
module load R/3.6.0-foss-2018a-bare
 
./scripts/20191130_hpc_hhg.R $counter