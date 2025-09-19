#!/bin/sh
# This script will call a bunch of bash scripts in parallel that will send job to array
# Doing this in 2 bash scripts is annoying but necessary so that things aren't sequential

#SBATCH --account=def-snakagaw
#SBATCH --job-name=meta_sim_boss
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=ejlundgr@fir.alliancecan.ca
#SBATCH --mem-per-cpu=50M
#SBATCH --cpus-per-task 1
#SBATCH --time 00:10:00
#SBATCH --output=outfiles/meta_sim_boss_%j.out

max_formulas=300
# number of chunks in this case

for (( i=1; i<=${max_formulas}; i++ )); 
  do
  export i
  sbatch sim_job.sh
  done
  
  
