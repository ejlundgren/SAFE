#!/bin/sh

# SLURM commands:

#SBATCH --account=def-snakagaw
#SBATCH --job-name=sim_job
#SBATCH --mail-user=ejlundgr@fir.alliancecan.ca
#SBATCH --mail-type=FAIL,END
#SBATCH --mem-per-cpu=3gb
#SBATCH --cpus-per-task 1
#SBATCH --time 15:00:00
#SBATCH --output=outfiles/B_sim_job_%j.out

module load gcc/13.3 r/4.5.0  


Rscript SAFE_simulation.R $i >logs/log_"$i".txt
