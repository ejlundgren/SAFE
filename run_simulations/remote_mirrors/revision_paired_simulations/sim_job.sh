#!/bin/sh

# SLURM commands:

#SBATCH --account=def-snakagaw
#SBATCH --job-name=revision_sim_job
#SBATCH --mail-user=ejlundgr@fir.alliancecan.ca
#SBATCH --mail-type=FAIL,END
#SBATCH --mem-per-cpu=6gb
#SBATCH --cpus-per-task 1
#SBATCH --time 6:00:00
#SBATCH --output=outfiles/sim_job_%j.out

module load gcc/13.3 r/4.5.0  

Rscript SAFE_revision_simulations.R $i >logs/log_"$i".txt
