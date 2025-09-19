# SAFE: Single, Accurate, Fast, Easy bootstrapping to calculate effect sizes and point estimates.

This repo contains an online tutorial as well as scripts to conduct Monte Carlo simulations that compare SAFE bootstrapping bias to plugin estimates, as well as the influence of bootstrap length on the dispersion of estimate.

Please create an Issue if you have any questions for find any errors.

## Repo structure is as follows:

scripts: Transcribe_effect_formulas.R *This script is for writing effect sizes, and associated transformations, into formulas* 
         SAFE_function.R *This script has the functions that calculate plugin and SAFE effect sizes and sampling variance*

builds: *Simulation output data. These are loaded in the tutorial*

data: effect_size_formulas.csv *This the data that guides the functions in SAFE_function.R*

run_simulations: *This folder has the scripts to manage cluster runs of the simulations, as well as a remote mirror of the cluster.*

run_simulations/manage_simulations: *Scripts to manage simulations, including creating scenarios, managing cluster runs, and summarizing results*

run_simulations/remote_mirrors: *This folder is synced to the Canada Alliance SLURM cluster and managed with the `.sh` shell scripts.*

run_simulations/remote_mirrors/SAFE_simulation.R *This is the key R script that conducts simulations*
                                      
