#
#
#
#

rm(list = ls())
library("data.table")
source('run_simulations/remote_mirrors/revision_paired_simulations/remote_universal_SAFE.R')

updateArray <- function(sh_path,
                        no_jobs){
  
  if(!file.exists(sh_path)){
    print("File does not exist!")
  }else{
    submit_array.script <- readLines(sh_path)
    original_formula <- submit_array.script[grepl("max_formulas=", submit_array.script)]
    forms <- paste0("max_formulas=",  no_jobs)
    submit_array.script.mod <- gsub(original_formula, forms, submit_array.script)
    submit_array.script.mod
    writeLines(submit_array.script.mod, sh_path)
    print("Job array updated")
  }
  
}

updateJob <- function(job_path,
                      gb,
                      time){
  
  if(sum(gregexpr(":", time, fixed=TRUE)[[1]] > 0) != 2){
    print("ERROR: time should be h:m:s format. E.g., '4:00:00'")
  }else if(!file.exists(job_path)){
    print("ERROR: File does not exist!")
    
  }else{
    job.script <- readLines(job_path)
    
    # gb:
    original_slurm <- job.script[grepl("#SBATCH --mem-per-cpu=", job.script)]
    new_slurm <- paste0("#SBATCH --mem-per-cpu=",  gb)
    job.script.mod <- gsub(original_slurm, new_slurm, job.script)
    
    # time:
    
    original_slurm <- job.script.mod[grepl("#SBATCH --time ", job.script.mod)]
    new_slurm <- paste0("#SBATCH --time ",  time)
    job.script.mod2 <- gsub(original_slurm, new_slurm, job.script.mod)
    
    writeLines(job.script.mod2, job_path)
    print("Job script updated")
  }
  
}


# Load data ---------------------------------------------------------------
#
full_guide <- readRDS("run_simulations/remote_mirrors/revision_paired_simulations/data/scenarios.Rds")
unique(full_guide$effect_type)

# >>> Round 1 -------------------------------------------------------------
guide <- copy(full_guide)

saveRDS(guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")

updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
            no_jobs = max(guide$chunk))

updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
          gb = "6gb",
          time = "4:00:00")

#
# 30267094 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267095 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267096 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267097 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267098 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267099 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267100 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267101 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267102 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267103 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267104 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267105 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267106 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267107 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267108 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267109 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267110 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267111 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267112 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267113 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267114 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267115 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267116 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267117 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267118 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267119 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267120 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267121 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267122 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267123 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267124 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267168 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267169 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267170 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267171 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30267172 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 

# >>> Round 2 -------------------------------------------------------------

#' [Error in lower bounds of lnCVR.]

guide <- full_guide[effect_type %in% "lnCVR", ]
unique(guide$effect_type)

# Why aren't these finished? 
unique(guide$batch_id)

guide[, chunk := .GRP, by = .(batch_id)]
guide
saveRDS(guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")

updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
            no_jobs = max(guide$chunk))

updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
          gb = "6gb",
          time = "4:00:00")
