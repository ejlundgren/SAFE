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
          time = "6:00:00")


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
# 30380955 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380956 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380957 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380958 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380959 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380960 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380961 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380962 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380963 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1        N/A      6G  (Priority) 
# 30380964 ejlundgr def-snakagaw revision_sim_j  PD    4:00:00     1    1 