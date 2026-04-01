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

guide <- copy(full_guide)

saveRDS(guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")

# >>> Test required allocation --------------------------------------------

updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
            no_jobs = 3)

updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
          gb = "6gb",
          time = "6:00:00")

# 30824314 ejlundgr def-snakagaw revision_sim_j   R    5:52:57     1    1        N/A      6G fc30560 (None) 
# 30824315 ejlundgr def-snakagaw revision_sim_j   R    5:52:57     1    1        N/A      6G fc30560 (None) 
# 30824316 ejlundgr def-snakagaw revision_sim_j   R    5:52:57     1    1        N/A      6G fc30560 (None) 
# This is for i in 1:1000
# Looks like only needs 8% of 6gb and about 3 hours

# >>> Round 1 -------------------------------------------------------------

guide <- guide[chunk > 3, ] #' [Shouldn't have omitted all of these]
guide[, chunk := .GRP, by = .(batch_id)]
unique(guide$chunk)

saveRDS(guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")

updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
            no_jobs = max(guide$chunk))

updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
          gb = "800M",
          time = "12:00:00")

# 30585449 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585450 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585451 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585452 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585453 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585454 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585455 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585456 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585457 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585458 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585459 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585460 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585461 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585462 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585463 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585464 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585465 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585466 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585467 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585468 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585469 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585470 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585471 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585472 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585473 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585474 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585475 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585476 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585477 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585478 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585479 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585480 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585481 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585482 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585483 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585484 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585485 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
# 30585486 ejlundgr def-snakagaw revision_sim_j  PD    6:00:00     1    1        N/A      6G  (None) 
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