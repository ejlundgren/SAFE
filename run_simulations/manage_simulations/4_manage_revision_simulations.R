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
# 
# updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
#             no_jobs = 3)
# 
# updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
#           gb = "6gb",
#           time = "6:00:00")

# 30824314 ejlundgr def-snakagaw revision_sim_j   R    5:52:57     1    1        N/A      6G fc30560 (None) 
# 30824315 ejlundgr def-snakagaw revision_sim_j   R    5:52:57     1    1        N/A      6G fc30560 (None) 
# 30824316 ejlundgr def-snakagaw revision_sim_j   R    5:52:57     1    1        N/A      6G fc30560 (None) 
# This is for i in 1:1000
# Looks like only needs 8% of 6gb and about 3 hours

# >>> Round 1 -------------------------------------------------------------

# guide <- guide[chunk > 3, ] #' [Shouldn't have omitted all of these]
guide[, chunk := .GRP, by = .(batch_id)]
unique(guide$chunk)

saveRDS(guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")

updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
            no_jobs = max(guide$chunk))

updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
          gb = "800M",
          time = "12:00:00")

# >>> Round 2 -------------------------------------------------------------

files <- list.files("run_simulations/remote_mirrors/revision_paired_simulations/outputs/")
files <- gsub(".Rds", "", files)
guide <- guide[!batch_id %in% files, ]
guide

guide[, chunk := .GRP, by = .(batch_id)]
guide
saveRDS(guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")

unique(guide$checkpoint_path)
checkpoint <- readRDS(paste0("run_simulations/remote_mirrors/revision_paired_simulations/",
                             unique(guide$checkpoint_path)))
length(checkpoint)
updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
            no_jobs = max(guide$chunk))

updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
          gb = "500M",
          time = "4:00:00")
# 32913733
#