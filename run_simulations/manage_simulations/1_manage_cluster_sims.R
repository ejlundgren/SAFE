# July 17th 2025
#
#
# Remote simulations. Use interactive sessions to update cluster runs
#
#
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# Prepare environment --------------------------------------------------

rm(list = ls())
gc()

library("data.table")
library("crayon")
library("MASS")

# >>> Load helper functions -----------------------------------------------
# These are for updating array and job scripts on each run:

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# Load scenarios ------------------------------------------------------

# >>> Test 1 --------------------------------------------------------------
# Let's make a small working_guide of just a few scenarios for testing.
run <- F
if(run){
  # This is run in an interactive session in terminal on the cluster
  library("data.table")
  scenarios <- readRDS("data/scenarios.Rds")
  
  # Save a small subset:
  # Select first of each effect type
  working_scenarios <- scenarios[, .SD[1], by = .(effect_type)]
  # Add a chunk ID:
  working_scenarios[, chunk := .GRP, by = .(run_ID)]
  unique(working_scenarios[, .(chunk, run_ID, scenario_id)])
  unique(working_scenarios[, .(chunk, run_ID)])
  
  max(working_scenarios$chunk)
  # Save working guide:
  saveRDS(working_scenarios, "data/working_scenarios.Rds")
  
  # FOR CHECKING logs and outfiles:
  paths <- list.files("logs", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
  paths <- list.files("outfiles", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
}

# Update LOCAL remote mirror shell scripts.
updateArray(sh_path = "remote_mirrors/final_simulations/submit_array.sh",
            no_jobs = 9) 
readLines("remote_mirrors/final_simulations/submit_array.sh")

updateJob(job_path = "remote_mirrors/final_simulations/sim_job.sh",
          gb = "500M", 
          time = "00:30:00")
readLines("remote_mirrors/final_simulations/sim_job.sh")
# These are then copied with Globus
# 1824613 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30106 (None) 
# 1824614 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30106 (None) 
# 1824615 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30106 (None) 
# 1824616 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30107 (None) 
# 1824617 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30107 (None) 
# 1824618 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30107 (None) 
# 1824619 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30110 (None) 
# 1824620 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30111 (None) 
# 1824621 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30111 (None) 
# 1824622 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30115 (None) 
# 1824623 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30116 (None) 
# 1824624 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30116 (None) 
# 1824625 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30118 (None) 
# 1824626 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30118 (None) 
# 1824627 ejlundgr def-snakagaw        sim_job   R    4:45:06     1    1        N/A      4G fc30118 (None) 


# >>> Full run 1 --------------------------------------------------------------
run <- F
if(run){
  # This is run in an interactive session in terminal
  library("data.table")
  scenarios <- readRDS("data/scenarios.Rds")
  
  files <- list.files("outputs/")
  files <- gsub(".Rds", "", files)
  
  # Filter out finished run_IDs:
  working_scenarios <- scenarios[!run_ID %in% files, ]
  
  # Update chunk:
  working_scenarios[, chunk := .GRP, by = .(run_ID)]
  max(working_scenarios$chunk)
  
  saveRDS(working_scenarios, "data/working_scenarios.Rds")
  
  # FOR CHECKING:
  paths <- list.files("logs", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
  paths <- list.files("outfiles", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
}

# Update LOCAL remote mirror shell scripts.
# <1e7 scenarios:
updateArray(sh_path = "remote_mirrors/final_simulations/submit_array.sh",
            no_jobs = 855) 
readLines("remote_mirrors/final_simulations/submit_array.sh")

updateJob(job_path = "remote_mirrors/final_simulations/sim_job.sh",
          gb = "1gb", 
          time = "12:00:00")
readLines("remote_mirrors/final_simulations/sim_job.sh")


# 1830949 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830950 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830951 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830952 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830953 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830954 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830955 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830956 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830957 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830958 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830959 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830960 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 
# 1830961 ejlundgr def-snakagaw        sim_job  PD    3:00:00     1    1        N/A      1G  (None) 

# >>> Full run 2 --------------------------------------------------------------
run <- F
if(run){
  # This is run in an interactive session in terminal
  library("data.table")
  scenarios <- readRDS("data/scenarios.Rds")
  unique(scenarios$boots)
  
  files <- list.files("outputs/")
  files <- gsub(".Rds", "", files)
  
  # Save remaining scenarios
    working_scenarios <- scenarios[!run_ID %in% files, ]
  working_scenarios[, chunk := .GRP, by = .(run_ID)]
  max(working_scenarios$chunk)
  #
  saveRDS(working_scenarios, "data/working_scenarios.Rds")
  
 
  # FOR CHECKING:
  paths <- list.files("logs", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
  paths <- list.files("outfiles", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
  class <- c()
  for(i in 1:nrow(working_scenarios)){
    class[i] <- lapply(readRDS(working_scenarios$checkpoint_path, class)) |> unlist() |> unique()
    cat(i)
  }
  class
  
}

# Update LOCAL remote mirror shell scripts.
# Let's start with a low-ball estimate for each. A lot finished within 15 minutes.
updateArray(sh_path = "remote_mirrors/final_simulations/submit_array.sh",
            no_jobs = 300) 
readLines("remote_mirrors/final_simulations/submit_array.sh")

updateJob(job_path = "remote_mirrors/final_simulations/sim_job.sh",
          gb = "3gb", 
          time = "15:00:00")
readLines("remote_mirrors/final_simulations/sim_job.sh")

# 
# 4809777 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809778 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809779 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809780 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809781 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809782 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809783 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809784 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809785 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809786 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809787 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809788 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809789 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809790 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809791 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809792 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809793 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809794 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809795 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809796 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809797 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809798 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809799 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809800 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809801 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809802 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809803 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809804 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809805 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809806 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809807 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809808 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
# 4809809 ejlundgr def-snakagaw        sim_job  PD   15:00:00     1    1        N/A      1G  (Priority) 
