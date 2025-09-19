# July 17th 2025
#
#
# Remote simulations
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

# First step is to use GLOBUS to copy the remote files.
# The SAFE_simulations.R script loads working_scenarios.Rds
# We change that locally (and the shell scripts) and then update remote with GLOBUS.

# >>> Test 1 --------------------------------------------------------------
# Let's make a small working_guide of just a few scenarios for testing.
run <- F
if(run){
  # This is run in an interactive session in terminal
  library("data.table")
  scenarios <- readRDS("data/scenarios.Rds")
  
  # Save a small subset:
  working_scenarios <- scenarios[, .SD[1:3], by = .(scenario_id)]
  working_scenarios[, chunk := .GRP, by = .(run_ID)]
  unique(working_scenarios[, .(chunk, run_ID, scenario_id)])
  unique(working_scenarios[, .(chunk, run_ID)])
  
  max(working_scenarios$chunk)
  saveRDS(working_scenarios, "data/working_scenarios.Rds")

  # FOR CHECKING:
  paths <- list.files("logs", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
  paths <- list.files("outfiles", full.names = T)
  logs <- lapply(paths, readLines)
  lapply(logs, tail, 5)
  
  len <- c()
  for(i in 1:nrow(working_scenarios)){
    len[i] <- length(readRDS(working_scenarios$checkpoint_path[i]))
    cat(i)
  }
  len
  
  class <- c()
  for(i in 1:nrow(working_scenarios)){
    class[i] <- lapply(readRDS(working_scenarios$checkpoint_path, class)) |> unlist() |> unique()
    cat(i)
  }
  class
  
}

# Update LOCAL remote mirror shell scripts.
updateArray(sh_path = "remote_mirrors/final_simulations/submit_array.sh",
            no_jobs = 27) 
readLines("remote_mirrors/final_simulations/submit_array.sh")

updateJob(job_path = "remote_mirrors/final_simulations/sim_job.sh",
          gb = "4gb", 
          time = "5:00:00")
readLines("remote_mirrors/final_simulations/sim_job.sh")

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

  working_scenarios <- scenarios[!run_ID %in% files, ]
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
  
  len <- c()
  for(i in 1:nrow(working_scenarios)){
    len[i] <- length(readRDS(working_scenarios$checkpoint_path[i]))
    cat(i)
  }
  len
  
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
            no_jobs = 1800) # Update to equal max(working_scenarios$chunk)
readLines("remote_mirrors/final_simulations/submit_array.sh")

updateJob(job_path = "remote_mirrors/final_simulations/sim_job.sh",
          gb = "1gb", 
          time = "3:00:00")
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
# Let's make a small working_guide of just a few scenarios for testing.
run <- F
if(run){
  # This is run in an interactive session in terminal
  library("data.table")
  scenarios <- readRDS("data/scenarios.Rds")
  unique(scenarios$boots)
  
  files <- list.files("outputs/")
  files <- gsub(".Rds", "", files)
  
  # Save remaining scenarios
  # First submit the < 1+07 (and use 1gb)
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

  # Check status of checkpoints:
  paths <- unique(working_scenarios$checkpoint_path)
  paths[!file.exists(paths)]
  paths <- paths[file.exists(paths)]
  len <- c()
  for(i in 1:nrow(working_scenarios)){
    len[i] <- length(readRDS(working_scenarios$checkpoint_path[i]))
    cat(i)
  }
  len
  names(len) <- paths[1:length(lens)]
  
  class <- c()
  for(i in 1:nrow(working_scenarios)){
    class[i] <- lapply(readRDS(working_scenarios$checkpoint_path, class)) |> unlist() |> unique()
    cat(i)
  }
  class
  
  
  library("data.table")
  working_scenarios <- readRDS("data/working_scenarios.Rds")
  unique(working_scenarios$boots)
  
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

# The <1e7 runs:
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

# The 1e7 runs:
# 4810658 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30509 (None) 
# 4810659 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30510 (None) 
# 4810660 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30510 (None) 
# 4810661 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30511 (None) 
# 4810662 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30511 (None) 
# 4810663 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30511 (None) 
# 4810664 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30511 (None) 
# 4810665 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30511 (None) 
# 4810666 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30511 (None) 
# 4810667 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30511 (None) 
# 4810668 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30512 (None) 
# 4810669 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30513 (None) 
# 4810670 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30513 (None) 
# 4810671 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30513 (None) 
# 4810672 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810673 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810674 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810675 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810676 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810677 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810678 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810679 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810680 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810681 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30515 (None) 
# 4810682 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810683 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810684 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810685 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810686 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810687 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810688 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810689 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30517 (None) 
# 4810690 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30519 (None) 
# 4810691 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30519 (None) 
# 4810692 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30519 (None) 
# 4810693 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30519 (None) 
# 4810694 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30524 (None) 
# 4810695 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30524 (None) 
# 4810696 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30524 (None) 
# 4810697 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30524 (None) 
# 4810698 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30526 (None) 
# 4810699 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30526 (None) 
# 4810700 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30527 (None) 
# 4810701 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810702 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810703 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810704 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810705 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810706 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810707 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810708 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810709 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810710 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30528 (None) 
# 4810711 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30530 (None) 
# 4810712 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30530 (None) 
# 4810713 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30530 (None) 
# 4810714 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30530 (None) 
# 4810715 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30530 (None) 
# 4810716 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30532 (None) 
# 4810717 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30532 (None) 
# 4810718 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30532 (None) 
# 4810719 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30532 (None) 
# 4810720 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30532 (None) 
# 4810721 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30538 (None) 
# 4810722 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30538 (None) 
# 4810723 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30538 (None) 
# 4810724 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30538 (None) 
# 4810725 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30538 (None) 
# 4810726 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30539 (None) 
# 4810727 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30541 (None) 
# 4810728 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30541 (None) 
# 4810729 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30541 (None) 
# 4810730 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30541 (None) 
# 4810731 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30541 (None) 
# 4810732 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810733 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810734 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810735 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810737 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810738 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810739 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810740 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810741 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810742 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810743 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810744 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810745 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810746 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810747 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30542 (None) 
# 4810748 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30543 (None) 
# 4810749 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810750 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810751 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810752 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810753 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810754 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810755 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810756 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810757 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810758 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810759 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810760 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810761 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30544 (None) 
# 4810762 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810763 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810764 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810765 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810766 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810767 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810768 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810769 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810770 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810771 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810772 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810773 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810774 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810775 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810776 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810777 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810778 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810779 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810780 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30545 (None) 
# 4810781 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30550 (None) 
# 4810782 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30550 (None) 
# 4810783 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30550 (None) 
# 4810784 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30551 (None) 
# 4810785 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30551 (None) 
# 4810786 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30551 (None) 
# 4810787 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30551 (None) 
# 4810788 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30551 (None) 
# 4810789 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30551 (None) 
# 4810790 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810791 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810792 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810793 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810794 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810795 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810796 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810797 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810798 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30552 (None) 
# 4810799 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30355 (None) 
# 4810800 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30360 (None) 
# 4810801 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30360 (None) 
# 4810802 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30361 (None) 
# 4810803 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30370 (None) 
# 4810804 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30371 (None) 
# 4810805 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30371 (None) 
# 4810806 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30372 (None) 
# 4810807 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30372 (None) 
# 4810808 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30372 (None) 
# 4810809 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30372 (None) 
# 4810810 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30401 (None) 
# 4810811 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30402 (None) 
# 4810812 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30402 (None) 
# 4810813 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30402 (None) 
# 4810814 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30402 (None) 
# 4810815 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30404 (None) 
# 4810816 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30404 (None) 
# 4810817 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30404 (None) 
# 4810818 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30404 (None) 
# 4810819 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30404 (None) 
# 4810820 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30404 (None) 
# 4810821 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30405 (None) 
# 4810822 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30405 (None) 
# 4810823 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30406 (None) 
# 4810824 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30406 (None) 
# 4810825 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30406 (None) 
# 4810826 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30406 (None) 
# 4810827 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30406 (None) 
# 4810828 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30406 (None) 
# 4810829 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30407 (None) 
# 4810830 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30407 (None) 
# 4810831 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30407 (None) 
# 4810832 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30407 (None) 
# 4810833 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30407 (None) 
# 4810834 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30407 (None) 
# 4810835 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30407 (None) 
# 4810836 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30408 (None) 
# 4810837 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30408 (None) 
# 4810838 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30408 (None) 
# 4810839 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30408 (None) 
# 4810840 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30409 (None) 
# 4810841 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30409 (None) 
# 4810842 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30409 (None) 
# 4810843 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30411 (None) 
# 4810844 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30411 (None) 
# 4810845 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30411 (None) 
# 4810846 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30411 (None) 
# 4810847 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30411 (None) 
# 4810848 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30411 (None) 
# 4810850 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30411 (None) 
# 4810851 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30413 (None) 
# 4810852 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30414 (None) 
# 4810853 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30414 (None) 
# 4810854 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30414 (None) 
# 4810855 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30414 (None) 
# 4810856 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30414 (None) 
# 4810857 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30415 (None) 
# 4810858 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30415 (None) 
# 4810859 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30415 (None) 
# 4810860 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30415 (None) 
# 4810861 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30416 (None) 
# 4810862 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30416 (None) 
# 4810863 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30416 (None) 
# 4810864 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30417 (None) 
# 4810865 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30418 (None) 
# 4810866 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30419 (None) 
# 4810867 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810868 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810869 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810870 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810871 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810872 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810873 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810874 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810875 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810876 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810877 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30420 (None) 
# 4810878 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30421 (None) 
# 4810879 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30421 (None) 
# 4810880 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30421 (None) 
# 4810881 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30421 (None) 
# 4810884 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30423 (None) 
# 4810885 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30424 (None) 
# 4810886 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30424 (None) 
# 4810887 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30424 (None) 
# 4810888 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30426 (None) 
# 4810889 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30426 (None) 
# 4810890 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30426 (None) 
# 4810891 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30426 (None) 
# 4810892 ejlundgr def-snakagaw        sim_job   R   14:54:17     1    1        N/A      3G fc30426 (None) 

