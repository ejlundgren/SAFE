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

#
# Load data ---------------------------------------------------------------
#
guide <- readRDS("run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")
guide <- guide[!effect_type %in% c("lnOR", "lnRR")]

files <- list.files("run_simulations/remote_mirrors/revision_paired_simulations/outputs/")
files[grepl("lnCVR", files)]
files[grepl("SMD", files)]

files <- gsub(".Rds", "", files)
length(files)

guide <- guide[!batch_id %in% files, ]
unique(guide$effect_type)

guide[, chunk := .GRP, by = .(batch_id)]
max(guide$chunk)
guide[, .(n = uniqueN(effect_type)), by = .(chunk)][n > 1]
# Must be 0 rows

# Why aren't these finished? 
unique(guide$batch_id)

sub <- guide[batch_id == "lnRoM_batch_1818"]
eff_size(x1 = sub$true_mean1, x2 = sub$true_mean2, 
         sd1 = sub$true_sd1, sd2 = sub$true_sd2,
         r = sub$r, n = sub$n,
         effect_type = "SMD_paired")

sub <- guide[batch_id == "lnCVR_batch_1647"]
eff_size(x1 = sub$true_mean1, x2 = sub$true_mean2, 
         sd1 = sub$true_sd1, sd2 = sub$true_sd2,
         r = sub$r, n = sub$n,
         effect_type = "lnCVR_paired")

sub <- guide[batch_id == "lnRoM_batch_4828"]
eff_size(x1 = sub$true_mean1, x2 = sub$true_mean2, 
         sd1 = sub$true_sd1, sd2 = sub$true_sd2,
         r = sub$r, n = sub$n,
         effect_type = "lnRoM_paired")


# sub <- guide[batch_id == "lnOR_batch_4909"]
# eff_size(a = sub$true_mean1, b = sub$true_mean2, 
#          c = sub$true_sd1, d = sub$true_sd2,
#          effect_type = "lnRoM_paired")

guide
saveRDS(guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")

updateArray(sh_path = "run_simulations/remote_mirrors/revision_paired_simulations/submit_array.sh",
            no_jobs = max(guide$chunk))

updateJob(job_path = "run_simulations/remote_mirrors/revision_paired_simulations/sim_job.sh",
          gb = "6gb",
          time = "4:00:00")

#



