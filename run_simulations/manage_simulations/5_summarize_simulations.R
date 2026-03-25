# Summarize the revision simulations
#
#
#
#
#
library("data.table")

files <- list.files("run_simulations/remote_mirrors/revision_paired_simulations/outputs/",
                    full.names = T)
files <- files[!grepl("lnOR", files)]
files <- files[!grepl("lnRR", files)]

dat <- lapply(files, readRDS)
# Uh oh
dat <- list()

errors <- c()
for(i in 1:length(files)){
  tryCatch({
    dat[[i]] <- readRDS(files[i])
  },
  error = function(e){
    errors <- i
  })
} 
dat
# errors
any(sapply(dat, is.null))
is_null_vector <- sapply(dat, is.null)
which(is_null_vector)
dat[[4061]]


files[4061]
file.exists(files[2182])
readRDS("run_simulations/remote_mirrors/revision_paired_simulations/outputs//SMD_batch_1806.Rds")
readRDS(files[4061])
readRDS(files[2183])
