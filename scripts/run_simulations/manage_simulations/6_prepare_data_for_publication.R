#
#
#
#
#

# We don't want to publish all the separate run_IDs. So simplify scenario table:
library("data.table")

#' [The publication repo is SAFE_meta/SAFE/. But the meta_simulations repo will continue to be the workhorse.]

# Copy raw scenarios file (after dropping confusing cluster columns)
scenarios <- readRDS("remote_mirrors/final_simulations/data/scenarios.Rds")

scenarios

scenarios <- unique(scenarios[, !c("run_ID", "seed", "chunk", "file_path", "checkpoint_path")])
scenarios

saveRDS(scenarios, "../SAFE_meta/SAFE/tutorial/data/scenarios.Rds")
#

# Copy summaries:
file.copy("remote_mirrors/final_simulations/summaries/all_scenarios_summarized.Rds",
          "../SAFE_meta/SAFE/tutorial/data/all_scenarios_summarized.Rds")
# original

file.exists("remote_mirrors/final_simulations/remote_universal_SAFE.R")
file.copy("remote_mirrors/final_simulations/remote_universal_SAFE.R",
          "../SAFE_meta/SAFE/tutorial/SAFE_function.R",
          overwrite = T)


file.copy("remote_mirrors/final_simulations/data/effect_size_formulas.csv",
          "../SAFE_meta/SAFE/tutorial/data/effect_size_formulas.csv",
          overwrite = T)


# scenarios[effect_type == "lnRoM"]
# Load and bind lnRoM for demo purposes -----------------------------------
paths <- list.files("remote_mirrors/final_simulations/outputs/", full.names = T)
paths <- paths[grepl("lnRoM", paths) & grepl("1e[+]06", paths)]
paths

sim <- lapply(paths, readRDS)
sim <- rbindlist(sim)
sim
sim[, .(n = .N), by = .(scenario_id)]


sim <- sim[, .SD[1:1e5], by = .(scenario_id)]
sim

x <- lapply(sim, function(x){
  if(all(is.na(x))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}) |> unlist()

sim <- sim[, names(x[x == TRUE]), with = F]
sim

sim <- sim[, !c("checkpoint_path", "file_path", "run_ID", "batch_ID",
                "seed", "total_iterations_needed", "iterations_per_core", 
                "number_of_cores_needed", "file_path", "checkpoint_path",
                "chunk")]
setnames(sim, c("sim_n1", "sim_n2"), c("sim_sample_size1", "sim_sample_size2"))

sim
saveRDS(sim, "../SAFE_meta/SAFE/tutorial/data/lnRoM_simulation.Rds")


