#
#
# Functions to calculate effect sizes based on plugin formulas and SAFE.
#
# 
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# FUNCTIONS ---------------------------------------------------------------

#' [FUNCTION DESCRIPTION:]
#' *4 functions:*
#' *1. eff_size:* This is the master function that executes both plugin and SAFE calculations and is meant to be the user-facing function
#' *2. calc_effect* This function evaluates the formulas to calculate point/SE effect size calculations
#' *3. SAFE_calc* This function manages the SAFE calculations, including transforming the hyperparameters and calculating bias-corrected point estimates
#' *4. parameter_cloud* This function is the most complex as it creates sigma matrices appropriate for different types of effect sizes. It returns a hyperparameter cloud
#' 
#' [CALLS:]
#' *eff_size* [->] *calc_effect*
#' *eff_size* [->] *SAFE_calc*
#' *SAFE_calc* [->] *parameter_cloud* [->] *calc_effect*

#' *MASTER FUNCTION*
eff_size <- function(..., 
                     effect_type = NULL,
                     SAFE = TRUE,
                     SAFE_boots = 1e6,
                     SAFE_distribution = NULL,
                     parallelize = FALSE,
                     verbose = T){
  
  # >>> Prepare function ----------------------------------------------------
  input_vars <- list(...)
  
  require("data.table")
  require("crayon")
  require("MASS")
  require("tmvtnorm")
  require("parallel")
  
  effect_formulas <- fread("data/effect_size_formulas.csv")
  setorder(effect_formulas, name, calc_type)
  
  # >>> Preliminary checks and filtering --------------------------------------------------
  
  if(is.null(effect_type)){
    cat(red(("\nMust specify an effect size type ('effect_type') and necessary variables (named in arguments to function call) to match formula equations.\n")), 
        blue("\nReturning effect size names & required variables for reference.\n\n"))
    return(unique(effect_formulas[, .(name, vars_required)]))
  }else{
    # filter to desired effect_type  and calculation
    effect_formulas.sub <- effect_formulas[name == effect_type, ]
  }

  # Check that vectors are the same length:
  if(length(unique(lengths(input_vars))) > 1){ return(cat("Input vectors", "(", red(paste(names(input_vars), collapse = ", ")), ")",  "are different lengths. Please double check inputs.")) }
  
  # Deal with missing 'r' 
  if(grepl("paired", effect_type) & !"r" %in% names(input_vars)){ 
    cat("Paired design selected", red("but 'r' not specified."), "Setting 'r' to 0.5")
    
    input_vars$r <- rep(0.5, max(lengths(input_vars)))
    
  }else if(!grepl("paired", effect_type) & !"r" %in% names(input_vars) ){
    # Set to 0 for unpaired effects
    input_vars$r <- rep(0, max(lengths(input_vars))) # This is necessary for the shared sigma_matrices of some effect sizes
  }
  
  # Check for missing variables.
  vars <- strsplit(unique(effect_formulas.sub$vars_required), split = ", ") |> unlist()
  if(length(setdiff(vars, names(input_vars))) > 0){ 
    return(cat("Missing the following variables:", 
               red(paste(setdiff(vars, names(input_vars)), collapse=", ")), "\n"))
  }
  
  # Print effect size specific warnings, e.g., 0 in lnOR and lnRR
  if(!(is.na(unique(effect_formulas.sub$special_warnings)) | all(effect_formulas.sub$special_warnings == "")) & 
            verbose == TRUE){
    cat(unique(effect_formulas.sub$special_warnings), 
        "Leaving it to user's discretion to check prior to execution. Negative values will be returned as NA.\n\n")
  }
  
  # Deal with alternative SAFE distributions.
  if(is.null(SAFE_distribution) & "yes" %in% effect_formulas.sub$default_safe_family){
    # If unspecified (SAFE_distribution == NULL & there are multiple options for default, then choose default
    effect_formulas.sub <- effect_formulas.sub[default_safe_family == "yes", ]
  }else if(!is.null(SAFE_distribution)){
    # If SAFE_distribution is specified, subset to SAFE_distribution
    effect_formulas.sub <- effect_formulas.sub[sim_family == SAFE_distribution, ]
  }
  # If unspecified (SAFE_distribution == NULL & effect_formulas.sub$default is all NA then do nothing)
  
  if(nrow(effect_formulas.sub) == 0){    
    return(cat(red("\nEffect size not available after filtering to type."), 
               "\n\nEffect sizes currently supported include:", paste(sort(unique(effect_formulas$name)), collapse = "; "),
               blue("\n\nTo add custom effect sizes please see XXXX")) )
  }
  
  if(verbose){
    effect_formulas.sub[, to_console := paste0(label, " <- ", formula)]
    cat("Using the formulas:\n\t", blue(paste(effect_formulas.sub$to_console, collapse = "\n\t ")), 
        "\nBe sure that all variables in formula are correctly named.\n\n")
  }
  
  # >>> Calculate plugin effect size: -------------------------------------------------
  plugins <- calc_effect(effect_formulas.sub,
                         input_vars)
  plugins
  
  if(SAFE == FALSE){
    return(plugins)
  }
  # >>> SAFE calculation ----------------------------------------------------------------
  # Extract reference plugin effect size. First order/definition
  plugin_effect_size <- plugins$yi_first
  
  index <- seq(1:max(lengths(input_vars)))
  k <- 1
  
  if(length(plugin_effect_size) != max(index)){ return(cat("Error 1")) }
  
  #' *For debugging:*
  # formulas = effect_formulas.sub
  # k <- 1
  # input_k = lapply(input_vars, "[[", k) # select the first element in each element...
  # plugin_effect_k = plugin_effect_size[k]
  # SAFE_boots = 1e6
  # index <- seq(1:5)
  # Run SAFE function for each element of input_vars:
  
  if(parallelize == TRUE){
    
    safe_out <- parallel::mclapply(index, function(k){
    if(verbose) cat("SAFE:", magenta(k, "/", max(index), "\r"))
    
    return(SAFE_calc(formulas = effect_formulas.sub,
              input_k = lapply(input_vars, "[[", k), # select the first element in each element...
              plugin_effect_k = plugin_effect_size[k],
              verbose = verbose,
              SAFE_boots = SAFE_boots)) 
    },
    mc.cores = (parallel::detectCores()-1),
    mc.allow.recursive = TRUE) |> 
    rbindlist()
  
  }else{
    
    safe_out <- lapply(index, function(k){
      if(verbose) cat("SAFE:", magenta(k, "/", max(index), "\r"))
      
      return(SAFE_calc(formulas = effect_formulas.sub,
                       input_k = lapply(input_vars, "[[", k), # select the first element in each element...
                       plugin_effect_k = plugin_effect_size[k],
                       verbose = verbose,
                       SAFE_boots = SAFE_boots)) 
    }) |> 
      rbindlist()
    
  }
  
  out <- cbind(plugins, safe_out)
  
  return(out)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
#' *PLUGIN EVALUATOR*
calc_effect <- function(formulas,
                        input){
  # Concatenate the formulas into a single formula, separated with ';'
  exec <- paste(paste(formulas$label, "<-", formulas$exec_formula), collapse = "; ")
  
  # This adds the effects/variances to the local env but with name assignation:
  eval(parse(text = exec))
  
  # Gather variables from local function environment and return as data.table
  return(eval(parse(text = paste0("data.table(", paste(unique(formulas$label), collapse = ", "), ")"))))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
#' *This calls parameter cloud and does calculations:*

SAFE_calc <- function(formulas,
                      input_k,
                      plugin_effect_k,
                      verbose = TRUE,
                      SAFE_boots = NULL){
  #' *For debugging:*
  # input = input_k
  # sigma_matrix = sigma_matrix_k #' if specified by user. Otherwise calculated based on sim_family
  # SAFE_boots = SAFE_boots
  # 
  
  paired <- ifelse(grepl("paired", unique(formulas$name)), 
                   "yes", "no")
  
  
  cloud <- parameter_cloud(formulas = formulas, 
                           paired = paired,
                           verbose = verbose,
                           input = input_k,
                           SAFE_boots = SAFE_boots)

  # Add missing inputs (e.g., n)
  cloud <- data.table(cloud,
                      input_k[!names(input_k) %in% names(cloud)] |> unlist() |> t() |> data.table())
  
  # Convert cloud
  cloud_trans <- calc_effect(formulas = formulas[calc_type == "effect_size" &
                                                   derivative == "first", ],
                             input = cloud)$yi_first
  
  # bias corrected estimate of sampling variance and SE:
  safe_SE <- sd(cloud_trans)
  safe_vi <- safe_SE^2
  
  bias_SAFE <- mean(cloud_trans) - plugin_effect_k
  
  safe_yi <- plugin_effect_k - bias_SAFE
  
  # return(data.table(yi_safe = safe_yi,
  #                   vi_safe = safe_vi,
  #                   SE_safe = safe_SE))
  # TESTING INFLUENCE OF 'r'. 
  safe_out <- data.table(yi_safe = safe_yi,
                         vi_safe = safe_vi,
                         SE_safe = safe_SE)
  
  # Now let's do an alternative:
  if(paired == "yes"){
    cor <- cor.test(cloud$x1, cloud$x2)
    cloud$r <- cor$estimate   
    
    # print(unique(cloud$r))
    # print(head(cloud))
    
    cloud_trans <- calc_effect(formulas = formulas[calc_type == "effect_size" &
                                                     derivative == "first", ],
                               input = cloud)$yi_first
    
    # bias corrected estimate of sampling variance and SE:
    safe_SE <- sd(cloud_trans)
    safe_vi <- safe_SE^2
    
    bias_SAFE <- mean(cloud_trans) - plugin_effect_k
    
    safe_yi <- plugin_effect_k - bias_SAFE
    
    safe_out <- data.table(safe_out,
                           yi_alternative_safe = safe_yi,
                           vi_alternative_safe = safe_vi,
                           safe_test_r = cor$estimate   )
  }
  
  return(safe_out)
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
#' *CLOUD MAKER*
parameter_cloud <- function(formulas,
                            paired = "no",
                            input,
                            verbose = TRUE,
                            SAFE_boots = NULL){

  if(verbose) cat("Running SAFE with", SAFE_boots, "bootstraps\n\n")
 
  sigma_matrix <- NULL
  # Construct sigma matrices ------------------------------------------------
  if(any(formulas$sim_family %in% "1_normal")){
    if(is.null(sigma_matrix)){
      sigma_matrix <- input$sd / sqrt(input$n)
    }
    means <- c(x = input$x)
    
  # }else if(any(formulas$sim_family %in% "2_multivariate_normal")){
  #   if(is.null(sigma_matrix)){
  #     
      #sigma_matrix <- matrix(data = c((input$sd1^2 / input$n1),                    (input$r*input$sd1*input$sd2)/input$n1, #  / n1 add this to sd1^2
      #                                (input$r*input$sd1*input$sd2)/input$n1,      (input$sd2^2 / input$n2)), #  / n2 add this to sd2^2
      #                      nrow = 2, ncol = 2)
      
    #   # TODO -shinichi change 
    #   sigma_matrix <- matrix(data = c((input$sd1^2 / input$n1), 
    #                                   (input$r * input$sd1 * input$sd2) / sqrt(input$n1 * input$n2),
    #                                   (input$r * input$sd1 * input$sd2) / sqrt(input$n1 * input$n2), 
    #                                   (input$sd2^2 / input$n2)), 
    #                          nrow = 2, ncol = 2)
    #   
    #   
    # }
    # means <- c(x1 = input$x1, x2 = input$x2)
    # TODO -shinichi change 
  }else if(any(formulas$sim_family == "2_multivariate_normal_indep")){
    if(is.null(sigma_matrix)){
      sigma_matrix <- matrix(
        c(input$sd1^2 / input$n1, 0,
          0, input$sd2^2 / input$n2),
        nrow = 2, ncol = 2, byrow = TRUE
      )
    }
    means <- c(x1 = input$x1, x2 = input$x2)
    
  }else if(any(formulas$sim_family == "2_multivariate_normal_paired")){
    if(is.null(sigma_matrix)){
      sigma_matrix <- matrix(
        c(input$sd1^2 / input$n,
          input$r * input$sd1 * input$sd2 / input$n,
          input$r * input$sd1 * input$sd2 / input$n,
          input$sd2^2 / input$n),
        nrow = 2, ncol = 2, byrow = TRUE
      )
    }
    means <- c(x1 = input$x1, x2 = input$x2)  
    
  }else if(any(formulas$sim_family == "4_multivariate_normal_chisq_indep")){
    
    means <- c(x1 = input$x1, x2 = input$x2)
    
    # no sigma_matrix needed here; SDs will be drawn from chi-square later  
    
  }else if(any(formulas$sim_family == "4_multivariate_normal_wishart_paired")){
    if(is.null(sigma_matrix)){
      sigma_matrix <- matrix(
        c(input$sd1^2, input$r * input$sd1 * input$sd2,
          input$r * input$sd1 * input$sd2, input$sd2^2),
        nrow = 2, ncol = 2, byrow = TRUE
      )
    }
    means <- c(x1 = input$x1, x2 = input$x2)
    # means <- c(x1 = input$x1, x2 = input$x2)
  }else if(any(formulas$sim_family == "4_multivariate_normal")){
    if(is.null(sigma_matrix)){
      sigma_matrix <- matrix(data = c(input$sd1^2/input$n1,                    (input$r * input$sd1 * input$sd2) / sqrt(input$n1 * input$n2),  0,                                                  0,
                                      (input$r * input$sd1 * input$sd2) / sqrt(input$n1 * input$n2), (input$sd2^2)/input$n2,                   0,                                                  0,
                                      0,                                      0,                                      (2*input$sd1^4)/(input$n1-1),                       (2*input$r^2*input$sd1^2*input$sd2^2)/sqrt((input$n1 -1) * (input$n2-1)),
                                      0,                                      0,                                      (2*input$r^2*input$sd1^2*input$sd2^2)/sqrt((input$n1 -1) * (input$n2-1)),           (2*input$sd2^4)/(input$n2-1)),
                             nrow = 4,
                             ncol = 4)
      
    }
    means <- c(x1 = input$x1, x2 = input$x2, 
               v1 = input$sd1^2, v2 = input$sd2^2)
    
    
  }else if(any(formulas$sim_family %in% c("2_multinomial_as_normal"))){

      if(is.null(sigma_matrix)){
      
      if(!"n1" %in% names(input)){
        input$n1 <- input$a + input$b
        input$n2 <- input$c + input$d
      }
      input$p1 <- input$a / input$n1
      input$p2 <- input$c / input$n2
      
      if(input$p1 == 1){
        input$p1 <- input$p1 - 0.1
      }
      if(input$p2 == 1){
        input$p2 <- input$p2 - 0.1
      }
      
      # This is variance, which is what mvrnorm wants:
      input$v1 <- input$p1 * (1 - input$p1) #/ input$n1
      input$v2 <- input$p2 * (1 - input$p2) #/ input$n2
      input$r <- 0
      
      sigma_matrix <- matrix(data = c((input$v1 / input$n1),                    (input$r*input$v1*input$v2)/input$n1, #  / n1 add this to sd1^2
                                      (input$r*input$v1*input$v2)/input$n1,      (input$v2 / input$n2)), #  / n2 add this to sd2^2
                             nrow = 2, ncol = 2)
      
    }
    means <- c(p1 = input$p1, p2 = input$p2)
  }
  
  # Parse upper and lower bounds for truncated normal ------------------------------------------------
  if(!(all(is.na(formulas$lower_filter)) | all(formulas$lower_filter == ""))){
    formulas$lower_filter
    
    lower <- data.table::tstrsplit(unique(formulas$lower_filter), ",") |> 
      unlist() |>
      tstrsplit("=")
    
    upper <- data.table::tstrsplit(unique(formulas$upper_filter), ",") |> 
      unlist() |>
      tstrsplit("=")
    
    var_guide <- data.table::data.table(variable = lower[[1]] |> trimws(),
                                         lower_bounds = lower[[2]] |> as.numeric(),
                                         upper_bounds = upper[[2]] |> as.numeric()) |>
      merge(data.table(mean=means, variable = names(means)),
            by = "variable")
    
    var_guide <- var_guide[order(match(names(means), variable))]
  
  }else if(!unique(formulas$sim_family) %in% c("2_binomial", "4_binomial", "3_multinomial") &
           (all(is.na(formulas$lower_filter)) | all(formulas$lower_filter == ""))){
    var_guide <- data.table(mean=means |> as.numeric(), 
                            variable = names(means),
                            lower_bounds = -Inf,
                            upper_bounds = Inf)
  }
  

  # Create Gaussian clouds ------------------------------------------------------------
  if(unique(formulas$sim_family == "1_normal")){
    
    out <- data.table(x = rnorm(n=SAFE_boots,
                                mean = var_guide$mean, 
                                sd = sigma_matrix))
    return(out)
    
  # }else if(unique(formulas$sim_family %in% c("4_multivariate_normal",
  #                                            "2_multivariate_normal",
  #                                            "2_multinomial_as_normal"))){
  }else if(unique(formulas$sim_family) %in% c("4_multivariate_normal",
                                             "2_multivariate_normal_indep",
                                             "2_multivariate_normal_paired",
                                             "2_multinomial_as_normal")){  
    out <- rtmvnorm(n = SAFE_boots,
                    mean = var_guide$mean,
                    sigma = sigma_matrix,
                    lower = var_guide$lower_bounds,
                    upper = var_guide$upper_bounds) |>
      as.data.frame() |>
      setDT()
    names(out) <- var_guide$variable
    
    if(unique(formulas$sim_family) == "4_multivariate_normal"){
      #' *Back convert the variance hyperparameters to SD*
      out[, `:=` (sd1 = sqrt(v1), sd2 = sqrt(v2))]
      out[, `:=` (v1 = NULL, v2 = NULL)]
    }
    if(unique(formulas$sim_family) == "2_multinomial_as_normal"){
      out[, `:=` (n1 = input$n1, n2 = input$n2)]
      out[, `:=` (a = round(p1 * n1),
                  c = round(p2 * n2))]
      out[, `:=` (b = n1 - a,
                  d = n2 - c)]
    }
    
    if(unique(formulas$name) == "lnRR"){
      out[a == 0, `:=` (a = a + 0.5,
                        n1 = n1 + 1) ]
      out[c == 0, `:=` (c = c + 0.5,
                        n2 = n2 + 1) ]
    }
    if(unique(formulas$name) == "lnOR"){
      out[(a == 0 | b == 0 | c == 0 | d == 0), `:=` 
          (a = a + 0.5,
            b = b + 0.5,
            c = c + 0.5,
            d = d + 0.5)]
    }

    return(out)
    
  }else if(unique(formulas$sim_family) == "4_multivariate_normal_chisq_indep"){
    
    x1_star <- rnorm(SAFE_boots, mean = input$x1, sd = input$sd1 / sqrt(input$n1))
    x2_star <- rnorm(SAFE_boots, mean = input$x2, sd = input$sd2 / sqrt(input$n2))
    
    s1_sq_star <- input$sd1^2 * stats::rchisq(SAFE_boots, df = input$n1 - 1) / (input$n1 - 1)
    s2_sq_star <- input$sd2^2 * stats::rchisq(SAFE_boots, df = input$n2 - 1) / (input$n2 - 1)
    
    out <- data.table(
      x1 = x1_star,
      x2 = x2_star,
      sd1 = sqrt(s1_sq_star),
      sd2 = sqrt(s2_sq_star)
    )
    
    return(out) 
    
  }else if(unique(formulas$sim_family) == "4_multivariate_normal_wishart_paired"){
    
    mean_sigma <- sigma_matrix / input$n
    
    out <- rtmvnorm(
      n = SAFE_boots,
      mean = var_guide$mean,
      sigma = mean_sigma,
      lower = var_guide$lower_bounds,
      upper = var_guide$upper_bounds
    ) |>
      as.data.frame() |>
      setDT()
    
    names(out) <- var_guide$variable
    
    wishart.out <- stats::rWishart(
      SAFE_boots,
      df = input$n - 1,
      Sigma = sigma_matrix
    )
    
    out[, sd1 := sqrt(wishart.out[1, 1, ] / (input$n - 1))]
    out[, sd2 := sqrt(wishart.out[2, 2, ] / (input$n - 1))]
    
    return(out)
  }
  
  # Count data clouds --------------------------------------------------------------
  if(any(formulas$sim_family == "2_binomial")){ # lnRR

    out <- data.table(a = rbinom(SAFE_boots, input$n1, input$a / input$n1) |> as.double(),
                      c = rbinom(SAFE_boots, input$n2, input$c / input$n2) |> as.double())
    out[, n1 := input$n1]
    out[, n2 := input$n2]
    
    out[a == 0, `:=` (a = a + 0.5,
                      n1 = n1 + 1) ]
    out[c == 0, `:=` (c = c + 0.5,
                      n2 = n2 + 1) ]
    return(out)
    
  }else if(any(formulas$sim_family == "4_binomial")){ # this is lnOR
    if(!all(c("n1", "n2") %in% names(input))){
      input$n1 <- input$a + input$b
      input$n2 <- input$c + input$d
    }
    out <- data.table(a = rbinom(SAFE_boots, input$n1, input$a / input$n1) |> as.double(),
                      c = rbinom(SAFE_boots, input$n2, input$c / input$n2) |> as.double()#,
                      )

    out[, `:=` (b = input$n1 - a,
                d = input$n2 - c)]
    
    # Add 0.5 to rows with ANY zero
    out[(a == 0 | b == 0 | c == 0 | d == 0), `:=` 
        (a = a + 0.5,
        b = b + 0.5,
        c = c + 0.5,
        d = d + 0.5)]

    return(out)
    
  }else if(any(formulas$sim_family == "3_multinomial")){
    N <- (input$n_AA + input$n_Aa + input$n_aa)
    out <- stats::rmultinom(n = SAFE_boots,
                            size = N,
                            prob = c(n_AA = input$n_AA/N,
                                     n_Aa = input$n_Aa/N,
                                     n_aa = input$n_aa/N)) |>
      t() |> 
      as.data.frame()
    
    data.table::setDT(out)
    out[, `:=` (n_AA = as.double(n_AA),
                n_Aa = as.double(n_Aa),
                n_aa = as.double(n_aa))]
    
    out[(n_AA == 0 | n_Aa == 0 | n_aa == 0), 
        `:=` (n_AA = n_AA + 0.5,
              n_Aa = n_Aa + 0.5,
              n_aa = n_aa + 0.5)]
    
    return(out)
  }
  return(cat("unexpected error 1: sim_family did not match"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------

# DEBUGGING / TESTING---------------------------------------------------------------

debugging <- F
if(debugging){
  
  rm(list = ls())
  library("data.table")
  library("MASS")
  library("tmvtnorm")
  source("scripts/SAFE_function.R")
  # source("run_simulations/remote_mirrors/revision_paired_simulations/remote_universal_SAFE.R")
  
  # So that subfunctions are in environment

  effect_formulas <- fread("run_simulations/remote_mirrors/revision_paired_simulations/data/effect_size_formulas.csv", )
  effect_formulas[name == 'lnCVR_paired', .(sim_family, lower_filter, upper_filter)]
  
  verbose = T
  SAFE_boots = 1e6
  sigma_matrix <- NULL
  SAFE = TRUE
  SAFE_distribution = NULL
  parallelize = TRUE
  
  test <- structure(list(sim_mean1 = c(12.3779610391397, 14.2583769514043, 
                                       13.329383274105, 9.74354339260358, 14.669253617849, 13.4535756537553, 
                                       8.21480474273108, 13.5945219946739, 14.6171578476033), sim_mean2 = c(15.0769171241879, 
                                                                                                            16.2085509481021, 15.7135950404538, 14.4220579236131, 16.9086479977439, 
                                                                                                            15.8900925689122, 11.2760271115986, 16.871109293977, 17.0798845170929
                                       ), 
                         n = c(5, 15, 100, 5, 15, 100, 5, 15, 100),
                         r = c(0, 0, 0, 0.5, 0.5, 0.5, 0.8, 0.8, 0.8),
                         sim_sd1 = c(4.71508750633714, 4.54226384679711, 4.73334393567547, 
                                                      6.2249224388095, 4.52407684460912, 4.75442468995487, 4.90467396539065, 
                                                      5.57160083432024, 4.41125525234012), sim_sd2 = c(2.96125343063369, 
                                                                                                       4.80791790302363, 4.10368278775713, 4.45070927198768, 2.98447571679791, 
                                                                                                       4.09947919096842, 4.05101291166281, 3.73040712842924, 3.83794522487116
                                                      )), row.names = c(NA, -9L), class = c("data.table", "data.frame"
                                                      ))
  test
  
  eff_size(x1 = test$sim_mean1, x2 = test$sim_mean2,
           sd1 = test$sim_sd1, sd2 = test$sim_sd2,
           n = test$n, r = test$r,
           parallelize = FALSE,
           effect_type = "SMD_paired")
  
  input_vars <- list(x1 = test$true_mean1, x2 = test$true_mean2,
                     sd1 = test$true_sd1, sd2 = test$true_sd2,
                     n = test$n, r = test$r)
  effect_type = "lnRoM_paired"
  
  
  eff_size(x1 = test$true_mean1, x2 = test$true_mean2,
           sd1 = test$true_sd1, sd2 = test$true_sd2,
           n1 = test$n, n2 = test$n, #r = test$r,
           effect_type = "lnRoM_paired")
  
  

}






