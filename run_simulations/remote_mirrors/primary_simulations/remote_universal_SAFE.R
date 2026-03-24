#
#
#
#
# 
#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------

# DEBUGGING ---------------------------------------------------------------

DEBUG <- F
if(DEBUG){
  
  rm(list = ls())
  library("data.table")
  library("MASS")
  library("tmvtnorm")
  source("remote_mirrors/final_simulations/remote_universal_SAFE.R")
  # So that subfunctions are in environment
  
  
  eff_size(a = 5, b = 15, c = 10, d = 15,
           effect_type = "lnOR",
           SAFE_distribution = "2_multinomial_as_normal",
           verbose = F,
           SAFE_boots = 1e6)
  eff_size(a = 5, b = 15, c = 10, d = 15,
           effect_type = "lnOR",
           SAFE_distribution = "4_binomial",
           verbose = F,
           SAFE_boots = 1e6)
  
  
  eff_size(a = 5*10, b = 15*10, c = 10*10, d = 15*10,
           effect_type = "lnOR",
           SAFE_distribution = "2_multinomial_as_normal",
           verbose = F,
           SAFE_boots = 1e6)
  eff_size(a = 5*10, b = 15*10, c = 10*10, d = 15*10,
           effect_type = "lnOR",
           SAFE_distribution = "4_binomial",
           verbose = F,
           SAFE_boots = 1e6)
  
  
  input_vars <- list(a = 5*10, b = 15*10, c = 10*10, d = 15*10)
  effect_type = "lnOR"
  SAFE_distribution = "2_multinomial_as_normal"
  verbose = F
  SAFE_boots = 1e6
  sigma_matrix <- NULL
  
  
  # ORbinomial <- eff_size(a = 5, b = 15, c = 10, d = 15,
  #                        effect_type = "lnOR",
  #                        SAFE_distribution = "4_binomial")
  # ORnormal
  # ORbinomial
  
  # input_vars <- list(a = c(1, 0.5, 5, 10, 1, 2, 5, 8, 1, 2, 4, 12, 2, 1, 4, 3, 3, 2.5,
  #                          3, 8, 3, 6.5, 18, 30, 4, 6, 14, 24, 3, 4, 15, 27, 4, 7, 10, 21,
  #                          3, 4, 14, 31, 3, 10, 28, 47, 4, 6, 29, 48, 5, 11, 25, 44, 3,
  #                          12, 19, 45, 4, 12, 21, 45, 9, 17, 39, 69, 9, 13, 36, 64, 5, 12,
  #                          37, 62, 5, 12, 28, 67, 4.5, 17, 34, 80, 10.5, 18.5, 44, 92, 9,
  #                          17, 45, 88, 8, 20.5, 42, 89, 8, 17, 41, 89, 8.5, 16, 48, 89),
  #                    b = c(9, 20.5, 45, 90, 9, 18, 45, 92, 9, 18, 46, 88, 8, 19, 46, 97,
  #                          7, 18.5, 47, 92, 7, 14.5, 32, 70, 6, 14, 36, 76, 7, 16, 35, 73,
  #                          6, 13, 40, 79, 7, 16, 36, 69, 7, 10, 22, 53, 6, 14, 21, 52, 5,
  #                          9, 25, 56, 7, 8, 31, 55, 6, 8, 29, 55, 1, 3, 11, 31, 1, 7, 14,
  #                          36, 5, 8, 13, 38, 5, 8, 22, 33, 6.5, 3, 16, 20, 0.5, 2.5, 6,
  #                          8, 1, 3, 5, 12, 2, 0.5, 8, 11, 2, 3, 9, 11, 2.5, 4, 2, 11),
  #                    c = c(2, 1.5, 7, 8, 3, 4, 19, 35, 5, 10, 22, 52, 5, 15, 34, 77, 9,
  #                          20.5, 47, 93, 2, 0.5, 2, 18, 5, 4, 9, 36, 3, 9, 25, 53, 7, 15,
  #                          36, 72, 8, 17, 46, 88, 2, 5, 2, 12, 3, 6, 15, 29, 8, 8, 28, 60,
  #                          7, 14, 33, 66, 8, 18, 42, 85, 3, 3, 5, 8, 4, 7, 21, 33, 3, 5,
  #                          23, 49, 7, 16, 34, 70, 10.5, 17, 46, 94, 0.5, 0.5, 6, 6, 3, 5,
  #                          13, 32, 5, 10.5, 32, 41, 7, 11, 32, 68, 10.5, 19, 42, 90),
  #                    d = c(8, 19.5, 43, 92, 7, 16, 31, 65, 5, 10, 28, 48, 5, 5, 16, 23,
  #                          1, 0.5, 3, 7, 8, 20.5, 48, 82, 5, 16, 41, 64, 7, 11, 25, 47,
  #                          3, 5, 14, 28, 2, 3, 4, 12, 8, 15, 48, 88, 7, 14, 35, 71, 2, 12,
  #                          22, 40, 3, 6, 17, 34, 2, 2, 8, 15, 7, 17, 45, 92, 6, 13, 29,
  #                          67, 7, 15, 27, 51, 3, 4, 16, 30, 0.5, 3, 4, 6, 10.5, 20.5, 44,
  #                          94, 7, 15, 37, 68, 5, 10.5, 18, 59, 3, 9, 18, 32, 0.5, 1, 8,
  #                          10))#x1=x1, x2=x2, sd1=sd1, sd2=sd2, n1=n1, n2=n2, r = 0)
  
  # input_vars <- list(a = c(1, 0.5, 5, 12, 1, 4, 9, 15, 1, 2, 7, 8, 3, 2, 4, 10, 2, 3, 
  #                          4, 10, 2, 8, 20, 36, 1, 4, 11, 28, 4, 4, 10, 27, 4, 5, 16, 25, 
  #                          4, 7, 10, 31, 4, 9, 31, 53, 5, 9, 27, 51, 6, 11, 21, 52, 5, 10, 
  #                          25, 54, 4, 13, 28, 49, 7, 13, 32, 68, 8, 12, 36, 78, 7, 18, 36, 
  #                          69, 5, 16, 30, 70, 6, 17, 38, 70, 10, 18, 42, 91, 6, 17, 47, 
  #                          87, 9, 19, 41, 86, 10, 17, 45, 88, 10, 19, 43, 93),
  #                    c = c(1, 2, 6, 9, 4, 10, 19, 30, 5, 9, 27, 49, 5, 13, 30, 66, 9, 
  #                          17, 47, 91, 2, 3, 6, 8, 3, 8, 15, 26, 7, 13, 27, 51, 10, 12, 
  #                          33, 72, 10, 18, 44, 90, 1, 2, 4, 8, 3, 4, 18, 36, 3, 9, 28, 49, 
  #                          7, 15, 36, 64, 9, 19, 41, 93, 1, 1, 4, 8, 4, 6, 21, 31, 7, 9, 
  #                          23, 57, 5, 17, 35, 67, 5, 19, 48, 91, 1, 3, 4, 6, 4, 4, 14, 36, 
  #                          4, 12, 26, 42, 8, 15, 38, 66, 10, 19, 48, 90),
  #                    n1 = c(10, 21, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 
  #                           50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 
  #                           20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 
  #                           10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 
  #                           100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 
  #                           50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 
  #                           20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100),  
  #                    n2 = c(10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 
  #                           50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 
  #                           20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 
  #                           10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 
  #                           100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 
  #                           50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100, 10, 
  #                           20, 50, 100, 10, 20, 50, 100, 10, 20, 50, 100))
  effect_type <- "SMD"
  unique(effect_formulas$name)
  effect_formulas.sub <- effect_formulas[name == effect_type, ]
  formulas <- effect_formulas[name == effect_type, ]
  unique(effect_formulas.sub$sim_family)
  SAFE_distribution <- "4_multivariate_normal"
  formulas <- formulas[sim_family %in% SAFE_distribution]
  formulas
  
  input_vars <- list(  x1 = 13.633949, sd1 = 3.391806,
                       x2 = 12.14842,
                       sd2 = 5.529664, n1 = 5, n2 = 3)
  input <- lapply(input_vars, "[[", 1)
  input

  out1 <- eff_size(x1 = 15, sd1 = 5, x2 = 12, sd2 = 4.5,
                   n1 = 25, n2 = 25,
                   # r = 0.5,
                   SAFE_distribution = "4_multivariate_normal",
                   effect_type = "SMD")
  
  unique(effect_formulas$sim_family)
  out1 <- eff_size(x1 = 15, sd1 = 5, x2 = 12, sd2 = 4.5,
                   n1 = 25, n2 = 25,
                   # r = 0.5,
                   SAFE_distribution = "4_multivariate_normal_wishart",
                   effect_type = "SMD")
  # 
  # out2 <- eff_size(x1 = 15, sd1 = 5, x2 = 12, sd2 = 4.5, 
  #                  n1 = 25, n2 = 25,
  #                  # r = 0.5,
  #                  SAFE_distribution = "4_multivariate_normal_wishart",
  #                  effect_type = "SMD")
  # 
  # out1[, SAFE_family := "Normal"]
  # out2[, SAFE_family := "Wishart"]
  # 
  # rbind(out1, out2)
  safe1e3 <- list()
  safe1e6 <- list()
  
  for(i in 1:50){
    safe1e3[[i]] <- eff_size(x1 = 15, sd1 = 5, x2 = 12, sd2 = 4.5,
                     n1 = 25, n2 = 25,
                     SAFE_distribution = "4_multivariate_normal_wishart",
                     verbose = F,
                     SAFE_boots = 1e2,
                     effect_type = "SMD")
    safe1e6[[i]] <- eff_size(x1 = 15, sd1 = 5, x2 = 12, sd2 = 4.5,
                             n1 = 25, n2 = 25,
                             SAFE_distribution = "4_multivariate_normal_wishart",
                             verbose = F,
                             SAFE_boots = 1e7,
                             effect_type = "SMD")
    
    print(i, "\r")
  }
  
  safe1e3.dt <- rbindlist(safe1e3)
  safe1e6.dt <- rbindlist(safe1e6)
  
  sd(safe1e3.dt$yi_safe) / sd(safe1e6.dt$yi_safe)
  
  sd(safe1e3.dt$vi_safe) / sd(safe1e6.dt$vi_safe)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# FUNCTIONS ---------------------------------------------------------------

#' [FUNCTION DESCRIPTION:]
#' *4 functions:*
#' *1. eff_size:* This is the master function that executes both plugin and SAFE calculations and is meant to be the user-facing function
#' *2. calc_effect* This function evaluates the formulas to calculate point/SE effect size calculations
#' *3. SAFE_calc* This function manages the SAFE calculations, including transforming the hyperparameters and calculating bias-corrected point estimates
#' *4. parameter_cloud* This function is the most complex as it creates sigma matrices appropriate for different types of effect sizes. It is called by SAFE_calc and returns a parameter cloud 
#' 
#' [CALLS:]
#' *eff_size* [->] *calc_effect*
#' *eff_size* [->] *SAFE_calc*
#' *SAFE_calc* [->] *parameter_cloud*

#' *MASTER FUNCTION*
eff_size <- function(..., #' [This is where the input variables are passed in.]
                     effect_type = NULL,
                     SAFE = TRUE,
                     SAFE_boots = 1e6,
                     SAFE_distribution = NULL, #' [Aug 2025: Looks like some of these should provide a choice...]
                     sigma_matrix = NULL, #' [Custom sigma matrix. Needs to be a list calculated off of data of hte same length as input_vars. Maybe down the road this could be a custom function]
                     verbose = T){
  
  # >>> Prepare function ----------------------------------------------------
  input_vars <- list(...)
  
  require("data.table")
  require("crayon")
  require("MASS")
  require("tmvtnorm")
  
  effect_formulas <- fread("data/effect_size_formulas.csv")
  setorder(effect_formulas, name, calc_type)
  
  # >>> Preliminary checks and filtering --------------------------------------------------
  
  #' *Could build some tutorial information into this:*
  if(is.null(effect_type)){
    cat(red(("\nMust specify an effect size type ('effect_type') and necessary variables (named in arguments to function call) to match formula equations.\n")), 
        blue("\nReturning effect size names & required variables for reference.\n\n"))
    return(unique(effect_formulas[, .(name, vars_required)]))
  }else{
    # filter to desired effect_type  and calculation
    effect_formulas.sub <- effect_formulas[name == effect_type, ]
  }

  if(length(unique(lengths(input_vars))) > 1){ return(cat("Input vectors", "(", red(paste(names(input_vars), collapse = ", ")), ")",  "are different lengths. Please double check inputs.")) }
  
  # Deal with missing 'r' 
  if(grepl("paired", effect_type) & !"r" %in% names(input_vars)){ 
    cat("Paired design selected", red("but 'r' not specified."), "Setting 'r' to 0.5")
    input_vars$r <- rep(0.5, max(lengths(input_vars)))
  }else if(!grepl("paired", effect_type) & !"r" %in% names(input_vars) ){
    #' [Set to 0 for non-paired effects. ]
    input_vars$r <- rep(0, max(lengths(input_vars))) # This is necessary for the shared sigma_matrices of some effect sizes
  }
  
  # Check for missing variables.
  vars <- strsplit(unique(effect_formulas.sub$vars_required), split = ", ") |> unlist()
  if(length(setdiff(vars, names(input_vars))) > 0){ 
    return(cat("Missing the following variables:", 
               red(paste(setdiff(vars, names(input_vars)), collapse=", ")), "\n"))
  }
  
  # Print effect size specific warnings, e.g., 0 in lnOR and lnRR
  if(!is.na(unique(effect_formulas.sub$special_warnings)) & verbose == TRUE){
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
  
  # Extract reference plugin effect size. First order.
  plugin_effect_size <- plugins$yi_first
  
  #' [Need to lapply through each element in input_vars. This would benefit from parallelization. But not on cluster..]
  index <- seq(1:max(lengths(input_vars)))
  k <- 1
  
  if(length(plugin_effect_size) != max(index)){ return(cat("Shit.")) }
  
  #' *For debugging:*
  # formulas = effect_formulas.sub
  # k <- 1
  # input_k = lapply(input_vars, "[[", k) # select the first element in each element...
  # plugin_effect_k = plugin_effect_size[k]
  # sigma_matrix_k = sigma_matrix[[k]] # submit custom sigma_matrix if it exists.
  # SAFE_boots = 1e6
  # index <- seq(1:5)
  # Run SAFE function for each element of input_vars:
  
  safe_out <- lapply(index, function(k){
    if(verbose) cat("SAFE:", magenta(k, "/", max(index), "\r"))
    
    return(SAFE_calc(formulas = effect_formulas.sub,
              input_k = lapply(input_vars, "[[", k), # select the first element in each element...
              plugin_effect_k = plugin_effect_size[k],
              sigma_matrix_k = sigma_matrix[[k]], # submit custom sigma_matrix if it exists.
              SAFE_boots = SAFE_boots)) 
  }) |> 
    rbindlist()
  
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
  
  # This gathers them:
  # out <- eval(parse(text = paste0("data.table(", paste(unique(formulas$label), collapse = ", "), ")")))
  
  return(eval(parse(text = paste0("data.table(", paste(unique(formulas$label), collapse = ", "), ")"))))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
#' *This calls parameter cloud and does calculations:*

SAFE_calc <- function(formulas,
                      input_k,
                      plugin_effect_k,
                      sigma_matrix_k,
                      SAFE_boots = NULL){
  #' *For debugging:*
  # input = input_k
  # sigma_matrix = sigma_matrix_k #' if specified by user. Otherwise calculated based on sim_family
  # SAFE_boots = SAFE_boots
  # 
  cloud <- parameter_cloud(formulas = formulas, 
                           paired = ifelse(grepl("paired", formulas$name), 
                                                 "yes", "no"),
                           input = input_k,
                           sigma_matrix = sigma_matrix_k, #' if specified by user. Otherwise calculated based on sim_family
                           SAFE_boots = SAFE_boots)
  # unique(cloud[, .(a, b, c, d)])
  
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
  
  return(data.table(yi_safe = safe_yi,
                    vi_safe = safe_vi,
                    SE_safe = safe_SE))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
#' *CLOUD MAKER*
parameter_cloud <- function(formulas,
                            paired = "no",
                            input,
                            sigma_matrix = NULL,
                            SAFE_boots = NULL){

  cat("Running SAFE with", SAFE_boots, "bootstraps\n\n")
  
  # Construct sigma matrices ------------------------------------------------
  if(any(formulas$sim_family %in% "1_normal")){
    if(is.null(sigma_matrix)){
      sigma_matrix <- input$sd / sqrt(input$n)
    }
    means <- c(x = input$x)
    
  }else if(any(formulas$sim_family %in% "2_multivariate_normal")){
    if(is.null(sigma_matrix)){
      
      sigma_matrix <- matrix(data = c((input$sd1^2 / input$n1),                    (input$r*input$sd1*input$sd2)/input$n1, #  / n1 add this to sd1^2
                                      (input$r*input$sd1*input$sd2)/input$n1,      (input$sd2^2 / input$n2)), #  / n2 add this to sd2^2
                             nrow = 2, ncol = 2)
      
    }
    means <- c(x1 = input$x1, x2 = input$x2)
    
  }else if(any(formulas$sim_family == "4_multivariate_normal_wishart")){
    if(is.null(sigma_matrix)){
      
      sigma_matrix <- matrix(c(input$sd1^2, input$r*input$sd1*input$sd2,
                               input$r*input$sd1*input$sd2, input$sd2^2), 
                             2, 2)
      
    }
    means <- c(x1 = input$x1, x2 = input$x2)
    
    # means <- c(x1 = input$x1, x2 = input$x2)
  }else if(any(formulas$sim_family == "4_multivariate_normal")){
    if(is.null(sigma_matrix)){
      
      sigma_matrix <- matrix(data = c(input$sd1^2/input$n1,                  (input$r*input$sd1*input$sd2)/input$n1,  0,                                                  0,
                                      (input$r*input$sd1*input$sd2)/input$n1, (input$sd2^2)/input$n2,                   0,                                                  0,
                                      0,                                      0,                                      (2*input$sd1^4)/(input$n1-1),                       ((2*input$r^2*input$sd1^2*input$sd2^2)/(input$n1-1)),
                                      0,                                      0,                                      (2*input$r^2*input$sd1^2*input$sd2^2)/(input$n1-1), (2*input$sd2^4)/(input$n2-1)),
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
  if(!all(is.na(formulas$lower_filter))){
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
    
    #' [NEED TO SORT by names of means object...]
    var_guide <- var_guide[order(match(names(means), variable))]
  
  }else if(!unique(formulas$sim_family) %in% c("2_binomial", "4_binomial", "3_multinomial") &
           all(is.na(formulas$lower_filter))){
    var_guide <- data.table(mean=means |> as.numeric(), 
                            variable = names(means),
                            lower = -Inf,
                            upper = Inf)
  }
  

  # Create Gaussian clouds ------------------------------------------------------------
  if(unique(formulas$sim_family == "1_normal")){
    
    out <- data.table(x = rnorm(n=SAFE_boots,
                                mean = var_guide$mean, 
                                sd = sigma_matrix))
    return(out)
    
  }else if(unique(formulas$sim_family %in% c("4_multivariate_normal",
                                             "2_multivariate_normal",
                                             "2_multinomial_as_normal"))){
    
    out <- rtmvnorm(n = SAFE_boots,
                    mean = var_guide$mean,
                    sigma = sigma_matrix,
                    lower = var_guide$lower_bounds,
                    upper = var_guide$upper_bounds) |>
      as.data.frame() |>
      setDT()
    names(out) <- var_guide$variable
    
    if(unique(formulas$sim_family == "4_multivariate_normal")){
      #' *Back convert the variance hyperparameters to SD*
      out[, `:=` (sd1 = sqrt(v1), sd2 = sqrt(v2))]
      out[, `:=` (v1 = NULL, v2 = NULL)]
    }
    if(unique(formulas$sim_family == "2_multinomial_as_normal")){
      out[, `:=` (n1 = input$n1, n2 = input$n2)]
      out[, `:=` (a = round(p1 * n1),
                  c = round(p2 * n2))]
      out[, `:=` (b = n1 - a,
                  d = n2 - c)]
    }
    #' [I really don't like this degree of specificity of effect_type manipulation inside the function]
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
  }else if(unique(formulas$sim_family %in% c("4_multivariate_normal_wishart"))){
    
    out <- rtmvnorm(n = SAFE_boots,
                    mean = var_guide$mean,
                    sigma = (sigma_matrix / c(input$n1, sqrt(input$n1*input$n2), sqrt(input$n1*input$n2), input$n2)),
                    lower = var_guide$lower_bounds,
                    upper = var_guide$upper_bounds) |>
      as.data.frame() |>
      setDT()
    names(out) <- var_guide$variable
    #
    wishart.out <-  stats::rWishart(SAFE_boots, 
                                    df = (max(c(input$n1, input$n2)) -1), 
                                    Sigma = sigma_matrix) 
    out[, sd1 := sqrt(wishart.out[1, 1, ] / (input$n1 - 1))]
    out[, sd2 := sqrt(wishart.out[2, 2, ] / (input$n2 - 1))]
    out
    
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
                      #b = rbinom(SAFE_boots, input$n1, input$b / input$n1) |> as.double(),
                      c = rbinom(SAFE_boots, input$n2, input$c / input$n2) |> as.double()#,
                      #d = rbinom(SAFE_boots, input$n2, input$d / input$n2) |> as.double()
                      )

    out[, `:=` (b = input$n1 - a,
                d = input$n2 - c)]
    # Add 0.5 to rows with ANY zero
    # if(nrow(out[(a == 0 | b == 0 | c == 0 | d == 0), ]) > 0){
    out[(a == 0 | b == 0 | c == 0 | d == 0), `:=` 
        (a = a + 0.5,
        b = b + 0.5,
        c = c + 0.5,
        d = d + 0.5)]
    # }
    return(out)
    
  }else if(any(formulas$sim_family == "3_multinomial")){
    N <- (input$n_AA + input$n_Aa + input$n_aa)
    out <- stats::rmultinom(n = SAFE_boots,
                            size = N,
                            prob = c(n_AA = input$n_AA/N,
                                     n_Aa = input$n_Aa/N,
                                     n_aa = input$n_aa/N)) |>
      t() |> # For some reason these are returned WIDE, with 3 rows and 1e6 columns. Weird. Was freezing computer
      as.data.frame()
    
    data.table::setDT(out)
    out[, `:=` (n_AA = as.double(n_AA),
                n_Aa = as.double(n_Aa),
                n_aa = as.double(n_aa))]
    
    # if(nrow(out[(n_AA == 0 | n_Aa == 0 | n_aa == 0), ]) > 0){
    out[(n_AA == 0 | n_Aa == 0 | n_aa == 0), 
        `:=` (n_AA = n_AA + 0.5,
              n_Aa = n_Aa + 0.5,
              n_aa = n_aa + 0.5)]
    # }
    
    return(out)
  }
  return(cat("unexpected error 1: sim_family did not match"))
}

