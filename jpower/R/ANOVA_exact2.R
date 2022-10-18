ANOVA_exact2 <- function(design_result, 
                         correction = "none", 
                         alpha_level = .05, 
                         verbose = FALSE,
                         emm = FALSE,
                         emm_model = "univariate",
                         contrast_type = "pairwise",
                         emm_comp,
                         liberal_lambda = FALSE) {
  
  #Need this to avoid "undefined" global error from occuring
  cohen_f <- partial_eta_squared <- non_centrality <- NULL
  #New checks for emmeans input
  if (missing(emm)) {
    emm = FALSE
  }
  
  if (missing(emm_model)) {
    emm_model = "multivariate"
  }
  
  #Follow if statements limit the possible input for emmeans specifications
  if (emm == TRUE) {
    if (is.element(emm_model, c("univariate", "multivariate")) == FALSE ) {
      stop("emm_model must be set to \"univariate\" or \"multivariate\". ")
    }
    if (is.element(contrast_type, 
                   c("pairwise", 
                     "revpairwise",
                     "eff",
                     "consec",
                     "poly",
                     "del.eff",
                     "trt.vs.ctrl",
                     "trt.vs.ctrl1",
                     "trt.vs.ctrlk",
                     "mean_chg"
                   )) == FALSE ) {
      stop("contrast_type must be of an accepted format. 
           The tukey & dunnett options currently not supported in ANOVA_exact. 
           See help(\"contrast-methods\") for details on the exact methods")
    }
  }
  if (is.element(correction, c("none", "GG", "HF")) == FALSE ) {
    stop("Correction for sphericity can only be none, GG, or HF")
  }

  #Check to ensure there is a within subject factor -- if none --> no MANOVA
  run_manova <- grepl("w", design_result$design)
  
  round_dig <- 4 #Set digits to which you want to round the output.
  
  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }
  
  if (alpha_level >= 1 | alpha_level <= 0  ) {
    stop("alpha_level must be less than 1 and greater than zero")
  }
  
  #Read in all variables from the design_result object
  design <- design_result$design #String used to specify the design
  factornames <- design_result$factornames #Get factor names
  n <- design_result$n
  if (length(n) != 1 ) {
    warning("Unequal n designs can only be passed to ANOVA_power")
  }
  mu = design_result$mu # population means - should match up with the design
  sd <- design_result$sd #population standard deviation (currently assumes equal variances)
  r <- design_result$r # correlation between within factors (currently only 1 value can be entered)
  factors <- design_result$factors
  design_factors <- design_result$design_factors
  sigmatrix <- design_result$sigmatrix
  dataframe_df <- monte_gen(design_result,
                            n = n)
  design_list <- design_result$design_list
  
  
  
  ###############
  #Specify factors for formula ----
  ###############
  
  frml1 <- design_result$frml1
  frml2 <- design_result$frml2
  
  aov_result_df <- suppressMessages({afex::aov_car(frml1, #here we use frml1 to enter formula 1 as designed above on the basis of the design
                                             data = dataframe_df, 
                                             include_aov = if (emm_model == "univariate"){
                                               TRUE
                                             } else {
                                               FALSE
                                             },
                                             anova_table = list(es = "pes",
                                                                correction = correction)) }) #This reports PES not GES
  anova_table_df <- as.data.frame(aov_result_df$anova_table)
  colnames(anova_table_df) <- c("num_Df", "den_Df", "MSE", "F", "pes", "p")

  
  #Setup df for emmeans
  if(emm == TRUE){
    #Call emmeans with specifcations given in the function
    #Limited to specs and model
    if(missing(emm_comp)){
      emm_comp = as.character(frml2)[2]
    }
    
    specs_formula <- as.formula(paste(contrast_type," ~ ",emm_comp))
    emm_result_DF <- suppressMessages({emmeans(aov_result_df, 
                                               specs = specs_formula,
                                               model = emm_model,
                                               adjust = "none")})
    
  } else{
    emm_result_DF = NULL
  }
  
  ###############
  # Set up dataframe for storing empirical results
  ###############
  
  #How many possible planned comparisons are there (to store p and es)
  possible_pc <- (((prod(
    as.numeric(strsplit(design, "\\D+")[[1]])
  )) ^ 2) - prod(as.numeric(strsplit(design, "\\D+")[[1]])))/2
  
  #create empty dataframe to store simulation results
  #number of columns for ANOVA results and planned comparisons, times 2 (p-values and effect sizes)
  sim_data <- as.data.frame(matrix(
    ncol = 2 * (2 ^ factors - 1) + 2 * possible_pc,
    nrow = 1
  ))
  
  paired_tests <- combn(unique(dataframe_df$cond),2)
  paired_p <- numeric(possible_pc)
  paired_d <- numeric(possible_pc)
  within_between <- sigmatrix[lower.tri(sigmatrix)] #based on whether correlation is 0 or not, we can determine if we should run a paired or independent t-test
  
  #Dynamically create names for the data we will store
  names(sim_data) = c(paste("anova_",
                            rownames(aov_result_df$anova_table),
                            sep = ""),
                      paste("anova_es_",
                            rownames(aov_result_df$anova_table),
                            sep = ""),
                      paste("p_",
                            paste(paired_tests[1,],paired_tests[2,],sep = "_"),
                            sep = ""),
                      paste("d_",
                            paste(paired_tests[1,],paired_tests[2,], sep = "_"),
                            sep = ""))
  
  #We simulate a new y variable, melt it in long format, and add it to the dataframe (surpressing messages)
  #empirical set to true to create "exact" dataset
  
  dataframe <- exact_gen(design_result)
  
  # We perform the ANOVA using AFEX
  aov_result <- suppressMessages({afex::aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design
                                          data = dataframe, include_aov = if(emm_model == "univariate"){
                                            TRUE
                                          } else {
                                            FALSE
                                          }, #Need development code to get aov_include function
                                          anova_table = list(es = "pes",
                                                             correction = correction))}) #This reports PES not GES
  
  
  
  #Add additional statistics
  #Create dataframe from afex results
  anova_table <- as.data.frame(aov_result$anova_table)
  
  colnames(anova_table) <- c("num_Df", "den_Df", "MSE", "F", "pes", "p")
  
  anova_table$num_Df = anova_table_df$num_Df
  anova_table$den_Df = anova_table_df$den_Df
  
  #Calculate cohen's f
  anova_table$f2 <- anova_table$pes/(1 - anova_table$pes)
  #Calculate noncentrality
  anova_table$lambda <- if(liberal_lambda == FALSE) {
    anova_table$f2 * anova_table$den_Df
  } else {
    anova_table$f2 * (anova_table$den_Df + anova_table$num_Df + 1)
  }
  
  #minusalpha<- 1-alpha_level
  anova_table$Ft <- qf((1 - alpha_level), anova_table$num_Df, anova_table$den_Df)
  #Calculate power
  anova_table$power <- power_ftest(
    num_df = anova_table$num_Df,
    den_df = anova_table$den_Df,
    cohen_f = sqrt(anova_table$f2),
    alpha_level = alpha_level
  )$power
  
  

  if(emm == TRUE){
    #Call emmeans with specifcations given in the function
    #Limited to specs and model
    if(missing(emm_comp)){
      emm_comp = as.character(frml2)[2]
    }
    
    specs_formula <- as.formula(paste(contrast_type," ~ ",emm_comp))
    emm_result <- suppressMessages({emmeans(aov_result, 
                                            specs = specs_formula,
                                            model = emm_model,
                                            adjust = "none")})
    emm_result = as.data.frame(emm_result$contrasts)
    #emm_result$df = as.data.frame(emm_result_DF$contrasts)$df
    
    #obtain pes
    pairs_result_df1 <- emmeans_power(emm_result, 
                                      alpha_level = alpha_level,
                                      liberal_lambda = liberal_lambda)
    
    pairs_result_df1$df_actual =  as.data.frame(emm_result_DF$contrasts)$df
    pairs_result_df1$t_actual = sqrt((
      -1 * pairs_result_df1$partial_eta_squared * pairs_result_df1$df_actual
    ) / (pairs_result_df1$partial_eta_squared - 1)
    )
    
    emm_result$t.ratio = pairs_result_df1$t_actual
    emm_result$df = pairs_result_df1$df_actual
    
    emm_result$SE = emm_result$estimate/emm_result$t.ratio
    pairs_result_df = emmeans_power(emm_result, 
                                    alpha_level = alpha_level,
                                    liberal_lambda = liberal_lambda)
    
  } else{
    pairs_result_df = NULL
    #plot_emm = NULL
    emm_result = NULL
  }
  
  for (j in 1:possible_pc) {
    x <- dataframe$y[which(dataframe$cond == paired_tests[1,j])]
    y <- dataframe$y[which(dataframe$cond == paired_tests[2,j])]
    #this can be sped up by tweaking the functions that are loaded to only give p and dz
    ifelse(within_between[j] == 0,
           t_test_res <- effect_size_d_exact2(x, y, sample_size = n,
                                              alpha_level = alpha_level),
           t_test_res <- effect_size_d_paired_exact2(x, y, sample_size = n,
                                                     alpha_level = alpha_level))
    paired_p[j] <- (t_test_res$power*100)
    paired_d[j] <- ifelse(within_between[j] == 0,
                          t_test_res$d,
                          t_test_res$d_z)
  }
  
  # store p-values and effect sizes for calculations
  sim_data[1,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                    aov_result$anova_table[[5]], #partial eta squared
                    paired_p, #power for paired comparisons, dropped correction for multiple comparisons
                    paired_d) #effect sizes
  
  ###############
  #Sumary of power and effect sizes of main effects and contrasts ----
  ###############
  #ANOVA
  main_results <- data.frame(anova_table$power,
                             anova_table$pes,
                             sqrt(anova_table$f2),
                             anova_table$lambda)
  
  rownames(main_results) <- rownames(anova_table)
  colnames(main_results) <- c("power", "partial_eta_squared", "cohen_f", "non_centrality")
  main_results$power <- main_results$power
  anova_table <- data.frame(effect = rownames(anova_table),
                            num_df = anova_table$num_Df,
                            den_df = anova_table$den_Df,
                            MSE = anova_table$MSE,
                            F.ratio = anova_table$`F`,
                            p.value = anova_table$p)
  
  
  #Data summary for pairwise comparisons
  power_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]), 2,
                                     function(x) x))
  
  es_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + possible_pc + 1):(2*(2 ^ factors - 1) + 2 * possible_pc)]), 2,
                                  function(x) x))
  
  pc_results <- data.frame(power_paired, es_paired)
  names(pc_results) = c("power","effect_size")
  

  
  # Return results in S3 sim_result
  
  structure(list(main_results = main_results,
                 #pc_results = pc_results,
                 #emm_results = pairs_result_df,
                 anova_table = anova_table,
                 #manova_table = manova_table,
                 #emmeans_table = emm_result,
                 alpha_level = alpha_level,
                 #plot = meansplot2,
                 method = "ANOVA_exact2"))
}


effect_size_d_exact2 <- function(x, y, sample_size, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  n1 <- sample_size
  n2 <- sample_size
  df <- n1 + n2 - 2
  m_diff <- mean(y) - mean(x)
  sd_pooled <- (sqrt((((n1 - 1) * ((sd1^2))) + (n2 - 1) * ((sd2^2))) / ((n1 + n2 - 2)))) #pooled standard deviation
  j <- (1 - 3/(4 * (n1 + n2 - 2) - 1))  #Calculate Hedges' correction.
  t_value <- m_diff / sqrt(sd_pooled^2 / n1 + sd_pooled^2 / n2)
  p_value = 2*pt(-abs(t_value), df = df)
  #Calculate power
  power = power.t.test(
    n = n1,
    delta = m_diff,
    sd = sd_pooled,
    type = "two.sample",
    alternative = "two.sided",
    strict = TRUE,
    sig.level = alpha_level
  )$power
  
  d <- m_diff / sd_pooled #Cohen's d
  d_unb <- d*j #Hedges g, of unbiased d
  
  invisible(list(d = d,
                 d_unb = d_unb,
                 p_value = p_value,
                 power = power))
}

effect_size_d_paired_exact2 <- function(x, y, sample_size, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(x - y) #standard deviation of the difference scores
  N <- sample_size
  df = N - 1
  s_av <- sqrt((sd1 ^ 2 + sd2 ^ 2) / 2) #averaged standard deviation of both measurements
  
  #Cohen's d_av, using s_av as standardizer
  m_diff <- mean(y - x)
  d_av <- m_diff / s_av
  d_av_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_av
  
  #get the t-value for the CI
  t_value <- m_diff / (s_diff / sqrt(N))
  p_value = 2 * pt(-abs(t_value), df = df)
  
  power = power.t.test(
    n = N,
    delta = m_diff,
    sd = s_diff,
    type = "paired",
    alternative = "two.sided",
    strict = TRUE,
    sig.level = alpha_level
  )$power
  
  #Cohen's d_z, using s_diff as standardizer
  d_z <- t_value / sqrt(N)
  d_z_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_z
  
  invisible(list(
    d_z = d_z,
    d_z_unb = d_z_unb,
    p_value = p_value,
    power = power
  ))
}