# Helper functions

power_ftest <- function(num_df = NULL, den_df = NULL,
                        cohen_f = NULL,
                        alpha_level = 0.5,
                        beta_level = NULL) 
{
  
  if (!is.null(cohen_f)) {
    if (any(cohen_f < 0)) {
      stop("cohen_f must be positive")
    }
  }
  if (!is.null(num_df) && any(num_df < 1)) {
    stop("degree of freedom num_df for numerator must be at least 1")
  }
  
  if (!is.null(den_df) && any(den_df < 1)) {
    stop("degree of freedom den_df for denominator must be at least 1")
  }
  if (!is.null(alpha_level) && !is.numeric(alpha_level) || any(0 > 
                                                               alpha_level | alpha_level > 1)) {
    stop(sQuote("alpha_level"), " must be numeric in [0, 1]")
  }
  
  if (!is.null(beta_level) && !is.numeric(beta_level) || any(0 > beta_level | 
                                                             beta_level > 1)) {
    stop(sQuote("beta_level"), " must be numeric in [0, 1].")
  } 
  

    
  p.body <- quote({
    pf(
      qf(alpha_level, num_df, den_df, lower.tail =  FALSE),
      num_df,
      den_df,
      cohen_f ^ 2 * (den_df),
      lower.tail = FALSE
    )
  })
  
  
  if (!is.null(beta_level)){
    pow = 1 - beta_level
  } 
  
  if (is.null(beta_level)){
    pow <- eval(p.body)
  } else if (is.null(num_df)) {
    p.body2 = p.body[2]
    p.body2 = gsub("alpha_level",
                   alpha_level,
                   p.body2)
    p.body2 = gsub("den_df",
                   den_df,
                   p.body2)
    p.body2 = gsub("cohen_f",
                   cohen_f,
                   p.body2)
    
    num_df = optimize(f = function(num_df) {
      abs(eval(parse(text=paste(p.body2)))-pow)
    }, c(0,1000))$min
    
    #num_df <- uniroot(function(num_df) eval(p.body) - pow, c(1, 100))$root
  }
  else if (is.null(den_df)) {
    
    den_df <- uniroot(function(den_df) eval(p.body) - pow, c(1 + 
                                                               1e-10, 1e+09))$root
  }
  else if (is.null(cohen_f)) {
    cohen_f <- uniroot(function(cohen_f) eval(p.body) - pow, c(1e-07, 
                                                               1e+07))$root
  }
  else if (is.null(alpha_level)) {
    alpha_level <- uniroot(function(alpha_level) eval(p.body) - 
                             pow, c(1e-10, 1 - 1e-10))$root
  }
  else {
    stop("internal error: exactly one of num_df, den_df, cohen_f, beta_level, and alpha_level must be NULL")
  }
  
  power_final = pow * 100
  beta_level = 1 - pow
  METHOD <- "Power Calculation for F-test"
  structure(list(num_df = num_df, 
                 den_df = den_df, 
                 cohen_f = cohen_f, 
                 alpha_level = alpha_level, 
                 beta_level = beta_level,
                 power = power_final, 
                 method = METHOD), class = "power.htest")
}


gen_df_n = function(n,
                    des_string,
                    mu_len,
                    cohen_f){
  des1 = ANOVA_design(des_string,
                                  mu = 1:mu_len,
                                  sd = 1,
                                  r = 0.5,
                                  n = n)
  
  aov1 = suppressMessages({
    as.data.frame(afex::aov_car(des1$frml1,
                                data = des1$dataframe,
                                include_aov = FALSE)$anova_table)
  })
  aov2 = aov1[1:2]
  colnames(aov2) = c("num_df","den_df")
  aov2$factor = rownames(aov2)
  aov2$n = n
  
  df_res = data.frame(factor = aov2$factor,
                      num_df = aov2$num_df,
                      den_df = aov2$den_df,
                      n = aov2$n)
}
