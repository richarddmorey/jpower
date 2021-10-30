
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm

muANOVAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "muANOVAClass",
    inherit = muANOVABase,
    private = list(
        .init = function() {
            designtab <- self$results$designtab
            row1 <- list()
            row1[['var[type]']] <- "Type of ANOVA"
            row1[['var[des]']] <- "Factors in ANOVA"
            row1[['var[n]']] <- "Sample Size per Condition"
            row1[['var[n_obs]']] <- "Total Number of Observations"
            row1[['var[n_tot]']] <- "Total Sample Size (Subjects)"
            
            row1[['val[type]']] <- ""
            row1[['val[des]']] <- ""
            row1[['val[n]']] <- ""
            row1[['val[n_obs]']] <- ""
            row1[['val[n_tot]']] <- ""
            
            
            
            designtab$setRow(rowNo=1, values=row1)
            
            table <- self$results$main
            table$addRow(rowKey="a", list(name="a"))
            tableRow <- list(num_df = "", 
                             den_df = "",
                             cohen_f = "",
                             alpha_level = "",
                             power = "")
            table$setRow(rowKey = "a", tableRow)
            
            #private$.initPowerTab()
            #private$.initPowerESTab()
            
        },
        .run = function() {
            
            n = self$options$n
            stdev = self$options$stdev
            withcorr = self$options$withcorr
            #pow = self$options$power
            #es = self$options$es
            dep <- self$options$get("dep")
            if (is.null(dep) || length(dep) == 0){
                stop("Must provide Effect Sizes.")
            }
            mu <- as.numeric(as.vector(self$data[[dep]]))
            mu <- mu[!is.na(mu)]
            alpha = self$options$alpha
            
            lev_fac_a = self$options$lev_fac_a
            lev_fac_b = self$options$lev_fac_b
            lev_fac_c = self$options$lev_fac_c
            type_fac_a = self$options$type_fac_a
            type_fac_b = self$options$type_fac_b
            type_fac_c = self$options$type_fac_c
            num_facs = self$options$num_facs
            
            if(num_facs == "one"){
                des_string = paste0(lev_fac_a,type_fac_a)
                mu_len = lev_fac_a
                n_obs = lev_fac_a*n
                n_tot = ifelse(type_fac_a == "b", lev_fac_a*n, n)
                tot_lvls = lev_fac_a
                fct_lvls = c("a")
                des_t = "One-way ANOVA"
                if(length(mu) != tot_lvls){
                    stop("Means must have a length of ", tot_lvls)
                }
                
            } else if(num_facs == "two"){
                des_string = paste0(lev_fac_a,type_fac_a,"*",lev_fac_b,type_fac_b)
                mu_len = lev_fac_a * lev_fac_b
                n_obs = mu_len*n
                mlt_a = ifelse(type_fac_a == "w", 1, lev_fac_a)
                mlt_b = ifelse(type_fac_b == "w", 1, lev_fac_b)
                n_tot = mlt_a*mlt_b
                fct_lvls = c("a", "b", "a:b")
                tot_lvls = lev_fac_a*lev_fac_b
                des_t = "Two-way ANOVA"
                if(length(mu) != tot_lvls){
                    stop("Means must have a length of ", tot_lvls)
                }
                
            } else {
                des_string = paste0(lev_fac_a,type_fac_a,"*",lev_fac_b,type_fac_b,"*",lev_fac_c,type_fac_c)
                mu_len = lev_fac_a*lev_fac_b*lev_fac_c
                n_obs = mu_len*n
                mlt_a = ifelse(type_fac_a == "w", 1, lev_fac_a)
                mlt_b = ifelse(type_fac_b == "w", 1, lev_fac_b)
                mlt_c = ifelse(type_fac_c == "w", 1, lev_fac_c)
                n_tot = mlt_a*mlt_b*mlt_c
                fct_lvls = c("a", "b", "c", "a:b", "a:c", "b:c", "a:b:c")
                tot_lvls = lev_fac_a*lev_fac_b*lev_fac_c
                des_t = "Three-way ANOVA"
                if(length(mu) != tot_lvls){
                    stop("Means must have a length of ", tot_lvls)
                }
            }
            
            des_res2 = gsub("\\*", " x ", des_string)
            des_res3 = gsub("w", "-levels within", des_res2)
            des_res4 = gsub("b", "-levels between", des_res3)
            des_res5 = gsub("x", "by", des_res4)
            
            des1 = ANOVA_design(design = des_string,
                                            mu = mu,
                                            sd = stdev,
                                            r = withcorr,
                                            n = n)
            exres <- ANOVA_exact2(
                des1,
                alpha_level = alpha,
                correction = "none",
                verbose = FALSE,
                emm = FALSE,
                liberal_lambda = FALSE
            )
            
            results = cbind(exres$main_results,exres$anova_table)
            results$factor = results$effect
            
            ## Populate table
            designtab <- self$results$designtab
            row1 <- list()
            row1[['var[type]']] <- "Type of ANOVA"
            row1[['var[des]']] <- "Factors in ANOVA"
            row1[['var[n]']] <- "Sample Size per Condition"
            row1[['var[n_obs]']] <- "Total Number of Observations"
            row1[['var[n_tot]']] <- "Total Sample Size (Subjects)"
            
            row1[['val[type]']] <- des_t
            row1[['val[des]']] <- des_res5
            row1[['val[n]']] <- n
            row1[['val[n_obs]']] <- n_obs
            row1[['val[n_tot]']] <- n_tot
            
            designtab$setRow(rowNo=1, values=row1)
            
            lst = list(
                des_string = des_string,
                n = n,
                mu_len = length(mu),
                cohen_f = results$cohen_f,
                alpha_level = alpha,
                n = n,
                n_tot = n_tot,
                n_obs = n_obs,
                des_t = des_t,
                fct_lvls = fct_lvls
            )

            private$.populateMainTable(results, lst)

        },
    .populateMainTable = function(results, lst) {
        
        table <- self$results$main
        facs = lst$fct_lvls
        

        
        for(fac in facs){
            res = results[which(results$factor == fac),]
            print(res)
            table$addRow(rowKey=fac, list(name=fac))
            tableRow <- list(num_df = res$num_df, 
                             den_df = res$den_df,
                             cohen_f = res$cohen_f,
                             alpha_level = lst$alpha_level,
                             power = res$power/100)
            table$setRow(rowKey = fac, tableRow)
        }
        
        
    }
))
