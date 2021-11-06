
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom afex aov_car
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
            
            lev_fac_a = self$options$lev_fac_a
            lev_fac_b = self$options$lev_fac_b
            lev_fac_c = self$options$lev_fac_c
            type_fac_a = self$options$type_fac_a
            type_fac_b = self$options$type_fac_b
            type_fac_c = self$options$type_fac_c
            num_facs = self$options$num_facs
            
            if(num_facs == "one"){
                des_string = paste0(lev_fac_a,type_fac_a)
                
                fct_lvls = c("a")

            } else if(num_facs == "two"){
                des_string = paste0(lev_fac_a,type_fac_a,"*",lev_fac_b,type_fac_b)

                fct_lvls = c("a", "b", "a:b")
                
            } else {
                des_string = paste0(lev_fac_a,type_fac_a,"*",lev_fac_b,type_fac_b,"*",lev_fac_c,type_fac_c)

                fct_lvls = c("a", "b", "c", "a:b", "a:c", "b:c", "a:b:c")

            }

            designtab$setRow(rowNo=1, values=row1)
            
            table <- self$results$main
            
            for(fac in fct_lvls){
                table$addRow(rowKey=fac, list(name=fac))
            }
            #table$addRow(rowKey="...",
            #             list(name="..."))

            #tableRow <- list(num_df = "", 
            #                 den_df = "",
            #                 cohen_f = "",
            #                 alpha_level = "",
            #                 power = "")
            #table$setRow(rowKey = as.factor("a"), tableRow)

            
        },
        .run = function() {
            
            n = self$options$n
            stdev = self$options$stdev
            withcorr = as.numeric(self$options$withcorr)
            #print(withcorr)
            #pow = self$options$power
            #es = self$options$es
            dep <- self$options$get("dep")
            if (is.null(dep) || length(dep) == 0){
                stop("Must provide Means.")
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
            
            DesPlot <- self$results$DesPlot
            
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
            
            DesPlot$setState(des1$meansplot)

            private$.populateMainTable(results, lst)
            results = data.frame(factor = results$factor,
                                 num_df = results$num_df,
                                 den_df = results$den_df,
                                 cohen_f = results$cohen_f,
                                 alpha_level = rep(alpha, length(results$factor)),
                                 power = results$power)
            private$.preparePowerDist(results, lst)
            private$.preparePowerCurveES(results, lst)
            private$.preparePowerCurveN(results, lst)

        },
    .populateMainTable = function(results, lst) {
        
        table <- self$results$main
        facs = lst$fct_lvls

        for(fac in facs){
            res = results[which(results$factor == fac),]
            #print(res)
            #table$addRow(rowKey=fac, list(name=fac))
            tableRow <- list(num_df = res$num_df, 
                             den_df = res$den_df,
                             cohen_f = res$cohen_f,
                             alpha_level = lst$alpha_level,
                             power = res$power/100)
            table$setRow(rowKey = fac, tableRow)
        }
        
    },
    .DesPlot = function(image, ...) {
        
        if (is.null(image$state)){
            return(FALSE)
        }
            
        
        p = image$state
        print(p)
        
        TRUE
    },
    
    
    #### Plot functions ----
    .preparePowerDist = function(results, lst) {
        
        image <- self$results$powerDist
        facs = results$factor
        #fac = "a"
        app_df = data.frame(x = NA,
                            ymax = NA,
                            ymin = NA,
                            group = NA,
                            factor = NA,
                            dof = NA)
        for(fac in facs){
            
            df1 = results[which(results$factor == fac), ]$num_df
            df2 = results[which(results$factor == fac), ]$den_df
            fac2 = fac
            dof = paste0("df1=",df1, ", df2=", df2)
            ncp = df2 * results[which(results$factor == fac), ]$cohen_f ^
                2
            
            crit = qf(p = 1 - results[which(results$factor == fac), ]$alpha_level,
                      df1 = df1,
                      df2 = df2)
            
            xlims = c(0, qf(.999, df1, df2, ncp))
            
            if (df1 > 2) {
                y.max <- (df1 - 2) / df1 * df2 / (df2 + 2)
            } else{
                y.max <- .2
            }
            
            y.max = df(y.max, df1, df2)
            
            xx = seq(xlims[1], xlims[2], len = 100)
            yy.null = df(xx, df1, df2)
            yy.alt = df(xx, df1, df2, ncp)
            
            curves <- data.frame(
                x = rep(xx, 2),
                ymin = rep(0, length(xx) * 2),
                ymax = c(yy.null, yy.alt),
                group = rep(c('Null', 'Alt'), each = length(xx)),
                factor = fac2,
                dof = dof
            )
            app_df = rbind(app_df, curves)
            rect <- data.frame(
                x1 = 0,
                x2 = crit,
                y1 = 0,
                y2 = y.max * 1.1
            )
            
            lims <- data.frame(xlim = c(xlims[1], xlims[2]),
                               ylim = c(0, y.max * 1.1))
            
        }
        
        app_df$factor = factor(app_df$factor,
                               levels = lst$fct_lvls)
        
        image$setState(list(curves=app_df, rect=rect, lims=lims))
        
    },
    .powerDist = function(image, ggtheme, ...) {
        
        if (is.null(image$state))
            return(FALSE)
        
        curves <- image$state$curves
        curves = subset(curves, !is.na(factor))
        rect <- image$state$rect
        lims <- image$state$lims
        
        themeSpec <-
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                legend.position = "none",
                strip.text = element_text(size = rel(1.1)),
                strip.background = element_rect(
                    fill = "lightgrey",
                    colour = "black",
                    size = 1
                )
            )
        
        p <- ggplot(data = curves) + 
            geom_ribbon(aes(x=x, 
                                              ymin=ymin, 
                                              ymax=ymax, 
                                              fill=group), alpha=.6) +
            geom_rect(data=rect, aes(xmin=x1, xmax=x2, 
                                                       ymin=y1, ymax=y2),
                               fill='white', alpha = 0.3) +
            geom_vline(data=rect, aes(xintercept = x1),
                                linetype = 'dashed') +
            geom_vline(data=rect, aes(xintercept = x2),
                                linetype = 'dashed') +
            coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, 
                                     expand = FALSE) +
            labs(x=paste0("F statistic"),
                          y='Probability Density') +
            facet_wrap(vars(factor,dof)) +
            ggtheme + themeSpec
        
        print(p)
        
        TRUE
    },
    .preparePowerCurveES = function(results, lst) {
        
        image <- self$results$powerCurveES
        
        ff = seq(0, 1, len = 100)
        
        facs = factor(results$factor,
                      levels = lst$fct_lvls)
        #fac = "a"
        app_curve = data.frame(
            x = NA,
            y = NA,
            factor = NA,
            cohen_f =NA
        )
        app_curve = app_curve[FALSE,]
        
        app_point = data.frame(
            x = NA,
            y = NA,
            factor = NA,
            cohen_f =NA
        )
        app_point = app_point[FALSE,]
        for(fac in facs){
            
            res = results[which(results$factor == fac), ]
            fac2 = fac
            dof = paste0("df1=",res$num_df, ", df2=", res$den_df)
            
            y = power_ftest(
                num_df = res$num_df,
                den_df = res$den_df,
                cohen_f = ff,
                alpha_level = res$alpha_level,
                beta_level = NULL
            )$power / 100
            y.at = power_ftest(
                num_df = res$num_df,
                den_df = res$den_df,
                cohen_f = res$cohen_f,
                alpha_level = res$alpha_level,
                beta_level = NULL
            )$power / 100
            
            curve <- data.frame(x = ff, y = y, 
                                factor = fac2, 
                                dof = dof)
            app_curve = rbind(curve, app_curve)
            
            point <- data.frame(x = res$cohen_f,
                                y = y.at,
                                factor = fac2,
                                dof = dof)
            app_point = rbind(app_point, point)
            
        }
        rects <- data.frame(x1 = c(0, 0), x2 = c(1, 1),
                            y1 = c(0, self$options$power), y2 = c(self$options$power, 1),
                            group = c('a', 'b'))
        
        image$setState(list(curve=app_curve, point=app_point, rects=rects, lst=lst))
        
    },
    .powerCurveES = function(image, ggtheme, ...) {
        
        if (is.null(image$state))
            return(FALSE)
        
        curve <- image$state$curve
        point <- image$state$point
        rects <- image$state$rects
        pow <- self$options$power
        lst = image$state$lst
        des_res = lst$des_string
        des_res2 = gsub("\\*", " x ", des_res)
        des_res3 = gsub("w", "-levels within", des_res2)
        des_res4 = gsub("b", "-levels between", des_res3)
        des_res5 = gsub("x", "by", des_res4)
        point$factor = factor(point$factor,
                              levels = lst$fct_lvls)
        curve$factor = factor(curve$factor,
                              levels = lst$fct_lvls)
        p <- ggplot() +
            geom_rect(data=rects, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), alpha = 0.3) +
            geom_line(data=curve, aes(x=x, y=y)) +
            coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.02), expand = FALSE) + 
            scale_y_continuous(limits = c(0,1.02),
                                        breaks = seq(0,1,.2)) +
            geom_segment(data=point, aes(x=0, xend=x, y=y, yend=y)) +
            geom_segment(data=point, aes(x=x, xend=x, y=0, yend=y)) +
            geom_point(data=point, aes(x, y), size = 3) +
            geom_hline(yintercept = pow, linetype = 'dashed') +
            labs(x="Cohen's f", y='Power', title = lst$des_t,
                          subtitle = des_res5,
                          caption = paste0("Number of Subjects: ",lst$n_tot)) +
            facet_wrap(vars(factor,dof)) +
            ggtheme +
            theme(legend.position="none",
                           strip.text = element_text(size = rel(1.1)),
                           strip.background = element_rect(
                               fill = "lightgrey",
                               colour = "black",
                               size = 1
                           ))
        
        
        print(p)
        
        TRUE
        
    },
    .preparePowerCurveN = function(results, lst) {
        
        image <- self$results$powerCurveN
        n_max = 100
        n_min = 5
        xmax = 100
        
        
        nn = seq(n_min, xmax)
        
        app_df = data.frame(factor = NA,
                            n = NA,
                            num_df = NA,
                            den_df = NA,
                            cohen_f = NA,
                            alpha_level = NA)
        app_df = app_df[FALSE,]
        
        for(nn1 in nn){
            df_res = gen_df_n(
                n = nn1,
                des_string = lst$des_string,
                mu_len = lst$mu_len
            )
            
            #df_res$cohen_f = round(lst$cohen_f, 4)
            df_res$cohen_f = paste0("Cohen's f = ",round(lst$cohen_f, 4))
            app_df = rbind(app_df, df_res)
        }
        
        app_df$power = power_ftest(
            num_df = app_df$num_df,
            den_df = app_df$den_df,
            cohen_f = lst$cohen_f,
            alpha_level = lst$alpha
        )$power / 100
        pow_at_n = subset(app_df, n == lst$n)
   
        curve <- data.frame(x=app_df$n, 
                            y=app_df$power,
                            factor = factor(app_df$factor,
                                            levels = lst$fct_lvls),
                            cohen_f = app_df$cohen_f)
        
        point <- data.frame(x=pow_at_n$n,
                            y=pow_at_n$power,
                            factor = factor(pow_at_n$factor,
                                            levels = lst$fct_lvls),
                            cohen_f = app_df$cohen_f)
        
        rects <- data.frame(x1 = c(n_min, n_min), x2 = c(xmax, xmax),
                            y1 = c(0, self$options$power), 
                            y2 = c(self$options$power, 1),
                            group = c('a', 'b'))
        
        limset <- data.frame(xlim = c(2, xmax), 
                             ylim = c(0, 1.01))
        
        image$setState(list(curve=curve, point=point, rects=rects, limset=limset,lst=lst))
        
    },
    .powerCurveN = function(image, ggtheme, ...) {
        
        if (is.null(image$state))
            return(FALSE)
        
        curve <- image$state$curve
        point <- image$state$point
        rects <- image$state$rects
        limset <- image$state$limset
        lst = image$state$lst
        pow <- self$options$power
        des_res = lst$des_string
        des_res2 = gsub("\\*", " x ", des_res)
        des_res3 = gsub("w", "-levels within", des_res2)
        des_res4 = gsub("b", "-levels between", des_res3)
        des_res5 = gsub("x", "by", des_res4)
        
        #rects$factor = factor(rects$factor,
        #                      levels = lst$fct_lvls)
        point$factor = factor(point$factor,
                              levels = lst$fct_lvls)
        curve$factor = factor(curve$factor,
                              levels = lst$fct_lvls)
        p <- ggplot() +
            geom_rect(data=rects, aes(xmin=x1, xmax=x2, 
                                                        ymin=y1, ymax=y2, 
                                                        fill=group), 
                               alpha = 0.3) +
            geom_line(data=curve, aes(x=x, y=y)) +
            coord_cartesian(xlim = limset$xlim, ylim = limset$ylim, 
                                     expand = FALSE) +
            geom_segment(data=point, aes(x=0, 
                                                           xend=x, y=y, 
                                                           yend=y)) +
            geom_segment(data=point, aes(x=x,
                                                           xend=x, y=0, 
                                                           yend=y)) +
            geom_point(data=point, aes(x, y), size = 3) +
            geom_hline(yintercept = pow, linetype = 'dashed') +
            labs(x='Sample Size (per condition)', y='Power', 
                          title = lst$des_t,
                          subtitle = des_res5)+
            facet_wrap(vars(factor,cohen_f)) +
            scale_y_continuous(limits = c(0,1.02),
                                        breaks = seq(0,1,.2)) +
            scale_x_continuous(limits = limset$xlim) +
            ggtheme +
            theme(legend.position="none",
                           strip.text = element_text(size = rel(1.1)),
                           strip.background = element_rect(
                               fill = "lightgrey",
                               colour = "black",
                               size = 1
                           ))
        
        print(p)
        
        TRUE
        
    }))
