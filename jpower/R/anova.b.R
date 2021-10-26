
# This file is a generated template, your changes will not be overwritten

anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = anovaBase,
    private = list(
        #### Init + run functions ----
        .run = function() {
            #modelTerms <- private$.modelTerms()
            ## Get options from interface
            n = self$options$n
            pow = self$options$power
            es = self$options$es

            alpha = self$options$alpha
            estype = self$options$estype
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
                fct_lvls = c("a")
                des_t = "One-way ANOVA"
                
            } else if(num_facs == "two"){
                des_string = paste0(lev_fac_a,type_fac_a,"*",lev_fac_b,type_fac_b)
                mu_len = lev_fac_a * lev_fac_b
                n_obs = mu_len*n
                mlt_a = ifelse(type_fac_a == "w", 1, lev_fac_a)
                mlt_b = ifelse(type_fac_b == "w", 1, lev_fac_b)
                n_tot = mlt_a*mlt_b
                fct_lvls = c("a", "b", "a:b")
                des_t = "Two-way ANOVA"
                
            } else {
                des_string = paste0(lev_fac_a,type_fac_a,"*",lev_fac_b,type_fac_b,"*",lev_fac_c,type_fac_c)
                mu_len = lev_fac_a*lev_fac_b*lev_fac_c
                n_obs = mu_len*n
                mlt_a = ifelse(type_fac_a == "w", 1, lev_fac_a)
                mlt_b = ifelse(type_fac_b == "w", 1, lev_fac_b)
                mlt_c = ifelse(type_fac_c == "w", 1, lev_fac_c)
                n_tot = mlt_a*mlt_b*mlt_c
                fct_lvls = c("a", "b", "c", "a:b", "a:c", "c:b", "a:c:b")
                des_t = "Three-way ANOVA"
            }
            des_res2 = gsub("\\*", " x ", des_string)
            des_res3 = gsub("w", "-levels within", des_res2)
            des_res4 = gsub("b", "-levels between", des_res3)
            des_res5 = gsub("x", "by", des_res4)
            
            des1 = Superpower::ANOVA_design(des_string,
                                            mu = 1:mu_len,
                                            sd = 1,
                                            r = 0.5,
                                            n = n,
                                            plot = FALSE)
            
            aov1 = suppressMessages({
                as.data.frame(afex::aov_car(des1$frml1,
                                            data = des1$dataframe,
                                            include_aov = FALSE)$anova_table)
            })
            aov2 = aov1[1:2]
            colnames(aov2) = c("num_df","den_df")
            aov2$factor = rownames(aov2)
            aov2$n = n
            #self$options$fac = aov2$factor
     
            des_res = paste0("The design is set as: ", des_string, "\n",
                             "Sample Size per Condition: ", n, "\n",
                             "Total # of Observations: ", n_obs, "\n",
                             "Total # of Subjects: ", n_tot, "\n")
            #self$results$text$setContent(des_string)
            
            # Convert omega to f
            if (estype == "omega" && es >= 1 )
                jmvcore::reject("Effect size \u03C9\u00B2 must be < 1.", code='error')
            
            if (estype == 'f') {
                f.es <- es 
            }
            else {
                f.es <- sqrt(es/(1-es))
            }
            
            pow_tab = data.frame(factor = aov2$factor,
                                 num_df = aov2$num_df,
                                 den_df = aov2$den_df,
                                 cohen_f = rep(f.es, length(aov2$factor)),
                                 alpha_level = rep(alpha, length(aov2$factor)),
                                 power = NA)
            pow_tab$power = try(power_ftest(num_df = pow_tab$num_df,
                                            den_df = pow_tab$den_df,
                                            cohen_f = pow_tab$cohen_f,
                                            alpha_level = pow_tab$alpha_level
                                            )$power,
                                silent = TRUE)
            
            ## Compute numbers for table
            #pow.n = try(pwr::pwr.anova.test(k = k, f = f.es, sig.level = alpha, power = pow)$n, silent=TRUE)
            #pow.es = try(pwr::pwr.anova.test(k = k, n = n, power = pow, sig.level = alpha)$f, silent=TRUE)
            #pow.pow = try(pwr::pwr.anova.test(k = k, n = n, f = f.es, sig.level = alpha)$power, silent=TRUE)
            #pow.alpha = try(pwr::pwr.anova.test(k = k, n = n, f = f.es, sig.level = NULL, power = pow)$sig.level, silent=TRUE)
            
            
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
            
            pow_tab$dpow = pow
            results <- pow_tab
            
            ## Prepare plots and populate table

            lst2 = list(n = n, n_tot = n_tot, n_obs = n_obs, 
                        cohen_f = f.es,
                        des_string = des_string,
                        des_t = des_t,
                        fct_lvls = fct_lvls)
            lst3 = list(des_string = des_string,
                        n = n,
                        mu_len = mu_len,
                        cohen_f = f.es,
                        alpha_level = alpha,
                        n = n, n_tot = n_tot, n_obs = n_obs, 
                        des_string = des_string,
                        des_t = des_t,
                        fct_lvls = fct_lvls)
            private$.populateMainTable(results, lst2)
            private$.preparePowerDist(results)
            private$.preparePowerCurveES(results, lst2)
            private$.preparePowerCurveN(lst3)
            
        },
        .populateMainTable = function(results, lst2) {
            
            table <- self$results$main
            facs = lst2$fct_lvls
            
            for(fac in facs){
                res = results[which(results$factor == fac),]
                table$addRow(rowKey=fac, list(name=fac))
                tableRow <- list(num_df = res$num_df, 
                                 den_df = res$den_df,
                                 cohen_f = res$cohen_f,
                                 alpha_level = res$alpha_level,
                                 power = res$power/100)
                table$setRow(rowKey = fac, tableRow)
            }

            
        },
    
        
        #### Plot functions ----
        .preparePowerDist = function(results) {
            
            image <- self$results$powerDist
            facs = results$factor
            fac = "a"
            app_df = data.frame(x = NA,
                                ymax = NA,
                                ymin = NA,
                                group = NA,
                                factor = NA)
            for(fac in facs){
                
                
                
                df1 = results[which(results$factor == fac), ]$num_df
                df2 = results[which(results$factor == fac), ]$den_df
                fac2 = paste0(fac, " (df1=", df1, ", df2=", df2, ")")
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
                    factor = fac2
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
                ggplot2::theme(
                    axis.text.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank(),
                    legend.position = "none",
                    strip.text = ggplot2::element_text(size = ggplot2::rel(1.1)),
                    strip.background = ggplot2::element_rect(
                        fill = "lightgrey",
                        colour = "black",
                        size = 1
                    )
                )
            
            p <- ggplot2::ggplot(data = curves) + 
                ggplot2::geom_ribbon(ggplot2::aes(x=x, 
                                                  ymin=ymin, 
                                                  ymax=ymax, 
                                                  fill=group), alpha=.6) +
                ggplot2::geom_rect(data=rect, ggplot2::aes(xmin=x1, xmax=x2, 
                                                           ymin=y1, ymax=y2),
                                   fill='white', alpha = 0.3) +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x1),
                                    linetype = 'dashed') +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x2),
                                    linetype = 'dashed') +
                ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, 
                                         expand = FALSE) +
                ggplot2::labs(x=paste0("F statistic"),
                              y='Probability Density') +
                ggplot2::facet_wrap(~factor) +
                ggtheme + themeSpec
            
            print(p)
            
            TRUE
        },
        .preparePowerCurveES = function(results, lst2) {
            
            image <- self$results$powerCurveES
            
            ff = seq(0, 1, len = 100)
            
            facs = factor(results$factor,
                          levels = lst2$fct_lvls)
            #fac = "a"
            app_curve = data.frame(
                x = NA,
                y = NA,
                factor = NA
            )
            app_curve = app_curve[FALSE,]
            
            app_point = data.frame(
                x = NA,
                y = NA,
                factor = NA
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
            
            image$setState(list(curve=app_curve, point=app_point, rects=rects, lst2=lst2))
            
        },
        .powerCurveES = function(image, ggtheme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            curve <- image$state$curve
            point <- image$state$point
            rects <- image$state$rects
            pow <- self$options$power
            lst2 = image$state$lst2
            des_res = lst2$des_string
            des_res2 = gsub("\\*", " x ", des_res)
            des_res3 = gsub("w", "-levels within", des_res2)
            des_res4 = gsub("b", "-levels between", des_res3)
            des_res5 = gsub("x", "by", des_res4)
            point$factor = factor(point$factor,
                                  levels = lst2$fct_lvls)
            curve$factor = factor(curve$factor,
                                  levels = lst2$fct_lvls)
            p <- ggplot2::ggplot() +
                ggplot2::geom_rect(data=rects, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), alpha = 0.3) +
                ggplot2::geom_line(data=curve, ggplot2::aes(x=x, y=y)) +
                ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.02), expand = FALSE) + 
                ggplot2::scale_y_continuous(limits = c(0,1.02),
                                            breaks = seq(0,1,.2)) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=0, xend=x, y=y, yend=y)) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=x, xend=x, y=0, yend=y)) +
                ggplot2::geom_point(data=point, ggplot2::aes(x, y), size = 3) +
                ggplot2::geom_hline(yintercept = pow, linetype = 'dashed') +
                ggplot2::labs(x=paste0("Cohen's f = ", lst2$cohen_f), y='Power', title = lst2$des_t,
                              subtitle = des_res5,
                              caption = paste0("Number of Subjects: ",lst2$n_tot)) +
                ggplot2::facet_wrap(ggplot2::vars(factor,dof)) +
                ggtheme +
                ggplot2::theme(legend.position="none",
                               strip.text = ggplot2::element_text(size = ggplot2::rel(1.1)),
                               strip.background = ggplot2::element_rect(
                                   fill = "lightgrey",
                                   colour = "black",
                                   size = 1
                               ))
                
            
            print(p)
            
            TRUE
            
        },
        .preparePowerCurveN = function(lst) {
            
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
                app_df = rbind(app_df, df_res)
            }
            
            app_df$power = power_ftest(
                num_df = app_df$num_df,
                den_df = app_df$den_df,
                cohen_f = lst$cohen_f,
                alpha_level = lst$alpha
            )$power / 100
            pow_at_n = subset(app_df, n == lst$n)
            # y.at = pwr::pwr.anova.test(k = lst$k, n = lst$n, f = lst$es, sig.level = lst$alpha)$power
            
            curve <- data.frame(x=app_df$n, 
                                y=app_df$power,
                                factor = factor(app_df$factor,
                                                levels = lst$fct_lvls))
            
            point <- data.frame(x=pow_at_n$n,
                                y=pow_at_n$power,
                                factor = factor(pow_at_n$factor,
                                                levels = lst$fct_lvls))
            
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
            p <- ggplot2::ggplot() +
                ggplot2::geom_rect(data=rects, ggplot2::aes(xmin=x1, xmax=x2, 
                                                            ymin=y1, ymax=y2, 
                                                            fill=group), 
                                   alpha = 0.3) +
                ggplot2::geom_line(data=curve, ggplot2::aes(x=x, y=y)) +
                ggplot2::coord_cartesian(xlim = limset$xlim, ylim = limset$ylim, 
                                         expand = FALSE) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=0, 
                                                               xend=x, y=y, 
                                                               yend=y)) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=x,
                                                               xend=x, y=0, 
                                                               yend=y)) +
                ggplot2::geom_point(data=point, ggplot2::aes(x, y), size = 3) +
                ggplot2::geom_hline(yintercept = pow, linetype = 'dashed') +
                ggplot2::labs(x='Sample Size (per condition)', y='Power', 
                              title = lst$des_t,
                              subtitle = des_res5,
                              caption = paste0("Cohen's f = ", lst$cohen_f) )+
                ggplot2::facet_wrap(~factor) +
                ggplot2::scale_y_continuous(limits = c(0,1.02),
                                            breaks = seq(0,1,.2)) +
                ggplot2::scale_x_continuous(limits = limset$xlim) +
                ggtheme +
                ggplot2::theme(legend.position="none",
                               strip.text = ggplot2::element_text(size = ggplot2::rel(1.1)),
                               strip.background = ggplot2::element_rect(
                                   fill = "lightgrey",
                                   colour = "black",
                                   size = 1
                               ))
            
            print(p)
            
            TRUE
            
        })
)
