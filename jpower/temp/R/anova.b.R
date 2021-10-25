
# This file is a generated template, your changes will not be overwritten

anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = anovaBase,
    private = list(
        #### Init + run functions ----
        .run = function() {
            
            ## Get options from interface
            n = self$options$n
            pow = self$options$power
            es = self$options$es
            k = self$options$k
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
                
            } else if(num_facs == "two"){
                des_string = paste0(lev_fac_a,type_fac_a,"*"lev_fac_b,type_fac_b)
                
            } else {
                des_string = paste0(lev_fac_a,type_fac_a,"*",lev_fac_b,type_fac_b,"*",lev_fac_c,type_fac_c)
                
            }
            
            
            self$results$text$setContent(des_string)
            
            # Convert omega to f
            if (estype == "omega" && es >= 1 )
                jmvcore::reject("Effect size \u03C9\u00B2 must be < 1.", code='error')
            
            if (estype == 'f') 
                f.es <- es 
            else 
                f.es <- sqrt(es/(1-es))
            
            ## Compute numbers for table
            pow.n = try(pwr::pwr.anova.test(k = k, f = f.es, sig.level = alpha, power = pow)$n, silent=TRUE)
            pow.es = try(pwr::pwr.anova.test(k = k, n = n, power = pow, sig.level = alpha)$f, silent=TRUE)
            pow.pow = try(pwr::pwr.anova.test(k = k, n = n, f = f.es, sig.level = alpha)$power, silent=TRUE)
            pow.alpha = try(pwr::pwr.anova.test(k = k, n = n, f = f.es, sig.level = NULL, power = pow)$sig.level, silent=TRUE)
            
            ## Populate table
            table <- self$results$powertab
            
            row <- list()
            row[['var[n]']] <- "N needed per group to achieve given power"
            row[['var[es]']] <- "Effect size giving desired power, given N"
            row[['var[power]']] <- "Power for design, given effect size"
            row[['var[alpha]']] <- "&alpha; giving desired power, given design and effect size"
            
            row[['val[n]']] <- ceiling(pow.n)
            row[['val[es]']] <- pow.es
            row[['val[power]']] <- pow.pow
            row[['val[alpha]']] <- pow.alpha
            
            table$setRow(rowNo=1, values=row)
            
            ## Prepare plots
            lst = list(n = n, pow = pow, es = f.es, k = k, alpha = alpha)
            
            private$.preparePowerDist(lst)
            private$.preparePowerCurveES(lst)
            private$.preparePowerCurveN(lst)
            
        },
        
        #### Plot functions ----
        .preparePowerDist = function(lst) {
            
            image <- self$results$powerDist
            
            df1 = (lst$k - 1)
            df2 = (lst$k - 1) * lst$n 
            ncp = lst$k * lst$n * lst$es^2
            
            crit = qf(p = 1 - lst$alpha,  df1 = df1, df2 = df2)
            
            xlims = c(0, qf(.999, df1, df2, ncp))
            
            if (df1 > 2)
                y.max <- (df1 - 2) / df1 * df2 / (df2 + 2)
            else
                y.max <- .2

            y.max = df(y.max, df1, df2)
            
            xx = seq(xlims[1], xlims[2], len = 100)
            yy.null = df(xx, df1, df2)
            yy.alt = df(xx, df1, df2, ncp)
            
            curves <- data.frame(x=rep(xx, 2), 
                                 ymin=rep(0,length(xx)*2), 
                                 ymax=c(yy.null, yy.alt), 
                                 group=rep(c('Null', 'Alt'), each=length(xx)))
            
            rect <- data.frame(x1 = 0, x2 = crit,
                               y1 = 0, y2 = y.max * 1.1)
            
            lims <- data.frame(xlim = c(xlims[1], xlims[2]), 
                               ylim = c(0, y.max * 1.1))
            
            
            
            image$setState(list(curves=curves, rect=rect, lims=lims))
            
        },
        .powerDist = function(image, ggtheme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            curves <- image$state$curves
            rect <- image$state$rect
            lims <- image$state$lims
            
            themeSpec <- ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                        axis.ticks.y = ggplot2::element_blank(),
                                        legend.position = "none")
            
            p <- ggplot2::ggplot() + 
                ggplot2::geom_ribbon(data=curves, ggplot2::aes(x=x, ymin=ymin, ymax=ymax, fill=group), alpha=.3) +
                ggplot2::geom_rect(data=rect, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='white', alpha = 0.5) +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x1), linetype = 'dashed') +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x2), linetype = 'dashed') +
                ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE) +
                ggplot2::labs(x=expression(paste(italic("F"), " statistic")), y='Probability Density') +
                ggtheme + themeSpec
            
            print(p)
            
            TRUE
        },
        .preparePowerCurveES = function(lst) {
            
            image <- self$results$powerCurveES
            
            ff = seq(0, 1, len = 100)
            
            y = pwr::pwr.anova.test(k = lst$k, n = lst$n, f = ff, sig.level = lst$alpha)$power
            y.at = pwr::pwr.anova.test(k = lst$k, n = lst$n, f = lst$es, sig.level = lst$alpha)$power
            
            curve <- data.frame(x=ff, y=y)
            
            point <- data.frame(x=lst$es, y=y.at)
            
            rects <- data.frame(x1 = c(0, 0), x2 = c(1, 1),
                                y1 = c(0, lst$pow), y2 = c(lst$pow, 1),
                                group = c('a', 'b'))
            
            image$setState(list(curve=curve, point=point, rects=rects))
            
        },
        .powerCurveES = function(image, ggtheme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            curve <- image$state$curve
            point <- image$state$point
            rects <- image$state$rects
            pow <- self$options$power
            
            p <- ggplot2::ggplot() +
                ggplot2::geom_rect(data=rects, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), alpha = 0.3) +
                ggplot2::geom_line(data=curve, ggplot2::aes(x=x, y=y)) +
                ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + 
                ggplot2::geom_segment(data=point, ggplot2::aes(x=0, xend=x, y=y, yend=y)) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=x, xend=x, y=0, yend=y)) +
                ggplot2::geom_point(data=point, ggplot2::aes(x, y), size = 3) +
                ggplot2::geom_hline(yintercept = pow, linetype = 'dashed') +
                ggplot2::labs(x='Effect Size', y='Power') +
                ggtheme +
                ggplot2::theme(legend.position="none")
                
            
            print(p)
            
            TRUE
            
        },
        .preparePowerCurveN = function(lst) {
            
            image <- self$results$powerCurveN
        
            xmax <- pwr::pwr.anova.test(power = 0.9, k = lst$k, f = lst$es, sig.level = lst$alpha)$n
            
            if (lst$n > xmax && lst$n >= 100) {
                xmax <- lst$n * 1.1
            } else if (xmax < 100) {
                xmax <- 100
            }
            
            nn = seq(2, xmax)
            
            y = pwr::pwr.anova.test(k = lst$k, n = nn, f = lst$es, sig.level = lst$alpha)$power
            y.at = pwr::pwr.anova.test(k = lst$k, n = lst$n, f = lst$es, sig.level = lst$alpha)$power
            
            curve <- data.frame(x=nn, y=y)
            
            point <- data.frame(x=lst$n, y=y.at)
            
            rects <- data.frame(x1 = c(2, 2), x2 = c(xmax, xmax),
                                y1 = c(0, lst$pow), y2 = c(lst$pow, 1),
                                group = c('a', 'b'))
            
            lims <- data.frame(xlim = c(2, xmax), 
                               ylim = c(0, 1))
            
            image$setState(list(curve=curve, point=point, rects=rects, lims=lims))
            
        },
        .powerCurveN = function(image, ggtheme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            curve <- image$state$curve
            point <- image$state$point
            rects <- image$state$rects
            lims <- image$state$lims
            pow <- self$options$power
            
            p <- ggplot2::ggplot() +
                ggplot2::geom_rect(data=rects, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), alpha = 0.3) +
                ggplot2::geom_line(data=curve, ggplot2::aes(x=x, y=y)) +
                ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=0, xend=x, y=y, yend=y)) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=x, xend=x, y=0, yend=y)) +
                ggplot2::geom_point(data=point, ggplot2::aes(x, y), size = 3) +
                ggplot2::geom_hline(yintercept = pow, linetype = 'dashed') +
                ggplot2::labs(x='Sample Size (per group)', y='Power') +
                ggtheme +
                ggplot2::theme(legend.position="none")
            
            print(p)
            
            TRUE
            
        })
)
