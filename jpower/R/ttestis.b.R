
# This file is a generated template, your changes will not be overwritten

ttestISClass <- R6::R6Class(
    "ttestISClass",
    inherit = ttestISBase,
    private = list(
        #### Init + run functions ----
        .run = function() {
            
            ## Get options from interface
            n = self$options$n
            pow = self$options$power
            alt = self$options$alt # only two-tailed is supported at the moment
            es = self$options$es
            alpha = self$options$alpha
            
            ## Compute numbers for table
            pow.n = try(pwr::pwr.t.test(d = es, sig.level = alpha, power = pow)$n, silent=TRUE)
            pow.es = try(pwr::pwr.t.test(n = n, power = pow, sig.level = alpha)$d, silent=TRUE)
            pow.pow = try(pwr::pwr.t.test(n = n, d = es, sig.level = alpha)$power, silent=TRUE)
            pow.alpha = try(pwr::pwr.t.test(n = n, d = es, sig.level = NULL, power = pow)$sig.level, silent=TRUE)
            
            if (class(pow.alpha) == 'try-error')
                pow.alpha <- 0
            
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
            
            lst = list(n = n, pow = pow, alt = alt, es = es, alpha = alpha)
            
            private$.preparePowerDist(lst)
            private$.preparePowerCurveES(lst)
            private$.preparePowerCurveN(lst)

        },
        
        #### Plot functions ----
        .preparePowerDist = function(lst) {
            
            image <- self$results$powerDist
            
            effN = lst$n / 2
            df = 2 * lst$n - 2
            ncp = sqrt(effN) * lst$es
            
            crit = qt(p = 1 - lst$alpha / 2, df = df)
            
            if(lst$es > 0) {
                xlims = c(qt(.001, df), qt(.999, df, ncp))
            }else {
                xlims = c(qt(.001, df, ncp), qt(.999, df))
            }
            
            y.max = dt(0, df)
            
            xx = seq(xlims[1], xlims[2], len = 100)
            yy.null = dt(xx, df)
            yy.alt = dt(xx, df, ncp)
            
            curves <- data.frame(x=rep(xx, 2), 
                                 ymin=rep(0,length(xx)*2), 
                                 ymax=c(yy.null, yy.alt), 
                                 group=rep(c('Null', 'Alt'), each=length(xx)))
            
            rect <- data.frame(x1 = -crit, x2 = crit,
                               y1 = 0, y2 = y.max * 1.1)
            
            lims <- data.frame(xlim = c(xlims[1], xlims[2]), 
                               ylim = c(0, y.max * 1.1))
            
            image$setState(list(curves=curves, rect=rect, lims=lims))
            
        },
        .powerDist = function(image, ...) {
            
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
                ggplot2::labs(x=expression(paste(italic("t"), " statistic")), y='Probability Density') +
                jmvTheme() + themeSpec
            
            print(p)
            
            TRUE
        },
        .preparePowerCurveES = function(lst) {
            
            image <- self$results$powerCurveES
            
            dd = seq(0, 2, len = 100)
            
            y = pwr::pwr.t.test(n = lst$n, d = dd, sig.level = lst$alpha)$power
            y.at = pwr::pwr.t.test(n = lst$n, d = lst$es, sig.level = lst$alpha)$power
            
            curve <- data.frame(x=dd, y=y)
            
            point <- data.frame(x=lst$es, y=y.at)
            
            rects <- data.frame(x1 = c(0, 0), x2 = c(2, 2),
                              y1 = c(0, lst$pow), y2 = c(lst$pow, 1),
                              group = c('a', 'b'))
            
            image$setState(list(curve=curve, point=point, rects=rects))
            
        },
        .powerCurveES = function(image, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            curve <- image$state$curve
            point <- image$state$point
            rects <- image$state$rects
            pow <- self$options$power
            
            p <- ggplot2::ggplot() +
                ggplot2::geom_rect(data=rects, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=group), alpha = 0.3) +
                ggplot2::geom_line(data=curve, ggplot2::aes(x=x, y=y)) +
                ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 1), expand = FALSE) + 
                ggplot2::geom_segment(data=point, ggplot2::aes(x=0, xend=x, y=y, yend=y)) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=x, xend=x, y=0, yend=y)) +
                ggplot2::geom_point(data=point, ggplot2::aes(x, y), size = 3) +
                ggplot2::geom_hline(yintercept = pow, linetype = 'dashed') +
                ggplot2::labs(x='Effect Size', y='Power') +
                jmvTheme() +
                ggplot2::theme(legend.position="none")
            
            print(p)
            
            TRUE
            
        },
        .preparePowerCurveN = function(lst) {
            
            image <- self$results$powerCurveN
            
            xmax <- pwr::pwr.t.test(power = 0.9, d = lst$es, sig.level = lst$alpha)$n
            
            if (lst$n > xmax && lst$n >= 100) {
                xmax <- lst$n * 1.1
            } else if (xmax < 100) {
                xmax <- 100
            }
            
            nn = seq(2, xmax)
            
            y = pwr::pwr.t.test(n = nn, d = lst$es, sig.level = lst$alpha)$power
            y.at = pwr::pwr.t.test(n = lst$n, d = lst$es, sig.level = lst$alpha)$power
            
            curve <- data.frame(x=nn, y=y)
            
            point <- data.frame(x=lst$n, y=y.at)
            
            rects <- data.frame(x1 = c(2, 2), x2 = c(xmax, xmax),
                                y1 = c(0, lst$pow), y2 = c(lst$pow, 1),
                                group = c('a', 'b'))
            
            lims <- data.frame(xlim = c(2, xmax), 
                               ylim = c(0, 1))
            
            image$setState(list(curve=curve, point=point, rects=rects, lims=lims))
            
        },
        .powerCurveN = function(image, ...) {
            
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
                jmvTheme() +
                ggplot2::theme(legend.position="none")
            
            print(p)
            
            TRUE
            
        })
)
