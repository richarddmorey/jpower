
# This file is a generated template, your changes will not be overwritten

ttestISClass <- R6::R6Class(
    "ttestISClass",
    inherit = ttestISBase,
    private = list(
        #### Init + run functions ----
        .init = function() {

            private$.initPowerTab()

        },
        .run = function() {

            ## Get options from interface
            n = self$options$n
            pow = self$options$power
            alt = self$options$alt # only two-tailed is supported at the moment
            es = self$options$es
            alpha = self$options$alpha

            stats <- list(n = n, pow = pow, alt = alt, es = es, alpha = alpha)

            ## Compute results
            results <- private$.compute(stats)

            ## Populate tables and plots
            private$.populatePowerTab(results)
            private$.preparePowerDist(results, stats)
            private$.preparePowerCurveES(results, stats)
            private$.preparePowerCurveN(results, stats)

        },

        #### Compute results ----
        .compute = function(stats) {

            ## Compute numbers for table
            pow.n = ceiling(try(pwr::pwr.t.test(d = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)$n, silent=TRUE))
            pow.es = try(pwr::pwr.t.test(n = stats$n, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$d, silent=TRUE)
            pow.pow = try(pwr::pwr.t.test(n = stats$n, d = stats$es, sig.level = stats$alpha, alternative = stats$alt)$power, silent=TRUE)
            pow.alpha = try(pwr::pwr.t.test(n = stats$n, d = stats$es, sig.level = NULL, power = stats$pow, alternative = stats$alt)$sig.level, silent=TRUE)

            if (class(pow.alpha) == 'try-error')
                pow.alpha <- 0

            return(list(n=pow.n, es=pow.es, power=pow.pow, alpha=pow.alpha))

        },

        #### Init table ----
        .initPowerTab = function() {

            table <- self$results$powertab

            calc <- self$options$calc

            if (calc == 'n')
                order <- c(1,2,3,4)
            else if (calc == 'es')
                order <- c(2,1,3,4)
            else if (calc == 'power')
                order <- c(3,1,2,4)
            else
                order <- c(4,1,2,3)

            colNames <- c("n", "es", "power", "alpha")
            colLabels <- c("N per Group", "Effect Size", "Power", "\u03B1")
            colType <- c("integer", "number", "number", "number")

            for (i in seq_along(order))
                table$addColumn(colNames[order[i]], title=colLabels[order[i]],
                                superTitle=if (i > 1) "User Defined" else NULL,
                                type=colType[order[i]])

            row <- list()
            for (i in 2:4)
                row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]

            table$setRow(rowNo=1, values=row)

        },

        #### Populate table ----
        .populatePowerTab = function(results) {

            table <- self$results$powertab

            calc <- self$options$calc

            r <- results[[calc]]

            row <- list()
            row[[calc]] <- r

            table$setRow(rowNo=1, values=row)

        },

        #### Plot functions ----
        .preparePowerCurveES = function(r, lst) {

            image <- self$results$powerCurveES

            calc <- self$options$calc

            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)

            dd = seq(0, 2, len = 100)

            y = pwr::pwr.t.test(n = n, d = dd, sig.level = alpha)$power

            curve <- data.frame(x=dd, y=y)

            point <- data.frame(x=d, y=power)

            rects <- data.frame(x1 = 0, x2 = 2,
                              y1 = power, y2 = 1)

            image$setState(list(curve=curve, point=point, rects=rects, n=n, alpha=alpha))

        },
        .powerCurveES = function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)

            curve <- image$state$curve
            point <- image$state$point
            rects <- image$state$rects
            pow <- self$options$power
            n <- image$state$n
            alpha <- image$state$alpha

            label <- jmvcore::format("  N = {}, \u03B1 = {}", n, round(alpha,3))

            p <- ggplot2::ggplot() +
                ggplot2::geom_rect(data=rects, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), alpha = 0.3) +
                ggplot2::geom_line(data=curve, ggplot2::aes(x=x, y=y, lty = 'a')) +
                ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 1), expand = FALSE) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=x, xend=x, y=0, yend=y), linetype = 'dashed') +
                ggplot2::geom_point(data=point, ggplot2::aes(x, y), size = 3) +
                ggplot2::geom_hline(yintercept = point$y, linetype = 'dashed') +
                ggplot2::labs(x='Effect Size', y='Power') +
                ggplot2::scale_linetype_manual(name='', labels=label, values='solid') +
                ggtheme +
                ggplot2::theme(legend.position='top',
                               legend.text=ggplot2::element_text(size=16))

            print(p)

            TRUE

        },
        .preparePowerCurveN = function(r, lst) {

            image <- self$results$powerCurveN

            calc <- self$options$calc

            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)

            xmax <- pwr::pwr.t.test(power = 0.9, d = d, sig.level = alpha)$n

            if (n > xmax && n >= 100) {
                xmax <- n * 1.1
            } else if (xmax < 100) {
                xmax <- 100
            }

            nn = seq(2, xmax)

            y = pwr::pwr.t.test(n = nn, d = d, sig.level = alpha)$power

            curve <- data.frame(x=nn, y=y)

            point <- data.frame(x=n, y=power)

            rects <- data.frame(x1 = 2, x2 = xmax,
                                y1 = power, y2 = 1)

            lims <- data.frame(xlim = c(2, xmax),
                               ylim = c(0, 1))

            image$setState(list(curve=curve, point=point, rects=rects, lims=lims, d=d, alpha=alpha))

        },
        .powerCurveN = function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)

            curve <- image$state$curve
            point <- image$state$point
            rects <- image$state$rects
            lims <- image$state$lims
            d <- image$state$d
            alpha <- image$state$alpha

            label <- jmvcore::format("  \u03B4 = {}, \u03B1 = {}", round(d,3), round(alpha,3))

            p <- ggplot2::ggplot() +
                ggplot2::geom_rect(data=rects, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), alpha = 0.3) +
                ggplot2::geom_line(data=curve, ggplot2::aes(x=x, y=y, lty = 'a')) +
                ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE) +
                ggplot2::geom_segment(data=point, ggplot2::aes(x=x, xend=x, y=0, yend=y), linetype = 'dashed') +
                ggplot2::geom_point(data=point, ggplot2::aes(x, y), size = 3) +
                ggplot2::geom_hline(yintercept = point$y, linetype = 'dashed') +
                ggplot2::labs(x='Sample Size (per group)', y='Power') +
                ggplot2::scale_linetype_manual(name='', labels=label, values='solid') +
                ggtheme +
                ggplot2::theme(legend.position="top", legend.text=ggplot2::element_text(size=16))

            print(p)

            TRUE

        },
        .preparePowerDist = function(r, lst) {

            image <- self$results$powerDist

            calc <- self$options$calc

            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)

            effN = n / 2
            df = 2 * n - 2
            ncp = sqrt(effN) * d

            crit = qt(p = 1 - alpha / 2, df = df)

            if(lst$es > 0) {
                xlims = c(qt(.001, df), qt(.999, df, ncp))
            } else {
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
                ggplot2::labs(x=expression(paste(italic("t"), " statistic")), y='Probability Density') +
                ggtheme + themeSpec

            print(p)

            TRUE
        })
)
