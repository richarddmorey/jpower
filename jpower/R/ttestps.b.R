
ttestPSClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ttestPSClass",
    inherit = ttestPSBase,
    private = list(
        #### Member variables ----
        probs_es = NULL,
        type = "paired",

        #### Init + run functions ----
        .init = function() {

            private$.initPowerTab()
            private$.initPowerESTab()

        },
        .run = function() {

            ## Get options from interface
            n = self$options$n
            pow = self$options$power
            alt = self$options$alt
            es = self$options$es
            alpha = self$options$alpha

            stats <- list(n=n, pow=pow, alt=alt, es=es, alpha=alpha)

            ## Compute results
            results <- private$.compute(stats)

            ## Populate tables and texts
            private$.populateIntro()
            private$.populatePowerTab(results)
            private$.populateTabText(results, stats)
            private$.populatepowerESTab()
            private$.populateContourText(results, stats)
            private$.populatePowerCurveESText(results, stats)
            private$.populatePowerCurveNText(results, stats)
            private$.populateDistText(results, stats)

            ## Populate plots
            private$.preparePowerContour(results, stats)
            private$.preparePowerCurveES(results, stats)
            private$.preparePowerCurveN(results, stats)
            private$.preparePowerDist(results, stats)

        },

        #### Compute results ----
        .compute = function(stats) {

            ## Compute numbers for table
            pow.n = ceiling(pwr::pwr.t.test(d=stats$es, sig.level=stats$alpha, power=stats$pow, alternative=stats$alt, type=private$type)$n)
            pow.es = pwr::pwr.t.test(n=stats$n, power=stats$pow, sig.level=stats$alpha, alternative=stats$alt, type=private$type)$d
            pow.pow = pwr::pwr.t.test(n=stats$n, d=stats$es, sig.level=stats$alpha, alternative=stats$alt, type=private$type)$power

            return(list(n=pow.n, es=pow.es, power=pow.pow))
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
            colLabels <- c("N", "Effect Size", "Power", "\u03B1")
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
        .initPowerESTab = function() {

            table <- self$results$powerEStab

            pow <- c('\u226450%', '50% \u2013 80%', '80% \u2013 95%', '\u226595%')
            desc <- c('Likely miss', 'Good chance of missing', 'Probably detect', 'Almost surely detect')

            for (i in 1:4) {
                row <- list('power' = pow[i], 'desc' = desc[i])
                table$setRow(rowNo=i, values=row)
            }
        },

        #### Populate texts ----
        .populateIntro = function() {

            calc <- self$options$calc

            html <- self$results$intro

            str = paste0("The purpose of a <i>power analysis</i> is to evaluate ",
                         "the sensitivity of a design and test. ")

            if(calc == "n"){
                str = paste0(str, "You have chosen to calculate the minimum sample size needed ",
                             "to have an experiment sensitive enough to consistently detect the specified hypothetical effect size.")
            }else if(calc == "es"){
                str = paste0(str, "You have chosen to calculate the minimum hypothetical effect size ",
                             "for which the chosen design will have the specified sensitivity.")
            }else if(calc == "power"){
                str = paste0(str, "You have chosen to calculate the sensitivity of the chosen design ",
                             "for detecting the specified effect size.")
            }

            html$setContent(str)

        },
        .populateTabText = function(r, lst) {

            html <- self$results$tabText

            ## Get options from interface
            calc <- self$options$calc
            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            n_text <- paste0("a sample size of ", n)

            tail_text = ifelse(alt == "two.sided",
                               "two-sided",
                               "one-sided"
            )

            probs = c(.5, .8, .95)
            probs_es = sapply(probs, function(p) {
                pwr::pwr.t.test(n=n, sig.level=alpha, power=p,
                                alternative=alt, type=private$type)$d
            })

            private$probs_es <- probs_es

            if (calc == "n") {
                str = paste0("We would need ", n_text," to reliably (with probability greater than ",
                             power, ") detect an effect size of ",
                             "<i>δ=</i>",d,", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>",alpha,
                             ".")
            } else if (calc == "es") {
                str = paste0("A design with ", n_text, "will reliably (with probability greater than ",
                             power, ") detect effect sizes of <i>δ\u2265</i>", round(d,3),
                             ", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>",alpha,
                             ".")
            } else if (calc == "power") {
                str = paste0("A design with ", n_text, " can detect an effect size of ",
                             "<i>δ=</i>", d, " with a probability of ",
                             round(power,3), ", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>",alpha,
                             ".")
            }

            hypo_text = ifelse(alt == "two.sided",
                               "<i>|δ|>0</i>",
                               "<i>δ>0</i>")

            str = paste0(str,"<p>To evaluate the design specified in the table, we can consider ",
                         "how sensitive it is to true effects of increasing sizes; that is, are we likely to ",
                         "correctly conclude that ", hypo_text, " when the effect size is large enough to care about?"
            )

            html$setContent(str)
        },
        .populateContourText = function(r, lst) {

            html <- self$results$contourText

            calc <- self$options$calc

            ## Get options from interface
            power <- ifelse(calc == 'power', r$power, lst$pow)

            str = paste0("<p>The power contour plot shows how the sensitivity of the ",
                         "test changes with the hypothetical effect size ",
                         "and the sample sizes in the design. As we increase the sample sizes, ",
                         "smaller effect sizes become reliably detectable.",
                         "<p>Conversely, if one is satisfied ",
                         "to reliably detect only larger effect sizes, smaller sample sizes are needed. ",
                         "The solid black curve on the contour plot shows sample size/effect size combinations
                       with a power of ",round(power, 3),". The point shows the specified ",
                         " design and effect size."
            )

            html$setContent(str)

        },
        .populatePowerCurveESText = function(r, lst){

            html <- self$results$curveESText

            ## Get options from interface
            calc <- self$options$calc
            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            d <- round(d,3)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            n_text <- paste0("sample sizes of ",n)

            if(alt == "two.sided"){
                tail_text = "two-sided"
                null_text = "<i>δ\u2264</i>0,"
                crit_text = "criteria"
            }else{
                tail_text = "one-sided"
                null_text = "<i>δ=</i>0,"
                crit_text = "criterion"
            }

            d50 = pwr::pwr.t.test(n = n, sig.level = alpha, power = .5, alternative = alt, type=private$type)$d

            str = paste0("<p>The power curve above shows how the sensitivity of the test and design ",
                         "is larger for larger effect sizes. If we obtained ", n_text,
                         " our test and design would only be sufficiently sensitive (power >", round(power, 3),
                         " to effect sizes of <i>δ\u2265</i>",d,". ",
                         "<p>We would be more than likely to miss (power less than 50%) effect sizes less than <i>δ=</i>",
                         round(d50,3),"."
            )

            html$setContent(str)

        },
        .populatePowerCurveNText = function(r, lst){

            html <- self$results$curveNText

            ## Get options from interface
            calc <- self$options$calc
            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            d <- round(d,3)
            power <- ifelse(calc == 'power', r$power, lst$pow)

            n_text = paste0("sample sizes of at least ",n)

            str = paste0("<p>The power curve above shows how the sensitivity of the test and design ",
                         "is larger for larger effect sizes. In order for our test and design to have sufficient sensitivity ",
                         "(power > ", round(power,3),") to detect an effect size of ", d, " or larger, ",
                         "we would need ", n_text, "."
            )

            html$setContent(str)
        },
        .populateDistText = function(r, lst){

            html <- self$results$distText

            ## Get options from interface
            calc <- self$options$calc
            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            d <- round(d,2)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            n_text = paste0("a sample size of ",n)

            if (alt == "two.sided"){
                tail_text = "two-sided"
                null_text = "<i>δ=</i>0,"
                crit_text = "criteria"
            } else{
                tail_text = "one-sided"
                null_text = "<i>δ\u2264</i>0,"
                crit_text = "criterion"
            }

            str = paste0("<p>The figure above shows two sampling distributions: the sampling distribution ",
                         "of the <i>estimated</i> effect size when <i>δ=</i>0 (left), and when <i>δ=</i>",d,
                         " (right). Both assume ",n_text,".",
                         "<p>The vertical dashed lines show the ",crit_text," we would set for a ", tail_text,
                         " test with <i>α=</i>",alpha,". When the observed effect size is far enough ",
                         "away from 0 to be more extreme than the ",crit_text," we say we 'reject' the null hypothesis. ",
                         "If the null hypothesis were true and ", null_text,
                         " the evidence would lead us to wrongly reject the null hypothesis at most ",100*alpha,"% of the time. ",
                         "<p>On the other hand, if <i>δ\u2265</i>",d,", the evidence would exceed the criterion ",
                         " &mdash; and hence we would correctly claim that <i>δ\u2265</i>0 &mdash; at least ",
                         100*round(power,3),"% of the time. The design's power for detecting effects <i>δ\u2265</i>",d,
                         " is thus ",round(power,3),".")


            html$setContent(str)
        },

        #### Populate table ----
        .populatePowerTab = function(results) {

            table <- self$results$powertab
            calc <- self$options$calc

            row <- list()
            row[[calc]] <- results[[calc]]

            table$setRow(rowNo=1, values=row)
        },
        .populatepowerESTab = function() {

            table <- self$results$powerEStab

            probs_es <- private$probs_es

            esText <- c(paste0('0 < δ \u2264 ', format(round(probs_es[1],3), nsmall=3)),
                        paste0(format(round(probs_es[1],3), nsmall=3),' < δ \u2264 ', format(round(probs_es[2],3), nsmall=3)),
                        paste0(format(round(probs_es[2],3), nsmall=3),' < δ \u2264 ', format(round(probs_es[3],3), nsmall=3)),
                        paste0('δ \u2265 ', format(round(probs_es[3],3), nsmall=3)))

            for (i in 1:4) {
                row <- list('es' = format(esText[i], nsmall=3))
                table$setRow(rowNo=i, values=row)
            }
        },

        #### Plot functions ----
        .preparePowerContour = function(r, lst) {

            image <- self$results$powerContour

            ps <- jpower::ttestPlotSettings

            calc <- self$options$calc

            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            if (n >= ps$maxn) {
                maxn <- ceiling(n * ps$max.scale)
            } else {
                maxn <- ps$maxn
            }

            minn = 3

            nn = unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens))-.001))
            dd = seq(ps$mind, ps$maxd, len = ps$lens)

            z.pwr = sapply(dd, function(delta){
                pwr::pwr.t.test(nn, d=delta, sig.level=alpha, alternative=alt, type=private$type)$power
            })

            z.delta = sapply(nn, function(N){
                pwr::pwr.t.test(N, sig.level=alpha, power=power, alternative=alt, type=private$type)$d
            })

            image$setState(list(z.pwr=z.pwr, z.delta=z.delta, ps=ps, nn=nn, dd=dd, n1=n,
                                delta=d, alpha=alpha, minn=minn, maxn=maxn))

        },
        .powerContour = function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)

            calc <- self$options$calc

            image <- self$results$powerContour

            z.delta <- image$state$z.delta
            z.pwr <- image$state$z.pwr
            ps <- image$state$ps
            pow <- image$state$pow
            n1 <- image$state$n1
            alpha <- image$state$alpha
            dd <- image$state$dd
            nn <- image$state$nn
            ps <- image$state$ps
            delta <- image$state$delta

            filled.contour(log(nn), dd, z.pwr, color.palette = ps$pal, nlevels = ps$pow.n.levels,
                           ylab = expression(paste("Hypothetical effect size (",delta,")", sep = "")),
                           xlab = "Sample size",
                           plot.axes = {
                               at.N = round(exp(seq(log(min(nn)), log(max(nn)), len = ps$x.axis.n)))
                               axis(1, at = log(at.N), lab = at.N)
                               axis(2)
                               jpower::striped.lines(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2], x = log(nn), y = z.delta, lwd = 2)
                               #contour(log(N), delta, z.pwr, add=TRUE)
                               if(calc == "n"){
                                   jpower::striped.Arrows(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                                          x1 = log(n1), y1 = par()$usr[3],
                                                          x0 = log(n1),
                                                          y0 = delta, lwd = 2, arr.adj = 1)
                                   jpower::striped.segments(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                                            x0 = log(n1), y0 = delta,
                                                            x1 = par()$usr[1], y1 = delta,
                                                            lwd = 2)
                               }else if(calc == "es"){
                                   jpower::striped.segments(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                                            x1 = log(n1), y1 = par()$usr[3],
                                                            x0 = log(n1),
                                                            y0 = delta, lwd = 2)
                                   jpower::striped.Arrows(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                                          x0 = log(n1), y0 = delta,
                                                          x1 = par()$usr[1], y1 = delta,
                                                          lwd = 2, arr.adj = 1)
                               }
                               points(log(n1), delta, pch = 21, col = "black", bg = "white", cex = 1.5)
                           }, key.title = {
                               mtext("Power",3, .5)
                           }
            )

            TRUE
        },
        .preparePowerCurveES = function(r, lst) {

            image <- self$results$powerCurveES

            ps <- jpower::ttestPlotSettings

            calc <- self$options$calc

            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            dd = seq(ps$mind, ps$maxd, len = ps$curve.n)

            y = pwr::pwr.t.test(n=n, d = dd, sig.level = alpha, alternative = alt, type=private$type)$power
            cols = ps$pal(ps$pow.n.levels)
            yrect = seq(0,1,1/ps$pow.n.levels)

            image$setState(list(cols = cols, dd=dd, y = y, yrect=yrect, n1=n, alpha=alpha, delta = d, pow = power))

        },
        .powerCurveES = function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)

            y <- image$state$y
            cols <- image$state$cols
            yrect <- image$state$yrect
            pow <- image$state$pow
            n1 <- image$state$n1
            alpha <- image$state$alpha
            dd <- image$state$dd
            delta <- image$state$delta

            ps <- jpower::ttestPlotSettings


            label <- jmvcore::format("  N = {}, \u03B1 = {}", n1, round(alpha,3))

            plot(dd, y, ty='n', ylim = c(0,1), las = 1, ylab = "Power",
                 xlab = expression(paste("Hypothetical effect size (",delta,")", sep = "")),
                 yaxs = 'i', xaxs = "i")
            mtext(substitute(paste(N==n1,", ",alpha==a),
                             list(a = alpha, n1 = n1)),
                  adj = 1)

            for(i in 1:ps$pow.n.levels){
                rect(par()$usr[1], yrect[i], par()$usr[2], yrect[i+1], border = NA,
                     col = cols[i])
            }

            jpower::striped.lines(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                  dd, y, lwd = 3)
            jpower::striped.Arrows(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                   x0 = delta, y0 = pow,
                                   x1 = delta,
                                   y1 = 0, lwd = 3, arr.adj = 1)
            jpower::striped.segments(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                     x0 = min(dd), y0 = pow,
                                     x1 = delta, y1 = pow,
                                     lwd = 3)

            TRUE

        },
        .preparePowerCurveN = function(r, lst) {

            image <- self$results$powerCurveN

            calc <- self$options$calc

            ps <- jpower::ttestPlotSettings

            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            if (n >= ps$maxn) {
                maxn <- ceiling(n * ps$max.scale)
            } else {
                maxn <- ps$maxn
            }

            minn = 3

            nn = seq(minn, maxn)

            y = pwr::pwr.t.test(n=nn, d=d, sig.level=alpha, alternative=alt, type=private$type)$power

            cols = ps$pal(ps$pow.n.levels)
            yrect = seq(0,1,1/ps$pow.n.levels)

            lims <- data.frame(xlim = c(minn, maxn),
                               ylim = c(0, 1))

            image$setState(list(n1 = n, cols = cols, nn = nn, y = y, yrect = yrect, lims=lims, delta=d, alpha=alpha, pow = power))

        },
        .powerCurveN = function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)

            cols <- image$state$cols
            yrect <- image$state$yrect
            lims <- image$state$lims
            delta <- image$state$delta
            alpha <- image$state$alpha
            nn <- image$state$nn
            pow <- image$state$pow
            n1 <- image$state$n1
            y <- image$state$y

            ps <- jpower::ttestPlotSettings

            plot(log(nn), y, ty='n', xlim = log(lims$xlim), ylim = lims$ylim, las = 1, ylab = "Power",
                 xlab = "Sample size",
                 yaxs = 'i', xaxs = "i", axes = FALSE)

            at.N = round(exp(seq(log(min(nn)), log(max(nn)), len = ps$x.axis.n)))
            axis(1, at = log(at.N), lab = at.N)
            axis(2, las = 1)

            mtext(substitute(paste(delta==d, ", ", alpha==a),
                             list(a = alpha, d = round(delta,3))),
                  adj = 1)

            for(i in 1:ps$pow.n.levels){
                rect(par()$usr[1], yrect[i], par()$usr[2], yrect[i+1], border = NA,
                     col = cols[i])
            }

            jpower::striped.lines(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                  log(nn), y, lwd = 3)
            jpower::striped.Arrows(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                   x0 = log(n1), y0 = pow,
                                   x1 = log(n1),
                                   y1 = 0, lwd = 3, arr.adj = 1)
            jpower::striped.segments(col1 = ps$stripe.cols[1], col2 = ps$stripe.cols[2],
                                     x0 = min(log(nn)), y0 = pow,
                                     x1 = log(n1), y1 = pow,
                                     lwd = 3)

            TRUE

        },
        .preparePowerDist = function(r, lst) {

            image <- self$results$powerDist

            calc <- self$options$calc

            n <- ifelse(calc == 'n', r$n, lst$n)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            effN = n
            df = n - 1
            ncp = sqrt(effN) * d

            if(alt == "two.sided"){
                crit = qt(p = 1 - alpha / 2, df = df) / sqrt(effN)
            }else{
                crit = qt(p = 1 - alpha, df = df) / sqrt(effN)
            }

            if (lst$es > 0) {
                xlims = c(qt(.001, df), qt(.999, df, ncp)) / sqrt(effN)
            } else {
                xlims = c(qt(.001, df, ncp), qt(.999, df)) / sqrt(effN)
            }

            y.max = dt(0, df) / sqrt(effN)

            xx = seq(xlims[1], xlims[2], len = 100)
            yy.null = dt(xx * sqrt(effN), df) / sqrt(effN)
            yy.alt = dt(xx * sqrt(effN), df, ncp) / sqrt(effN)

            curves <- data.frame(x=rep(xx, 2),
                                 ymin=rep(0,length(xx)*2),
                                 ymax=c(yy.null, yy.alt),
                                 group=rep(c('Null', 'Alt'), each=length(xx)))

            if(alt == "two.sided"){
                rect <- data.frame(x1 = -crit, x2 = crit,
                                   y1 = 0, y2 = y.max * 1.1)
            }else{
                rect <- data.frame(x1 = xlims[1] - 1, x2 = crit,
                                   y1 = 0, y2 = y.max * 1.1)
            }

            lims <- data.frame(xlim = c(xlims[1], xlims[2]),
                               ylim = c(0, y.max * 1.1))

            image$setState(list(curves=curves, rect=rect, lims=lims))

        },
        .powerDist = function(image, ggtheme, ...) {

            ps <- jpower::ttestPlotSettings

            if (is.null(image$state))
                return(FALSE)

            curves <- image$state$curves
            rect <- image$state$rect
            lims <- image$state$lims
            alt = self$options$alt

            themeSpec <- ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                        axis.ticks.y = ggplot2::element_blank(),
                                        legend.position = "none")

            p <- ggplot2::ggplot() +
                ggplot2::geom_ribbon(data=curves, ggplot2::aes(x=x, ymin=ymin, ymax=ymax, fill=group), color='#333333', alpha=.6) +
                ggplot2::geom_rect(data=rect, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='white', alpha = 0.4) +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x1), linetype = 'dashed') +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x2), linetype = 'dashed') +
                ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE) +
                ggplot2::labs(x="Observed standardized effect size (d)", y='Probability Density') +
                ggtheme + themeSpec + ggplot2::scale_fill_manual(values=ps$pal(5)[c(4,1)])

            print(p)

            TRUE
        }
    )
)
