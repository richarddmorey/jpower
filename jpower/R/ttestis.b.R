
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
            n_ratio = self$options$n_ratio
            pow = self$options$power
            alt = self$options$alt 
            es = self$options$es
            alpha = self$options$alpha

            stats <- list(n1 = n, 
                          n2 = ceiling(n_ratio * n),
                          n_ratio = n_ratio, 
                          pow = pow,
                          alt = alt,
                          es = es, 
                          alpha = alpha)
            
            plotSettings = list(
              lens = 20,
              x.axis.n = 8,
              pow.n.levels = 10,
              curve.n = 128,
              
              maxn = 100,
              max.scale = 1.5,
              
              mind = 0,
              maxd = 2,
              
              background.alpha = .7,
              #stripe.cols = pal(pow.n.levels)[c(1,pow.n.levels)]
              stripe.cols = c("black", "black") 
            )
            plotSettings$pal = function(...) 
              viridis::viridis(..., alpha = plotSettings$background.alpha)
            
            ## Compute results
            results <- private$.compute(stats)

            ## Populate tables and plots
            private$.populatePowerTab(results)
            private$.preparePowerDist(results, stats)
            private$.preparePowerContour(results, stats, plotSettings)
            private$.preparePowerCurveES(results, stats, plotSettings)
            private$.preparePowerCurveN(results, stats, plotSettings)

        },

        #### Compute results ----
        .compute = function(stats) {

            
            ## Compute numbers for table
            pow.n = try(ceiling(jpower::pwr.t2n.ratio(n_ratio = stats$n_ratio, d = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)), silent=TRUE)
            pow.es = try(jpower::pwr.t2n.test(n1 = stats$n1, n2 = stats$n2, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$d, silent=TRUE)
            pow.pow = try(jpower::pwr.t2n.test(n1 = stats$n1, n2 = stats$n2, d = stats$es, sig.level = stats$alpha, alternative = stats$alt)$power, silent=TRUE)
#            pow.alpha = try(jpower::pwr.t2n.test(n1 = stats$n1, n2 = stats$n2, d = stats$es, sig.level = NULL, power = stats$pow, alternative = stats$alt)$sig.level, silent=TRUE)

#            if (class(pow.alpha) == 'try-error')
#                pow.alpha <- 0

            
            return(list(n1=pow.n, n2=ceiling(pow.n * stats$n_ratio), es=pow.es, power=pow.pow))

        },

        #### Init table ----
        .initPowerTab = function() {

            table <- self$results$powertab

            calc <- self$options$calc

            if (calc == 'n')
                order <- c(1,2,3,4,5)
            else if (calc == 'es')
                order <- c(3,1,2,4,5)
            else if (calc == 'power')
                order <- c(4,1,2,3,5)
            else
                order <- c(5,1,2,3,4)

            colNames <- c("n1", "n2", "es", "power", "alpha")
            colLabels <- c("N\u2081", "N\u2082", "Effect Size", "Power", "\u03B1")
            colType <- c("integer", "integer", "number", "number", "number")

            for (i in seq_along(order))
                table$addColumn(colNames[order[i]], title=colLabels[order[i]],
                                superTitle=if (i > 1) "User Defined" else NULL,
                                type=colType[order[i]])

            row <- list()
            for (i in 2:5)
              row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]

            row[["n1"]] = self$options[["n"]]
            row[["n2"]] = ceiling(self$options[["n"]] * self$options[["n_ratio"]])
            
            table$setRow(rowNo=1, values=row)

        },

        #### Populate table ----
        .populatePowerTab = function(results) {

            table <- self$results$powertab

            calc <- self$options$calc

            row <- list()
            
            if(calc == "n"){
              row[["n1"]] <- results[["n1"]]
              row[["n2"]] <- results[["n2"]]
            }else{
              row[[calc]] <- results[[calc]]
            }
            table$setRow(rowNo=1, values=row)

        },

        #### Plot functions ----
        .preparePowerContour = function(r, lst, ps) {
          
          image <- self$results$powerContour
          
          calc <- self$options$calc
          
          n_ratio <- lst$n_ratio
          n1 <- ifelse(calc == 'n', r$n1, lst$n1)
          n2 <- ifelse(calc == 'n', r$n2, lst$n2)
          d <- ifelse(calc == 'es', r$es, lst$es)
          power <- ifelse(calc == 'power', r$power, lst$pow)
          alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
          alt <- lst$alt
          
          
          maxn <- jpower::pwr.t2n.ratio(n_ratio = n_ratio, 
                                        power = max(0.9, power), 
                                        d = d,
                                        sig.level = alpha,
                                        alternative = alt)
          
          if (n1 > maxn && n1 >= ps$maxn) {
            maxn <- n1 * ps$max.scale
          } else if (maxn < ps$maxn) {
            maxn <- ps$maxn
          }
          
          
          minn = ifelse(n_ratio<1, 
                        max(ceiling( 3 / (1 + n_ratio) ), 2 / n_ratio),
                        max(ceiling( 3 / (1 + n_ratio) ), 2 * n_ratio))
          
          nn = unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens))-.001))
          dd = seq(ps$mind, ps$maxd, len = ps$lens)
          nn2 = ceiling(n_ratio * nn)
          
          z.pwr = sapply(dd, function(delta){
            jpower::pwr.t2n.test(nn, nn2, 
                              d = delta, 
                              sig.level = alpha,
                              alternative = alt)$power
          })
          
          z.delta = sapply(nn, function(N){
            n2 = ceiling(n_ratio * N)
            jpower::pwr.t2n.test(N, n2, 
                              sig.level = alpha,
                              power = power,
                              alternative = alt)$d
          })
          

          
          image$setState(list(z.pwr = z.pwr, 
                              z.delta = z.delta,
                              ps = ps,
                              nn = nn,
                              dd = dd, 
                              n1 = n1,
                              n_ratio = n_ratio,
                              delta = d,
                              alpha = alpha
                              ))
          
        },
        .powerContour = function(image, ggtheme, ...){
          
          calc <- self$options$calc
          
          image <- self$results$powerContour
          
          z.delta <- image$state$z.delta
          z.pwr <- image$state$z.pwr
          ps <- image$state$ps
          pow <- image$state$pow
          n1 <- image$state$n1
          n2 <- image$state$n2
          alpha <- image$state$alpha
          dd <- image$state$dd
          nn <- image$state$nn
          ps <- image$state$ps
          delta <- image$state$delta
          n_ratio <- image$state$n_ratio

          filled.contour(log(nn), dd, z.pwr, color.palette = ps$pal, nlevels = ps$pow.n.levels, 
                         ylab = expression(paste("Hypothetical effect size (",delta,")", sep = "")),               
                         xlab = "Sample size (group 1)",
                         plot.axes = {
                           at.N = round(exp(seq(log(min(nn)), log(max(nn)), len = ps$x.axis.n)))
                           axis(1, at = log(at.N), lab = at.N)
                           axis(2)
                           if(n_ratio!=1){
                             axis(3, at = log(at.N), lab = ceiling(at.N * n_ratio))
                             mtext("Sample size (group 2)", 3, line = par()$mgp[1],  adj = .5)
                           }
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
        .preparePowerCurveES = function(r, lst, ps) {

            image <- self$results$powerCurveES

            calc <- self$options$calc

            n1 <- ifelse(calc == 'n', r$n1, lst$n1)
            n2 <- ifelse(calc == 'n', r$n2, lst$n2)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt
            
            dd = seq(ps$mind, ps$maxd, len = ps$curve.n)
            
            y = jpower::pwr.t2n.test(n1 = n1, n2 = n2, d = dd, sig.level = alpha, alternative = alt)$power
            cols = ps$pal(ps$pow.n.levels)
            yrect = seq(0,1,1/ps$pow.n.levels)
            
            image$setState(list(cols = cols, dd=dd, y = y, yrect=yrect, n1=n1, n2=n2, alpha=alpha, delta = d, pow = power, ps = ps))

        },
        .powerCurveES = function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)

            y <- image$state$y
            cols <- image$state$cols
            yrect <- image$state$yrect
            pow <- image$state$pow
            n1 <- image$state$n1
            n2 <- image$state$n2
            alpha <- image$state$alpha
            dd <- image$state$dd
            ps <- image$state$ps
            delta <- image$state$delta
            

            label <- jmvcore::format("  N\u2081 = {}, N\u2082 = {}, \u03B1 = {}", n1, n2, round(alpha,3))

            plot(dd, y, ty='n', ylim = c(0,1), las = 1, ylab = "Power", 
                 xlab = expression(paste("Hypothetical effect size (",delta,")", sep = "")),
                 yaxs = 'i', xaxs = "i")
            mtext(substitute(paste(N[1]==n1,", ",N[2]==n2,", ",alpha==a), 
                             list(a = alpha, n1 = n1,n2=n2)),
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
        .preparePowerCurveN = function(r, lst, ps) {

            image <- self$results$powerCurveN

            calc <- self$options$calc

            n1 <- ifelse(calc == 'n', r$n1, lst$n1)
            n2 <- ifelse(calc == 'n', r$n2, lst$n2)
            n_ratio = lst$n_ratio
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt
            
            maxn <- jpower::pwr.t2n.ratio(n_ratio = n_ratio, 
                                          power = max(0.9, power), 
                                          d = d,
                                          sig.level = alpha,
                                          alternative = alt)

            if (n1 > maxn && n1 >= ps$maxn) {
              maxn <- n1 * ps$max.scale
            } else if (maxn < ps$maxn) {
              maxn <- ps$maxn
            }
            
            
            minn = ifelse(n_ratio<1, 
                          max(ceiling( 3 / (1 + n_ratio) ), 2 / n_ratio),
                          max(ceiling( 3 / (1 + n_ratio) ), 2 * n_ratio))
            
            nn = seq(minn, maxn)

            y = jpower::pwr.t2n.test(n1 = nn, 
                                  n2 = ceiling(nn * lst$n_ratio), 
                                  d = d, sig.level = alpha, alternative = alt)$power

            cols = ps$pal(ps$pow.n.levels)
            yrect = seq(0,1,1/ps$pow.n.levels)
            
            lims <- data.frame(xlim = c(minn, maxn),
                               ylim = c(0, 1))

            image$setState(list(n1 = n1, cols = cols, nn = nn, y = y, yrect = yrect, lims=lims, delta=d, alpha=alpha, n_ratio = n_ratio, pow = power, ps = ps))

        },
        .powerCurveN = function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)
            
            cols <- image$state$cols
            yrect <- image$state$yrect
            lims <- image$state$lims
            delta <- image$state$delta
            alpha <- image$state$alpha
            n_ratio <- image$state$n_ratio
            nn <- image$state$nn
            pow <- image$state$pow
            n1 <- image$state$n1
            y <- image$state$y
            ps <- image$state$ps
            
            
            label <- jmvcore::format(" N\u2082 = {} \u00D7 N\u2081,  \u03B4 = {}, \u03B1 = {}", round(n_ratio, 3), round(delta,3), round(alpha,3))

            plot(log(nn), y, ty='n', xlim = log(lims$xlim), ylim = lims$ylim, las = 1, ylab = "Power", 
                 xlab = "Sample size (group 1)",
                 yaxs = 'i', xaxs = "i", axes = FALSE)
          
            at.N = round(exp(seq(log(min(nn)), log(max(nn)), len = ps$x.axis.n)))
            axis(1, at = log(at.N), lab = at.N)
            axis(2, las = 1)
            
            mtext(substitute(paste(delta==d, ", ", N[2]==nr %*% N[1],", ", alpha==a), 
                             list(a = alpha, nr = n_ratio, d = round(delta,3))),
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

            n1 <- ifelse(calc == 'n', r$n1, lst$n1)
            n2 <- ifelse(calc == 'n', r$n2, lst$n2)
            d <- ifelse(calc == 'es', r$es, lst$es)
            power <- ifelse(calc == 'power', r$power, lst$pow)
            alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
            alt <- lst$alt

            effN = n1 * n2 / (n1 + n2)
            df = n1 + n2 - 2
            ncp = sqrt(effN) * d
            
            if(alt == "two.sided"){
              crit = qt(p = 1 - alpha / 2, df = df) / sqrt(effN)
            }else{
              crit = qt(p = 1 - alpha, df = df) / sqrt(effN)
            }

            if(lst$es > 0) {
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
                ggplot2::geom_ribbon(data=curves, ggplot2::aes(x=x, ymin=ymin, ymax=ymax, fill=group), alpha=.3) +
                ggplot2::geom_rect(data=rect, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='white', alpha = 0.5) +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x1), linetype = 'dashed') +
                ggplot2::geom_vline(data=rect, ggplot2::aes(xintercept = x2), linetype = 'dashed') +
                ggplot2::coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE) +
                ggplot2::labs(x="Observed standardized effect size (d)", y='Probability Density') +
                ggtheme + themeSpec

            print(p)

            TRUE
        })
)
