
ttestISClass <- R6::R6Class(
    "ttestISClass",
    inherit = ttestISBase,
    private = list(
        #### Init + run functions ----
        .init = function() {

            private$.initPowerTab()
            private$.initPowerESTab()

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

            ## Compute results
            results <- private$.compute(stats)

            ## Populate tables and plots
            private$.populateIntro()
            private$.populateTabText(results, stats)
            private$.populatePowerTab(results)
            private$.preparePowerDist(results, stats)
            private$.populateContourText(results, stats)
            private$.preparePowerContour(results, stats)
            private$.preparePowerCurveES(results, stats)
            private$.populatePowerCurveESText(results, stats)
            private$.populatePowerCurveNText(results, stats)
            private$.populateDistText(results, stats)
            private$.preparePowerCurveN(results, stats)

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

            d50 = jpower::pwr.t2n.test(n1 = stats$n1,
                                       n2 = stats$n2,
                                       sig.level = stats$alpha,
                                       power = .5, alternative = stats$alt)$d

            return(list(n1=pow.n, n2=ceiling(pow.n * stats$n_ratio), es=pow.es, power=pow.pow, d50 = d50))

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

            colNames <- c("n1", "n2", "es", "power", "alpha","d50")
            colLabels <- c("N\u2081", "N\u2082", "Effect Size", "Power", "\u03B1", "ES for design<br/>to have 50% power")
            colType <- c("integer", "integer", "number", "number", "number", "number")

            for (i in seq_along(order))
                table$addColumn(colNames[order[i]], title=colLabels[order[i]],
                                superTitle=if (calc == 'n' && i > 2 || calc != 'n' && i > 1) "User Defined" else NULL,
                                type=colType[order[i]])

            row <- list()
            for (i in 2:5)
              row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]

            if (self$options$calc != 'n') {
                row[["n1"]] = self$options[["n"]]
                row[["n2"]] = ceiling(self$options[["n"]] * self$options[["n_ratio"]])
            }

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
        .populateIntro = function(){

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
        .populateTabText = function(r, lst){

          html <- self$results$tabText
          table <- self$results$powerEStab

          ## Get options from interface
          calc <- self$options$calc
          n_ratio <- lst$n_ratio
          n1 <- ifelse(calc == 'n', r$n1, lst$n1)
          n2 <- ifelse(calc == 'n', r$n2, lst$n2)
          d <- ifelse(calc == 'es', r$es, lst$es)
          power <- ifelse(calc == 'power', r$power, lst$pow)
          alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
          alt <- lst$alt

          n_text = ifelse(n1==n2,
                              paste0("a sample size of ",n1," in each group "),
                              paste0("group sample sizes of ", n1, " and ", n2, ", respectively, ")
                              )
          tail_text = ifelse(alt == "two.sided",
                             "two-sided",
                             "one-sided"
                             )

          probs = c(.5, .8, .95)
          probs_es = sapply(probs, function(p){
            jpower::pwr.t2n.test(n1 = n1, n2 = n2,
                                 sig.level = alpha, power = p,
                                 alternative = alt)$d
          })

          if(calc == "n"){
            str = paste0("We would need ", n_text," to reliably (with probability greater than ",
                         power, ") detect an effect size of ",
                         "<i>\u03B4=</i>",d,", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>",alpha,
                         ".")
          }else if(calc == "es"){
            str = paste0("A design with ", n_text, "will reliably (with probability greater than ",
                         power, ") detect effect sizes of <i>\u03B4\u2265</i>", round(d,3),
                         ", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>",alpha,
                         ".")
          }else if(calc == "power"){
            str = paste0("A design with ", n_text, " can detect an effect size of ",
                         "<i>\u03B4=</i>", d, " with a probability of ",
                         round(power,3), ", assuming a ", tail_text, " criterion for detection that allows for a maximum Type I error rate of <i>α=</i>",alpha,
                         ".")
          }

          hypo_text = ifelse(alt == "two.sided",
                             "<i>|\u03B4|>0</i>",
                             "<i>\u03B4>0</i>")

          str = paste0(str,"<p>To evaluate the design specified in the table, we can consider ",
                       "how sensitive it is to true effects of increasing sizes; that is, are we likely to ",
                       "correctly conclude that ", hypo_text, " when the effect size is large enough to care about?"
          )

          html$setContent(str)

          esText <- c(paste0('0 < \u03B4 \u2264 ', format(round(probs_es[1],3), nsmall=3)),
                      paste0(format(round(probs_es[1],3), nsmall=3),' < \u03B4 \u2264 ', format(round(probs_es[2],3), nsmall=3)),
                      paste0(format(round(probs_es[2],3), nsmall=3),' < \u03B4 \u2264 ', format(round(probs_es[3],3), nsmall=3)),
                      paste0('\u03B4 \u2265 ', format(round(probs_es[3],3), nsmall=3)))

          for (i in 1:4) {
              row <- list('es' = format(esText[i], nsmall=3))
              table$setRow(rowNo=i, values=row)
          }

        },
        #### Populate table ----
        .populatePowerTab = function(results) {

            table <- self$results$powertab

            calc <- self$options$calc

            row <- list()

            row[["d50"]] = results[["d50"]]

            if(calc == "n"){
              row[["n1"]] <- results[["n1"]]
              row[["n2"]] <- results[["n2"]]
            }else{
              row[[calc]] <- results[[calc]]
            }
            table$setRow(rowNo=1, values=row)

        },

        #### Plot functions ----
        .preparePowerContour = function(r, lst) {

          image <- self$results$powerContour

          ps <- jpower::ttestPlotSettings

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
            maxn <- ceiling(n1 * ps$max.scale)
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
                              alpha = alpha,
                              minn = minn,
                              maxn = maxn
                              ))

        },
        .powerContour = function(image, ggtheme, ...){

            if (is.null(image$state))
                return(FALSE)

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
        .populateContourText = function(r, lst){

          html <- self$results$contourText

          ## Get options from interface
          calc <- self$options$calc
          n_ratio <- lst$n_ratio
          n1 <- ifelse(calc == 'n', r$n1, lst$n1)
          n2 <- ifelse(calc == 'n', r$n2, lst$n2)
          d <- ifelse(calc == 'es', r$es, lst$es)
          power <- ifelse(calc == 'power', r$power, lst$pow)
          alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
          alt <- lst$alt

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
        .preparePowerCurveES = function(r, lst) {

            image <- self$results$powerCurveES

            ps <- jpower::ttestPlotSettings

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

            image$setState(list(cols = cols, dd=dd, y = y, yrect=yrect, n1=n1, n2=n2, alpha=alpha, delta = d, pow = power))

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
            delta <- image$state$delta

            ps <- jpower::ttestPlotSettings


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
        .populatePowerCurveESText = function(r, lst){

          html <- self$results$curveESText

          ## Get options from interface
          calc <- self$options$calc
          n_ratio <- lst$n_ratio
          n1 <- ifelse(calc == 'n', r$n1, lst$n1)
          n2 <- ifelse(calc == 'n', r$n2, lst$n2)
          d <- ifelse(calc == 'es', r$es, lst$es)
          d <- round(d,3)
          power <- ifelse(calc == 'power', r$power, lst$pow)
          alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
          alt <- lst$alt

          n_text = ifelse(n1==n2,
                          paste0("sample sizes of ",n1," in each group"),
                          paste0("group sample sizes of ", n1, " and ", n2, ", respectively")
          )

          if(alt == "two.sided"){
            tail_text = "two-sided"
            null_text = "<i>\u03B4\u2264</i>0,"
            crit_text = "criteria"
          }else{
            tail_text = "one-sided"
            null_text = "<i>\u03B4=</i>0,"
            crit_text = "criterion"
          }

          d50 = jpower::pwr.t2n.test(n1 = n1, n2 = n2, sig.level = alpha, power = .5, alternative = alt)$d

          str = paste0("<p>The power curve above shows how the sensitivity of the test and design ",
                       "is larger for larger effect sizes. If we obtained ", n_text,
                       " our test and design would only be sufficiently sensitive (power >", round(power, 3),
                       " to effect sizes of <i>\u03B4\u2265</i>",d,". ",
                       "<p>We would be more than likely to miss (power less than 50%) effect sizes less than <i>\u03B4=</i>",
                       round(d50,3),"."
          )

          html$setContent(str)

        },
        .preparePowerCurveN = function(r, lst) {

            image <- self$results$powerCurveN

            calc <- self$options$calc

            ps <- jpower::ttestPlotSettings

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
              maxn <- ceiling(n1 * ps$max.scale)
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

            image$setState(list(n1 = n1, cols = cols, nn = nn, y = y, yrect = yrect, lims=lims, delta=d, alpha=alpha, n_ratio = n_ratio, pow = power))

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

            ps <- jpower::ttestPlotSettings

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
        .populatePowerCurveNText = function(r, lst){

          html <- self$results$curveNText

          ## Get options from interface
          calc <- self$options$calc
          n_ratio <- lst$n_ratio
          n1 <- ifelse(calc == 'n', r$n1, lst$n1)
          n2 <- ifelse(calc == 'n', r$n2, lst$n2)
          d <- ifelse(calc == 'es', r$es, lst$es)
          d <- round(d,3)
          power <- ifelse(calc == 'power', r$power, lst$pow)
          alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
          alt <- lst$alt

          n_text = ifelse(n1==n2,
                          paste0("sample sizes of at least ",n1," in each group"),
                          paste0("group sample sizes of at least ", n1, " and ", n2, ", respectively")
          )

          if(alt == "two.sided"){
            tail_text = "two-sided"
            null_text = "<i>\u03B4\u2264</i>0,"
            crit_text = "criteria"
          }else{
            tail_text = "one-sided"
            null_text = "<i>\u03B4=</i>0,"
            crit_text = "criterion"
          }

          str = paste0("<p>The power curve above shows how the sensitivity of the test and design ",
                       "is larger for larger effect sizes. In order for our test and design to have sufficient sensitivity ",
                       "(power > ", round(power,3),") to detect an effect size of ", d, " or larger, ",
                       "we would need ", n_text, "."
          )

          html$setContent(str)

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
        },
        .populateDistText = function(r, lst){

          html <- self$results$distText

          ## Get options from interface
          calc <- self$options$calc
          n_ratio <- lst$n_ratio
          n1 <- ifelse(calc == 'n', r$n1, lst$n1)
          n2 <- ifelse(calc == 'n', r$n2, lst$n2)
          d <- ifelse(calc == 'es', r$es, lst$es)
          d <- round(d,2)
          power <- ifelse(calc == 'power', r$power, lst$pow)
          alpha <- ifelse(calc == 'alpha', r$alpha, lst$alpha)
          alt <- lst$alt

          n_text = ifelse(n1==n2,
                          paste0("a sample size of ",n1," in each group"),
                          paste0("group sample sizes of ", n1, " and ", n2, ", respectively")
          )

          if(alt == "two.sided"){
            tail_text = "two-sided"
            null_text = "<i>\u03B4=</i>0,"
            crit_text = "criteria"
          }else{
            tail_text = "one-sided"
            null_text = "<i>\u03B4\u2264</i>0,"
            crit_text = "criterion"
          }

          str = paste0("<p>The figure above shows two sampling distributions: the sampling distribution ",
                       "of the <i>estimated</i> effect size when <i>\u03B4=</i>0 (left), and when <i>\u03B4=</i>",d,
                       " (right). Both assume ",n_text,".",
                       "<p>The vertical dashed lines show the ",crit_text," we would set for a ", tail_text,
                       " test with <i>α=</i>",alpha,". When the observed effect size is far enough ",
                       "away from 0 to be more extreme than the ",crit_text," we say we 'reject' the null hypothesis. ",
                       "If the null hypothesis were true and ", null_text,
                       " the evidence would lead us to wrongly reject the null hypothesis at most ",100*alpha,"% of the time. ",
                       "<p>On the other hand, if <i>\u03B4\u2265</i>",d,", the evidence would exceed the criterion ",
                       " &mdash; and hence we would correctly claim that <i>\u03B4\u2265</i>0 &mdash; at least ",
                       100*round(power,3),"% of the time. The design's power for detecting effects <i>\u03B4\u2265</i>",d,
                       " is thus ",round(power,3),".")


          html$setContent(str)

        })
)
