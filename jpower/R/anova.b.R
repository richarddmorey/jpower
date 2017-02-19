
# This file is a generated template, your changes will not be overwritten

anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = anovaBase,
    private = list(
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        
        ## Get options from interface
        
        n = self$options$n
        pow = self$options$power
        es = self$options$es
        k = self$options$k
        alpha = self$options$alpha
        estype = self$options$estype
        
        if(estype == "<i>&omega;<sup>2</sup></i>" & (es >= 1) ){
          stop("Effect size <i>&omega;<sup>2</sup></i> must be < 1.")
        }
        
        f.es = ifelse(estype == "<i>f</i>", es, sqrt(es/(1-es)))
        
        ## Compute numbers for table
        
        pow.n = pwr::pwr.anova.test(k = k,
                                f = f.es,
                                sig.level = alpha,
                                power = pow)$n
        pow.es = pwr::pwr.anova.test(k = k,
                                 n = n,
                                 power = pow,
                                 sig.level = alpha)$f
        pow.pow = pwr::pwr.anova.test(k = k,
                                  n = n,
                                  f = f.es,
                                  sig.level = alpha)$power
        pow.alpha = pwr::pwr.anova.test(k = k,
                                        n = n,
                                    f = f.es,
                                    sig.level = NULL,
                                    power = pow)$sig.level
        
        ## Populate table
        
        table <- self$results$powertab
        
        table$setRow(rowNo=1, values=list(
          var="N needed per group to achieve given power",
          val=ceiling(pow.n)
        ))
        
        table$setRow(rowNo=2, values=list(
          var="Effect size giving desired power, given N",
          val=pow.es
        ))
        
        table$setRow(rowNo=3, values=list(
          var="Power for design, given effect size",
          val=pow.pow
        ))
        
        table$setRow(rowNo=4, values=list(
          var="&alpha; giving desired power, given design and effect size",
          val=pow.alpha
        ))
        
        ## End populate table
        
        ## Prepare plots
        lst = list(n = n,
                   pow = pow,
                   es = f.es,
                   k = k,
                   alpha = alpha)
        
        self$results$powercurveES$setState(lst)
        self$results$powercurveN$setState(lst)
        self$results$powerDist$setState(lst)
        
      },
      .powercurveES=function(image, ...) {
        lst <- image$state
        
        ff = seq(0, 1, len = 100)
        
        y = pwr::pwr.anova.test(k = lst$k,
                                n = lst$n,
                                f = ff,
                            sig.level = lst$alpha)$power
        
        y.at = pwr::pwr.anova.test(k = lst$k,
                                   n = lst$n,
                               f = lst$es,
                               sig.level = lst$alpha)$power
        
        par(las = 1, xaxs = "i", yaxs = "i")
        plot(ff, y, typ = 'l', 
             ylab = "Power", xlab = "Effect size",
             ylim = c(0, 1), lwd = 2)
        segments(lst$es, par()$usr[3], lst$es, y.at)
        segments(par()$usr[1], y.at , lst$es, y.at)
        points(lst$es, y.at, pch = 19)
        
        rect(par()$usr[1], lst$pow, par()$usr[2], 1,
             border = NA, col = rgb(0, 0 , 1, .1))
        rect(par()$usr[1], lst$pow, par()$usr[2], 0,
             border = NA, col = rgb(1, 0 , 0, .1))
        
        TRUE
      },
      .powercurveN=function(image, ...) {
        lst <- image$state
        
        nn = seq(2, 100)
        
        y = pwr::pwr.anova.test(k = lst$k,
                                n = nn,
                            f = lst$es,
                            sig.level = lst$alpha)$power
        
        y.at = pwr::pwr.anova.test(k = lst$k,
                                   n = lst$n,
                               f = lst$es,
                               sig.level = lst$alpha)$power
        
        par(las = 1, xaxs = "i", yaxs = "i")
        plot(nn, y, typ = 'l', 
             ylab = "Power", xlab = "Sample size (per group)",
             ylim = c(0, 1), lwd = 2)
        
        segments(lst$n, par()$usr[3], lst$n, y.at)
        segments(par()$usr[1], y.at , lst$n, y.at)
        points(lst$n, y.at, pch = 19)
        
        rect(par()$usr[1], lst$pow, par()$usr[2], 1,
             border = NA, col = rgb(0, 0 , 1, .1))
        rect(par()$usr[1], lst$pow, par()$usr[2], 0,
             border = NA, col = rgb(1, 0 , 0, .1))
        
        TRUE
      },
      .powerDist=function(image, ...) {

        lst <- image$state
        
        df1 = (lst$k - 1)
        df2 = (lst$k - 1) * lst$n 
        ncp = lst$k * lst$n * lst$es^2
        
        crit = qf(p = 1 - lst$alpha, 
                  df1 = df1,
                  df2 = df2)
        
        xlims = c(0, qf(.999, df1, df2, ncp))

        y.max = ifelse(df1 > 2,
                       (df1 - 2) / df1 * df2 / (df2 + 2),
                       .2)
        y.max = df(y.max, df1, df2)
        
        par(las = 1, xaxs = "i", yaxs = "i")
        plot(xlims, xlims, typ = 'n', 
             ylab = "Probability density", xlab = "F statistic",
             axes=FALSE, xlim = xlims, 
             ylim = c(0, y.max * 1.1))
        
        axis(1)
        
        ## significant
        xx = seq(crit, xlims[2], len = 100)
        yy.null = df(xx, df1, df2)
        yy.alt = df(xx, df1, df2, ncp)
        polygon( c(xx, rev(xx)), c( yy.null, 0*yy.null), 
                 col = rgb(0,1,1,.3), border = NA)
        polygon( c(xx, rev(xx)), c( yy.alt, 0*yy.alt), 
                 col = rgb(1,0,1,.3), border = NA)
        
        ## non-significant
        xx = seq(0, crit, len = 100)
        yy.null = df(xx, df1, df2)
        yy.alt = df(xx, df1, df2, ncp)
        polygon( c(xx, rev(xx)), c( yy.null, 0*yy.null), 
                 col = rgb(0,1,1,.1), border = NA)
        polygon( c(xx, rev(xx)), c( yy.alt, 0*yy.alt), 
                 col = rgb(1,0,1,.1), border = NA)
        
        
        abline(v = crit)      
        box()
        
        TRUE
      })
)
