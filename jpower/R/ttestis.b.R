
# This file is a generated template, your changes will not be overwritten

ttestISClass <- R6::R6Class(
    "ttestISClass",
    inherit = ttestISBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          
          ## Get options from interface
          
          n = self$options$n
          pow = self$options$power
          alt = self$options$alt
          es = self$options$es
          alpha = self$options$alpha
          plotby = self$options$plotby
            
          ## Compute numbers for table
          
          pow.n = pwr::pwr.t.test(d = es,
                                sig.level = alpha,
                                power = pow)$n
          pow.es = pwr::pwr.t.test(n = n,
                                   power = pow,
                                   sig.level = alpha)$d
          pow.pow = pwr::pwr.t.test(n = n,
                                    d = es,
                                    sig.level = alpha)$power
          pow.alpha = pwr::pwr.t.test(n = n,
                                      d = es,
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
                     alt = alt,
                     es = es,
                     alpha = alpha,
                     plotby = plotby)
          
          self$results$powercurveES$setState(lst)
          self$results$powercurveN$setState(lst)
          self$results$powerDist$setState(lst)
          
        },
        .powercurveES=function(image, ...) {
            lst <- image$state
            
            dd = seq(0, 2, len = 100)
            
            y = pwr::pwr.t.test(n = lst$n,
                                d = dd,
                                sig.level = lst$alpha)$power
            
            y.at = pwr::pwr.t.test(n = lst$n,
                                d = lst$es,
                                sig.level = lst$alpha)$power
            
            
            plot(dd, y, typ = 'l', 
                 ylab = "Power", xlab = "Effect size",
                 ylim = c(0, 1))
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
          
          y = pwr::pwr.t.test(n = nn,
                              d = lst$es,
                              sig.level = lst$alpha)$power
          
          y.at = pwr::pwr.t.test(n = lst$n,
                                 d = lst$es,
                                 sig.level = lst$alpha)$power
          
          
          plot(nn, y, typ = 'l', 
               ylab = "Power", xlab = "Sample size",
               ylim = c(0, 1))
          
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
          
          effN = lst$n / 2
          df = 2*lst$n - 2
          ncp = sqrt(effN) * lst$es

          crit = qt(p = 1 - lst$alpha/2, 
                      df = 2*lst$n - 2 )
          
          if(lst$es > 0){
            xlims = c(qt(.001, df), 
                      qt(.999, df, ncp))
          }else{
            xlims = c(qt(.001, df, ncp),
                      qt(.999, df))
          }  
          
          y.max = dt(0, df)
          
          plot(xlims, xlims, typ = 'n', 
               ylab = "Probability density", xlab = "t statistic",
               axes=FALSE, xlim = xlims, 
               ylim = c(0, y.max))
          
          axis(1)

          ## significant below
          xx = seq(xlims[1], -crit, len = 100)
          yy.null = dt(xx, df)
          yy.alt = dt(xx, df, ncp)
          polygon( c(xx, rev(xx)), c( yy.null, 0*yy.null), 
                   col = rgb(0,1,1,.3), border = NA)
          polygon( c(xx, rev(xx)), c( yy.alt, 0*yy.alt), 
                   col = rgb(1,0,1,.3), border = NA)

          ## significant above
          xx = seq(crit, xlims[2], len = 100)
          yy.null = dt(xx, df)
          yy.alt = dt(xx, df, ncp)
          polygon( c(xx, rev(xx)), c( yy.null, 0*yy.null), 
                   col = rgb(0,1,1,.3), border = NA)
          polygon( c(xx, rev(xx)), c( yy.alt, 0*yy.alt), 
                   col = rgb(1,0,1,.3), border = NA)
          
          ## non-significant
          xx = seq(-crit, crit, len = 100)
          yy.null = dt(xx, df)
          yy.alt = dt(xx, df, ncp)
          polygon( c(xx, rev(xx)), c( yy.null, 0*yy.null), 
                   col = rgb(0,1,1,.1), border = NA)
          polygon( c(xx, rev(xx)), c( yy.alt, 0*yy.alt), 
                   col = rgb(1,0,1,.1), border = NA)
          

          abline(v = c(-1,1)*crit)      
          box()

          TRUE
        })
)
