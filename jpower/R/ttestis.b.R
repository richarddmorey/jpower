
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

          ## Prepare plot
          image <- self$results$powercurve
          image$setState(list(x = 1:10))
          
          
        },
        .powercurve=function(image, ...) {
            plotData <- image$state
            plot(plotData$x)
            TRUE
        })
)
