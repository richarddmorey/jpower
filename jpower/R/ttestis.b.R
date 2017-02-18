
# This file is a generated template, your changes will not be overwritten

ttestISClass <- R6::R6Class(
    "ttestISClass",
    inherit = ttestISBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          
          results <- "hi!"
            
          textResults <- self$results$text
          textResults$content <- results  
          
        })
)
