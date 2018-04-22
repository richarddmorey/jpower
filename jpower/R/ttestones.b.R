
ttestOneSClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ttestOneSClass",
    inherit = ttestPSClass,
    private = list(
        type = 'one.sample',
        .init=function() {

            super$.init()

            self$results$setTitle('One Sample T-Test')

        }
    ),
    public = list(
        asSource=function() {
            paste0(private$.package, '::', 'ttestOneS', '(', private$.asArgs(), ')')
        },
        initialize=function(...) {
            super$initialize(...)
            private$.name <- 'ttestOneS'
        }
    )
)
