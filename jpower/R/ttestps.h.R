
# This file is automatically generated, you probably don't want to edit this

ttestPSOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "ttestPSOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            calc = "n",
            es = 0.5,
            power = 0.9,
            n = 20,
            alt = "two.sided",
            alpha = 0.05,
            powerContour = TRUE,
            powerDist = FALSE,
            powerCurveES = TRUE,
            powerCurveN = FALSE,
            text = TRUE, ...) {

            super$initialize(
                package="jpower",
                name="ttestPS",
                requiresData=FALSE,
                ...)

            private$..calc <- jmvcore::OptionList$new(
                "calc",
                calc,
                options=list(
                    "n",
                    "power",
                    "es"),
                default="n")
            private$..es <- jmvcore::OptionNumber$new(
                "es",
                es,
                min=0.01,
                default=0.5)
            private$..power <- jmvcore::OptionNumber$new(
                "power",
                power,
                min=0,
                max=1,
                default=0.9)
            private$..n <- jmvcore::OptionInteger$new(
                "n",
                n,
                min=2,
                default=20)
            private$..alt <- jmvcore::OptionList$new(
                "alt",
                alt,
                options=list(
                    "two.sided",
                    "less",
                    "greater"),
                default="two.sided")
            private$..alpha <- jmvcore::OptionNumber$new(
                "alpha",
                alpha,
                min=0,
                default=0.05)
            private$..powerContour <- jmvcore::OptionBool$new(
                "powerContour",
                powerContour,
                default=TRUE)
            private$..powerDist <- jmvcore::OptionBool$new(
                "powerDist",
                powerDist,
                default=FALSE)
            private$..powerCurveES <- jmvcore::OptionBool$new(
                "powerCurveES",
                powerCurveES,
                default=TRUE)
            private$..powerCurveN <- jmvcore::OptionBool$new(
                "powerCurveN",
                powerCurveN,
                default=FALSE)
            private$..text <- jmvcore::OptionBool$new(
                "text",
                text,
                default=TRUE)

            self$.addOption(private$..calc)
            self$.addOption(private$..es)
            self$.addOption(private$..power)
            self$.addOption(private$..n)
            self$.addOption(private$..alt)
            self$.addOption(private$..alpha)
            self$.addOption(private$..powerContour)
            self$.addOption(private$..powerDist)
            self$.addOption(private$..powerCurveES)
            self$.addOption(private$..powerCurveN)
            self$.addOption(private$..text)
        }),
    active = list(
        calc = function() private$..calc$value,
        es = function() private$..es$value,
        power = function() private$..power$value,
        n = function() private$..n$value,
        alt = function() private$..alt$value,
        alpha = function() private$..alpha$value,
        powerContour = function() private$..powerContour$value,
        powerDist = function() private$..powerDist$value,
        powerCurveES = function() private$..powerCurveES$value,
        powerCurveN = function() private$..powerCurveN$value,
        text = function() private$..text$value),
    private = list(
        ..calc = NA,
        ..es = NA,
        ..power = NA,
        ..n = NA,
        ..alt = NA,
        ..alpha = NA,
        ..powerContour = NA,
        ..powerDist = NA,
        ..powerCurveES = NA,
        ..powerCurveN = NA,
        ..text = NA)
)

ttestPSResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "ttestPSResults",
    inherit = jmvcore::Group,
    active = list(
        intro = function() private$.items[["intro"]],
        powertab = function() private$.items[["powertab"]],
        tabText = function() private$.items[["tabText"]],
        powerEStab = function() private$.items[["powerEStab"]],
        powerContour = function() private$.items[["powerContour"]],
        contourText = function() private$.items[["contourText"]],
        powerCurveES = function() private$.items[["powerCurveES"]],
        curveESText = function() private$.items[["curveESText"]],
        powerCurveN = function() private$.items[["powerCurveN"]],
        curveNText = function() private$.items[["curveNText"]],
        powerDist = function() private$.items[["powerDist"]],
        distText = function() private$.items[["distText"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Paired Samples T-Test")
            self$add(jmvcore::Html$new(
                options=options,
                name="intro",
                title="Introduction",
                visible="(text)"))
            self$add(jmvcore::Table$new(
                options=options,
                name="powertab",
                title="A Priori Power Analysis",
                rows=1,
                clearWith=list(
                    "es",
                    "power",
                    "n",
                    "alt",
                    "alpha",
                    "calc"),
                columns=list()))
            self$add(jmvcore::Html$new(
                options=options,
                name="tabText",
                title="Table context",
                visible="(text)"))
            self$add(jmvcore::Table$new(
                options=options,
                name="powerEStab",
                title="Power by Effect Size",
                rows=4,
                visible="(text)",
                clearWith=list(
                    "es",
                    "power",
                    "n",
                    "alt",
                    "alpha",
                    "calc"),
                columns=list(
                    list(
                        `name`="es", 
                        `title`="True effect size", 
                        `type`="number"),
                    list(
                        `name`="power", 
                        `title`="Power to detect", 
                        `type`="text"),
                    list(
                        `name`="desc", 
                        `title`="Description", 
                        `type`="text"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="powerContour",
                title="Power Contour",
                width=400,
                height=350,
                renderFun=".powerContour",
                visible="(powerContour)",
                clearWith=list(
                    "es",
                    "power",
                    "n",
                    "alt",
                    "alpha",
                    "calc")))
            self$add(jmvcore::Html$new(
                options=options,
                name="contourText",
                title="Power contour context",
                visible="(text & powerContour)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="powerCurveES",
                title="Power Curve by Effect Size",
                width=400,
                height=350,
                renderFun=".powerCurveES",
                visible="(powerCurveES & !calc:n)",
                clearWith=list(
                    "es",
                    "power",
                    "n",
                    "alt",
                    "alpha",
                    "calc")))
            self$add(jmvcore::Html$new(
                options=options,
                name="curveESText",
                title="Power contour by effect size context",
                visible="(text & powerCurveES & !calc:n)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="powerCurveN",
                title="Power Curve by N",
                width=400,
                height=350,
                renderFun=".powerCurveN",
                visible="(powerCurveN & !calc:es)",
                clearWith=list(
                    "es",
                    "power",
                    "n",
                    "alt",
                    "alpha",
                    "calc")))
            self$add(jmvcore::Html$new(
                options=options,
                name="curveNText",
                title="Power contour by N context",
                visible="(text & powerCurveN & !calc:es)"))
            self$add(jmvcore::Image$new(
                options=options,
                name="powerDist",
                title="Power Demonstration",
                width=400,
                height=300,
                renderFun=".powerDist",
                visible="(powerDist)",
                clearWith=list(
                    "es",
                    "power",
                    "n",
                    "alt",
                    "alpha",
                    "calc",
                    "n_ratio")))
            self$add(jmvcore::Html$new(
                options=options,
                name="distText",
                title="Sampling distributions",
                visible="(text & powerDist)"))}))

ttestPSBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "ttestPSBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "jpower",
                name = "ttestPS",
                version = c(1,0,0),
                options = options,
                results = ttestPSResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'na')
        }))

#' Paired Samples T-Test
#'
#' 
#' @param calc .
#' @param es .
#' @param power .
#' @param n .
#' @param alt .
#' @param alpha .
#' @param powerContour .
#' @param powerDist .
#' @param powerCurveES .
#' @param powerCurveN .
#' @param text .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$intro} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powertab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$tabText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powerEStab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$powerContour} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$contourText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powerCurveES} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$curveESText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powerCurveN} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$curveNText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$powerDist} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$distText} \tab \tab \tab \tab \tab a html \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$powertab$asDF}
#'
#' \code{as.data.frame(results$powertab)}
#'
#' @export
ttestPS <- function(
    calc = "n",
    es = 0.5,
    power = 0.9,
    n = 20,
    alt = "two.sided",
    alpha = 0.05,
    powerContour = TRUE,
    powerDist = FALSE,
    powerCurveES = TRUE,
    powerCurveN = FALSE,
    text = TRUE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("ttestPS requires jmvcore to be installed (restart may be required)")


    options <- ttestPSOptions$new(
        calc = calc,
        es = es,
        power = power,
        n = n,
        alt = alt,
        alpha = alpha,
        powerContour = powerContour,
        powerDist = powerDist,
        powerCurveES = powerCurveES,
        powerCurveN = powerCurveN,
        text = text)

    analysis <- ttestPSClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

