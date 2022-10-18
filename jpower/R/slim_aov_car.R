
# @importFrom car Anova
# @importFrom reshape2 dcast
# @importFrom lme4 findbars nobars 
# @importFrom stats terms as.formula xtabs contrasts<- coef

slim_aov_car <- function(formula,
                         data,
                         fun_aggregate = NULL,
                         type = 3,
                         factorize = TRUE,
                         check_contrasts = TRUE,
                         observed = NULL,
                         anova_table = list(),
                         include_aov = FALSE,
                         return = "afex_aov",
                         ...)
{
  return <- match.arg(return, 
                      c("Anova", "lm", "data", "nice", "afex_aov", 
                        "univariate", "marginal", "aov"))
  dots <- list(...)
  
  ### deprercate old argument names:
  if("check.contrasts" %in% names(dots)) {  
    warn_deprecated_arg("check.contrasts", "check_contrasts")
    check_contrasts <- dots$check.contrasts
    dots <- dots[names(dots) != "check.contrasts"]
  }
  if("fun.aggregate" %in% names(dots)) { 
    warn_deprecated_arg("fun.aggregate", "fun_aggregate")
    fun_aggregate <- dots$fun.aggregate
    dots <- dots[names(dots) != "fun.aggregate"]
  }
  
  # transform to data.frame if necessary (e.g., when using dplyr)
  data <- as.data.frame(data)
  
  # stuff copied from aov:
  Terms <- terms(formula, "Error", data = data)
  indError <- attr(Terms, "specials")$Error
  if (length(indError) > 1L) 
    stop(sprintf(ngettext(length(indError), 
                          "there are %d Error terms: only 1 is allowed", 
                          "there are %d Error terms: only 1 is allowed"), 
                 length(indError)), 
         domain = NA)
  
  # from here, code by Henrik Singmann:
  if (is.null(indError)) {
    stop("formula needs an error term identifying the ID column.")
  }
  
  vars <- all.vars(formula)
  #--- Russ Lenth added/modified code to detect transformed responses:
  lhs <- all.names(formula[[2]])
  transf <- setdiff(lhs, all.vars(formula[[2]]))
  if (length(transf) == 0)
    transf = NULL
  if (!is.null(transf)) {
    origdv <- setdiff(lhs, transf)
    dv <- paste0(transf[1], ".", origdv)
    data[[dv]] <- eval(formula[[2]], envir = data)  # add transformed version
    vars <- vars[!(vars %in% lhs)]
  }
  else {
    dv <- vars[1]
    if (!is.numeric(data[,dv])) stop("dv needs to be numeric.") #check if dv is numeric
    vars <- vars[-1]
  }
  #--- end RL changes
  parts <- attr(terms(formula, "Error", data = data), "term.labels")
  error.term <- parts[grepl("^Error\\(", parts)]
  
  id <- all.vars(parse(text = error.term))[1]
  within <- all.vars(parse(text = error.term))[-1]
  between <- vars[!(vars %in% c(id, within))]
  
  dv.escaped <- escape_vars(dv)
  id.escaped <- escape_vars(id)
  within.escaped  <- escape_vars(within)
  between.escaped <- escape_vars(between)

  effect.parts <- parts[!grepl("^Error\\(", parts)]
  
  if (length(within) > 0) {
    effect.parts.no.within <- character()
    for (term in effect.parts) {
      components <- decomposeTerm(term)
      if ( ! any(within %in% components))
        effect.parts.no.within <- c(effect.parts.no.within, term)
    }
  } else {
    effect.parts.no.within <- effect.parts
  }
   #afex:::decomposeTerm()
  data <- droplevels(data) #remove empty levels.
  # make id and within variables to factors:
  if (!(is.factor(data[,id]))) data[,id] <- factor(data[,id])
  
  # factorize if necessary
  if (factorize) {
    if (any(!vapply(data[, between, drop = FALSE], is.factor, TRUE))) {
      to.factor <- between[!vapply(data[,between, drop = FALSE], is.factor, TRUE)]
      message(paste0("Converting to factor: ", paste0(to.factor, collapse = ", ")))
      for (tmp.c in to.factor) {
        data[,tmp.c] <- factor(data[,tmp.c])
      }
    }
  } else {
    # check if numeric variables are centered.
    c.ns <- between[vapply(data[, between, drop = FALSE], is.numeric, TRUE)]
    if (length(c.ns) > 0) {
      non.null <- 
        c.ns[!abs(vapply(data[, c.ns, drop = FALSE], mean, 0)) < 
               .Machine$double.eps ^ 0.5]
      if (length(non.null) > 0) 
        warning(paste0(
          "Numerical variables NOT centered on 0 (i.e., likely bogus results): ", 
          paste0(non.null, collapse = ", ")), call. = FALSE)
    }
  }
  
  for (i in c(between, within)) {
    if (is.factor(data[,i]) && length(unique(data[,i])) == 1) 
      stop(paste0("Factor \"", i, 
                  "\" consists of one level only. Remove factor from model?"))
  }
  
  # make formulas
  rh2 <- if (length(between.escaped) > 0) {
    paste0(effect.parts.no.within, collapse = "+") 
  } else "1"
  lh1 <- mypaste(id, 
                 if (length(between.escaped) > 0) 
                   paste0(between.escaped, collapse = "+") 
                 else NULL, 
                 sep = "+")
  rh1 <- paste0(within.escaped, collapse = "+")
  rh3 <- paste0(within.escaped, collapse = "*")
  # afex:::mypaste()
  # converting all within subject factors to factors and 
  # add a leading charcter (x) if starting with a digit.
  for (within.factor in within) {
    if (is.factor(data[,within.factor])) 
      levels(data[,within.factor]) <- make.names(levels(data[,within.factor]), 
                                                 unique = TRUE)
    else 
      data[,within.factor] <- 
        factor(as.character(data[,within.factor]), 
               levels = unique(as.character(data[,within.factor])), 
               labels = make.names(unique(as.character(data[,within.factor])), 
                                   unique=TRUE))
  }
  
  # Check if each id is in only one between subjects cell.
  between.factors <- between[vapply(data[, between, drop = FALSE], is.factor, TRUE)]
  if (length(between.factors) > 0) {
    split.data <- split(data, lapply(between.factors, function(x) data[,x]))
    ids.per.condition <- 
      lapply(split.data, function(x) unique(as.character(x[,id])))
    ids.in.more.condition <- 
      unique(unlist(
        lapply(seq_along(ids.per.condition), 
               function(x) unique(unlist(
                 lapply(ids.per.condition[-x], 
                        function(y, z = ids.per.condition[[x]]) 
                          intersect(z, y)))))))
    if (length(ids.in.more.condition) > 0) {
      stop(
        paste0("Following ids are in more than one between subjects condition:\n", 
               paste0(ids.in.more.condition, collapse = ", ")))
    }
  }
  
  ## check for structurally missing data
  # within-subjects
  if ((length(within) > 0) && any(table(data[within]) == 0)) {
    stop("Empty cells in within-subjects design", 
         " (i.e., bad data structure).\n", 
         "", paste0("table(data[", deparse(within), "])"), "\n# ",
         paste(utils::capture.output(table(data[within])), collapse = "\n# "),
         call. = FALSE)
  }
  
  # Is fun_aggregate NULL and aggregation necessary?
  if (is.null(fun_aggregate)) {
    if (any(xtabs(
      as.formula(paste0("~", id.escaped, if (length(within) > 0) "+", rh1)), 
      data = data) > 1)) {
      warning("More than one observation per design cell, aggregating data using `fun_aggregate = mean`.\nTo turn off this warning, pass `fun_aggregate = mean` explicitly.", 
              call. = FALSE)
      fun_aggregate <- mean
    }
  } 
  
  # prepare the data:
  tmp.dat <- do.call(
    reshape2::dcast, 
    args = 
      c(data = list(data), 
        formula = as.formula(paste(lh1, 
                                   if (length(within) > 0) rh1 
                                   else ".", sep = "~")), 
        fun.aggregate = fun_aggregate,  value.var = dv))
  
  # check for missing values:
  if (any(is.na(tmp.dat))) {
    missing.values <- apply(tmp.dat, 1, function(x) any(is.na(x)))
    missing_ids <- unique(tmp.dat[missing.values,1])
    warning(paste0("Missing values for following ID(s):\n", 
                   paste0(missing_ids, collapse = ", "), 
                   "\nRemoving those cases from the analysis."), call. = FALSE) 
    tmp.dat <- tmp.dat[!missing.values,]
    data <- data[ !(data[,id] %in% missing_ids),]
    if ((nrow(data) == 0 ) | (nrow(tmp.dat) == 0)) {
      stop("No observations remain after removing missing values.", 
           "\n  Try adding to ANOVA call: na.rm = TRUE", call. = FALSE)
    }
  } else {
    missing_ids <- NULL
  }
  # if (length(between_nn) > 0 && any(table(data[between_nn]) == 0)) {
  #   stop("Empty cells in between-subjects design ", 
  #        " (i.e., bad data structure).\n",  
  #        "", paste0("table(data[", deparse(between_nn), "])"), "\n# ",
  #        paste(utils::capture.output(table(data[between_nn])), collapse = "\n# "),
  #        call. = FALSE)
  # }
  
  #   if (length(between) > 0) {
  #     n_data_points <- xtabs(as.formula(paste("~", paste(between, collapse = "+"))), data = tmp.dat)
  #     if (any(n_data_points == 0)) warning("Some cells of the fully crossed between-subjects design are empty. A full model might not be estimable.")
  #   }
  
  # marginals: (disabled in April 2015), dat.ret is now used for aov()
  dat.ret <- do.call(
    reshape2::dcast, 
    args = c(data = list(data), 
             formula = as.formula(paste0(mypaste(lh1, 
                                                 if (length(within) > 0) rh1 
                                                 else NULL, sep = "+"), "~.")), 
             fun.aggregate = fun_aggregate, 
             #dots, 
             value.var = dv))
  colnames(dat.ret)[length(colnames(dat.ret))] <- dv
  if (suppressWarnings(!isTRUE(
    all.equal(target = data[,c(id, between, within, dv)], 
              current = dat.ret[,c(id, between, within, dv)], 
              check.attributes = FALSE)
  ))) {
    data_changed <- TRUE
  } else {
    data_changed <- FALSE
  }
  
  if (length(between) > 0) {
    tmp.dat <- check_contrasts(
      data = tmp.dat,
      factors = between,
      check_contrasts = check_contrasts,
      type = type
    )
  }
  if (return %in% c("aov")) include_aov <- TRUE
  if(include_aov){
    if (check_contrasts) {
      factor_vars <- 
        vapply(dat.ret[,c(within, between), drop = FALSE], is.factor, NA)
      contrasts <- as.list(rep("contr.sum", sum(factor_vars)))
      names(contrasts) <- c(within, between)[factor_vars]
    }
    tmp_formula <- formula(paste(dv.escaped, "~", 
                                 if (length(within) > 0) {
                                   paste(
                                     if (rh2 == "1") {
                                       paste(within.escaped, collapse="*")
                                     } else {
                                       paste("(" ,rh2, ")*(", paste(within.escaped, collapse="*"), ")")
                                     }, "+Error(", id.escaped, "/(", 
                                     paste(within.escaped, collapse="*"), "))")
                                 } else rh2))
    aov <- aov(tmp_formula, data=dat.ret, contrasts = contrasts)
  } else {
    aov <- NULL
  }
  if(return == "aov") return(aov)
  data.l <- list(long = dat.ret, wide = tmp.dat)
  if (return == "data") return(tmp.dat)
  
  # branching based on type of ANOVA
  if (length(within) > 0) {  # if within-subject factors are present:
    # make idata argument
    if (length(within) > 1) {
      within.levels <- lapply(lapply(data[,within], levels), factor)
      idata <- rev(expand.grid(rev(within.levels)))
    } else {
      idata <- data.frame(levels(data[,within]), stringsAsFactors = TRUE)
      colnames(idata) <- within
    }
    tmp.lm <- do.call(
      "lm", 
      list(formula = 
             as.formula(paste0("cbind(", 
                               paste0(colnames(
                                 tmp.dat[-(seq_along(c(id, between)))]), 
                                 collapse = ", "), 
                               ") ~ ", 
                               rh2)), 
           data = tmp.dat))
    if (any(is.na(coef(tmp.lm)))) {
      between_design_error(
        data = tmp.dat, 
        between = between, 
        bad_vars = names(which(apply(is.na(coef(tmp.lm)), 1, any)))
      ) 
    }
    if (return == "lm") return(tmp.lm)
    Anova.out <- car::Anova(tmp.lm, 
                       idata = idata, 
                       idesign = as.formula(paste0("~", rh3)), 
                       type = type)
    data.l <- c(data.l, idata = list(idata))
    
  } else { # if NO within-subject factors are present (i.e., purely between ANOVA):
    colnames(tmp.dat)[ncol(tmp.dat)] <- "dv"
    tmp.lm <- do.call("lm", 
                      list(formula = as.formula(paste0("dv ~ ", rh2)), 
                           data = tmp.dat))
    if (any(is.na(coef(tmp.lm)))) {
      between_design_error(
        data = tmp.dat, 
        between = between, 
        bad_vars = names(which(is.na(coef(tmp.lm))))
      ) 
    }
    if (return == "lm") return(tmp.lm)
    Anova.out <- car::Anova(tmp.lm, type = type)
  }
  if (return == "afex_aov") {
    afex_aov <- list(
      anova_table = NULL, 
      aov = aov,
      Anova = Anova.out,
      lm = tmp.lm,
      data = data.l
    )
    class(afex_aov) <- "afex_aov"
    attr(afex_aov, "dv") <- dv
    attr(afex_aov, "id") <- id
    attr(afex_aov, "within") <- 
      if (length(within) > 0) lapply(data[, within, drop = FALSE], 
                                     levels) else list()
    attr(afex_aov, "between") <- 
      if (length(between) > 0) lapply(data[, between, drop = FALSE], 
                                      levels) else list()
    attr(afex_aov, "type") <- type
    attr(afex_aov, "transf") <- transf
    attr(afex_aov, "incomplete_cases") <- missing_ids
    attr(afex_aov, "data_changed") <- data_changed
    afex_aov$anova_table <- 
      do.call("anova", 
              args = c(object = list(afex_aov), observed = list(observed), 
                       anova_table))
    return(afex_aov)
  }
  if (return == "Anova") return(Anova.out)
  else if (return == "univariate") {
    if (inherits(Anova.out, "Anova.mlm")) 
      return(summary(Anova.out, multivariate = FALSE))
    else 
      return(Anova.out)
  }
  else if (return == "nice") {
    afex_aov <- list(
      anova_table = NULL,
      Anova = Anova.out
    )
    class(afex_aov) <- "afex_aov"
    attr(afex_aov, "dv") <- dv
    attr(afex_aov, "id") <- id
    attr(afex_aov, "within") <- 
      if (length(within) > 0) lapply(data[,within, drop = FALSE], 
                                     levels) else list()
    attr(afex_aov, "between") <- 
      if (length(between) > 0) lapply(data[,between,drop=FALSE], 
                                      levels) else list()
    attr(afex_aov, "type") <- type
    afex_aov$anova_table <- 
      do.call("anova", 
              args = c(object = list(afex_aov), 
                       observed = list(observed), 
                       anova_table))
    return(do.call("nice", 
                   args = c(object = list(afex_aov), 
                            observed = list(observed), 
                            anova_table)))
  }
  

}



between_design_error <- function(data, between, bad_vars) {
  ## check between-subjects design for completeness
  ## select all factor variables
  between_nn <- between[!vapply(data[between], is.numeric, NA)]
  stop(
    "Rank deficient model matrix; insufficient data to estimate full model.\n", 
    "Model coefficient(s) estimated as NA: ", 
    paste(bad_vars, collapse = ", "),
    "\nLikely empty cells in between-subjects design ",
    "(i.e., bad data structure).\n",
    "", paste0("table(data[", deparse(between_nn), "])"), "\n# ",
    paste(utils::capture.output(table(data[between_nn])), collapse = "\n# "),
    call. = FALSE)
}

escape_vars = function (names) {
  if (length(names) == 0) 
    return(names)
  names <- vapply(names, function(name) {
    if (make.names(name) != name) {
      name <- gsub("\\", "\\\\", name, fixed = TRUE)
      name <- gsub("`", "\\`", name, fixed = TRUE)
      name <- paste0("`", name, "`")
    }
    name
  }, FUN.VALUE = "", USE.NAMES = FALSE)
  names
}

decomposeTerm = function (term) 
{
  chars <- strsplit(term, "")[[1]]
  components <- character()
  componentChars <- character()
  inQuote <- FALSE
  i <- 1
  n <- length(chars)
  while (i <= n) {
    char <- chars[i]
    if (char == "`") {
      inQuote <- !inQuote
    }
    else if (char == "\\") {
      i <- i + 1
      char <- chars[i]
      componentChars <- c(componentChars, char)
    }
    else if (char == ":" && inQuote == FALSE) {
      component <- paste0(componentChars, collapse = "")
      components <- c(components, component)
      componentChars <- character()
    }
    else {
      componentChars <- c(componentChars, char)
    }
    i <- i + 1
  }
  component <- paste0(componentChars, collapse = "")
  components <- c(components, component)
  components
}

mypaste = function (..., sep) 
{
  tmp <- paste(..., sep = sep)
  if (substr(tmp, nchar(tmp), nchar(tmp)) == sep) {
    tmp <- substr(tmp, 1, nchar(tmp) - 1)
  }
  if (substr(tmp, 1, 1) == sep) {
    tmp <- substr(tmp, 2, nchar(tmp))
  }
  tmp
}
