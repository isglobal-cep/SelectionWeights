#' Estimate weights to reduce bias due to selection
#'
#' @description
#' This is the main function to estimate balancing weights for the reduction
#' of bias due to selection (e.g., loss to follow-up). It is essentially a
#' wrapper around the \link[WeightIt]{weightit} function. Please refer to its
#' documentation for further details.
#' Essentially, the function takes as input a dataset and a formula describing
#' how the selection process might depend on the chosen covariates. It will
#' then estimate weights such that the distribution of the covariates among
#' selected and non-selected subjects is *similar*. These weights can finally
#' be used in most functions to fit regression models (e.g. the `weights`
#' parameter in the \link[stats]{glm} function).
#' If the formula is not passed as input, the function will simply create
#' a main-effects only formula (e.g., no interactions).
#' More information can be found in specialized texts, like
#' Hernán, M.A. and Robins, J.M., 2010. *Causal inference*.
#' @md
#'
#' @param dat The dataset containing a column for the identifier, and all
#' the covariates to be used when estimating the weights. A dataframe.
#' @param id_str A string indicating the name of the column in `dat`
#' that contains the identifiers. A string.
#' @param ids_not_censored A vector of identifiers corresponding to
#' the subjects not censored. That is, the subjects that were selected for
#' e.g., the follow-up. It must be a subset of the identifiers contained in
#' the `id_str` column of `dat`. A vector.
#' @param formula A string representing the formula to be passed to
#' \link[WeightIt]{weightit}. It must be of the form `"covariate_1 + ..."`.
#' It can include interactions and functions of the individual covariates
#' (e.g., a cubic spline). A string.
#' @param method_estimation The type of model to fit to estimate the
#' balancing weights. It must be supported by the \link[WeightIt] R package.
#' The list of specific estimation methods can be found
#' [here](https://ngreifer.github.io/WeightIt/reference/index.html). A string.
#' @param link_function The link used in the generalized linear model
#' for the propensity scores. If `glm` is used to estimate the balancing
#' weights, this simply is the model link function (e.g., `logit`). A string.
#' @param stabilized For the methods that estimate propensity scores,
#' whether to stabilize the weights or not. That is, whether to multiply
#' the individual weights by the proportion of subjects in their
#' *treatment* group. A boolean.
#' @param winsorization
#' @param estimate_by
#' @param sampling_weights
#' @param moments
#' @param interactions
#' @param library_sl
#' @param cv_control_sl
#' @param discrete_sl
#'
#' @return
#' @export
estimate_selection_weights <- function(dat,
                                       id_str,
                                       ids_not_censored,
                                       formula,
                                       method_estimation,
                                       link_function,
                                       stabilized,
                                       winsorization,
                                       estimate_by,
                                       sampling_weights,
                                       moments,
                                       interactions,
                                       library_sl,
                                       cv_control_sl,
                                       discrete_sl) {
  # Checks
  ## Check all covariates are available
  ## Check IDs match
  ## Check formula
  ## Check winsorization
  ## Check grouping variable is available
  ## Check sampling weights
  ## Check moments
  ##############################################################################

  # Process data
  ## Add indicator variable representing selection into e.g., follow-up
  dat <- dat |>
    dplyr::mutate(
      selected = ifelse(
        .data[[id_str]] %in% ids_not_censored,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::relocate(
      selected, .after = dplyr::all_of(id_str)
    )
  assertthat::assert_that(
    sum(dat[["selected"]]) == length(ids_not_censored),
    msg = "Mismatch in the number of selected individuals."
  )
  ##############################################################################

  # Estimate weights
  ## Eventually create formula
  if (is.null(formula)) {
    covariates <- setdiff(
      colnames(dat),
      c(id_str, "selected")
    )
    formula <- paste0(
      "selected ~ ",
      paste0(covariates, collapse = " + ")
    )
  } else {
    formula <- paste0(
      "selected ~ ",
      formula
    )
  }

  ## Raw weights
  raw_weights <- WeightIt::weightit(
    formula = as.formula(formula),
    data = dat,
    method = method_estimation,
    link = link_function,
    stabilize = stabilized,
    by = estimate_by,
    s.weights = sampling_weights,
    moments = moments,
    int = interactions,
    missing = "ind",
    include.obj = TRUE,
    verbose = TRUE,
    SL.library = library_sl,
    cvControl = cv_control_sl,
    discrete = discrete_sl
  )

  ## Eventually trim weights
  win_weights <- NULL
  if (!is.null(winsorization)) {
    win_weights <- WeightIt::trim(
      raw_weights,
      at = winsorization,
      lower = TRUE
    )
  }
  ##############################################################################

  return(list(
    dat = dat,
    formula = formula,
    raw_weights = raw_weights,
    win_weights = win_weights
  ))
} # End function estimate_selection_weights
################################################################################
