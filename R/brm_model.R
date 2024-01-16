#' @title Basic MMRM
#' @export
#' @family models
#' @description Fit a basic MMRM model using `brms`.
#' @inheritSection brm_formula Parameterization
#' @return A fitted model object from `brms`.
#' @param data A tidy data frame with one row per patient per discrete
#'   time point.
#' @param formula An object of class `"brmsformula"` from [brm_formula()]
#'   or `brms::brmsformula()`. Should include the full parameterization
#'   of the model, including fixed effects, residual correlation,
#'   and heterogeneity in the discrete-time-specific residual variance
#'   components.
#' @param prior Either `NULL` for default priors
#'   or a `"brmsprior"` object from `brms::prior()`.
#' @param ... Arguments to `brms::brm()` other than `data`, `formula`,
#'   and `prior`.
#' @examples
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' set.seed(0L)
#' data <- brm_data(
#'   data = brm_simulate_simple()$data,
#'   outcome = "response",
#'   role = "response",
#'   group = "group",
#'   time = "time",
#'   patient = "patient",
#'   reference_group = "group_1",
#'   reference_time = "time_1"
#' )
#' formula <- brm_formula(
#'   data = data,
#'   baseline = FALSE,
#'   baseline_time = FALSE
#' )
#' # Optional: set the contrast option, which determines the model matrix.
#' options(contrasts = c(unordered = "contr.SAS", ordered = "contr.poly"))
#' # See the fixed effect parameterization you get from the data:
#' head(brms::make_standata(formula = formula, data = data)$X)
#' # Specify a different contrast method to use an alternative
#' # parameterization when fitting the model with brm_model():
#' options(
#'   contrasts = c(unordered = "contr.treatment", ordered = "contr.poly")
#' )
#' # different model matrix than before:
#' head(brms::make_standata(formula = formula, data = data)$X)
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       model <- brm_model(
#'         data = data,
#'         formula = formula,
#'         chains = 1,
#'         iter = 100,
#'         refresh = 0
#'       )
#'     )
#'   )
#' )
#' # The output model is a brms model fit object.
#' model
#' # The `prior_summary()` function shows the full prior specification
#' # which reflects the fully realized fixed effects parameterization.
#' brms::prior_summary(model)
#' }
brm_model <- function(
  data,
  formula,
  prior = NULL,
  ...
) {
  brm_data_validate(data = data)
  assert(
    inherits(formula, "brmsformula"),
    message = "formula arg must be a \"brmsformula\" object."
  )
  assert(
    inherits(prior %|||% brms::prior("normal(0, 1)"), "brmsprior"),
    message = "prior arg must be a \"brmsprior\" object or NULL."
  )
  brms::brm(
    data = data[!is.na(data[[attr(data, "brm_outcome")]]), ],
    formula = formula,
    prior = prior,
    ...
  )
}
