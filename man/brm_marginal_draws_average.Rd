% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brm_marginal_draws_average.R
\name{brm_marginal_draws_average}
\alias{brm_marginal_draws_average}
\title{Average marginal MCMC draws across time points.}
\usage{
brm_marginal_draws_average(draws, data, times = NULL, label = "average")
}
\arguments{
\item{draws}{List of posterior draws from \code{\link[=brm_marginal_draws]{brm_marginal_draws()}}.}

\item{data}{A classed data frame from \code{\link[=brm_data]{brm_data()}}, or an informative
prior archetype from a function like \code{\link[=brm_archetype_successive_cells]{brm_archetype_successive_cells()}}.}

\item{times}{Character vector of discrete time point levels
over which to average the MCMC samples within treatment group levels.
Set to \code{NULL} to average across all time points. Levels are automatically
sanitized with \code{make.names(unique = FALSE, allow_ = TRUE)} to ensure
agreement with \code{brms} variable names in downstream computations.}

\item{label}{Character of length 1, time point label for the averages.
Automatically sanitized with \code{make.names(unique = FALSE, allow_ = TRUE)}.
Must not conflict with any existing time point labels in the data
after the label and time points are sanitized.}
}
\value{
A named list of tibbles of MCMC draws of the marginal posterior
distribution of each treatment group and time point
(or group-by-subgroup-by-time, if applicable).
See \code{\link[=brm_marginal_draws]{brm_marginal_draws()}} for the full details of the return value.
The only difference is that \code{\link[=brm_marginal_draws_average]{brm_marginal_draws_average()}} returns
a single pseudo-time-point to represent the average across
multiple real time points.
}
\description{
Simple un-weighted arithmetic mean of
marginal MCMC draws across time points.
}
\section{Separation string}{

Post-processing in \code{\link[=brm_marginal_draws]{brm_marginal_draws()}} names each of the
group-by-time marginal means with the delimiting character string
from \code{Sys.getenv("BRM_SEP", unset = "|")}. Neither the column names
nor element names of the group and time variables can contain
this string. To set a custom string yourself, use
\code{Sys.setenv(BRM_SEP = "YOUR_CUSTOM_STRING")}.
}

\examples{
if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
set.seed(0L)
data <- brm_data(
  data = brm_simulate_simple()$data,
  outcome = "response",
  group = "group",
  time = "time",
  patient = "patient",
  reference_group = "group_1",
  reference_time = "time_1"
)
formula <- brm_formula(
  data = data,
  baseline = FALSE,
  baseline_time = FALSE
)
tmp <- utils::capture.output(
  suppressMessages(
    suppressWarnings(
      model <- brm_model(
        data = data,
        formula = formula,
        chains = 1,
        iter = 100,
        refresh = 0
      )
    )
  )
)
draws <- brm_marginal_draws(data = data, formula = formula, model = model)
brm_marginal_draws_average(draws = draws, data = data)
brm_marginal_draws_average(
  draws = draws,
  data = data,
  times = c("time_1", "time_2"),
  label = "mean"
)
}
}
\seealso{
Other marginals: 
\code{\link{brm_marginal_data}()},
\code{\link{brm_marginal_draws}()},
\code{\link{brm_marginal_grid}()},
\code{\link{brm_marginal_probabilities}()},
\code{\link{brm_marginal_summaries}()}
}
\concept{marginals}
