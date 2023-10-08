#' cdystonia dataset
#'
#' Subset of data from a multicenter, randomized controlled trial of
#' botulinum toxin type B (BotB) in patients with cervical dystonia.
#' The endpoint of the trial was the total score on the Toronto Western
#' Spasmodic Torticollis Rating Scale (TWSTRS). The dataset contains data from
#' 72 patients and is limited to 2 (of originally 3) treatment arms.
#'
#' @format A tibble with 420 rows and 6 variables
#'
#' \describe{
#'   \item{id}{Character. Unique subject identifier}
#'   \item{age}{Integer.  Age of subject in years}
#'   \item{sex}{Character.  Sex of subject (`F`, female; `M`, male)}
#'   \item{treat}{Character. Treatment (`Placebo`, placebo, `X10000U`,
#'   10,000 units of BotB)}
#'   \item{week}{Character. Week of study visit}
#'   \item{twstrs}{Integer. Total TWSTRS score}
#' }
#' @source The data were drawn from the \pkg{Hmisc} package.
#' For further details see
#' \url{https://hbiostat.org/data/repo/cdystonia}.
#' @usage data(cdystonia)
#' @keywords datasets
#' @examples
#' data(cdystonia)
#' head(cdystonia)
"cdystonia"
