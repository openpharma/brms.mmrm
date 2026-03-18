# Model

The `brms.mmrm` R package implements a mixed model of repeated measures
(MMRM), a popular and flexible model to analyze continuous longitudinal
outcomes (Mallinckrodt et al. (2008), Mallinckrodt and Lipkovich (2017),
Holzhauer and Weber (2024)). `brms.mmrm` focuses on marginal MMRMs for
randomized controlled parallel studies with discrete time points, where
each patient shares the same set of time points. Whereas the
[`mmrm`](https://openpharma.github.io/mmrm/) package is frequentist,
`brms.mmrm` fits models in Bayesian fashion using
[`brms`](https://paulbuerkner.com/brms/) (Bürkner 2017).

## Model

Let $y_{1},\ldots,y_{N}$ be independent data points observed for
individual patients in a clinical trial. Each $y_{n}$ is a numeric
vector of length $T$, where $T$ is the number of discrete time points in
the dataset (e.g. patient visits in the study protocol). We model
$y_{n}$ as follows:

$$\begin{array}{r}
{y_{n} \sim \text{Multivariate-Normal}\left( \text{mean} = X_{n}b,\ \text{variance} = \Sigma_{n} \right)}
\end{array}$$ Above, $X_{n}$ is the fixed effect model matrix of patient
$n$, and its specific makeup is determined by arguments such as
`intercept` and `group` in
[`brm_formula()`](../reference/brm_formula.md). $b$ is a constant-length
vector of fixed effect parameters.

The MMRM in `brms.mmrm` is a [distributional
model](https://paulbuerkner.com/brms/articles/brms_distreg.html), which
means it uses a linear regression structure for both the mean and the
variance of the multivariate normal likelihood. In particular, the
$T \times T$ symmetric positive-definite residual covariance matrix
$\Sigma_{n}$ of patient $n$ decomposes as follows:

$$\begin{aligned}
\Sigma_{n} & {= \text{diag}\left( \sigma_{n} \right) \cdot \Lambda \cdot \text{diag}\left( \sigma_{n} \right)} \\
\sigma_{n} & {= \text{exp}\left( Z_{n}b_{\sigma} \right)}
\end{aligned}$$

Above, $\sigma_{n}$ is a vector of $T$ time-specific scalar standard
deviations, and $\text{diag}\left( \sigma_{n} \right)$ is a diagonal
$T \times T$ matrix. $Z_{n}$ is a patient-specific matrix which controls
how the [distributional
parameters](https://paulbuerkner.com/brms/articles/brms_distreg.html)
$b_{\sigma}$ map to the more intuitive standard deviation vector
$\sigma_{n}$. The specific makeup of $Z_{n}$ is determined by the
`sigma` argument of [`brm_formula()`](../reference/brm_formula.md),
which in turn is produced by
[`brm_formula_sigma()`](../reference/brm_formula_sigma.md).

$\Lambda$ is a symmetric positive-definite correlation matrix with
diagonal elements equal to 1 and off-diagonal elements between -1 and 1.
The structure of $\Lambda$ depends on the `correlation` argument of
[`brm_formula()`](../reference/brm_formula.md), which could describe an
unstructured parameterization, ARMA, compound symmetry, etc. These
alternative structures and priors are available directly through `brms`.
For specific details, please consult
<https://paulbuerkner.com/brms/reference/autocor-terms.html> and
[`?brms.mmrm::brm_formula`](../reference/brm_formula.md).

## Priors

The scalar components of $b$ are modeled as independent with
user-defined priors specified through the `prior` argument of
[`brm_model()`](../reference/brm_model.md). The hyperparameters of these
priors are constant. The default priors are improper uniform for
non-intercept terms and a data-dependent Student-t distribution for the
intercept. The variance-related distributional parameters $b_{\sigma}$
are given similar priors

For the correlation matrix $\Lambda$, the default prior in `brms.mmrm`
is the [LKJ correlation
distribution](https://mc-stan.org/docs/functions-reference/correlation_matrix_distributions.html#lkj-correlation)
with shape parameter equal to 1. This choice of prior is only valid for
unstructured correlation matrices. Other correlation structures, such
ARMA, will parameterize $\Lambda$ and allow users to set priors on those
new specialized parameters.

## Sampling

`brms.mmrm`, through [`brms`](https://paulbuerkner.com/brms/), fits the
model to the data using the Markov chain Monte Carlo (MCMC) capabilities
of [Stan](https://mc-stan.org/) (Stan Development Team 2023). Please
read <https://mc-stan.org/users/documentation/> for more details on the
methodology of [Stan](https://mc-stan.org/). The result of MCMC is a
collection of draws from the full joint posterior distribution of the
parameters given the data. Individual draws of scalar parameters such as
$\beta_{3}$ are considered draws from the marginal posterior
distribution of e.g. $\beta_{3}$ given the data.

## Imputation of missing outcomes

Under the missing at random (MAR) assumptions, MMRMs do not require
imputation (Holzhauer and Weber (2024)). However, if the outcomes in
your data are not missing at random, or if you are targeting an
alternative estimand, then you may need to impute missing outcomes.
`brms.mmrm` can leverage either of the two alternative solutions
described at
<https://paulbuerkner.com/brms/articles/brms_missings.html>. Please see
the [usage
vignette](https://openpharma.github.io/brms.mmrm/articles/usage.html)
for details on the implementation and interface.

## References

Bürkner, P.-C. (2017), “brms: An R package for Bayesian multilevel
models using Stan,” *Journal of Statistical Software*, 80, 1–28.
<https://doi.org/10.18637/jss.v080.i01>.

Holzhauer, B., and Weber, S. (2024), “[Bayesian Mixed effects Model for
Repeated
Measures](https://opensource.nibr.com/bamdd/src/02h_mmrm.html),” in
*Applied Modeling in Drug Development*, Novartis AG.

Mallinckrodt, C. H., Lane, P. W., Schnell, D., and others (2008),
“Recommendations for the primary analysis of continuous endpoints in
longitudinal clinical trials,” *Therapeutic Innovation and Regulatory
Science*, 42, 303–319. <https://doi.org/10.1177/009286150804200402>.

Mallinckrodt, C. H., and Lipkovich, I. (2017), *Analyzing longitudinal
clinical trial data: A practical guide*, CRC Press, Taylor; Francis
Group.

Stan Development Team (2023), *[Stan modeling language users guide and
reference manual](https://mc-stan.org)*.
