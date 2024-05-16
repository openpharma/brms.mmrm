
# brms.mmrm <img src="man/figures/logo.svg" align="right" height="139" alt="https://openpharma.github.io/brms.mmrm/" />

[![CRAN](https://www.r-pkg.org/badges/version/brms.mmrm)](https://CRAN.R-project.org/package=brms.mmrm)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/openpharma/brms.mmrm/workflows/check/badge.svg)](https://github.com/openpharma/brms.mmrm/actions?query=workflow%3Acheck)
[![cover](https://github.com/openpharma/brms.mmrm/workflows/cover/badge.svg)](https://github.com/openpharma/brms.mmrm/actions?query=workflow%3Acover)
[![lint](https://github.com/openpharma/brms.mmrm/workflows/lint/badge.svg)](https://github.com/openpharma/brms.mmrm/actions?query=workflow%3Alint)

The [mixed model for repeated measures
(MMRM)](https://link.springer.com/article/10.1177/009286150804200402) is
a popular model for longitudinal clinical trial data with continuous
endpoints, and [`brms`](https://paul-buerkner.github.io/brms/) is
powerful and versatile package for fitting Bayesian regression models.
The `brms.mmrm` R package leverages
[`brms`](https://paul-buerkner.github.io/brms/) to run
[MMRMs](https://link.springer.com/article/10.1177/009286150804200402),
and it supports a simplified interface to reduce difficulty and align
with best practices for the life sciences.

## Installation

| Type        | Source     | Command                                                                      |
|-------------|------------|------------------------------------------------------------------------------|
| Release     | CRAN       | `install.packages("brms.mmrm")`                                              |
| Development | GitHub     | `remotes::install_github("openpharma/brms.mmrm")`                            |
| Development | openpharma | `install.packages("brms.mmrm", repos = "https://openpharma.r-universe.dev")` |

## Documentation

The documentation website at <https://openpharma.github.io/brms.mmrm/>
has a complete function reference and tutorial vignettes.

## Validation

To ensure the correctness of the model and its implementation, this
package has been validated using simulation-based calibration and
comparisons against the frequentist
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/) package on two
example datasets. The analyses and results are described in the package
vignettes linked below:

- [Simulation-based
  calibration](https://openpharma.github.io/brms.mmrm/articles/sbc.html)
- [FEV1 data comparison between Bayesian and frequentist
  MMRMs](https://openpharma.github.io/brms.mmrm/articles/fev1.html).
- [BCVA data comparison between Bayesian and frequentist
  MMRMs](https://openpharma.github.io/brms.mmrm/articles/bcva.html).

Notably,
[FEV1](https://openpharma.github.io/mmrm/latest-tag/reference/fev_data.html)
and
[BCVA](https://openpharma.github.io/mmrm/latest-tag/reference/bcva_data.html)
are the same datasets that
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/) uses to compare
itself against SAS in [this
vignette](https://openpharma.github.io/mmrm/latest-tag/articles/mmrm_review_methods.html).
For additional validation in your functional area or domain of
expertise, you may choose to run similar analyses on your own
datasets to compare `brms.mmrm` against
[`mmrm`](https://openpharma.github.io/mmrm/latest-tag/) and/or SAS.

## Help

Please report questions and problems as [GitHub
discussions](https://github.com/openpharma/brms.mmrm) and [GitHub
issues](https://github.com/openpharma/brms.mmrm), respectively.

## Thanks

Thanks to [`openstatsware`](https://www.openstatsware.org/) and the [R
Consortium](https://www.r-consortium.org/) for providing professional
networks to recruit skilled statisticians and developers.

## Code of conduct

Please note that the brms.mmrm project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Citation

    To cite package 'brms.mmrm' in publications use:

      Landau WM, Kunzmann K, Sidi Y, Stock C (????). _brms.mmrm: Bayesian
      MMRMs using 'brms'_. R package version 0.1.0.9001,
      <https://github.com/openpharma/brms.mmrm>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {brms.mmrm: Bayesian MMRMs using 'brms'},
        author = {William Michael Landau and Kevin Kunzmann and Yoni Sidi and Christian Stock},
        note = {R package version 0.1.0.9001},
        url = {https://github.com/openpharma/brms.mmrm},
      }

## References

- Paul-Christian Bürkner (2017). brms: An R Package for Bayesian
  Multilevel Models Using Stan. Journal of Statistical Software, 80(1),
  1-28.
- Mallinckrodt, C.H., Lane, P.W., Schnell, D. et al. Recommendations for
  the Primary Analysis of Continuous Endpoints in Longitudinal Clinical
  Trials. Ther Innov Regul Sci 42, 303–319 (2008).
