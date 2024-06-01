# Simulation-based calibration of `brms.mmrm`

This subdirectory contains materials for a simulation-based calibration (SBC) study to validate the implementation of the MMRM modeling code in `brms.mmrm`. The overall approach follows the overall technique described at <https://docs.ropensci.org/stantargets/articles/simulation.html#simulation-based-calibration>.

# Usage

The SBC pipeline is implemented with [`targets`](https://docs.ropensci.org/targets/) using a Sun Grid Engine (SGE) cluster. It runs with a simple `tar_make()`, and computation takes several hours to complete.

```r
library(targets)
tar_make()
```

For more information on [`targets`](https://docs.ropensci.org/targets/), please visit <https://docs.ropensci.org/targets/>. To learn how to configure resources or switch to a computing platform, please visit <https://books.ropensci.org/targets/crew.html>.

# Sources

* Kim, S., Moon, H., Modrák, M., and Säilynoja, T. (2022), SBC: Simulation based calibration for rstan/cmdstanr models.
* Modrák, M., Moon, A. H., Kim, S., Bürkner, P., Huurre, N., Faltejsková, K., Gelman, A., and Vehtari, A. (2024), “Simulation-Based Calibration Checking for Bayesian Computation: The Choice of Test Quantities Shapes Sensitivity,” Bayesian Analysis, forthcoming. https://doi.org/10.1214/23-BA1404.
