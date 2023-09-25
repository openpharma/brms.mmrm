# Simulation-based calibration of `brms.mmrm`

This subdirectory contains materials for a simulation-based calibration (SBC) study to validate the implementation of the MMRM modeling code in `brms.mmrm`. The overall approach follows the overall technique described at <https://docs.ropensci.org/stantargets/articles/simulation.html#simulation-based-calibration>.

# Usage

The SBC pipeline is implemented with [`targets`](https://docs.ropensci.org/targets/) using a Sun Grid Engine (SGE) cluster. It runs with a simple `tar_make()`, and computation takes several hours to complete.

```r
library(targets)
tar_make()
```

For more information on [`targets`](https://docs.ropensci.org/targets/), please visit <https://docs.ropensci.org/targets/>. To learn how to configure resources or swtich to a different cluster, please visit <https://books.ropensci.org/targets/crew.html>.

# Sources

* Carpenter, Bob. 2017. “Bayesian Posteriors are Calibrated by Definition.” https://statmodeling.stat.columbia.edu/2017/04/12/bayesian-posteriors-calibrated/.
* Cook, Samantha R., Andrew Gelman, and Donald B. Rubin. 2006. “Validation of Software for Bayesian Models Using Posterior Quantiles.” Journal of Computational and Graphical Statistics 15 (3): 675–92. http://www.jstor.org/stable/27594203.
* Kim, Shinyoung, Hyunji Moon, Martin Modrák, and Teemu Säilynoja. 2022. SBC: Simulation Based Calibration for Rstan/Cmdstanr Models.
* Talts, Sean, Michael Betancourt, Daniel Simpson, Aki Vehtari, and Andrew Gelman. 2020. “Validating Bayesian Inference Algorithms with Simulation-Based Calibration.” https://arxiv.org/abs/1804.06788.