
# remRats <img src='man/figures/logo.png' align="right" height="250" />

### Efficient estimation of (small mammal) population size under removal experiment designs for disease surveillance

![remRats lifecycle](man/figures/lifecycle-experimental.svg) ![remRats
CRAN badge](man/figures/badge-cran.svg)

## Author/s

  - Dr. Fernando Arce, Mississippi State University. (Author, Developer,
    Mantainer)
  - Dr. Manuel Ruiz-Aravena, Mississippi State University. (Contributor)
  - Dr. Michael W. Sandel, Mississippi State University. (Contributor)

## Funding

Funding was provided by the U.S. Department of Agriculture, Agricultural Research Service, grant number: 58-6064-3-017

Project title: Developing Detection and Modeling Tools for the
Geospatial and Environmental Epidemiology of Animal Disease

Additional support was provided by USDA - Forest Service International Programs, Agreement 22-DG-11132762-347

## Overview

remRats offers a simple but effective platform to estimate the size of a
population subject to removal experiments (either temporal or permanent
removal).

Removal experiments are common employed in ecological/conservation
research. They are used to estimate population size, comunity
diversity/composition, to manage both endangered or invasive
populations, or for disease surveillance. However, data from removal
experiments is oftern presented in a diverse set of ways, making them
hardly comparably between studies. Futhermore, it is often de case where
it is only reported the actual number of individuals sampled (i.e. for
disease surveillance) that hamper its own usefulness as with these data
delivery there is no way to assess the representability of the samples
with respect to the population where they have been collected.

remRats fills some of the mentioned limitations by making analysis of
removal data as simple and consistent as possible. We also conceived
remRats as a tool to be used ‘on-the-ground’, that is, during the
actual removal experiment. During the process of collecting
individuals, and after each session, remRats can be used to evaluate
the performance of the removal experiment, assesing the uncertainty of
estimates, calculating the proportion of the population already being
sampled (of critical importance for disease surveillance), and
projecting the observed trends to evaluate the extra effort still
needed given the requirements of the project or the cost-benefit of
keep running the sampling scheme).

remRats all the several steps mentioned above into a single function
`remRats` that generates some of the outputs and allow to display them
in a Shiny app where the user is allowed to, on the basis on the
information already collected, define a projection time window
(i.e. extra sampling sessions) to see the expected outcome of these
efforts ansd evaluate the feasibility or achiveing given the current
field conditions.

## Installation:

You need to have an R version of 3.5.0 or newer. You can check
which R version you have installed in your machine by typing:

``` r
R.Version()
```

You will need package
[remotes](https://cran.r-project.org/package=remotes) to install remRats
from this repository until it lands on CRAN.

``` r
## install.packages('remotes')

remotes::install_github('WILDONE-MSState/remRats')
```

In adition, you need to have locally installed in your computer programs
[MARK](http://www.phidot.org/software/mark/) to operate with some
of the functiones ofered in {remRats}.

Simplest usage

```r
## minimal package example
library(remRats)
library(ggplot2)

rem <- remSim(N=500,p=.35,j=5)
popEst <- remRats(rem)
shinyView(popEst)
```

## Problems and bugs

If you have encountered a bug or have found an inconsistent/unexpected
behaviour while using remRats, go to
[issues](https://github.com/WILDONE-MSState/remRats/issues) (top-left of
this web) and check first if there is already an issue with the same
problem either open or closed. Otherwise, you are welcome to create a
new issue there. Just make sure to include all the relevant information,
including a reproducible example. If you don’t know what is a minimal
reproducible example or how to make it, I recommend the use of package
[reprex](https://cran.r-project.org/package=reprex) to create it.

## Methods

The simplest method available is the Moran-Zippin estimator. Designed
for just two removal events (Moran 1951, Zippin 1956, 1958) we do not
recomend to use but it is presented here as sometimes there are only two
trapping events per session. While ideally it should just perform, it
still requires of large capture probability of individuals, and, in the
case of single catch traps, a reasonable large ammount of traps in
relation to the (unknown) size of the population. Also, it requires the
number of individuals removed in the second session being smaller than
in the first to generate estimate.

An alternative to it is to fit a model of extreme behavioural response
(complete trap-shyness) as suggested in Otis et al. (1978). This model,
often dennoted as M\_tbh consider that individuals once are trapped,
cannot be re-trapped, thus generating an observed capture history
equivalent to those generated by genuine removal sampling.

## References:

  - Moran, P. A. P. (1951). A mathematical theory of animal trapping.
    *Biometrica*, 38, 307-311.
  - Otis, D. L., Burnham, K. P., White, G. C., and Anderson, D. R.
    (1978). Statistical inference from capture data on closed animal
    populations. *Wildlife monographs*, 62, 1-135.
  - Royle, A. J. (2009). Analysis of Capture-Recapture Models with
    Individual Covariates Using Data Augmentation. *Biometrics*, 65,
    267-274.
  - Zippin, C. (1956). An evaluation of the removal method of estimating
    animal populations. *Biometrics*, 12, 163-189.
  - Zippin, C. (1958). The removal method of population estimation.
    *Journal of Wildlife Management*, 22, 82-90.
