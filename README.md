
# {remRats} <img src='man/figures/logo.png' align="right" height="250" />

### Efficient estimation of (small mammal) population size under removal experiment designs

![remRats lifecycle](man/figures/lifecycle-experimental.svg) {remRats}
offers a simple platform to efficiently estimate the size of a
population subject to removal experiments (both temporal and permanent
removal). Removal experiments are common in ecological research. They
are used to estimate population size, comunity composition, to manage
both endangered or invasive populations, or for disease surveillance.
However, data from removal experiments is oftern presented in a diverse
set of ways, making them hardly comparably between studies. Futhermore,
it is often de case where it is only reported the actual number of
individuals sampled (i.e. for disease surveillance) that hamper its own
usefulness as fromn these ways of representing data there is no way to
assess its representability with respecto to the population being
sampled.

{remRats} fills some of the mentioned limitations by making analysis of
removal data as simple and cosistent as possible. We also make it aiming
to be used ‘on-the-ground’, that is, during the actual removal
experiment, after each session, to evaluate the performance of the
removal experiment, checking agaionst theactual goals (precission,
proportion of the actual opopulation already being sampled/collected).

## Installation:

You should have an R version \>=4.0.0 installed (the latest the better).
You can check in which R version are you working by typing:

``` r
R.Version()
```

Until it lands on CRAN repository, you will need package
[remotes](https://cran.r-project.org/package=remotes) to install
{remRats} from its GitHub repository

``` r
## install.packages('remotes')

remotes::install_github('WILDONE-MSState/remRats')
```

In adition, you need to have locally installed in your computer programs
[MARK](http://www.phidot.org/software/mark/) and [Just Another Gibbs
Sampler (JAGS)](https://mcmc-jags.sourceforge.io/) to operate with some
of the functiones ofered in {remRats}.

## Problems and bugs

If you have encountered a bug or have found an consistent behaviour when
using {remRats}, go to
[issues](https://github.com/WILDONE-MSState/remRats/issues) and check
first if there is already an issue with the same problem either open or
closed. Otherwise, you are welcome to create a new issue there. Just
make sure to include all the relevant information, including a
reproducible example. If you don’t know what is a minimal reproducible
example or how to make it, I recommend the use of package
[reprex](https://cran.r-project.org/package=reprex).

Estimation of population parameters of fish (electrofishing),
erradication of invasive species, translocation of populations of
endangered species, community ecology of small organism (insects and
smaller), population monitoring of small mammals, and disease
surveillance.

## Methods

The simplest method available is the Moran-Zippin for two sessions
(Moran 1951, Zippin 1956, 1958) which we do not recomend to use. While
ideally it should perform, it requires of large capture probability of
individuals, and, in the case of single catch traps, a reasonable
ammount of traps in relation to the (unknown) size of the population.

An alternative to it is to fit a model of extreme behavioural response
(complete trap-shyness) as suggested in Otis et al. (1978). The best for
non spatially-explicit would be population size estimation using data
augmentation and fiting a zero-inflated model of occupancy..

### References:

  - Moran, P. A. P. (1951). A mathematical theory of animal trapping.
    *Biometrica*, 38, 307-311.
  - Otis, D. L., Burnham, K. P., White, G. C., and Anderson, D. R.
    (1978). Statistical inference from capture data on closed animal
    populations. *Wildlife monographs*, 62, 1-135.
  - Zippin, C. (1956). An evaluation of the removal method of estimating
    animal populations. *Biometrics*, 12, 163-189.
  - Zippin, C. (1958). The removal method of population estimation.
    *Journal of Wildlife Management*, 22, 82-90.
