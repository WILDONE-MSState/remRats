
# {remRats} \><img src='man/figures/logo.png' align="right" height="250" />\</

### Efficient estimation of (small mammal) population size under removal experiment designs

{remRats} offers a simple platform to efficiently estimate the size of a
population subject to removal experiments (both temporal and permanent
removal). Removal experiments are common in ecological research. They
are used to estimate population size, comunity composition, to manage
both endangered or invasive populations, or for disease surveillance.

{remRats} fill the gaps by making analysis of removal data as simple and
cosistent as possible, to the point of being capable of, if using
‘on-the-ground’, to evaluate the performance of the removal
experiment, checking agaionst the goals (precission, proportion of the
actual opoipulation being sampled)

## Installation:

You should have an R version \>=4.0.0 installed (the latest the better).
You can check in which R version you are by typing

``` r
R.Version()
```

And then, you need package remotes to install R packages from GitHub
repositories

``` r
## install.packages('remotes')

remotes::install_github('WILDONE-MSState/remRats')
```

In adition, you need to have locally installed in your computer programs
[MARK](http://www.phidot.org/software/mark/) and [jags (Just Another
Gibbs Sampler)](https://mcmc-jags.sourceforge.io/) to operate with some
of the functiones ofered in {remRats}

## Introduction

However, as the fundamentals are different, there is a lack of unifying
approaches both to estimate the parameters of interest, or on how to
report the results. They also operates at diferent timeframes, shere the
impact of removals may be relevant or not, also due to population
processes (for example, how long an area with rodents will need to be
recolonized, and how it is impacted by the density is unknown).

Estimation of population parameters of fish (electrofishing),
erradication of invasive species, translocation of populations of
endangered species, community ecology of small organism (insects and
smaller), population monitoring of small mammals, and disease
surveillance.

remRats introducS a suite of different functions aiming to close some of
these gaps, with special emphasis on disease surveillance in natural
populations of animals as our motivation.

## Methods

The simplest method available is the Moran-Zippin for two sessions
(Moran 1951, Zippin 1956, 1958) which we do not recomend to use. While
ideally it should perform, it requires of large capture probability of
individuals., and, in the case of single catch traps, a reaosnable
ammount of traps in relation to the (unknown) size of the population.

An alternative to it is to fit a model of extreme behavioural response
(complete trap-shyness) as suggested in Otis et al. (1978). The best for
non spatially-explicit would be population size estimation using data
augmentation and fiting a zero-inflated model of occupancy..

Wheter the objective of the ecological research require temporal vs
permanent removal () we advocate for a analysis allow to use metrics
that are easily comparable between different researches.

In fact permanent removal of indivduals in a regular monitoring/sampling
chema is by definition a strong disturbance to it, limiting then the
realibility of the outcome from an ecological point fo view, can be
viewd as a chronic disturbance to the population., like an adding
“Predation” that may act as a selection force against specific
behaviors

The body of lierature came from a long way, including specific
monographies like the one from White, G. et al (1982 Capture-Recapture
and rmeoval methods for sampling closed populatioins.)

While models have been discussed in the past (Otis et al 1987), there
has been little to no attention to this family of models until recently.
The need to remove endangered species in the uk, but also the need to
offer robust inference. Including removal of alien or invasive species

The lack of robust estimates prevent for undertand duisease prevalencve,
disease diynamics

Eficient disease surveillance requires specific ammounts of samples,
depending on the size of the population of interest. To acomplish it,
field teams net to know the population size while they are sampling it.
So, we offer here a set of tools in an simplified R package and shiny
app that can be run every day after sample collection

Simplest methods like Moran-Zippin for two sessions (Moran 1951, Zippin
1956, 1958) which we do not recomend to use. An alternative to it is to
fit a model of extreme behavioural response (complete trap-shyness) as
suggested in Otis et al. (1978). The best for non spatially-explicit
would be population size estimation using data augmentation and fiting a
zero-inflated model of occupancy..

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
