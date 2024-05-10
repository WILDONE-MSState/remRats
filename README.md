# remRats: Efficient estimation of small mammal population size under removal experiment designs

Experimental installation:

remotes::install_github('WILDONE-MSState/remRats')

## Efficient estimation of population size from data collected under removal experiments

### Intro

Removal experiments are ubicuos in the area of ecological research.

The lack of an unified reportiong and analysuis jeopardice posterior inference, making also the integration of data from sources unfeasible.

Wheter the objective of the ecological research require  temporal vs permanent removal () we advocate for a analysis allow to use metrics that are easily comparable between different researches.

In fact permanent removal of indivduals in  a regular monitoring/sampling chema is by definition a strong disturbance to it, limiting then the realibility of the outcome from an ecological point fo view, can be viewd as a chronic disturbance to the population., like an adding "Predation" that may act as a selection force against specific behaviors

The body of lierature came from a long way,  including specific monographies like the one from White, G. et al (1982 Capture-Recapture and rmeoval methods for sampling closed populatioins.) 

The removal can be permanent (individuals never return to the environment) or temporal (individuals are released back to the environment.)

While models have been discussed in the past (Otis et al 1987), there has been little to no attention to this family of models until recently. The need to remove endangered species in the uk, but also the need to offer robust inference. Including removal of alien or invasive species

While we don't advocate the use of removal experiments as a broad method to monitor natural populations, there are circumstances were they may be used, like for disease surveillance. Disease surveillance, conservation management.

The lack of robust estimates prevent for undertand duisease prevalencve, disease diynamics

Eficient disease surveillance requires specific ammounts of samples, depending on the size of the population of interest. To acomplish it, field teams net to know the population size while they are sampling it. So, we offer here a set of tools in an simplified R package and shiny app that can be run every day after sample collection

Simplest methods like Moran-Zippin for two sessions (Moran 1951, Zippin 1956, 1958) which we do not recomend to use. An alternative to it is to fit a model of extreme behavioural response (complete trap-shyness) as suggested in Otis et al. (1978). The best for non spatially-explicit would be population size estimation using data augmentation and fiting a zero-inflated model of occupancy..


Population here is vaguely defined as the ammount of individuals opf a species of interest that have any probability to came across a trap.

### References:
- Moran, P. A. P. (1951). A mathematical theory of animal trapping. *Biometrica*, 38, 307-311.
- Otis, D. L., Burnham, K. P., White, G. C., and Anderson, D. R. (1978). Statistical inference from capture data on closed animal populations. *Wildlife monographs*, 62, 1-135.
- Zippin, C. (1956). An evaluation of the removal method of estimating animal populations. *Biometrics*, 12, 163-189.
- Zippin, C. (1958). The removal method of population estimation. *Journal of Wildlife Management*, 22, 82-90.
