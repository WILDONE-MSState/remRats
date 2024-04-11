# remRats
Set of utility functions for estimate population size and proportion of population being removed using occupancy models with data augmentation.

remRats allows you to estimate population size:

- By maximum likelihood using a multinomial approach (faster, at penalty of not using individual covariates)

- Using a bayesian framewrok by implementing a zero-inflated occupancy model with data augmentation. With the benefit of adding flexibility to add individual covariates.

