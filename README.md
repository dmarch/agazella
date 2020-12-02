# agazella
Satellite tracking of Antarctic fur seal (Arctocephalus gazella)


# Workflow


### Download CMEMS data

Find the scripts in `cmems/`.


### Process animal trajectories


* scr/01_process_location_data.R: This script processes location data. Includes standardization and filtering.

* scr/02_animated_maps.R: Generated animated gifs to explore the animal tracks



## Simulations

Hindell et al 2020: Locations at the animal’s home colony, and locations at known terrestrial resting sites, were fixed at the corresponding time and date in the simulated tracks, accurately simulating central place foraging behaviour.

We chose the number of simulations by plotting the relationship between the number of simulated tracks and the environmental space (rather than the geographic space) encompassed by the set of simulated tracks Tracking of marine predators to protect Southern Ocean ecosystems 4 in each case. This environmental space was characterised as the mean, standard deviation and variance of each of the 19 environmental covariates (described below) (e.g., Supplementary Figure S1). In a few cases the environmental space had not stabilized by 50 simulations but using more than 50 simulations in
the analyses that followed was often computationally infeasible