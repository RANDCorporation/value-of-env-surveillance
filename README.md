# Genomic Surveillance Value of Information Code Repository

## About this repository

This repository contains code to explore the Value of Information of Genomic sequencing technologies using infectious disease models.

## Housekeeping
This repository will likely be released to github upon submission of the paper, so please commit only what you would be comfortable showing the world. Do not commit html or large files.

## Dependencies:

Clone the [c19model](https://code.rand.org/vaccination-research/c19model) R package and install it first. Then, open the `R/library.R` file and let Rstudio install your missing packages.

An `renv.lock` file is provided as a courtesy and to ensure we have a registry of the dependencies used. If you prefer to use `renv`, see the `.Rprofile` file and re-start your R session.

## Next Tasks

*Manuscript*

**Introduction**

- [ ] One paragraph reviewing prior modeling work on genomic surveillance.

**Methods**

- [ ] Model Section [Pedro]
- [ ] Data section [Jing]
- [ ] Code table of parameters [Jing]

**Results**

**Discussion**

**Supplementary Appendix**

*Implementation*

- [x] Implement c19model R6 class for the odin model [Pedro]
- [x] Implement first version of a multi-jurisdiction stochastic model [Pedro]
- [x] Implement NPIs on the multi-jurisdiction model. [Pedro]
- [x] Test lagged NPI control suggested by Henry [Pedro]
- [x] Have the model use non-scalar inputs from inputs object. [Pedro]
- [ ] Translate all parameters from Laura into model inputs and use those as baseline. [Jing]
- [ ] Code post-processing functions to compute net monetary benefit of surveillance considering:
- [ ] Epi costs of intervention (as a function of IFR * I, and aggregate cost of infection)
- [ ] NPIs and a cost function.
- [ ] Costs of surveillance.
- [ ] integrate parameter inputs into the model. [Jing]
- [x] Fix stochastic metapopulation Model NaN's.
- [ ] Create function to compute derived inputs (ie, R0 = beta/gamma). We will scale the beta matrix such that 
 - R0 = tau.eff * k * beta_pop
   beta_pop = k * p' beta_matrix p. 
   Tau.eff is the weighted sum of the infectious durations under this simple model, or can be obtained with the next-matrix generation for a more involved model with branches.
- [ ] Integrate Derek’s data on travel into the model.
- [ ] Code experimental design function.
- [ ] Run boundary case experiments to verify model and explore model behavior to refine the paper experimental design.
- [ ] Implement travel in the stochastic model. [Sarah, WIP]

*Model Inputs*

- [ ] Create table of model parameters following Laura's document. Find References whenever appropriate. This table should be read into the model appendix file. [Jing]
- [ ] Code mobility matrix creation using Derek's mobility information. [Jing]

## License
TBD.
