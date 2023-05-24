# Genomic Surveillance Value of Information Code Repository

## About this repository

This repository contains code to explore the Value of Information of Genomic sequencing technologies using infectious disease models.

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
- [ ] Implement NPIs on the multi-jurisdiction model. [Pedro]
- [ ] Test lagged NPI control suggested by Henry [Pedro]

Possible tasks:
- [ ] Implement testing and wastewater surveillance explicitly (it is unclear if that will be useful. It might be useful we we impose a testing constraint, but it may have a negligible effect during the onset of a surge).

*Model Inputs*

- [ ] Create table of model parameters following Laura's document. Find References whenever appropriate. This table should be read into the model appendix file. [Jing]
- [ ] Code mobility matrix creation using Derek's mobility information. [Jing]

## Code Dependencies

Install the R package [c19model](https://code.rand.org/vaccination-research/c19model) before using this repository.

## License
TBD.
