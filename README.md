# Genomic Surveillance Value of Information Code Repository

## About this repository

This repository contains code to explore the Value of Information of Genomic sequencing technologies using infectious disease models.

## Dependencies:

Clone the [R6Sim](https://github.com/randcorporation/R6Sim/) R package and install it first. Then, open the `R/library.R` file and install your missing packages Rstudio install your missing packages.

If you prefer to use [`renv`](https://rstudio.github.io/renv/articles/renv.html) to install your packages, an `renv.lock` file is provided to ensure we have a registry of the dependencies used. To use `renv`, see the `.Rprofile`, uncomment the line indicated and re-start your R session.

## Reproducing the analysis:

Run the `./R/scripts/00_run_analysis.R` file to reproduce our analysis.

## Organization

- `./cpp`: This folder will be created by `odin` and will house your compiled c++ models. You do not need to edit the files within that folder.
- `./data`: Contains data inputs for all models.
- `./R`: Contains R `scripts`, `functions`, `scripts` and `odin_models`. The `dev` folder can contain work in progress work. The `odin_model` folder contains [odin](https://mrc-ide.github.io/odin/index.html) models; those are not R scripts and are saved with the `.R` extension only for syntax highlighting.
- `settings.yml` contains settings that would otherwise be hard-coded. Settings are saved in the `s` list, which is a global variable.

## License
TBD.
