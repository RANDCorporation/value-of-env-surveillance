# Genomic Surveillance Value of Information Code Repository

## About this repository

This repository contains code to reproduce *Nascimento de Lima et al. (2024). The value of environmental surveillance for pandemic response*. The `run_analysis.R` script produces the figures and tables in the paper. All our results can be reproduced with free software.

## Dependencies

Use the `install_dependencies.R` script to install your dependencies. Alternatively, you can use `renv` to use the same package versions used in our paper.

### Installing dependencies with `renv`

The `renv.lock` file contains the packages and versions used in this project. If you have an up-to-date R Studio version, you should be able to do the following to install the same package versions used in this project:

```r
# install renv in case you do not have it:
# install.packages("renv")
renv::restore()
```

should do most of the work for you once you open the `genomic-surveillance-voi.Rproj` file. If you are unfamiliar with `renv, read [this](https://rstudio.github.io/renv/articles/renv.html). If that doesn't work, read on. 

### Installing dependencies without `renv`

#### Install `R6Sim`

[R6Sim](https://github.com/randcorporation/R6Sim/) is an R6-based R package that contains base classes and infrastructure for developing and running simulation models using the encaspsulated object-oriented approach offered by `R6`. The model we use is an `R6Sim` model and, we use the `R6Experiment` class to organize and run our experiments in parallel.

**Interim instructions while R6Sim is not open source:**

First, make sure you have access to the [R6Sim](https://github.com/randcorporation/R6Sim/) code repository. If not, ask Pedro Nascimento de Lima to give access to your github account. Then, generate a [github personal access token (PAT)](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) with access to your account. Once you do those steps, you can run on the R console:

```r
Sys.setenv(GITHUB_PAT = "your_pat")
```

Install [R6Sim](https://github.com/randcorporation/R6Sim/) from github:

```r
# install the remotes package you do not have it yet
# install.packages("remotes")

remotes::install_github("randcorporation/r6sim")
```

#### Install other R dependencies

Open the `R/library.R` file with RStudio and install the missing packages Rstudio identifies. All dependencies should be listed in that file.


## Where to start

Use the `run_analysis.R` file to produce the tables and figures in our analysis.

## Organization

- `./R`: Contains R `scripts`, `functions`, `scripts` and `odin_models`. The `odin_model` folder contains the [odin](https://mrc-ide.github.io/odin/index.html) model files; those are not R scripts and are saved with the `.R` extension for syntax highlighting.
- `settings.yml` contains settings that would otherwise be hard-coded. Settings are saved in the `s` list, which is a global variable.
- `./cpp`: This folder will be created by `odin` and will house your compiled c++ models. You do not need to edit the files within that folder.
- `./data`: Contains data inputs.
- `./archive`: Contains archive data.

## License
TBD.
