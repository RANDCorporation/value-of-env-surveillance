# Genomic Surveillance Value of Information Code Repository

## About this repository

This repository contains code to explore the Value of Information of Genomic sequencing technologies using infectious disease models.

## Dependencies:

# Install `R6Sim`

[R6Sim](https://github.com/randcorporation/R6Sim/) is an R6-based R package that contains base classes and infrastructure for developing and running simulation models using the encaspsulated object-oriented approach offered by `R6`.

**Interim instructions while R6SIm is not open source**
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

# Install other R dependencies

Open the `R/library.R` file with RStudio and install the missing packages Rstudio identifies. All dependencies should be listed in that file.

If you prefer to use [`renv`](https://rstudio.github.io/renv/articles/renv.html) to install your packages, an `renv.lock` file is provided to ensure we have a registry of the dependencies used. To use `renv`, see the `.Rprofile`, uncomment the line indicated and re-start your R session.

## Reproducing the analysis:

Run the `./R/scripts/01_run_model_experiments.R` file to run a set of model experiments. You may also use that file as a starting point for defining and running your own experimental designs.

## Organization

- `./R`: Contains R `scripts`, `functions`, `scripts` and `odin_models`. The `dev` folder can contain work in progress scripts. The `odin_model` folder contains [odin](https://mrc-ide.github.io/odin/index.html) models; those are not R scripts and are saved with the `.R` extension for syntax highlighting.
- `settings.yml` contains settings that would otherwise be hard-coded. Settings are saved in the `s` list, which is a global variable.
- `./cpp`: This folder will be created by `odin` and will house your compiled c++ models. You do not need to edit the files within that folder.
- `./data`: Contains data inputs for all models.

## License
TBD.
