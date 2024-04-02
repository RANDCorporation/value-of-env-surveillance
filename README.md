# Genomic Surveillance Value of Information Code Repository

## About this repository

This repository contains code to reproduce *Nascimento de Lima et al. (2024). The value of environmental surveillance for pandemic response*. The `run_analysis.R` script produces the figures and tables in the paper. All our results can be reproduced with free software.

## Dependencies

Use the `install_dependencies.sh` script (or step through the `install_dependencies.R`) to install your dependencies using your local library. 

# Using `renv`
You can use `renv` to use the same package versions used in our paper by setting the `use_renv` variable in the `settings.yml` file and doing the same. If you do use `renv`, activate the renv by uncommenting the `source("renv/activate.R")` line in the `.Rprofile` script.


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

## Where to start

Use the `run_all_analyses.R` file or the `run_all_analyses.sh` to run all analytical scripts. Once they do run and after you work through any other dependency issues, open the `settings.yml` file and increase the `n_reps` setting to 1,000 to use the actual settings used to produce our results.

## Organization

- `./R`: Contains R `scripts`, `functions`, `scripts` and `odin_models`. The `odin_model` folder contains the [odin](https://mrc-ide.github.io/odin/index.html) model files; those are not R scripts and are saved with the `.R` extension for syntax highlighting.
- `settings.yml` contains settings that would otherwise be hard-coded. Settings are saved in the `s` list, which is a global variable.
- `./cpp`: This folder will be created by `odin` and will house your compiled c++ models. You do not need to edit the files within that folder.
- `./data`: Contains data inputs.
- `./archive`: Contains archive data.

## Contact

Reach out to [Pedro Nascimento de Lima](https://www.rand.org/about/people/l/lima_pedro_nascimento_de.html) for
questions related to this repository.

## License

Copyright (C) 2024 by The [RAND Corporation](https://www.rand.org). This
repository is released as open-source software under a GPL-3.0 license.
See the LICENSE file.
