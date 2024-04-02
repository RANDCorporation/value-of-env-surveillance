# Genomic Surveillance Value of Information Code Repository

## About this repository

This repository contains code to reproduce *Nascimento de Lima et al. (forthcoming). The value of environmental surveillance for pandemic response*. The `run_analysess.R` script runs the model and produces the figures and tables in the paper. All our results can be reproduced with R and only use free software.

## Dependencies

First, install the [R6Sim](https://github.com/randcorporation/R6Sim/) R package. Then, install the remaining dependencies by running `bash install_dependencies.sh` or step through the `install_dependencies.R` script using RStudio.

### Using `renv`
You can use `renv` to use the same package versions used in our paper by setting the `use_renv` variable in the `settings.yml` file. If you do use `renv`, activate it by uncommenting the `source("renv/activate.R")` line in the `.Rprofile` script.

## Where to start

*After* installing R dependencies, use the `run_all_analyses.R` file or run `bash run_all_analyses.sh` to run all scripts. Once they do run, open the `settings.yml` file and increase the `n_reps` setting to 1,000 to use the actual settings used to produce our results.

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
