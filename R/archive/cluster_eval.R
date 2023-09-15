
library(odin)
library(dde)


# Load settings:
s <- yaml::read_yaml("settings.yml")

# Clone this repository https://gitlab.com/covidpaths/c19model
# into the parent folder of the c19-paths repository
# this will be necessary until we release the package externally
if(s$c19model_load_all) {
  devtools::load_all("../R6Sim")
} else {
  library(R6Sim)
}

# Load stochastic metapopulation model package:
devtools::load_all("./cpp/stochastic_metapopulation/")


# Source all functions
invisible(sapply(setdiff(list.files(path = s$lib_path, pattern = "*.R",full.names = T),
                         s$lib_file),
                 source,
                 echo = F)
)
