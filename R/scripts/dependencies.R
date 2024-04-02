

# Use code below to discover dependencies and document licenses

# install.packages("renv")
# install.packages("packrat")

dependencies <- renv::dependencies(path = "./R")

packages <- unique(dependencies$Package)

# The following is the packages list within those dependencies:

# if you need to write down the dependencies somewhere, you can
# dput(packages)

# The code
cran_packages <- c(
  "remotes", "Hmisc", "R6", "tidyr", "odin", "dplyr", "ggplot2", "dde",
  "gt", "lemon", "lubridate", "parallel", "purrr", "readxl", "scales", "writexl", "yaml"
)

rand_packages_github <- c("R6Sim", "randplot")


# Use the function below to discover licenses:

ListLicenses = function(packages) {

  licenses <- character(length = 0)

  for (package_name in sort(packages)) {

    first_order_dependencies <- packrat:::getPackageDependencies(packages, lib.loc = .libPaths()[1], fields = c("Depends", "Imports"))

    nth_order_dependencies = packrat:::recursivePackageDependencies(package_name, lib.loc = .libPaths()[1], ignores = c(), fields = c("Depends", "Imports"))

    nth_order_dependencies <- nth_order_dependencies[!nth_order_dependencies %in% first_order_dependencies]

    # Originally used, but only works for CRAN packages:

    #dependencies = tools::package_dependencies(package_name, which = c("Depends", "Imports"), recursive = TRUE)

    license_info = packageDescription(package_name, fields="License")

    licenses <- c(licenses, license_info)

    print(glue::glue("{package_name}: {license_info}"))

    print("1st Order Dependencies:")

    for (dependency in unlist(first_order_dependencies)) {

      dependency_license_info = packageDescription(dependency, fields="License")

      licenses <- c(licenses, dependency_license_info)

      print(glue::glue("\t{dependency}: {dependency_license_info}"))

    }

    print("2-Nth Order Dependencies:")

    for (dependency in unlist(nth_order_dependencies)) {

      dependency_license_info = packageDescription(dependency, fields="License")

      licenses <- c(licenses, dependency_license_info)

      print(glue::glue("\t{dependency}: {dependency_license_info}"))

    }

  }

  unique_licenses <- paste(sort(unique(licenses)), collapse = ", ")

  print(glue::glue("Unique Licenses: {unique_licenses}"))

}

# Enter all the libraries explicitly referenced in your code, here.

sink("licenses.txt")

ListLicenses( c(cran_packages, rand_packages_github))

sink()
