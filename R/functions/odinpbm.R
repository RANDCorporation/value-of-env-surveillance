


#------------------------------------------------------------------------------#
# Code repository for Analysis of Genomic Sequencing information
#
# Author: Pedro Nascimento de Lima
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# odinpbm model class
#
# An R6 class to represents odin models
#------------------------------------------------------------------------------#

odinpbm <- R6::R6Class(
  classname = "odinpbm",
  inherit = c19model,
  public = list(

    #' @field o odin model object
    o = NULL,

    #' @description
    #' Create a new `odinpbm` object.
    #' @param odin_file odin model file under R/odin_models. Should have an .R extension.
    #' @return s new `odinpbm` object.
    initialize = function(odin_file, ...) {

      super$initialize(name = odin_file)

      model_path <- paste0("./R/odin_models/", odin_file)

      odin_workdir <- paste0("./cpp/", substr(odin_file,start = 1,
                                              stop = nchar(odin_file)-2), "/")

      # create the odin constructor
      # We may not need to save this, so I don't.
      odin_constructor <- odin::odin(model_path, workdir = odin_workdir)

      # initialize the model with default parameters, and additional parameters
      # we may want to provide additional wrappers around odin functions
      # in the future
      self$o <- odin_constructor$new(...)

    }

  )
)


