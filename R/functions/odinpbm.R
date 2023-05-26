


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
    #' @param inputs_file spreadsheet with model inputs
    #' @return s new `odinpbm` object.
    initialize = function(odin_file, inputs_file, ...) {

      super$initialize(name = odin_file)

      model_path <- paste0("./R/odin_models/", odin_file)

      odin_workdir <- paste0("./cpp/", substr(odin_file,start = 1,
                                              stop = nchar(odin_file)-2), "/")

      self$get_inputs(inputs_file)

      inputs <- self$collect_default_inputs()

      # create the odin constructor
      # We may not need to save this, so I don't.
      odin_constructor <- odin::odin(model_path, workdir = odin_workdir)


      # initialize the model with default parameters, and additional parameters
      # we may want to provide additional wrappers around odin functions
      # in the future
      self$o <- do.call(odin_constructor$new, inputs)

    },


    # document
    run = function(step, y = NULL, use_names = TRUE, reps = 1){

      # result comes as an array of matrices
      res <- replicate(n = reps, expr = self$o$run(step, y = y, use_names = use_names))

      # convert to a nice list
      res_list <- lapply(seq(dim(res)[3]), function(x) res[ , , x] %>% as.data.frame(.) %>% mutate(rep = x))

      # return as a data.frame
      do.call(rbind, res_list)

    },

    collect_default_inputs = function() {

      # set model inputs from spreadsheet

      # for parameters, I can put all parameters into a named list:
      p_list <- list()
      for(i in 1:nrow(self$inputs$parameters)) {
        p_list[self$inputs$parameters$parameter[i]] = self$inputs$parameters$baseline[i]
      }

      # beta matrix
      # read from spreadsheet
      p_list$beta <- structure(c(1, 0, 0, 1), dim = c(2L, 2L))

      p_list$C <- structure(c(-0.01, 0.01, 0.01, -0.01), dim = c(2L, 2L))

      p_list$nr_patches <- 2

      p_list$mp <- c(1, 1, 0.5, 1, 1)

      # p_list should be a named list of all the user() inputs the model needs
      return(p_list)

    }

  )
)


