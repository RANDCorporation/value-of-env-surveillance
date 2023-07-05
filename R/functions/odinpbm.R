


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

    #' @field oi odin model inputs
    oi = NULL,

    #' @field res day-level results
    res = NULL,

    #' @field res jurisdiction level results
    res_long = NULL,

    #' @field res rep-level results
    summary = NULL,

    #' @description
    #' Create a new `odinpbm` object.
    #' @param odin_file odin model file under R/odin_models. Should have an .R extension.
    #' @param inputs_file spreadsheet with model inputs
    #' @param ... additional inputs to te odin model.
    #' @return s new `odinpbm` object.
    initialize = function(odin_file, inputs_file, ...) {

      super$initialize(name = odin_file)

      model_path <- paste0("./R/odin_models/", odin_file)

      odin_workdir <- paste0("./cpp/", substr(odin_file,start = 1,
                                              stop = nchar(odin_file)-2), "/")

      if(!missing(inputs_file)) {
        # get inputs from the spreadsheet
        self$get_inputs(inputs_file)
      }

      # collect odin inputs from the inputs file
      inputs <- self$collect_default_inputs()

      # collect settings inputs

      inputs_settings <- self$collect_inputs(tab_name="settings", key_name="setting", val_name="value")


      # add custom inputs and override original inputs by name
      self$oi <- modifyList(inputs, list(...))

      # create the odin constructor
      # We don't use the constructor after this step, so I don't save it
      odin_constructor <- odin::odin(model_path, workdir = odin_workdir)

      # initialize the model with default parameters, and additional parameters
      # we may want to provide additional wrappers around other odin functions
      # in the future
      self$o <- do.call(odin_constructor$new, self$oi)

    },

    # runs the model for a set of replications
    run = function(step, y = NULL, use_names = TRUE, reps = 1){

      # result comes as an array of matrices
      res <- replicate(n = reps, expr = self$o$run(step, y = y, use_names = use_names))

      # convert to a nice list
      res_list <- lapply(seq(dim(res)[3]), function(x) res[ , , x] %>% as.data.frame(.) %>% mutate(rep = x))

      # return as a data.frame
      self$res <- do.call(rbind, res_list)

    },

    collect_default_inputs = function() {

      # by default, we get all parameters from the inputs spreadsheet:
      # for parameters, I can put all parameters into a named list:
      inputs <- list()

      if(!is.null(self$inputs$parameters)) {
        for(i in 1:nrow(self$inputs$parameters)) {
          inputs[self$inputs$parameters$parameter[i]] = self$inputs$parameters$baseline[i]
        }
      }

      # inputs should be a named list, containing the user() inputs the model needs
      return(inputs)
    },


    collect_inputs = function(tab_name, key_name, val_name) {
      # by default, we get all parameters from the inputs spreadsheet:
      # for parameters, I can put all parameters into a named list:
      inputs <- list()

      outer <- self$inputs[[tab_name]]
      if(!is.null(outer)) {
        keys = outer[[key_name]]
        vals = outer[[val_name]]
        for(ix in 1:nrow(outer)) {
          inputs[keys[ix]] = vals[ix]
        }
      }
      # inputs should be a named list, containing the user() inputs the model needs
      return(inputs)
    }
  )
)

