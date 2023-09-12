


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
  inherit = R6Sim,
  public = list(

    #' @field o odin model object
    o = NULL,

    #' @field oi odin model inputs
    oi = NULL,

    #' @field odin_parms vector of odin user parameter values
    odin_parms = NULL,

    #' @field res day-level results
    res = NULL,

    #' @field res jurisdiction level results
    res_long = NULL,

    #' @field res rep-level results
    #summary = NULL,
    summary_jurisdiction = NULL,
    summary_all = NULL,

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


      # Build odin model first:
      odin_constructor <- odin::odin(model_path, workdir = odin_workdir, debug_enable = F)

      # initialize the model with default parameters, and additional parameters
      # we may want to provide additional wrappers around other odin functions
      # in the future
      # Here, we construct the model passing parameters to it:
      # self$o <- do.call(odin_constructor$new, self$oi)

      self$odin_parms <- odin_constructor$private_fields$user

      # Then, get spreadsheet inputs:
      self$get_inputs(inputs_file)

      # set default parameters as inputs
      self$set_default_params()

      # In the future, we can allow arbitrary
      # Add user-specified as inputs, if we wanted to.

      # add custom inputs and override original inputs by name
      # Here, we can use set_input instead of doing this direct assignment.
      #self$oi <- modifyList(inputs, list(...))

      # Pre-process inputs (which will also assign any new parameters to the odin model)
      self$pre_process_inputs()

      # Create list of odin inputs

      # Then, build odin model with pre-processed inputs:

      odin_inputs <- self$inputs[self$odin_parms]

      odin_inputs_missing <- self$odin_parms[!self$odin_parms %in% names(self$inputs)]

      if(!length(odin_inputs_missing)==0) {
        stop(paste0("odin inputs missing: ", paste0(odin_inputs_missing, collapse = ", ")))
      }

      # Alternatively, we can simply create a new odin model without custom inputs and set them later
      # closer to run time. This is safer.
      self$o <- do.call(odin_constructor$new, odin_inputs)

      return(invisible(self))


    },

    pre_process_inputs = function() {
      stop("Function must be implemented in class that inherits this model")
    },

    #' @description
    #' Set Input
    #'
    #' @details
    #' Use this function to add a new input to the model object.
    #'
    #' @param name character string defining the input name
    #' @param value input value. Can be a single value, a list or a vector.
    #' @param type optional character string defining the type of input. Useful when one wants to only write inputs of a certain type to json.
    set_input = function(name, value, type = NA_character_) {

      # If the model has been instantiated, and the input type is odin, set odin inptut
      if(!is.null(self$o)) {

        # check that this is an input the odin model needs:

        if(name %in% self$odin_parms) {

          input <- list()
          input[[name]] <- value

          do.call(self$o$set_user, input)
        }

      }

      super$set_input(name = name, value = value, type = type)

      return(invisible(self))

    },

    # simulates the model for a set of replications
    simulate = function(step, y = NULL, use_names = TRUE, reps = 1){

      # result comes as an array of matrices
      res <- replicate(n = reps, expr = self$o$run(step, y = y, use_names = use_names))

      # convert to a nice list
      res_list <- lapply(seq(dim(res)[3]), function(x) res[ , , x] %>% as.data.frame(.) %>% mutate(rep = x))

      # return as a data.frame
      self$res <- do.call(rbind, res_list)

      return(invisible(self))

    },

    # set default parameters from model inputs:
    set_default_params = function() {

      if(!is.null(self$inputs$parameters)) {
        for(i in 1:nrow(self$inputs$parameters)) {
          self$set_input(name = self$inputs$parameters$parameter[i], value = self$inputs$parameters$baseline[i])
        }
      }
      return(invisible(self))
    }

    # We should probably rename this because it is confusing.
    # This is deprecated.
    # collect_inputs = function(tab_name, key_name, val_name) {
    #
    #   # by default, we get all parameters from the inputs spreadsheet:
    #   # for parameters, I can put all parameters into a named list:
    #   inputs <- list()
    #
    #   outer <- self$inputs[[tab_name]]
    #   if(!is.null(outer)) {
    #     keys = outer[[key_name]]
    #     vals = outer[[val_name]]
    #     for(ix in 1:nrow(outer)) {
    #       inputs[keys[ix]] = vals[ix]
    #     }
    #   }
    #   # inputs should be a named list, containing the user() inputs the model needs
    #   return(inputs)
    # }
  )
)

