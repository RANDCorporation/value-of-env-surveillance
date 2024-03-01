# test of coordination

test_coordination <- function(model){
  # TEST FOR WHEN JURISDICTIONS 1 and 2 coordinate and jurisdiction 3 doesn't
  g <- select(model$res_long, rep, step, jurisdiction.id, L)
  h <- g %>% pivot_wider(names_from = jurisdiction.id, id_cols=c(step, rep), values_from= L)

  # function to check if only has digits
  numbers_only <- function(x) !grepl("\\D", x)


  numerical_col_names = sort(as.numeric(colnames(h)[numbers_only(colnames(h))]))
  for (row in 1:nrow(model$inputs$npi_coord)){
    for (col in 1:ncol(model$inputs$npi_coord)){
      if (model$inputs$npi_coord[row,col]==1){
        # make sure coordinating jurisdictions have same NPI level
        if (any(h[as.character(row)] != h[as.character(col)])){
          warning("Jurisdictions not coordinating when they should!")
        }
      }
      # Make sure non-coordinating ones have different NPI levels at some point
      # NOTE: This may not always need to be true (maybe not if they have full mixing
      # and the same responses to the same levels of COVID?)
      else if (all(h[as.character(row)] == h[as.character(col)])){
        warning("Jurisdictions MAY be coordinationg when they shouldn't")
      }
    }
  }
}
