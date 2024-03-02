# From: https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
clear.labels <- function(x) {
  if (is.list(x)) {
    for (i in 1:length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), "labelled")
    for (i in 1:length(x)) attr(x[[i]], "label") <- NULL
  } else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}
