#' Creates a grid that can be filled in using the function grid_search
#'
#' create_grid creates a grid that can be filled in using the function
#' grid_search. It is kept separate from the grid_search functions so that
#' the user can create a different grid if they choose.
#' @param x Matrix or data frame containing the dependent variables..
#' @param y Vector of responses. Can either be a factor or a numeric vector.
#' @param method Model to be fit. Choices are "ada" for adaboost, "en" for
#' elastic net, "gbm" for gradient boosting machines, and "svm" for support
#' vector machines.
#' @return Returns a dataset that forms the grid for the search. It contains
#' the data frame has a column for each of the tuning parameters and a
#' column for the Accuracy/MSE, AUC/MAE, and one for the time in seconds.
#'
#' @seealso \code{\link{grid_search}}
#'
#' @export
#'

create_grid <- function(x, y, method) {

  type <- ifelse(length(unique(y)) > 2, "reg", "bin")[1]
  nm <- paste(method, type, sep = ".")
  param <- switch(nm,
                  svm.bin = svm.bin.grid(),
                  svm.reg = svm.reg.grid(),
                  gbm.bin = gbm.bin.grid(),
                  gbm.reg = gbm.reg.grid(),
                  en.bin = en.bin.grid(x = x, y = y),
                  en.reg = en.reg.grid(x = x, y = y),
                  ada.bin = ada.bin.grid())

  as.data.frame(param)

}

