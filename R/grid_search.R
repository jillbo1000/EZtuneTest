#' Performs a grid search for a dataset
#'
#' grid_search does a grid search for a dataset with a specified model.
#' @param x Matrix or data frame containing the dependent variables..
#' @param y Vector of responses. Can either be a factor or a numeric vector.
#' @param grid A data frame that contains the grid that will be filled by the
#' function. The grid can be one generated the function create_grid.R or one
#' created by the user. The Details section provides information about how
#' make a grid data frame.
#' @param cross Number of folds for nfold cross-validation.
#' @param data_name Name of the dataset. Used to name the output file and
#' as an identifier within the output dataset.
#' @param method Model to be fit. Choices are "ada" for adaboost, "en" for
#' elastic net, "gbm" for gradient boosting machines, and "svm" for support
#' vector machines.
#' @param start Start location of the grid to be processed in the run. The
#' size of the grids created using create_grid.R are:
#' @param end Stop location of the grid to be processed in the run.
#' @param path This is the path to the directory that contains the
#' grid result output files.#'
#' @return Returns a dataset that has all of the outputs from the
#' grid search. Typically, a folder will contain the tests for one dataset.
#' The dataset contains the following variables:
#'
#' @details
#' The function create_grid.R can be used to create the grid, but you can
#' create your own grid that will be filled in with the function. Each data.frame
#' must have the variables for the tuning variables filled in with the values
#' of the tuning variables. The variables for Accuracy, AUC, MSE, MAE, and time
#' can be filled with any value, because those values will be replaced, but it is
#' recommended that they are filled with NA so that models that cannot be
#' computed for a given set of grid values can be easily identified. The grids
#' must be a data.frame with the following specifications:
#'
#' SVM: The column names for the data.frame for the classification model must be
#' Cost, Gamma, Accuracy, AUC, and Time. The column names for the regression
#' model are Cost, Gamma, Epsilon, MSE, MAE, and Time. The columns for
#' Cost, Gamma, and Epsilon must be numeric vectors. The values for the
#' Gamma and Cost variables are the 2^x; that is, they are the exponent, not
#' the actual value for Gamma.
#'
#' GBM: The column names for the data.frame for the classification model must be
#' NumTrees, MinNode, Shrinkage, IntDepth, Accuracy, AUC, Time. The column
#' names for the regression models are NumTrees, MinNode, Shrinkage,
#' IntDepth, MSE, MAE, Time.
#'
#' Elastic Net: The column names for the data.frame for the classification model
#' must be Alpha, logLambda, Accuracy, AUC, Time. The column names for the
#' regression models are Alpha, logLambda, MSE, MAE, Time. Note that the value
#' in the logLambda variable is the natural log of lambda.
#'
#' Adaboost: The adaboost model can only be computed for classificaton models.
#' The column names for the grid are Nu, Iter, Maxdepth, Accuracy, AUC, and
#' Time.
#'
#' @seealso \code{\link{binned_stats}}, \code{\link{average_metric}}
#'
#' @export
#'

grid_search <- function(x, y, grid, cross = 10, data_name, method,
                        start = NULL, end = NULL, path = ".") {

  if(is.null(start)) start <- 1
  if(is.null(end)) end <- nrow(grid)

  grid <- grid[start:end, ]
  type <- ifelse(length(unique(y)) > 2, "reg", "bin")[1]
  nm <- paste(method, type, sep = ".")

  return_grid <- switch(nm,
                        svm.bin = svm.bin.results(x, y, grid, cross),
                        svm.reg = svm.reg.results(x, y, grid, cross),
                        gbm.bin = gbm.bin.results(x, y, grid, cross),
                        gbm.reg = gbm.reg.results(x, y, grid, cross),
                        en.bin = en.bin.results(x, y, grid, cross),
                        en.reg = en.reg.results(x, y, grid, cross),
                        ada.bin = ada.bin.results(x, y, grid, cross))

  return_grid <- cbind(data_name, return_grid)
  colnames(return_grid)[1] <- "Data"

  f <- paste(data_name, method, type, start, end, sep ="_")
  f <- paste(f, ".csv", sep = "")
  f <- paste(path, "/", f, sep = "")
  f <- gsub("//", "/", f)

  write.csv(return_grid, f, row.names = FALSE)

}
