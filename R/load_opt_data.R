#' Reads in data files from the optimization tests for EZtune
#'
#' load_opt_data reads all of the csv files in a folder that contain
#' the output from the optimization tests.
#' @param path This is the path to the directory that contains the
#' optimization output files.
#' @return Returns a dataset that has all of the outputs from the
#' optimization tests recorded in the files in a folder. Typically,
#' a folder will contain the tests for one dataset. The dataset
#' contains the following variables:
#'
#' \item{data}{Name of the dataset.}
#' \item{method}{Type of model that was fit. It will an abbreviation
#' for adaboost, elastic net, gradient boosting machines, or
#' support vector machines.}
#' \item{optimizer}{Type of optimizer used. It will either be ga
#' for a genetic algorithm or hjn for a Hookes-Jeeves algorithm.}
#' \item{fast}{The argument passed to the fast option. If it is a 1,
#' a value of TRUE was passed and if it was 0 a value of FALSE was
#' passed.}
#' \item{cross}{n for n-fold cross-validation in the optimization. It
#' was only used if fast was FALSE.}
#' \item{loss_type}{|ar_ref - 0.5|}
#' \item{seed}{-log10(av_stat), where 0 is replaced with 0.001 to avoid
#' taking the log of 0.}
#' \item{loss}{Type of loss used as an optimizer. If the dataset has a
#' continuous response, the options are mse for mean squared error and
#' mae for mean absolute error. If the response is binary, the options
#' are acc for accuracy and auc for area under the ROC curve.}
#' \item{loss_mse_acc_10}{Estimate of the accuracy or mean squared error
#' as computed using the eztune_cv function with 10 fold cross validation.}
#' \item{loss_mae_auc_10}{Estimate of the area under the curve or mean absolute
#' error as computed using the eztune_cv function with 10 fold cross
#' validation.}
#'
#' @seealso \code{\link{binned_stats}}, \code{\link{average_metric}}
#'
#' @export
#'

load_opt_data <- function(path, ...) {
  nms <- list.files(path, full.names = TRUE, ...)
  dat <- NULL
  for(i in 1:length(nms)) {
    tmp <- read.csv(nms[i])
    dat <- rbind(dat, tmp)
  }
  dat
}
