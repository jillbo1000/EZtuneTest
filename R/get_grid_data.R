#' Makes a data.frame that contains the best error rates from a
#' grid search
#'
#' get_grid_data creates a data.frame that has the datasets in
#' the first column and the best error rate obtained in the grid
#' search in the second column.
#' @param path File path to folder with the files that hold the
#' grid results.
#' @param pattern An optional character vector that can be used to
#' select a subset of files in the folder.
#' @param dataset Name of the dataset for which the grid search
#' was done.
#' @param method A character string that specifies the method used to create
#' the grid. Choices are "svm", "gbm", "en", and "ada". This is
#' added to the datasets to minimize ambiguity in downstream analysis.
#' @param model_type Character string of either "binary" or "regression"
#' that specifies the type of model. This is needed because some of
#' the earlier grid searches had inconsistent loss scales.
#' @return Returns a list with a series of data.frames that can
#' be used to create plots of the grid surface or find the best
#' error rates. Note that some of the datasets in the list may have
#' more observations than indicated by the measures. This is because
#' there are a substantial number of ties. The datasets in the list are:
#' \item{data}{Complete dataset of grid results.}
#' \item{dat20loss}{Dataset containing only the best 20\% in terms of loss
#' (classification error, AUC, MSE, or MAE).}
#' \item{dat10loss}{Dataset containing only the best 10\% in terms of loss.}
#' \item{dat5loss}{Dataset containing only the best 5\% in terms of loss.}
#' \item{dat1loss}{Dataset containing only the best 1\% in terms of loss.}
#' \item{dat20time}{Dataset containing only the best 20\% in terms of computation time.}
#' \item{dat10time}{Dataset containing only the best 10\% in terms of computation time.}
#' \item{dat5time}{Dataset containing only the best 5\% in terms of computation time.}
#' \item{dat1time}{Dataset containing only the best 1\% in terms of computation time.}
#' \item{top20loss}{Twenty grid locations with the best loss.}
#' \item{top20time}{Twenty grid locations with the best computation times.}
#' Each of the datasets has the following variables:
#' \item{Data}{Name of the dataset used to create the grid.}
#' \item{Method}{Method used for EZtune. Should be "svm", "gbm", "en", or
#' "ada". It will be exactly as it is entered into the method argument.}
#' \item{Tuning_Variables}{These fields contain the tuning variables for the
#' method. For svm they are Cost and Gamma (Note that Gamma is really
#' log2(Gamma)), and Epsilon for regression models; gbm is NumTrees, MinNode,
#' Shrinkage, IntDepth; en is Alpha and logLambda; ada is Nu, Iter, and
#' MaxDepth.}
#' \item{Loss}{The loss measure used in the grid search. It is typically
#' classification error or MSE, but it can be AUC or MAE as well. It is
#' computed using 10-fold cross validation. }
#' \item{LossUCL}{A measure of stability for the Loss measure. The test loss
#' for each of the folds in 10-fold cross validation were used to compute a
#' 95\% upper confidence interval for the loss. If it differs substantially
#' from the Loss it indicates that results for the model with those tuning
#' parameters are unstable. }
#' \item{Time}{Computation time in seconds.}
#'
#' @seealso \code{\link{grid_search}}, \code{\link{eztune_table}},
#' \code{\link{get_best_grid}}, \code{\link{grid_plot}}
#'
#' @export
#'

get_grid_data <- function(path = ".", pattern = NULL, dataset = "Data",
                          method = NULL, model_type = NULL) {

  allF <- list.files(path, pattern = pattern, full.names = TRUE)
  dat <- NULL

  for(i in 1:length(allF)) {
    dat <- rbind(dat, read.csv(allF[i]))
  }

  dat <- data.frame(Data = rep(dataset, nrow(dat)),
                    Method = rep(method, nrow(dat)), dat)

  # ---------------------------------------------------------------------------
  # This section is needed to interface between data from grid searches
  # completed before this package was written

  colnames(dat)[grepl("LCL|UCL", colnames(dat))] <- "LossUCL"

  if(model_type == "binary") {

    acc <- FALSE
    if(sum(colnames(dat) %in% c("Accuracy", "accuracy"))) acc <- TRUE

    colnames(dat)[colnames(dat) %in% c("Accuracy", "accuracy", "loss",
                                       "Error", "Err", "error",
                                       "err")] <- "Loss"

    if(max(dat$Loss > 1)) {
      dat$Loss <- dat$Loss * 0.01
      dat$LossUCL <- dat$LossUCL * 0.01
    }

    if(acc) {
      dat$Loss <- 1 - dat$Loss
      dat$LossUCL <- 1 - dat$LossUCL
    }
  } else if (model_type == "regression") {
    colnames(dat)[colnames(dat) %in% c("MSE", "MAE", "loss",
                                       "mse", "mae")] <- "Loss"
  }

  # ---------------------------------------------------------------------------

  dat20loss <- dat[dat$Loss <= quantile(dat$Loss, 0.20, na.rm = TRUE), ]
  dat10loss <- dat[dat$Loss <= quantile(dat$Loss, 0.10, na.rm = TRUE), ]
  dat5loss <- dat[dat$Loss <= quantile(dat$Loss, 0.05, na.rm = TRUE), ]
  dat1loss <- dat[dat$Loss <= quantile(dat$Loss, 0.01, na.rm = TRUE), ]

  dat20time <- dat[dat$Time <= quantile(dat$Time, 0.20, na.rm = TRUE), ]
  dat10time <- dat[dat$Time <= quantile(dat$Time, 0.10, na.rm = TRUE), ]
  dat5time <- dat[dat$Time <= quantile(dat$Time, 0.05, na.rm = TRUE), ]
  dat1time <- dat[dat$Time <= quantile(dat$Time, 0.01, na.rm = TRUE), ]

  l20 <- dat$Loss[order(dat$Loss, decreasing = FALSE)][20]
  top20loss <- dat[dat$Loss <= l20, ]
  t20 <- dat$Time[order(dat$Time, decreasing = FALSE)][20]
  top20time <- dat[dat$Time <= t20, ]

  list(data = dat, dat20loss = dat20loss, dat10loss = dat10loss,
       dat5loss = dat5loss, dat1loss = dat1loss, dat20time = dat20time,
       dat10time = dat10time, dat5time = dat5time, dat1time = dat1time,
       top20loss = top20loss, top20time = top20time)
}

