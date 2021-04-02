#' Tunes a model via tidymodels and returns
#'
#' \code{ez_benchmark} tunes a model with eztune and returns a one row
#' \code{data.frame} with the results. \pkg{rsample} is used to split
#' the dataset into training and test datasets. The results returned
#' @param x Matrix or data frame containing the dependent variables.
#' @param y Vector of responses. Can either be a factor or a numeric vector.
#' @param name Name of the dataset. Used to name the output file and
#' as an identifier within the output dataset.
#' @param method Model to be fit. Choices are "ada" for adaboost, "en" for
#' elastic net, "gbm" for gradient boosting machines, and "svm" for support
#' vector machines. This argument is passed to \code{eztune}. See
#' \code{\link{EZtune::eztune}} for more information.
#' @param optimizer Optimization method to be used. May be either "hjn" or
#' "ga". This argument is passed to \code{eztune}. See
#' \code{\link{EZtune::eztune}} for more information.
#' @param fast Value for \code{fast} argument in \code{eztune}. Options are
#' TRUE, FALSE, a positive integer, or a number between 0 and 1. See
#' \code{\link{EZtune::eztune}} for more information.
#' @param cross Value for \code{cross} argument in \code{eztune}.Options are
#' NULL or a positive integer to represent the number of folds. See
#' \code{\link{EZtune::eztune}} for more information.
#' @param loss Value for \code{loss} argument in \code{eztune}. Options are
#' "default", "class", "auc", "mse", or "mae". See
#' \code{\link{EZtune::eztune}} for more information.
#' @return Saves a matrix to the indicated path that contains the results
#' for each of the runs. The final file contains the following variables:
#' \item{data}{Name of the dataset.}
#' \item{package}{This will always be "EZtune".}
#' \item{method}{Type of model that was fit. It will "gbm" for
#' gradient boosting machines, "svm" for support vector machines,
#' "en" for elastic net, or "ada" for adaboost.}
#' \item{optimizer}{Optimization method. It will be "Genetic algorithm" or
#' "Hooke-Jeeves".}
#' \item{assess}{The method used to assess the model during tuning. It will
#' be either cross-validation or some version of FAST.}
#' \item{tuned_on}{Metric that was used as the loss function.}
#' \item{acc_rmse}{The best accuracy or RMSE obtained from the model as
#' determined by a validation dataset.}
#' \item{auc_mae}{The best AUC or MAE obtained from the model as
#' determined by a validation dataset.}
#' \item{time}{Time to complete the calculations in seconds.}
#' as computed using the eztune_cv function with 10 fold cross validation.}
#' The models are verified using rsample to split the data into  training
#' and testing datasets.}
#'
#' @seealso \code{\link{tm_benchmark}}, \code{\link{EZtune::eztune}}
#'
#' @export
#'

ez_benchmark <- function(x, y, name = "Data", method = "svm", optimizer = "hjn",
                     fast = TRUE, cross = NULL, loss = "default") {

  dat <- data.frame(y, x)
  colnames(dat)[1] <- "y"

  if(length(unique(y)) == 2) {
    dat_split <- rsample::initial_split(dat, strata = y)
    meth <- paste0(method, ".bin")
  } else {
    dat_split <- rsample::initial_split(dat)
    meth <- paste0(method, ".reg")
  }

  dat_train <- rsample::training(dat_split)
  dat_test  <- rsample::testing(dat_split)

  t1 <- Sys.time()

  mod <- EZtune::eztune(dat_train[, -1],dat_train$y,  method = method,
                        optimizer = optimizer, fast = fast, cross = cross,
                        loss = loss)

  #========================= Make output

  t2 <- Sys.time()
  run_time <- as.numeric(difftime(t2, t1, units = "secs"))

  res <- predict(mod, dat_test)

  if(is.data.frame(res)) {
    acc_rmse <- yardstick::roc_auc_vec(truth = dat_test[, 1], estimate = res[, 2])
    auc_mae <- yardstick::accuracy_vec(truth = dat_test[, 1], estimate = res[, 1])
  } else {
    acc_rmse <- yardstick::rmse_vec(truth = dat_test[, 1], estimate = res)
    auc_mae <- yardstick::mae_vec(truth = dat_test[, 1], estimate = res)
  }

  if(optimizer == "hjn") {
    test <- "Hooke-Jeeves"
  } else if(optimizer == "ga") {
    test <- "Genetic algorithm"
  }

  if(fast) {
    assess = paste0("fast = ", fast)
  } else {
    assess = paste0(cross, "-fold cross-validation")
  }

  out <- data.frame(data = name, package = "EZtune", method = method,
                    optimizer = test, assess = assess, acc_rmse = acc_rmse,
                    auc_mae = auc_mae)

  if(loss == "default") {
    if(grepl(".bin", meth)) {
      tuned_on <- "accuracy"
    } else {
      tuned_on <- "rmse"
    }
  } else if(loss == "class") {
    tuned_on <- "accuracy"
  } else if(loss == "mse") {
    tuned_on <- "rmse"
  } else {
    tuned_on <- loss
  }

  data.frame(out[, 1:5], tuned_on, out[, 6:7], time = run_time)

}
