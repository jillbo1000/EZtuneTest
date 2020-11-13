#' Performs a specified number of eztune results
#'
#' eztune_results creates a line plot with the results of the optimization
#' tests.
#' @param x Matrix or data frame containing the dependent variables.
#' @param y Vector of responses. Can either be a factor or a numeric vector.
#' @param data_name Name of the dataset. Used to name the output file and
#' as an identifier within the output dataset.
#' @param method Model to be fit. Choices are "ada" for adaboost, "en" for
#' elastic net, "gbm" for gradient boosting machines, and "svm" for support
#' vector machines.
#' @param optimizer Optimization method. Options are "ga" for a genetic
#' algorithm and "hjn" for a Hooke-Jeeves optimizer.
#' @param fast Indicates if the function should use a subset of the
#' observations when optimizing to speed up calculation time. A value of
#' TRUE will use the smaller of 50% of the data or 200 observations for
#' model fitting, a number between 0 and 1 specifies the proportion of
#' data to be used to fit the model, and a postive integer specifies the
#' number of observations to be used to fit the model. A model is computed
#' using a random selection of data and the remaining data are used to
#' validate model performance. The validation error measure is used as
#' the optimization criterion.
#' @param cross If an integer k > 1 is specified, k-fold cross-validation
#' is used to fit the model. This method is very slow for large datasets.
#' This parameter is ignored unless fast = FALSE.
#' @param loss The type of loss function used for optimization. Options for
#' models with a binary response are "class" for classification error and
#' "auc" for area under the curve. Options for models with a continuous
#' response are "mse" for mean squared error and "mae" for mean absolute
#' error. If the option "default" is selected, or no loss is specified,
#' the classification accuracy will be used for a binary response model
#' and the MSE will be use for models with a continuous model.
#' @param iterations Number of times to run the model.
#' @param path Where the file should be saved.
#' @return Returns a parallel coordinate plot created by ggplot.
#' The x-axis shows the different optimization methods used, the
#' y-axis shows the standardized error rate for each dataset.
#' Within each dataset, the optimization method with the largest
#' loss is assigned a value of 1 and the smallest is assigned a
#' value of 0. If a faceting variable is selected, standardization
#' will be done regardless of the faceted variable. Standardizing is
#' done within each dataset so each dataset should have a value of 1
#' and a value of 0 on the graph. Datasets that are all NA for an
#' optimization method are denoted with a hollow circle and a value
#' of 1.
#'
#' @seealso \code{\link{load_opt_data}}, \code{\link{average_metric}}
#'
#' @export
#'

eztune_results <- function(x, y, data_name, method = NULL, optimizer = NULL,
                           fast = NULL, cross = NULL, loss = NULL,
                           iterations = 10, path = ".") {

  if(is.null(x) | is.null(y) | is.null(data_name) | is.null(method) |
     is.null(optimizer) | is.null(fast) | is.null(cross) |
     is.null(loss)) {
    stop("x, y, data_name, method, optimizer, fast, cross, loss, and path all must be specified")
  }

  file_name <- paste(data_name, method, optimizer, fast, cross, loss, sep = "_")
  loc <- paste(path, "/", file_name, ".csv", sep = "")
  loc <- gsub("//", "/", loc)

  mat <- data.frame(matrix(nrow = iterations, ncol = 9))
  colnames(mat) <- c("data", "method", "optimizer", "fast", "cross",
                     "time", "loss", "loss_mse_acc_10", "loss_mae_auc_10")

  mat$data <- data_name
  mat$method <- method
  mat$optimizer <- optimizer
  mat$fast <- fast
  mat$cross <- cross
  mat$loss <- loss

  for(i in 1:iterations) {

    t1 <- Sys.time()
    tmp <- eztune(x, y, method = method, optimizer = optimizer, fast = fast,
                  cross = cross, loss = loss)
    mat$loss[i] <- tmp$loss

    tmp_cv <- eztune_cv(x, y, model = tmp, cross = 10)

    if(is.null(tmp_cv$accuracy)) {
      mat$loss_mse_acc_10[i] <- tmp_cv$mse
      mat$loss_mae_auc_10[i] <- tmp_cv$mae
    } else {
      mat$loss_mse_acc_10[i] <- tmp_cv$accuracy
      mat$loss_mae_auc_10[i] <- tmp_cv$auc
    }

    t2 <- Sys.time()
    mat$time[i] <- as.numeric(t2 - t1)

    write.csv(mat, loc, quote = FALSE, row.names = FALSE)
  }
}
