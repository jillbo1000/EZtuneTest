#' Tunes a model via elastic net with \code{glmnet} and returns
#'
#' \code{en_benchmark} tunes a model with \code{glmnet} and returns a one row
#' \code{data.frame} with the results. \pkg{rsample} is used to split
#' the dataset into training and test datasets. The results returned
#' @param x Matrix or data frame containing the dependent variables.
#' @param y Vector of responses. Can either be a factor or a numeric vector.
#' @param name Name of the dataset. Used to name the output file and
#' as an identifier within the output dataset.
#' @param loss Value for \code{loss} argument in \code{glmnet}. Options are
#' "default", "class", "auc", "mse", or "mae". If "default" is selected,
#'
#' @return Saves a matrix to the indicated path that contains the results
#' for each of the runs. The final file contains the following variables:
#' \item{data}{Name of the dataset.}
#' \item{package}{This will always be "glmnet".}
#' \item{method}{Type of model that was fit. This will always be "en"
#' for elastic net.}
#' \item{optimizer}{Optimization method. It will be "1se" or "min" to
#' reflect the criteria used for lambda selection by glmnet.}
#' \item{assess}{The method used to assess the model during tuning. It will
#' always be cross-validation.}
#' \item{tuned_on}{Metric that was used as the loss function.}
#' \item{acc_rmse}{The best accuracy or RMSE obtained from the model as
#' determined by a validation dataset.}
#' \item{auc_mae}{The best AUC or MAE obtained from the model as
#' determined by a validation dataset.}
#' \item{time}{Time to complete the calculations in seconds.}
#' The models are verified using rsample to split the data into  training
#' and testing datasets.
#'
#' @seealso \code{\link{tm_benchmark}}, \code{\link{EZtune::eztune}}
#'
#' @export
#'

en_benchmark <- function(x, y, name = "Data", loss = "default") {

  method <- "en"

  # convert x to a numeric matrix
  newdat <- data.frame(a = rep(0, nrow(x)))
  for(i in 1:ncol(x)) {
    if(is.numeric(x[, i])) {
      newdat <- cbind(newdat, x[, i])
      colnames(newdat)[ncol(newdat)] <- colnames(x)[i]
    } else {
      dum <- data.frame(stats::model.matrix( ~ x[, i] - 1)[, -1])
      colnames(dum) <- paste(colnames(x[i]), ".", 1:ncol(dum), sep = "")
      newdat <- cbind(newdat, dum)
    }
  }
  x <- newdat[, -1]

  dat <- data.frame(y, x)
  colnames(dat)[1] <- "y"

  if(length(unique(y)) == 2) {
    dat_split <- rsample::initial_split(dat, strata = y)
    meth <- paste0(method, ".bin")
    if(loss == "default" ) loss <- "class"
    family = "binomial"
  } else {
    dat_split <- rsample::initial_split(dat)
    meth <- paste0(method, ".reg")
    family <- "gaussian"
    if(loss == "default") loss <- "mse"
  }

  dat_train <- rsample::training(dat_split)
  dat_test  <- rsample::testing(dat_split)

  t1 <- Sys.time()

  foldid <- sample(1:10, size = nrow(dat_train), replace = TRUE)
  alpha <- seq(0, 1, 0.1)
  alpha_data <- data.frame(alpha = alpha, lambda.min = NA, lambda.1se = NA,
                           loss.min = NA, loss.1se = NA)
  alpha_cv <- NULL
  for(i in 1:length(alpha)) {
    alpha_cv[[i]] <- glmnet::cv.glmnet(x = as.matrix(dat_train[, -1]),
                                       y = dat_train[, 1], family = family,
                                       type.measure = loss,
                                       foldid = foldid, alpha = alpha[i])
    alpha_data[i, -1] <- c(alpha_cv[[i]]$lambda.min, alpha_cv[[i]]$lambda.1se,
                           alpha_cv[[i]]$cvm[alpha_cv[[i]]$lambda == alpha_cv[[i]]$lambda.min],
                           alpha_cv[[i]]$cvm[alpha_cv[[i]]$lambda == alpha_cv[[i]]$lambda.1se])
  }

  mod.1se <- glmnet::glmnet(x = as.matrix(dat_train[, -1]), y = dat_train[, 1],
                            family = family,
                            lambda = alpha_data$lambda.1se[alpha_data$loss.1se == min(alpha_data$loss.1se)],
                            alpha = alpha_data$alpha[alpha_data$loss.1se == min(alpha_data$loss.1se)],
                            type.measure = loss)

  mod.min <- glmnet::glmnet(x = as.matrix(dat_train[, -1]), y = dat_train[, 1],
                            family = family,
                            lambda = alpha_data$lambda.min[alpha_data$loss.min == min(alpha_data$loss.min)],
                            alpha = alpha_data$alpha[alpha_data$loss.min == min(alpha_data$loss.min)],
                            type.measure = loss)

  #========================= Make output

  t2 <- Sys.time()
  run_time <- as.numeric(difftime(t2, t1, units = "secs"))

  res.1se <- predict(mod.1se, as.matrix(dat_test[, -1]), type = "response")
  res.min <- predict(mod.min, as.matrix(dat_test[, -1]), type = "response")

  if(family == "binomial") {
    dat_test[, 1] <- as.factor(as.numeric(dat_test[, 1]) - 1)
    res.1se.r <- as.factor(round(res.1se))
    res.min.r <- as.factor(round(res.min))
  }

  if(family == "binomial") {
    acc_rmse.1se <- yardstick::accuracy_vec(truth = dat_test[, 1], estimate = res.1se.r)
    auc_mae.1se <- yardstick::roc_auc_vec(truth = dat_test[, 1], estimate = res.1se[, 1],
                                          event_level = "second")
    acc_rmse.min <- yardstick::accuracy_vec(truth = dat_test[, 1], estimate = res.min.r)
    auc_mae.min <- yardstick::roc_auc_vec(truth = dat_test[, 1], estimate = res.min[, 1],
                                          event_level = "second")
  } else {
    acc_rmse.1se <- yardstick::rmse_vec(truth = dat_test[, 1], estimate = res.1se[, 1])
    auc_mae.1se <- yardstick::mae_vec(truth = dat_test[, 1], estimate = res.1se[, 1])
    acc_rmse.min <- yardstick::rmse_vec(truth = dat_test[, 1], estimate = res.min[, 1])
    auc_mae.min <- yardstick::mae_vec(truth = dat_test[, 1], estimate = res.min[, 1])
  }

  out <- data.frame(data = name, package = "glmnet", method = method,
                    optimizer = c("1se", "min"), assess = "cross-validation",
                    acc_rmse = c(acc_rmse.1se, acc_rmse.min),
                    auc_mae = c(auc_mae.1se, auc_mae.min))

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
