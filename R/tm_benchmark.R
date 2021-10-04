#' Tunes a model via tidymodels and returns benchmarking results
#'
#' tm_benchmark tunes a model with tidymodels and returns the accuracy
#' results and computation time in seconds.
#' @param x Matrix or data frame containing the dependent variables.
#' @param y Vector of responses. Can either be a factor or a numeric vector.
#' @param name Name of the dataset. Used to name the output file and
#' as an identifier within the output dataset.
#' @param method Model to be fit. Choices are "ada" for adaboost, "en" for
#' elastic net, "gbm" for gradient boosting machines, and "svm" for support
#' vector machines.
#' @param test Type of tuning to be done. Choices are "bayes" for iterative
#' Bayesian optimization and "grid" for grid search.
#' @param grid_size Number of values for each hyperparameter for tidymodels
#' grid. This is ignored if iterative Bayesian optimization is selected.
#' @param bayes_iter Number of iterations for the Bayesian optimization.
#' This is ignored if test = "grid"
#' @return Returns a single row matrix with the following elements:
#' \item{data}{Name of the dataset.}
#' \item{package}{This will always be "tidymodels".}
#' \item{method}{Type of model that was fit. It will gbm for
#' gradient boosting machines or svm for support vector machines.}
#' \item{optimizer}{Type of optimizer used. It will be Grid  or
#' Iterative Bayes.}
#' \item{assess}{The method used to assess the model during tuning. For
#' tidymodels it will be cross-validation.}
#' \item{tuned_on}{Metric that was used as the loss function}
#' \item{acc_rmse}{The best accuracy or RMSE obtained from the model as
#' determined by a validation dataset.}
#' \item{auc_mae}{The best AUC or MAE obtained from the model as
#' determined by a validation dataset.}
#' \item{time}{Time to complete the calculations in seconds.}
#' The models are verified using rsample to split the data into  training
#' and testing datasets.
#'
#' @seealso \code{\link{ez_benchmark}}, \code{\link{EZtune::eztune}}
#'
#' @export
#'

tm_benchmark <- function(x, y, name = "Data", method = "svm", test = "grid",
                    grid_size = 5, bayes_iter = 10) {

  dat <- data.frame(y, x)
  colnames(dat)[1] <- "y"
  for(i in 1:length(colnames(dat))) {
    if(is.character((dat[, i]))) dat[, i] <- as.factor(dat[, i])
  }

  #================= Partition data and create folds for CV

  if(length(unique(y)) == 2) {
    dat_split <- rsample::initial_split(dat, strata = y)
    meth <- paste0(method, ".bin")
  } else {
    dat_split <- rsample::initial_split(dat)
    meth <- paste0(method, ".reg")
  }

  dat_train <- rsample::training(dat_split)
  dat_test  <- rsample::testing(dat_split)

  dat_folds <- rsample::vfold_cv(dat_train)

  if(length(unique(dat_train$y)) == 2) {
    meth <- paste0(method, ".bin")
  } else {
    meth <- paste0(method, ".reg")
  }

  t1 <- Sys.time()

  #========================= Setup models
  if(meth == "svm.bin") {
    tune_mod <- parsnip::svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("classification")
    pow <- 2
    mets <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

  } else if(meth == "gbm.bin") {
    tune_mod <- parsnip::boost_tree(trees = tune(), tree_depth = tune(),
                                    learn_rate = tune(),
                                    min_n = tune()) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("classification")
    pow <- 4
    mets <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)

  } else if(meth == "svm.reg") {
    tune_mod <- parsnip::svm_rbf(cost = tune(), rbf_sigma = tune(),
                                 margin = tune()) %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("regression")
    pow <- 3
    mets <- yardstick::metric_set(yardstick::rmse, yardstick::mae)

  } else if(meth == "gbm.reg") {
    tune_mod <- parsnip::boost_tree(trees = tune(), tree_depth = tune(),
                                    learn_rate = tune(),
                                    min_n = tune()) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("regression")
    pow <- 4
    mets <- yardstick::metric_set(yardstick::rmse, yardstick::mae)

  }

  mod_wf <- workflows::workflow() %>%
    workflows::add_model(tune_mod) %>%
    workflows::add_formula(y ~ .)

  #========================= Tune
  if(test == "grid") {
    mod_res <- mod_wf %>%
      tune::tune_grid(resamples = dat_folds, grid = grid_size^pow,
                      metrics = mets)

  } else if(test == "bayes") {
    mod_set <- tune::parameters(mod_wf)
    mod_res <- mod_wf %>%
      tune::tune_bayes(resamples = dat_folds, param_info = mod_set,
                       initial = 5, iter = bayes_iter, metrics = mets)

  } else {
    print("Invalid value for 'test'. Grid search done.")
    mod_res <- mod_wf %>%
      tune::tune_grid(resamples = dat_folds, grid = grid_size^pow,
                      metrics = mets)
  }

  #========================= Select best models
  if(grepl(".bin", meth)) {
    best_mod_acc_mse <- mod_res %>% tune::select_best("accuracy")
    best_mod_auc_mae <- mod_res %>% tune::select_best("roc_auc")

  } else {
    best_mod_acc_mse <- mod_res %>% tune::select_best("rmse")
    best_mod_auc_mae <- mod_res %>% tune::select_best("mae")
  }

  #========================= AUC/MAE model
  final_wf_auc_mae <- mod_wf %>%
    tune::finalize_workflow(best_mod_auc_mae)
  final_wf_auc_mae

  final_mod_auc_mae <- final_wf_auc_mae %>%
    parsnip::fit(data = dat_train)
  final_mod_auc_mae

  final_fit_auc_mae <- final_wf_auc_mae %>%
    tune::last_fit(dat_split, metrics = mets)

  res_auc_mae <- final_fit_auc_mae %>% tune::collect_metrics()
  # res_auc_mae

  #========================= Acc/RMSE
  final_wf_acc_mse <- mod_wf %>%
    tune::finalize_workflow(best_mod_acc_mse)
  # final_wf_acc_mse

  final_mod_acc_mse <- final_wf_acc_mse %>%
    parsnip::fit(data = dat_train)
  # final_mod_acc_mse

  final_fit_acc_mse <- final_wf_acc_mse %>%
    tune::last_fit(dat_split, metrics = mets)

  res_acc_mse <- final_fit_acc_mse %>% tune::collect_metrics()
  # res_acc_mse

  #========================= Make output

  t2 <- Sys.time()
  run_time <- as.numeric(difftime(t2, t1, units = "secs"))

  if(test == "grid") {
    test <- "Grid"
  } else if(test == "bayes") {
    test <- "Iterative Bayes"
  }

  out <- data.frame(data = name, package = "tidymodels", method = method,
                    optimizer = test, assess = "cross-validation")
  out <- rbind(out, out)
  out2 <- data.frame(rbind(t(res_auc_mae[, 3]), t(res_acc_mse[, 3])))
  colnames(out2) <- c("acc_rmse", "auc_mae")
  rownames(out2) <- c(1, 2)
  if(grepl(".bin", meth)) {
    tuned_on <- c("auc", "accuracy")
  } else {
    tuned_on <- c("mae", "rmse")
  }

  data.frame(out, tuned_on, out2, time = rep(run_time, 2))

}

