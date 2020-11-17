
#==============================================================================
#                             SVM Functions
#==============================================================================

svm.bin.results <- function(x, y, grid, cross) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- cbind(y, x)
  colnames(dat)[1] <- "y"

  for(j in 1:nrow(grid)) {
    t1 <- Sys.time()
    yval <- rep(NA, nrow(dat))
    xvs <- rep(1:cross, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      svm.t <- NULL
      try(svm.t <- e1071::svm(as.factor(y) ~ ., data = train, cost = 2^grid$Cost[j],
                              gamma = 2^grid$Gamma[j], probability = TRUE))
      if(!is.null(svm.t)) {
        pr <- stats::predict(svm.t, newdata = test, probability = TRUE)
        yval[xvs == i] <- attr(pr, "probabilities")[, colnames(attr(pr, "probabilities")) == "1"]
      }
    }

    try(grid$Accuracy[j] <- loss.bin(pred = yval, true_y = dat$y, loss = "class"))
    try(grid$AUC[j] <- loss.bin(pred = yval, true_y = dat$y, loss = "auc"))

    t2 <- Sys.time()
    grid$Time[j] <- as.numeric(t2 - t1, units = "secs")
  }
  grid
}


svm.reg.results <- function(x, y, grid, cross) {

  dat <- cbind(y, x)
  colnames(dat)[1] <- "y"

  for(j in 1:nrow(grid)) {
    t1 <- Sys.time()
    yval <- rep(0, nrow(dat))
    xvs <- rep(1:cross, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      svm.t <- NULL
      try(svm.t <- e1071::svm(y ~ ., data = train, cost = 2^grid$Cost[j],
                              gamma = 2^grid$Gamma[j], epsilon = grid$Epsilon[j]))
      if(!is.null(svm.t)) {
        yval[xvs == i] <- stats::predict(svm.t, newdata = test[, -1])
      }
    }

    try(grid$MSE[j] <- loss.reg(pred = yval, true_y = dat$y, loss = "mse"))
    try(grid$MAE[j] <- loss.reg(pred = yval, true_y = dat$y, loss = "mae"))

    t2 <- Sys.time()
    grid$Time[j] <- as.numeric(t2 - t1, units = "secs")
  }
  grid
}


#==============================================================================
#                             gbm Functions
#==============================================================================

gbm.bin.results <- function(x, y, grid, cross) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- cbind(y, x)
  colnames(dat)[1] <- "y"

  for(j in 1:nrow(grid)) {
    t1 <- Sys.time()
    yval <- rep(NA, nrow(dat))
    xvs <- rep(1:cross, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      gbm.t <- NULL
      try(gbm.t <- gbm::gbm(y ~ ., distribution = "bernoulli",
                            interaction.depth = grid$IntDepth[j],
                            n.trees = grid$NumTrees[j],
                            shrinkage = grid$Shrinkage[j],
                            n.minobsinnode = grid$MinNode[j],
                            data = train))
      if(!is.null(gbm.t)) {
        yval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                           n.trees = grid$NumTrees[j])
      }
    }

    try(grid$Accuracy[j] <- loss.bin(pred = yval, true_y = dat$y, loss = "class"))
    try(grid$AUC[j] <- loss.bin(pred = yval, true_y = dat$y, loss = "auc"))

    t2 <- Sys.time()
    grid$Time[j] <- as.numeric(t2 - t1, units = "secs")

  }
  grid
}


gbm.reg.results <- function(x, y, grid, cross) {

  dat <- cbind(y, x)
  colnames(dat)[1] <- "y"

  for(j in 1:nrow(grid)) {
    t1 <- Sys.time()
    yval <- rep(NA, nrow(dat))
    xvs <- rep(1:cross, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      gbm.t <- NULL
      try(gbm.t <- gbm::gbm(y ~ ., distribution = "gaussian",
                            interaction.depth = grid$IntDepth[j],
                            n.trees = grid$NumTrees[j],
                            shrinkage = grid$Shrinkage[j],
                            n.minobsinnode = grid$MinNode[j],
                            data = train))
      if(!is.null(gbm.t)) {
        yval[xvs == i] <- gbm::predict.gbm(gbm.t, newdata = test, type="response",
                                           n.trees = grid$NumTrees[j])
      }
    }

    try(grid$MSE[j] <- loss.reg(pred = yval, true_y = dat$y, loss = "mse"))
    try(grid$MAE[j] <- loss.reg(pred = yval, true_y = dat$y, loss = "mae"))

    t2 <- Sys.time()
    grid$Time[j] <- as.numeric(t2 - t1, units = "secs")

  }
  grid
}


#==============================================================================
#                           elastic net Functions
#==============================================================================

en.bin.results <- function(x, y, grid, cross) {

  y <- as.factor(as.numeric(as.factor(y)) - 1)
  x <- as.matrix(dummy(x))

  for(j in 1:nrow(grid)) {
    t1 <- Sys.time()
    yval <- rep(NA, nrow(dat))
    xvs <- rep(1:cross, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train.x <- x[xvs != i, ]
      train.y <- y[xvs != i]
      test.x <- x[xvs == i, ]
      test.y <- y[xvs == i]
      tt1 <- rbind(test.x, train.x)
      try(en.t <- glmnet::glmnet(as.matrix(train.x), train.y, family = "binomial",
                                 alpha = grid$Alpha[j], lambda = exp(grid$logLambda[j])))
      if(!is.null(en.t)) {
        yval[xvs == i] <- as.numeric(stats::predict(en.t, newx = tt1,
                                                    type = "class"))[1:nrow(test.x)]
      }
    }

    try(grid$Accuracy[j] <- loss.bin(pred = yval, true_y = y, loss = "class"))
    try(grid$AUC[j] <- loss.bin(pred = yval, true_y = y, loss = "auc"))

    t2 <- Sys.time()
    grid$Time[j] <- as.numeric(t2 - t1, units = "secs")
  }
  grid
}


en.reg.results <- function(x, y, grid, cross) {

  x <- as.matrix(dummy(x))
  yval <- rep(0, nrow(x))
  xvs <- rep(1:cross, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, cross)

  for(j in 1:nrow(grid)) {
    t1 <- Sys.time()
    yval <- rep(NA, nrow(dat))
    xvs <- rep(1:cross, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train.x <- x[xvs != i, ]
      train.y <- y[xvs != i]
      test.x <- x[xvs == i, ]
      test.y <- y[xvs == i]
      tt1 <- rbind(test.x, train.x)
      try(en.t <- glmnet::glmnet(train.x, train.y, family = "gaussian",
                                 alpha = grid$Alpha[j], lambda = exp(grid$logLambda[j])))
      if(!is.null(en.t)) {
        yval[xvs == i] <- as.numeric(stats::predict(en.t, newx = tt1,
                                                    type = "response"))[1:nrow(test.x)]
      }
    }

    try(grid$MSE[j] <- loss.reg(pred = yval, true_y = y, loss = "mse"))
    try(grid$MAE[j] <- loss.reg(pred = yval, true_y = y, loss = "mae"))

    t2 <- Sys.time()
    grid$Time[j] <- as.numeric(t2 - t1, units = "secs")
  }
  grid
}


#==============================================================================
#                              adaboost Functions
#==============================================================================

ada.bin.results <- function(x, y, grid, cross) {

  y <- as.numeric(as.factor(y)) - 1
  dat <- as.data.frame(cbind(y, x))

  for(j in 1:nrow(grid)) {
    t1 <- Sys.time()
    yval <- rep(NA, nrow(dat))
    xvs <- rep(1:cross, length = nrow(dat))
    xvs <- sample(xvs)
    for(i in 1:cross) {
      train <- dat[xvs != i, ]
      test <- dat[xvs == i, ]
      tt1 <- rbind(test, train)
      try(ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential",
                        nu = grid$Nu[j], iter = grid$Iter[j], data = train,
                        control = rpart::rpart.control(maxdepth = grid$Maxdepth[j])))
      if(!is.null(ada.t)) {
        yval[xvs == i] <- stats::predict(ada.t, newdata = tt1, type = "prob")[1:nrow(test),2]
      }
    }

    try(grid$Accuracy[j] <- loss.bin(pred = yval, true_y = y, loss = "class"))
    try(grid$AUC[j] <- loss.bin(pred = yval, true_y = y, loss = "auc"))

    t2 <- Sys.time()
    grid$Time[j] <- as.numeric(t2 - t1, units = "secs")
  }
  grid
}








