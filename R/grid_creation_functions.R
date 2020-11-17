# Contains all of the functions to create the grids using the regions I tried.

# Create a grid for svm binary models
svm.bin.grid <- function() {
  param <- NULL
  for(i in -10:25) {
    param <- rbind(param, cbind(i, -25:10, NA, NA, NA))
  }
  colnames(param) <- c("Cost", "Gamma", "Accuracy", "AUC", "Time")
  param
}

# Create a grid for svm regression models
svm.reg.grid <- function() {
  eps <- seq(0, 1, 0.1)
  param <- NULL
  for(i in -10:25) {
    for(j in 1:length(eps)) {
      param <- rbind(param, cbind(i, -25:10, eps[j], NA, NA, NA))
    }
  }
  colnames(param) <- c("Cost", "Gamma", "Epsilon", "MSE", "MAE", "Time")
  param
}

# Create a grid for gbm binary models
gbm.bin.grid <- function() {
  p <- NULL
  for(i in 1:40) {
    p <- rbind(p, cbind(i * 500, 5:15))
  }

  p2 <- NULL
  for(i in 1:3) {
    p2 <- rbind(p2, cbind(p, 10^(-i)))
  }
  head(p2)
  tail(p2)

  param <- NULL
  for(i in 1:10) {
    param <- rbind(param, cbind(p2, (2 * i - 1), NA, NA, NA))
  }

  colnames(param) <- c("NumTrees", "MinNode", "Shrinkage", "IntDepth", "Accuracy", "AUC", "Time")
  param
}

# Create a grid for gbm regression models
gbm.reg.grid <- function() {
  p <- NULL
  for(i in 1:40) {
    p <- rbind(p, cbind(i * 50, 5:12))
  }

  p2 <- NULL
  for(i in 1:3) {
    p2 <- rbind(p2, cbind(p, -i))
  }
  head(p2)
  tail(p2)

  param <- NULL
  for(i in 1:10) {
    param <- rbind(param, cbind(p2, (2 * i - 1), NA, NA, NA))
  }

  colnames(param) <- c("NumTrees", "MinNode", "Shrinkage", "IntDepth", "MSE", "MAE", "Time")
  param
}

# Create a grid for en binary models
en.bin.grid <- function(x = x, y = y) {

  x <- dummy(x)
  y <- as.numeric(as.factor(y)) - 1
  lmin <- log(min(glmnet::cv.glmnet(as.matrix(x), y, family = "binomial", alpha = 1)$lambda))
  lmax <- log(max(glmnet::cv.glmnet(as.matrix(x), y, family = "binomial", alpha = 0)$lambda))

  alpha <- seq(0, 1, length.out = 500)
  lambda <- seq(lmin, lmax, length.out = 500) # Small lambdas - log(lambda)

  # Grid
  param <- NULL
  for(i in 1:length(lambda)) {
    param <- rbind(param, cbind(alpha, lambda[i], NA, NA, NA))
  }

  colnames(param) <- c("Alpha", "logLambda", "Accuracy", "AUC", "Time")
  param
}


# Create a grid for en regression models
en.reg.grid <- function(x = x, y = y) {

  x <- dummy(x)
  lmin <- log(min(glmnet::cv.glmnet(as.matrix(x), y, family = "gaussian", alpha = 1)$lambda))
  lmax <- log(max(glmnet::cv.glmnet(as.matrix(x), y, family = "gaussian", alpha = 0)$lambda))

  alpha <- seq(0, 1, length.out = 500)
  lambda <- seq(lmin, lmax, length.out = 500) # Small lambdas - log(lambda)

  # Grid
  param <- NULL
  for(i in 1:length(lambda)) {
    param <- rbind(param, cbind(alpha, lambda[i], NA, NA, NA))
  }

  colnames(param) <- c("Alpha", "logLambda", "MSE", "MAE", "Time")
  param
}

# Create a grid for ada binary models
ada.bin.grid <- function() {

  tmp <- c(0.01, 0.1, 0.3, 0.5, 0.7, 1)
  p <- NULL
  for(i in 1:length(tmp)) {
    p <- rbind(p, cbind(tmp[i], seq(100, 1400, 100)))
  }

  p2 <- NULL
  tmp2 <- c(1:6, 8, 10, 12, 14, 16, 18, 20)
  for(i in 1:length(tmp2)) {
    p2 <- rbind(p2, cbind(p, tmp2[i]))
  }
  head(p2)
  tail(p2)

  param <- NULL
  for(i in 1:10) {
    param <- rbind(param, cbind(p2, NA, NA, NA))
  }

  colnames(param) <- c("Nu", "Iter", "Maxdepth", "Accuracy", "AUC", "Time")
  param

}


