#------------------------------------------------------------------------------
#                       loss function for binary data
#------------------------------------------------------------------------------


loss.bin <- function(pred, true_y, loss) {
  if(loss == "class") {
    mean(round(pred) == true_y)
  } else if (loss == "auc") {
    predA <- ROCR::prediction(pred, true_y)
    unlist(ROCR::performance(predA, "auc")@y.values)
  } else if (loss == "deviance") {
    stop("deviance only valid for elastic net - choose 'class' or 'auc' for loss")
  } else {
    stop("invalid optimization criterion - choose 'default', 'class', or 'auc' for loss")
  }
}


#------------------------------------------------------------------------------
#                       loss function for continuous data
#------------------------------------------------------------------------------


loss.reg <- function(pred, true_y, loss) {
  if(loss == "mse") {
    mean((pred - true_y)^2)
  } else if (loss == "mae") {
    mean(abs(pred - true_y))
  } else {
    stop("invalid optimization criterion - choose 'default', 'mse', or 'mae' for loss")
  }
}
