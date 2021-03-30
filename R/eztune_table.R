#' Makes a table of performance results
#'
#' eztune_table creates a table that contains the average error rates
#' and run times for a series of eztune runs and compares them to
#' the best results obtained from a grid search.
#' @param dat Dataset created from load_opt_data or with several
#' datasets from load_opt_data combined with rbind. Information
#' from this dataset will be used to populate the majority of
#' the table.
#' @param value the value from dat that should be used as the
#' error metric. This will typically be either loss_mse_acc_10 or
#' loss_mae_auc_10
#' @param best_grid A dataset that contains the best error rate
#' obtained from the grid search. The first column must contain
#' the names of the datasets used to populate the tables. The
#' names must be identical to those in the dat object. The second
#' column is numeric and contains the best error measure obtained
#' by the grid search for that dataset. If it is omitted the table will
#' be made without the grid information. The function get_best_grid
#' can be used to create
#' @return Returns a data.frame with the performance metrics from the
#' EZtune tests. Each column is a character vector. This data.frame
#' can be passed to the function eztune_table_latex to create a latex
#' version of the table.
#'
#' @seealso \code{\link{load_opt_data}}, \code{\link{get_best_grid}},
#' \code{\link{get_grid_data}}, \code{\link{eztune_table_latex}}
#'
#' @export
#'

eztune_table <- function(dat, value = "loss_mse_acc_10", best_grid = NULL) {

  data_sets <- unique(dat$data)[order(unique(dat$data))]

  colnames(dat)[colnames(dat) == value] <- "value"

  summ <- dplyr::group_by(dat, data, optimizer, fast, cross) %>%
    dplyr::summarize(mean = mean(value, na.rm = TRUE),
                     time = mean(time, na.rm = TRUE))
  summ$round <- rep(NA, nrow(summ))
  summ$int <- rep(NA, nrow(summ))
  for(i in 1:nrow(summ)) {
    if(!is.na(summ$mean[i])) {
      if(summ$mean[i] > 999) {
        summ$round[i] <- as.character(signif(summ$mean[i], digits = 3))
      } else if(summ$mean[i] > 99) {
        summ$round[i] <- stringr::str_trunc(as.character(summ$mean[i]), width = 3, side = "right", ellipsis = "")
      } else if(summ$mean[i] > 1) {
        summ$round[i] <- stringr::str_trunc(as.character(summ$mean[i]), width = 4, side = "right", ellipsis = "")
      } else {
        summ$round[i] <- stringr::str_trunc(as.character(summ$mean[i]), width = 5, side = "right", ellipsis = "")
      }
      summ$int[i] <- paste("(", summ$round[i], ", ", round(summ$time[i]), "s)",
                           sep = "")
    }
  }

  summ$optimizer <- ifelse(summ$optimizer == "ga", "Genetic algorithm", "Hooke-Jeeves")
  summ$method <- ifelse(summ$fast != 0, paste0("Fast = ", summ$fast), NA)
  summ$method <- ifelse(summ$method == "Fast = 1", "Fast = TRUE", summ$method)
  summ$method <- ifelse(summ$cross != "resub" & summ$fast == 0, paste0("CV = ", summ$cross), summ$method)
  summ$method <- ifelse(summ$cross == "resub", "Resub", summ$method)

  summ <- dplyr::ungroup(summ) %>%
    dplyr::select(data, optimizer, method, int)
  newtab <- tidyr::pivot_wider(summ, names_from = data, values_from = int)

  if(!is.null(best_grid)) {
    newtab <- rbind(newtab, c("Best Grid", "", rep(NA, ncol(newtab) - 2)))

    for(i in 1:nrow(best_grid)) {
      if(best_grid[i, 2] > 999) {
        value <- as.character(signif(best_grid[i, 2], digits = 3))
      } else if(best_grid[i, 2] > 99) {
        value <- stringr::str_trunc(as.character(best_grid[i, 2]), width = 3, side = "right", ellipsis = "")
      } else {
        value <- stringr::str_trunc(as.character(best_grid[i, 2]), width = 4, side = "right", ellipsis = "")
      }

      newtab[nrow(newtab), colnames(newtab) == best_grid[i, 1]] <- value
    }
  }

  colnames(newtab)[1:2] <- c("Optimizer", "Type")
  newtab
}
