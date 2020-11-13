#' Makes a line plot of the results from the optimization tests
#'
#' opt_plot creates a line plot with the results of the optimization
#' tests.
#' @param dat Dataset created from load_opt_data or with several
#' datasets from load_opt_data combined with rbind.
#' @param value Specifies which error metric should be plotted.
#' This should be the name of a variable in the dataset, dat.
#' @param facet Specifies a faceting variable. The typical faceting
#' variable is optimizer, but other options include data or method.
#' @param cols A color palette to use with the data. If it is not
#' specified, the Dark2 palette from color brewer will be used.
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

opt_dot_plot <- function(dat, value, facet = NULL, cols = NULL) {

  dat$data <- as.character(dat$data)
  datasets <- unique(dat$data)

  dat2 <- dplyr::group_by(dat, data, method, optimizer, fast, cross, loss_type) %>%
    summarize(loss = mean(loss, na.rm = TRUE),
              loss_mse_acc_10 = mean(loss_mse_acc_10, na.rm =TRUE),
              loss_mae_auc_10 = mean(loss_mae_auc_10, na.rm =TRUE)) %>%
    dplyr::ungroup()

  colnames(dat2)[colnames(dat) == value] <- "value"

  dat2 <- dplyr::group_by(dat2, data) %>%
    mutate(ErrStd = ((value - min(value, na.rm = TRUE)) /
                       (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))),
           missing = ifelse(is.na(value), "Yes", "No"),
           value = ifelse(is.na(value), 1, value)) %>%
    dplyr::ungroup()

  if(facet == "optimizer") {
    dat2$opt <- ifelse(dat2$fast != 0,
                       paste("fast", dat2$fast, dat2$loss_type, sep = "_"),
                       paste("CV", dat2$cross, dat2$loss_type, sep = "_"))
  } else if(facet == "loss_type") {
    dat2$opt <- ifelse(dat2$fast != 0,
                       paste("fast", dat2$fast, dat2$optimizer, sep = "_"),
                       paste("CV", dat2$cross, dat2$optimizer, sep = "_"))
  } else if(facet == "method") {
    dat2$opt <- ifelse(dat2$fast != 0,
                       paste("fast", dat2$fast, dat2$loss_type, sep = "_"),
                       paste("CV", dat2$cross, dat2$loss_type, sep = "_"))
  }

  dat2$missing <- factor(dat2$missing, levels = c("Yes", "No"))
  dat2 <- dat2[order(dat2$opt), ]

  if(is.null(cols)) {
    cols <- RColorBrewer::brewer.pal(length(unique(dat2$data)), "Dark2")
  }

  dat3 <- dplyr::select(dat2, data, optimizer, fast, cross, loss_type, opt, ErrStd) %>%
    tidyr::pivot_wider(names_from = optimizer, values_from = ErrStd)

  ggplot2::ggplot(dat3, aes(x = ga, y = hjn)) +
    geom_point(aes(color = opt)) +
    # scale_color_manual("", values = brewer.pal(12, "Dark2")) +
    theme_bw()

  # Note that alpha makes the lines disappear in the graph view window, but
  # they are there when you open a zoom window or save to a file.
  g1 <- ggplot2::ggplot(dat2, ggplot2::aes(x = opt, y = ErrStd, group = factor(data),
                                           color = factor(data), shape = missing)) +
    ggplot2::geom_point(show.legend = FALSE) +
    ggplot2::geom_line(ggplot2::aes(color = factor(data)), alpha = 0.7, lwd = 1.2) +
    ggplot2::scale_color_manual(name = "", values = cols)

  if(!is.null(facet)) {
    colnames(dat2)[colnames(dat2) == facet] <- "facet"
    g1 <- g1 + facet_wrap(~dat2$facet, ncol = 1)
  }

  g1 <- g1  +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  g1
}
