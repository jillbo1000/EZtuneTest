#' Makes a data.frame that contains the best error rates from a
#' grid search
#'
#' get_best_grid creates a data.frame that has the datasets in
#' the first column and the best error rate obtained in the grid
#' search in the second column.
#' @param grid_data data.frame obtained from get_grid_data or with
#' several datasets from get_grid_data combined with rbind.
#' @return Returns a data.frame with the names of the datasets
#' in the first column and the best loss value in the second
#' column. The first column is named "Data" and the second column
#' is named "Best"
#'
#' @seealso \code{\link{get_grid_data}}, \code{\link{eztune_table}},
#' \code{\link{grid_search}},
#'
#' @export
#'

get_best_grid <- function(grid_data) {

  best <- dplyr::group_by(grid_data, Data) %>%
    dplyr::summarize(Best = min(Loss, na.rm = TRUE))

  as.data.frame(best)

}
