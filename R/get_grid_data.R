#' Makes a data.frame that contains the best error rates from a
#' grid search
#'
#' get_grid_data creates a data.frame that has the datasets in
#' the first column and the best error rate obtained in the grid
#' search in the second column.
#' @param grid_dat data.frame obtained from get_grid_data or with
#' several datasets from get_grid_data combined with rbind.
#' @return Returns a data.frame with the names of the datasets
#' in the first column and the best error rate in the second
#' column. The first column is named "Data" and the second column
#' is named "Best"
#'
#' @seealso \code{\link{grid_search}}, \code{\link{eztune_table}},
#' \code{\link{grid_search}},
#'
#' @export
#'

get_grid_data <- function(path, ...) {

}
