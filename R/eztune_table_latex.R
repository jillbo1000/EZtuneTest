#' Makes a table that can be pasted directly into latex
#'
#' eztune_table_latex creates a table created with eztune_table
#' that can be pasted directly into latex. Only the horizontal
#' lines need to be added.
#' @param tab A table created with eztune_table.
#' @param vert_lines If TRUE, vertical lines will be placed between the
#' columns. If FALSE, no vertical lines will appear in the table.
#' @return Returns a character vector that contains the header and the
#' lines for a latex table version of an eztune_table. Commands to
#' end the table and horizontal line commands need to be added later.
#'
#' @seealso \code{\link{eztune_table}}
#'
#' @export
#'

eztune_table_latex <- function(tab, vert_lines = TRUE, sideways = TRUE) {

  if(sideways) lat1 <- "\\begin{sidewaystable}[ph!]" else lat1 <- "\begin{table}[ht]"

  tmp <- rep("l", ncol(tab))

  if(vert_lines) {
    tmp <- paste(rep("l", ncol(tab)), collapse = "|")
    tmp <- paste0("|", tmp, "|")
  } else {
    tmp <- paste(rep("l", ncol(tab)), collapse = "")
  }

  lat1 <- c(lat1, paste0("\\begin{tabular}{", tmp, "}"))

  tab <- rbind(colnames(tab), tab)

  lat2 <- NULL
  for(i in 1:nrow(tab)) {
    tmp <- paste(tab[i, ], collapse = " & ")
    lat2 <- c(lat2, paste(tmp, "\\\\"))
  }


  c(lat1, lat2)
}
