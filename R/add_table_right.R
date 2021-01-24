#' Add aggregation column to table
#'
#' @param ftable data.frame or data.table
#' @param ffun aggregation function such as sum, mean, etc.
#' Default value is sum.
#' @param colname string. Default value is 'Result'.
#' @param fcols vector. Column names to be included in the
#' aggregation (only if numeric). Default value is NA, i.e. all
#' columns are included.
#'
#' @return data.frame or data.table
#' @export
#'
#' @import data.table
#'
#' @examples
#' #' \dontrun{add_table_right(dt)}
add_table_right = function(ftable, ffun = sum, colname = 'Result', fcols = NA){
  # store original class
  dclass = class(ftable[0])

  # data.table
  setDT(ftable)

  # columns to consider
  # calc class
  dtable = ftable[, sapply(.SD, class)]
  dnames = names(dtable)[dtable %in% c('numeric', 'integer')]

  # select columns
  if (!is.na(fcols[1])){
    dnames = dnames[dnames %in% fcols]
  }

  # calc aggregation
  ftable = ftable[, c(colname) := apply(.SD, 1, ffun, na.rm = TRUE), .SDcols = dnames]

  # retrieve class
  class(ftable) = dclass

  return (ftable)
}
