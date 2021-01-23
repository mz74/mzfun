#' Adds
#'
#' @param ftable data.frame or data.table
#' @param ffun aggregation function such as sum, mean, etc.
#' Default value is sum.
#' @param fname named vector c(a='b'), where a is a column name
#' in ftable and b the assigned cell name of the aggregation.
#' E.g. fname = c(Object = 'Result'). Default value is NA.
#'
#' @return data.frame or data.table
#' @export
#'
#' @import data.table
#'
#' @examples
#' \dontrun{add_table_bottom(dt)}
add_table_bottom = function(ftable, ffun = sum, fname=NA){

  # store original class
  dclass = class(ftable)

  # data.table
  setDT(ftable)

  # helper function
  dfun = function(dx){
    if (class(dx) %in% c('numeric', 'integer')) return (ffun(dx, na.rm = TRUE))
    return (NA)
  }

  # calc aggregation
  dtable = ftable[, lapply(.SD, function(x) dfun(x))]

  # apply fname
  if (!is.na(fname)){
    dtable[, c(names(fname)) := fname]
  }

  # bind tables
  dtable = rbind(ftable, dtable)

  # retrieve class
  class(dtable) = dclass
  class(ftable) = dclass

  return(dtable)
}

