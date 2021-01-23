#' Test function for data.table
#' #'
#' @param a data.frame
#'
#' @return data.table
#' @export
#'
#' @importFrom data.table :=
#'
#' @examples
#' \dontrun{zdt(tab)}
zdt = function(a){
  id = NULL
  new = data.table::setDT(a)
  new[, id := 'hallo']
  return (new)
}
