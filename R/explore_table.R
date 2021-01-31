#' Summary statistics and plots of the table columns.
#'
#' @param ftable data.frame or data.table
#' @param ftarget string: columname of ftable including the target.
#' Optional. Use ftarget = NA to explore the table without target column.
#' @param fassign_classes TRUE or FALSE. TRUE assigns a new class to each column.
#' @param fadd_plots TRUE or FALSE. Add or not add a plot.
#' @param fprm_na TRUE or FALSE. Remove or not remove NAs before plotting.
#' @param fpmax_numlevels Integer. If number of numerics is smaller, then plot
#' bar plot instead of line plot.
#'
#' @return data.frame or data.table of the summary statistics.
#' @export
#'
#' @import ggplot2
#' @import data.table
#'
#' @examples
#' \dontrun{explore_table(dt)}
explore_table = function(ftable,
                         ftarget = NA,
                         fassign_classes = TRUE,
                         fadd_plots = FALSE,
                         fprm_na = FALSE,
                         fpmax_numlevels = 0
){

  # prepare
  dummy__target = NULL
  x = NULL
  target = NULL
  count = NULL
  . = NULL

  # store original class
  dclass = class(ftable[0])

  # data.table
  setDT(ftable)

  # columns to explore
  dcols = names(ftable[0])

  # target column
  if (is.na(ftarget)){
    ftable[, dummy__target := 'target']
  } else {
    ftable[, dummy__target := copy(get(ftarget))]
  }
  ftable[dummy__target == '', dummy__target := NA]
  ftable[, dummy__target := factor(dummy__target)]

  ### assign classes numeric or factor
  # i = dcols[5]
  if (fassign_classes == TRUE){
    for (i in dcols){
      ftable[, c(i) := as.character(get(i))]
      # check for numeric
      d1 = ftable[get(i) != ''][!is.na(get(i)), get(i)]
      d2 = suppressWarnings(as.numeric(d1))
      d3 = d2[!is.na(d2)]
      if (length(d1) == length(d3)){

        ftable[, c(i) := as.numeric(get(i))]
      } else {
        ftable[, c(i) := factor(get(i))]
      }
    }
  }

  ### calc summary
  dtable = data.table()
  #i = dcols[3]
  for (i in dcols){
    # numeric and integer summary

    dnumber = nrow(ftable)
    dlevels = ftable[!is.na(get(i))][!duplicated(get(i)), .N]
    dexamples = ftable[!is.na(get(i))][!duplicated(get(i)), get(i)][1:(min(dlevels, 3))]
    dexamples = paste(dexamples, collapse = ', ')
    dempty  = ftable[is.na(get(i)) | get(i) == '', .N]
    dmean   = ''
    dmedian = ''
    if (class(ftable[[i]]) %in% c('numeric', 'integer')){
      dmean   = ftable[, mean(get(i), na.rm = TRUE)]
      dmedian = ftable[, stats::median(get(i), na.rm = TRUE)]
    }
    d1 = data.table(
      column = i,
      class  = class(ftable[[i]]),
      number = dnumber,
      levels = dlevels,
      examples = dexamples,
      `empty rows abs` = dempty,
      `empty rows rel` = dempty / dnumber,
      mean = dmean,
      median = dmedian
    )
    dtable = rbind(dtable, d1)
  }

  res = list(summary = dtable)

  ### plots
  i = dcols[3]
  if (fadd_plots == TRUE){
    for (i in dcols){
      # remove or not remove NAs
      dftable = ftable[, .(x = get(i), dummy__target)]
      if (fprm_na == TRUE){
        dftable = dftable[!is.na(x)][x != ''][!is.na(dummy__target)][dummy__target != '']
      }
      dftable[x == '', c(i) := NA]

      if (class(ftable[[i]]) %in% c('numeric', 'integer')){
        dplotclass = 'numeric'
      } else {
        dplotclass = 'factor'
      }

      dlevels = dftable[!is.na(x)][!duplicated(x), .N]

      if (dplotclass == 'numeric' & dlevels > fpmax_numlevels){
        dtab = dftable[, .(x, dummy__target)]

        dplot_count = ggplot(data=dtab, aes(x, color=dummy__target)) + geom_density() + guides(color=guide_legend(ftarget))
        dplot_rel = ggplot(data=dtab, aes(x, color=dummy__target)) + geom_density() + guides(color=guide_legend(ftarget))
        #print(i)
        #print(dplot_count)
      } else {
        dtab = dftable[, .(count = .N), by=.(dummy__target, col=x)][, rel := count/sum(count), by=.(dummy__target)]
        if (dplotclass == 'numeric') {
          dtab = dtab[order(col)]
          dtab[, col := factor(col, levels = unique(dtab$col))]
        } else {
          dtab = dtab[order(count)]
          dtab[, col := factor(col, levels = unique(dtab$col))]
        }
        dplot_count = ggplot(data=dtab, aes(x=col, y=count, fill=dummy__target)) +
          geom_bar(stat='identity', position = position_dodge2()) +
          coord_flip()+ guides(fill=guide_legend(ftarget))
        dplot_rel = ggplot(data=dtab, aes(x=col, y=rel, fill=dummy__target)) +
          geom_bar(stat='identity', position = position_dodge2()) +
          coord_flip() + guides(fill=guide_legend(ftarget))
      }
      res$plot[[i]]$count = dplot_count + xlab(i) + theme_minimal() +
        ggtitle(paste0(i, ' - absolute values'))
      res$plot[[i]]$rel = dplot_rel + xlab(i) + theme_minimal()+
        ggtitle(paste0(i, ' - relative values'))

      # remove legende
      if (is.na(ftarget)){
        res$plot[[i]]$count = res$plot[[i]]$count + theme(legend.position = "none")
        res$plot[[i]]$rel   = res$plot[[i]]$rel   + theme(legend.position = "none")
      }

    }
  }
  return(res)
}
