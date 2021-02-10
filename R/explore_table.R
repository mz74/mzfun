#' Summary statistics and plots of the table columns.
#'
#' @param ftable data.frame or data.table
#' @param ftarget string: columname of ftable including the target.
#' Optional. Use ftarget = NA to explore the table without target column.
#' @param fassign_classes TRUE or FALSE. TRUE assigns a new class to each column.
#' @param fadd_tables TRUE or FALSE. Add or not add a table.
#' @param fadd_plots TRUE or FALSE. Add or not add a plot.
#' @param fprm_na TRUE or FALSE. Remove or not remove NAs before plotting.
#' @param fpmax_numlevels Integer. If number of numerics is smaller, then plot
#' bar plot instead of line plot.
#' @param fpmax_faclevels Integer. Maximum number of bars to be plotted for each segment.
#'
#' @return list with several elements:
#' _summary_: the summary statistics.
#' _plot$name$count_: ggplot of absolute frequencies of property _name_.
#' _plot$name$rel_: ggplot of relative frequencies of property _name_.
#'
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
                         fadd_tables = TRUE,
                         fadd_plots = FALSE,
                         fprm_na = FALSE,
                         fpmax_numlevels = 0,
                         fpmax_faclevels = 50
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
  ftable = copy(ftable)
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
        #ftable[, c(i) := as.numeric(..i)]
      } else {
        ftable[, c(i) := factor(get(i))]
        #ftable[, c(i) := factor(..i)]
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
    dempty_rel = 100*signif(dempty/dnumber, 3)
    dempty_rel = format(dempty_rel, big.mark   = '.', decimal.mark = ',')
    dempty_rel = paste0(dempty_rel, '%')
    dmean   = ''
    dmedian = ''
    if (class(ftable[[i]]) %in% c('numeric', 'integer')){
      dmean   = ftable[, mean(get(i), na.rm = TRUE)]
      dmean   = ifelse(dmean >= 100, round(dmean, 0), signif(dmean, 3))
      dmean   = format(dmean, big.mark   = '.', decimal.mark = ',')
      dmedian = ftable[, stats::median(get(i), na.rm = TRUE)]
      dmedian   = ifelse(dmedian >= 100, round(dmedian, 0), signif(dmedian, 3))
      dmedian   = format(dmedian, big.mark   = '.', decimal.mark = ',')
    }
    d1 = data.table(
      column = i,
      class  = class(ftable[[i]]),
      number = format(dnumber, big.mark   = '.', decimal.mark = ','),
      levels = format(dlevels, big.mark   = '.', decimal.mark = ','),
      examples = dexamples,
      `empty rows abs` = format(dempty, big.mark   = '.', decimal.mark = ','),
      `empty rows rel` = dempty_rel,
      mean = dmean,
      median = dmedian
    )
    dtable = rbind(dtable, d1)
  }

  res = list(summary = dtable)

  # ----------------------------------------------------------------------
  # tables
  # ----------------------------------------------------------------------

  i = dcols[2]
  if (fadd_tables == TRUE){
    for (i in dcols){

      # remove or not remove NAs
      dftable = ftable[, .(x = get(i), dummy__target)]
      if (fprm_na == TRUE){
        dftable = dftable[!is.na(x)][x != ''][!is.na(dummy__target)][dummy__target != '']
      }
      dftable[x == '', x := NA]

      if (class(ftable[[i]]) %in% c('numeric', 'integer')){
        dplotclass = 'numeric'
      } else {
        dplotclass = 'factor'
      }

      dlevels = dftable[!is.na(x)][!duplicated(x), .N]

      # create steps
      if (dplotclass == 'numeric' & dlevels > fpmax_numlevels){
        dtab = dftable[, .(x, dummy__target)]
        dmax = max(dtab$x, na.rm = TRUE)
        dmin = min(dtab$x, na.rm=TRUE)
        dscale = dmax - dmin
        dtab[, x2 := (x - dmin)/dscale]
        dtab[, x3 := x2*fpmax_numlevels]
        dtab[, xmin := floor(x3)]
        dtab[, xmax := xmin+1]
        dtab[, xmin := xmin*dscale/fpmax_numlevels + dmin]
        dtab[, xmax := xmax*dscale/fpmax_numlevels + dmin]
        dtab[, new_x := paste0(xmin, ' - ', xmax)]
        d2levels = dtab[order(x)][!duplicated(new_x)][, new_x]
        dftable = dtab[, .(x=factor(new_x, levels = d2levels), dummy__target)]
      }

      dtab1 = dftable[, .(Anzahl = .N), by=.(tar1 = x, tar2 = dummy__target)]
      #dtab1 = dtab1[, ':=' (Relativ = Anzahl/sum(Anzahl)), by=.(tar2)]
      #dtab1 = dtab1[, ':=' (Anteil = Anzahl/sum(Anzahl)), by=.(tar1)]
      dtab2 = data.table::dcast(dtab1, tar1~tar2, value.var=c('Anzahl'))
      #dtab3 = data.table::dcast(dtab1, tar1~tar2, value.var=c('Relativ'))
      #dtab4 = data.table::dcast(dtab1, tar1~tar2, value.var=c('Anteil'))
      dtab = dtab1[, .(Gesamt = sum(Anzahl)), by=.(tar1)][, Relativ := Gesamt/sum(Gesamt)]

      # format
      dtab[, Gesamt := format(Gesamt, big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')]
      dtab[, Relativ := format(signif(100*Relativ, 3), big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')]
      dtab[, Relativ := paste0(Relativ, '%')]

      d2cols = names(dtab2)[-1]
      dtab2[, c(d2cols) := lapply(.SD, function(x) {format(x, big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')}), .SDcols = d2cols]

      if (!is.na(ftarget)){
        dtab = merge(dtab, dtab2, by='tar1')
      }
      dtab[dtab == 'NA - NA'] = ''
      dtab[dtab == 'NA'] = ''
      names(dtab)[names(dtab) == 'tar1'] = i
      res$table[[i]] = dtab
    }

  }

  # ----------------------------------------------------------------------
  # plots
  # ----------------------------------------------------------------------
  #i = dcols[3]
  if (fadd_plots == TRUE){
    for (i in dcols){
      # remove or not remove NAs
      dftable = ftable[, .(x = get(i), dummy__target)]
      if (fprm_na == TRUE){
        dftable = dftable[!is.na(x)][x != ''][!is.na(dummy__target)][dummy__target != '']
      }
      dftable[x == '', x := NA]

      if (class(ftable[[i]]) %in% c('numeric', 'integer')){
        dplotclass = 'numeric'
      } else {
        dplotclass = 'factor'
      }

      dlevels = dftable[!is.na(x)][!duplicated(x), .N]

      # -------------------
      # plot numeric with high number of levels
      # -------------------

      if (dplotclass == 'numeric' & dlevels > fpmax_numlevels){
        dtab = dftable[, .(x, dummy__target)]

        dplot_count = ggplot(data=dtab, aes(x, fill=dummy__target)) + geom_histogram(position="dodge") +
          guides(fill=guide_legend(ftarget)) + ylab('Anzahl') +
          scale_y_continuous(labels = function(x) format(x, big.mark = '.', decimal.mark = ',')) +
          scale_x_continuous(labels = function(x) format(x, big.mark = '.', decimal.mark = ',')) +
          ggtitle(paste0(i, ' - Absolute H\u00e4ufigkeiten')) +
          scale_fill_hue(c=50, l= 60, h=c(60, 220))

        dplot_rel = ggplot(data=dtab, aes(x, color=dummy__target)) + geom_density() +
          guides(color=guide_legend(ftarget))+ ylab('Dichte') +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(labels = function(x) format(x, big.mark = '.', decimal.mark = ',')) +
          ggtitle(paste0(i, ' - Dichteverteilung je Segment')) +
          scale_color_hue(c=50, l= 60, h=c(60, 220))
      } else {
        dtab = dftable[, .(count = .N), by=.(dummy__target, col=x)][, rel := count/sum(count), by=.(dummy__target)]
        # reduce number of levels
        d2levels = length(unique(dtab$col))
        if (d2levels > fpmax_faclevels){
          dtab = dtab[order(count, decreasing = TRUE)]
          d3levels = unique(dtab$col)[1:fpmax_faclevels]
          dtab = dtab[col %in% d3levels]
        }


        # -------------------
        # plot numeric with low number of levels
        # -------------------
        if (dplotclass == 'numeric') {
          dtab = dtab[order(col)]
          dtab[, col := factor(col, levels = unique(dtab$col))]
        } else {
          # -------------------
          # plot factor
          # -------------------
          dtab = dtab[order(count)]
          dtab[, col := factor(col, levels = unique(dtab$col))]
        }
        dplot_count = ggplot(data=dtab, aes(x=col, y=count, fill=dummy__target)) +
          geom_bar(stat='identity', position = position_dodge2()) +
          coord_flip()+ guides(fill=guide_legend(ftarget)) + ylab('Anzahl') +
          scale_y_continuous(labels = function(x) format(x, big.mark = '.', decimal.mark = ',')) +
          ggtitle(paste0(i, ' - Absolute H\u00e4ufigkeiten')) +
          scale_fill_hue(c=50, l= 60, h=c(60, 220))

        dplot_rel = ggplot(data=dtab, aes(x=col, y=rel, fill=dummy__target)) +
          geom_bar(stat='identity', position = position_dodge2()) +
          coord_flip() + guides(fill=guide_legend(ftarget)) + ylab('Anteil') +
          scale_y_continuous(labels = function(x) paste0(format(100*x, big.mark = '.', decimal.mark = ','), '%')) +
          ggtitle(paste0(i, ' - Bedingte H\u00e4ufigkeiten je Segment'))+
          scale_fill_hue(c=50, l= 60, h=c(60, 220))

      }
      res$plot[[i]]$count = dplot_count + xlab(i) + theme_minimal()
      res$plot[[i]]$rel = dplot_rel + xlab(i) + theme_minimal()

      # remove legende
      if (is.na(ftarget)){
        res$plot[[i]]$count = res$plot[[i]]$count + theme(legend.position = "none")
        res$plot[[i]]$rel   = res$plot[[i]]$rel   + theme(legend.position = "none")
      }

    }
  }
  return(res)
}
