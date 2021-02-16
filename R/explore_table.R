#' Summary statistics and plots of the table columns.
#'
#' @param ftable data.frame or data.table
#' @param ftarget string: columname of ftable including the target.
#' Optional. Use ftarget = NA to explore the table without target column.
#' @param fassign_classes TRUE or FALSE. TRUE assigns a new class to each column.
#' @param fadd_tables TRUE or FALSE. Add or not add a table.
#' @param fadd_plots TRUE or FALSE. Add or not add a plot.
#' @param fprm_na TRUE or FALSE. Remove or not remove NAs before plotting.
#' @param fptext_labels TRUE or FALSE. Add or not add text labels to bar plots.
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
                         fptext_labels = FALSE,
                         fpmax_numlevels = 0,
                         fpmax_faclevels = 20
){

  # prepare
  dummy__target = NULL
  x             = NULL
  new_x         = NULL
  target        = NULL
  count         = NULL

  Anzahl        = NULL
  Anteil        = NULL
  Absolut       = NULL
  Relativ       = NULL
  tar1          = NULL
  tar2          = NULL

  text1         = NULL
  text2         = NULL
  pos1          = NULL
  pos2          = NULL

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

  i = dcols[1]
  if (fadd_tables == TRUE){
    for (i in dcols){

      # remove or not remove NAs
      dftable = ftable[, .(x = get(i), dummy__target)]
      if (fprm_na == TRUE){
        dftable = dftable[!is.na(x)][x != ''][!is.na(dummy__target)][dummy__target != '']
      }
      dftable[x == '', x := NA]

      # is it numeric or factor ??
      if (class(ftable[[i]]) %in% c('numeric', 'integer')){
        dplotclass = 'numeric'
      } else {
        dplotclass = 'factor'
        dftable[is.na(x), x := 'NA']  # new level
      }

      # nb of levels
      dlevels = dftable[!is.na(x)][!duplicated(x), .N]

      # create steps
      if (dplotclass == 'numeric' & dlevels > fpmax_numlevels){
        dtab = dftable[, .(x, dummy__target)]
        dmax = max(dtab$x, na.rm = TRUE)
        dmin = min(dtab$x, na.rm=TRUE)
        dpretty = pretty(c(dmin, dmax), n=fpmax_numlevels,
                         min.n = fpmax_numlevels%/%2)
        dtab[, new_x := cut(x, dpretty, include.lowest=TRUE)]

        # order und levels
        dtab[is.na(new_x), new_x := 'NA']
        dtab = dtab[order(x)]
        d2levels = unique(dtab$new_x)

        dftable = dtab[, .(x=factor(new_x, levels = d2levels), dummy__target)]
      } else if (dplotclass == 'numeric') {
        # define levels and order
        dftable = dftable[order(x)]
        dftable[, x := as.character(x)]
        dftable[is.na(x), x := 'NA']
        d2levels = unique(dftable$x)
        dftable[, x := factor(x, levels = d2levels)]
      }

      dtab1 = dftable[, .(Anzahl = .N), by=.(tar1 = x, tar2 = dummy__target)]
      dtab1 = dtab1[, ':=' (Relativ = Anzahl/sum(Anzahl)), by=.(tar2)]
      dtab1 = dtab1[, ':=' (Anteil = Anzahl/sum(Anzahl)), by=.(tar1)]

      # Abs je Segment
      dtab2 = data.table::dcast(dtab1, tar1~tar2, value.var=c('Anzahl'))
      #dtab3 = data.table::dcast(dtab1, tar1~tar2, value.var=c('Relativ'))

      # Anteil je Segment
      dtab3 = data.table::dcast(dtab1, tar1~tar2, value.var=c('Relativ'))

      # Anteil je Segment
      dtab4 = data.table::dcast(dtab1, tar1~tar2, value.var=c('Anteil'))

      # Abs and Rel overal
      dtab = dtab1[, .(Absolut = sum(Anzahl)), by=.(tar1)][, Relativ := Absolut/sum(Absolut)]

      # for ordering
      if (dplotclass == 'factor'){
        dtab = dtab[order(Absolut, decreasing = TRUE)]
        dtab[, tar1 := factor(tar1, levels = unique(dtab$tar1))]
      }

      # add gesamt
      dtab  = add_table_bottom(dtab,  ffun = sum, fname = c(tar1 = 'Gesamt'))
      dtab2 = add_table_bottom(dtab2, ffun = sum, fname = c(tar1 = 'Gesamt'))
      dtab3 = add_table_bottom(dtab3, ffun = sum, fname = c(tar1 = 'Gesamt'))

      # add gesamt to Anteile
      d1 = dftable[, .(tar1 = 'Gesamt', Anzahl = .N), by=.(tar2 = dummy__target)]
      d1[, Anteil := Anzahl/sum(Anzahl)]
      d1 = data.table::dcast(d1, tar1~tar2, value.var=c('Anteil'))
      dtab4 = rbind(dtab4, d1)

      # rename
      names(dtab3)[-1] = paste0(names(dtab3)[-1], ' Relativ')
      names(dtab4)[-1] = paste0(names(dtab4)[-1], ' Anteil')
      # prepare order
      # order

      # format
      dtab[, Absolut := format(Absolut, big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')]
      dtab[, Relativ := format(signif(100*Relativ, 3), big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')]
      dtab[, Relativ := paste0(Relativ, '%')]

      ## for Segments
      d2cols = names(dtab2)[-1]
      dtab2[, c(d2cols) := lapply(.SD, function(x) {format(x, big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')}), .SDcols = d2cols]

      ## for Relativ
      d2cols = names(dtab3)[-1]
      dtab3[, c(d2cols) := lapply(.SD, function(x) {format(signif(100*x, 3), big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')}), .SDcols = d2cols]
      dtab3[, c(d2cols) := lapply(.SD, function(x) {paste0(x, '%')}), .SDcols = d2cols]

      ## for Anteile
      d2cols = names(dtab4)[-1]
      dtab4[, c(d2cols) := lapply(.SD, function(x) {format(signif(100*x, 3), big.mark   = '.', decimal.mark = ',', trim=TRUE, justify = 'right')}), .SDcols = d2cols]
      dtab4[, c(d2cols) := lapply(.SD, function(x) {paste0(x, '%')}), .SDcols = d2cols]


      if (!is.na(ftarget)){
        dtab = merge(dtab, dtab2, by='tar1')
        dtab = merge(dtab, dtab3, by='tar1')
        dtab = merge(dtab, dtab4, by='tar1')
      }

      # order
      dtab = dtab[order(tar1)]
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
          scale_fill_hue(c=50, l= 60, h=c(60, 220)) +
          theme_minimal()

        dplot_rel = ggplot(data=dtab, aes(x, color=dummy__target)) + geom_density() +
          guides(color=guide_legend(ftarget))+ ylab('Dichte') +
          scale_y_continuous(breaks = c(0)) +
          scale_x_continuous(labels = function(x) format(x, big.mark = '.', decimal.mark = ',')) +
          ggtitle(paste0(i, ' - Dichteverteilung je Segment')) +
          scale_color_hue(c=50, l= 60, h=c(60, 220)) +
          theme_minimal()
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
        # add text and position
        dtab[, text1 := format(count, big.mark = '.', decimal.mark = ',', trim=TRUE, justify='left')]
        dtab[, pos1  := 0.01*max(dtab$count, na.rm=TRUE)]
        dtab[, text2 := signif(100*rel, 3)]
        dtab[, text2 := format(text2, big.mark = '.', decimal.mark = ',', drop0trailing = TRUE, trim=TRUE, justify='left')]
        dtab[, text2 := paste0(text2, '%')]
        dtab[, pos2  := 0.01*max(dtab$rel, na.rm=TRUE)]

        # plot
        dplot_count = ggplot(data=dtab, aes(x=col, y=count, fill=dummy__target)) +
          geom_bar(stat='identity', position = position_dodge2()) +
          coord_flip()+ guides(fill=guide_legend(ftarget)) + ylab('Anzahl') +
          scale_y_continuous(labels = function(x) format(x, big.mark = '.', decimal.mark = ',')) +
          ggtitle(paste0(i, ' - Absolute H\u00e4ufigkeiten')) +
          scale_fill_hue(c=50, l= 60, h=c(60, 220)) +
          theme_minimal() +
          theme(panel.grid.major.y = element_blank())

        if (fptext_labels == TRUE){
          dplot_count = dplot_count +
            geom_text(aes(label = text1, y=pos1),
                      position=position_dodge2(width = 0.9),
                      hjust = 0,
                      color = '#E0E0E0',
                      size = 3) +
            theme(panel.grid.major.x = element_blank()) +
            theme(panel.grid.minor.x = element_blank()) +
            theme(axis.text.x = element_blank())
        }



        dplot_rel = ggplot(data=dtab, aes(x=col, y=rel, fill=dummy__target)) +
          geom_bar(stat='identity', position = position_dodge2()) +
          coord_flip() + guides(fill=guide_legend(ftarget)) + ylab('Anteil') +
          scale_y_continuous(labels = function(x) paste0(format(100*x, big.mark = '.', decimal.mark = ','), '%')) +
          ggtitle(paste0(i, ' - Bedingte H\u00e4ufigkeiten je Segment'))+
          scale_fill_hue(c=50, l= 60, h=c(60, 220)) +
          theme_minimal() +
          theme(panel.grid.major.y = element_blank())

        if (fptext_labels == TRUE){
          dplot_rel = dplot_rel +
            geom_text(aes(label = text2, y=pos2),
                      position=position_dodge2(width = 0.9),
                      hjust = 0,
                      color = '#E0E0E0',
                      size = 3) +
            theme(panel.grid.major.x = element_blank()) +
            theme(panel.grid.minor.x = element_blank()) +
            theme(axis.text.x = element_blank())
        }

      }
      res$plot[[i]]$count = dplot_count + xlab(i)
      res$plot[[i]]$rel = dplot_rel + xlab(i)

      # remove legende
      if (is.na(ftarget)){
        res$plot[[i]]$count = res$plot[[i]]$count + theme(legend.position = "none")
        res$plot[[i]]$rel   = res$plot[[i]]$rel   + theme(legend.position = "none")
      }

    }
  }
  return(res)
}
