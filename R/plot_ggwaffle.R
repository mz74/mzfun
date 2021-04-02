#' Waffle Plot
#'
#' @param dp List of parameters
#'
#' \itemize{
#' \item tab: table to be plotted, aggregated values
#' \item key_label = '': column name with plot labels
#' \item key_family = '': column name with font family
#' \item key_frequ = '': column name with frequencies
#' \item key_color = '': column name with colors
#' \item key_group = '': column name with groups
#' \item key_facet = '': column name with facet groups
#' \item label = '\\u25A0': labels to be plotted, only if key_label is not defined
#' \item family = NA: font family for labels, only if key_family is not defined
#' \item frequ = 1: frequency, only if key_frequ is not defined
#' \item color = '#bb873c': vector with colors, only if key_color is not defined
#'
#' \item cols = 5: number of columns in waffle chart
#' \item size = 10: size of plotted labels
#' \item orientation = 'vertical': orientation of waffel plot pillars (vertical or horizontal)
#' \item align = 'bottom': alignment of waffle plot pillars (top, bottom, center).
#' E.g. centered facet pillars
#' \item facet_ncol = 5: maximum number of facet columns
#'
#' \item legend = 'none': legend position (top, right, left, bottom, none, label)
#' 'label' uses labels as legend symbols, works only in some situations with
#' position always right
#' \item legend_symbol_size = 3: size of legend symbols
#' \item legend_text_size = 15: size of legend text
#' \item label_legend_size = 10: size of label-legend symbols and text
#' \item label_legend_color = '#A9A9A9': color of label-legend text
#'
#' \item facet_margin = 2: (-2 if legend = 'label'), inner margin between neighboring
#' facets.
#' \item chart_xy_margin = 1: (3 if legend = 'label' to give enough space),
#' extra xy-limit for each plot (increases x-axis limits)
#' \item margin_bottom = 1: to increase y-axis at bottom side
#' \item margin_top = 1: to increase y-axis at top side
#' \item outermargin_left = 0: chart margin left
#' \item outermargin_right = 1: chart margin right
#'
#' \item facet_text_size = 25: size of facet title
#' \item facet_text_color = '#707070': color of facet title
#' \item facet_title_position = 'bottom': position of facet title (top, left, bottom, right)
#'
#' \item font = '': font family of chart
#'
#'}
#'
#'
#' @return a ggplot waffle chart
#' @export
#'
#' @import ggplot2
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @section Resources:
#' Unicode table: \url{https://unicode-table.com/de/}
#'
#' @examples
#' \dontrun{plot_ggwaffel(dp)}
#' \dontrun{
#' dp = list(NULL)
#' dp$tab = tab
#' fplot = plot_ggwaffel(dp)
#' fplot
#'
#' # -----------------------
#'
#' library(emojifont)
#' library(mzfun)
#'
#' tab1 = data.table(
#'   group = 'Chart 1',
#'   segment = c('A', 'B', 'C'),
#'   frequ = c(12, 13, 14),
#'   color = c('blue', 'green', 'grey'),
#'   label= fontawesome('fa-car'),
#'   family='fontawesome-webfont',
#'   label2 = emoji('airplane'),
#'   family2 ='EmojiOne'
#' )
#'
#' tab2 = data.table(
#'   group = 'Chart 2',
#'   segment = c('A', 'B', 'C'),
#'   frequ = c(15, 16, 17),
#'   color = c('blue', 'green', 'grey'),
#'   label= fontawesome('fa-twitter'),
#'   family='fontawesome-webfont',
#'   label2 = emoji('airplane'),
#'   family2 ='EmojiOne'
#' )
#'
#' tab3 = rbind(tab1, tab2)
#'
#' dp = list(NULL)
#' dp$tab = tab3
#' dp$key_frequ = 'frequ'
#' dp$key_group = 'segment'
#' dp$key_facet = 'group'
#' dp$key_color = 'color'
#' dp$key_label = 'label'
#' dp$key_family = 'family'
#' #dp$label = fontawesome('fa-female')
#' #dp$family ='fontawesome-webfont'
#' #dp$label = 'a'
#' dp$size = 15
#' dp$cols = 4
#' dp$orientation = 'horizontal'
#' #dp$color = c('grey', 'black')
#' dp$facet_ncol = 1
#' dp$align = 'center'
#' dp$legend = 'label'
#' dp$facet_title_position = 'top'
#' dp$font = 'serif'
#' dp$facet_margin = 2
#'
#' fplot = plot_ggwaffle(dp)
#' fplot
#'
#' # ------------------------
#'
#' dp$key_label = 'label2'
#' dp$key_family = 'family2'
#'
#' fplot = plot_ggwaffle(dp)
#' fplot
#' }
#'
plot_ggwaffle = function(dp){

  label_ = NULL
  family_ = NULL
  frequ_ = NULL
  color_ = NULL
  group_ = NULL
  facet_ = NULL
  facet = NULL
  . = NULL
  group = NULL
  color = NULL
  label = NULL
  family = NULL
  x = NULL
  y = NULL


  grey_dark = '#707070'
  grey_mid  = '#A9A9A9'

  dtab = dp$tab %>% copy %>% setDT

  fcols      =  ifelse('cols' %in% names(dp), dp$cols, 5)
  fsize      =  ifelse('size' %in% names(dp), dp$size, 10)
  flabel    = ifelse('label' %in% names(dp), dp$label, '\u25A0')
  ffamily  = ifelse('family' %in% names(dp), dp$family, NA)
  forientation  = ifelse('orientation' %in% names(dp), dp$orientation, 'vertical')
  facet_ncol     = ifelse('facet_ncol' %in% names(dp), dp$facet_ncol, 5)
  falign              = ifelse('align' %in% names(dp), dp$align, 'bottom')
  # legend in label, top, right, left, bottom
  flegend          = ifelse('legend' %in% names(dp), dp$legend, 'none')
  legend_pos  = ifelse(flegend != 'label', flegend, 'none')
  flegend_symbol_size  = ifelse('legend_symbol_size' %in% names(dp), dp$legend_symbol_size, 3)
  flegend_text_size  = ifelse('legend_text_size' %in% names(dp), dp$legend_text_size, 15)
  # label legend
  flabel_legend_size  = ifelse('label_legend_size' %in% names(dp), dp$label_legend_size, 10)
  flabel_legend_color  = ifelse('label_legend_color' %in% names(dp), dp$label_legend_color, grey_mid)

  # facet margin
  dfacet_margin = 2
  if (flegend == 'label'){dfacet_margin = -2}
  facet_margin  = ifelse('facet_margin' %in% names(dp), dp$facet_margin, dfacet_margin)
  # extra symmetric plot margin needed for label - legend symbols
  dfchart_xy_margin = 1
  if (flegend == 'label'){dfchart_xy_margin = 3}
  fchart_xy_margin  = ifelse('chart_xy_margin' %in% names(dp), dp$chart_xy_margin, dfchart_xy_margin)
  # margin top/bottom of facet
  fmargin_bottom  = ifelse('margin_bottom' %in% names(dp), dp$margin_bottom, 1)
  fmargin_top = ifelse('margin_top' %in% names(dp), dp$margin_top, 1)
  # right/left outer chart margin - may be negativ
  foutermargin_left   = ifelse('outermargin_left' %in% names(dp), dp$outermargin_left, 0)
  foutermargin_right = ifelse('outermargin_right' %in% names(dp), dp$outermargin_right, 1)
  # facet text
  facet_text_size   = ifelse('facet_text_size' %in% names(dp), dp$facet_text_size, 25)
  facet_text_color = ifelse('facet_text_color' %in% names(dp), dp$facet_text_color, grey_dark)
  facet_title_position = ifelse('facet_title_position' %in% names(dp), dp$facet_title_position, 'bottom')  # top, left, bottom, right

  ffont = ifelse('font'     %in% names(dp), dp[['font']],    '')

  # prepare tab colnames
  key_label   = ifelse('key_label' %in% names(dp), dp$key_label, '')
  key_family = ifelse('key_family' %in% names(dp), dp$key_family, '')
  key_frequ  = ifelse('key_frequ' %in% names(dp), dp$key_frequ, '')
  key_color  = ifelse('key_color' %in% names(dp), dp$key_color, '')
  key_group = ifelse('key_group' %in% names(dp), dp$key_group, '')
  key_facet   = ifelse('key_facet' %in% names(dp), dp$key_facet, '')

  if (key_label != ''){
    names(dtab)[names(dtab) == key_label] = 'label_'
  } else {
    dtab[, label_ := flabel]
  }

  if (key_family != ''){
    names(dtab)[names(dtab) == key_family] = 'family_'
  } else {
    dtab[, family_ := ffamily]
  }

  if (key_frequ != ''){
    names(dtab)[names(dtab) == key_frequ] = 'frequ_'
  } else {
    dtab[, frequ_ := 1]
  }

  if (key_color != ''){
    names(dtab)[names(dtab) == key_color] = 'color_'
  } else {
    # maybe there is a color vector
    if ('color' %in% names(dp)){
      dcolor = dp$color %>% rep(length.out = nrow(dtab))
      dtab[, color_ := dcolor]
    } else {
      dtab[, color_ := '#bb873c']
    }
  }

  if (key_group != ''){
    names(dtab)[names(dtab) == key_group] = 'group_'
  } else {
    dtab[, group_ := 'group']
  }

  if (key_facet != ''){
    names(dtab)[names(dtab) == key_facet] = 'facet_'
  } else {
    dtab[, facet_ := 'facet']
  }

  # prepare levels for factor
  levels_facet = dtab$facet_ %>% unique()
  levels_group = dtab$group_ %>% unique()

  # prepare xy-template with max. spread per facet
  d1 = dtab[, .(Anzahl = frequ_ %>% sum(na.rm = TRUE)), by=.(facet_)]
  dmax = max(d1$Anzahl, na.rm = TRUE) # max nb of elements in a facet
  d3 = fcols
  d4 = (dmax / d3) %>% ceiling
  kmax = d3*d4 # max spread of chart

  # define grid depending on chartr orientation
  if (forientation == 'horizontal'){
    d5 = expand.grid(y=seq(1,d3),x= seq(1,d4))
  } else {
    d5 = expand.grid(x=seq(1,d3),y= seq(1,d4))
  }

  # prepare table with coord system
  ktab = data.table(
    x = d5$x,
    y = d5$y
  )


  # create table
  tab = data.table()
  dfacet = dtab$facet_ %>% unique
  i = dfacet[1]
  # each facet
  for (i in dfacet){
    dfrequ = dtab[facet_ == i, frequ_ %>% sum(na.rm=TRUE)]
    ftab = data.table(id = (dfrequ %>% seq), group = '', color = '', facet = i)
    # each group
    dgroup = dtab[facet_ == i]$group_ %>% unique()
    nend = 0
    j = dgroup[1]
    for (j in dgroup){
      nstart = nend + 1
      nend  = nstart + dtab[facet_ == i & group_ ==j]$frequ_ - 1
      ftab[c(nstart:nend), group := j]
      ftab[c(nstart:nend), color := dtab[facet_ == i & group_ ==j]$color_]
      ftab[c(nstart:nend), label := dtab[facet_ == i & group_ ==j]$label_]
      ftab[c(nstart:nend), family := dtab[facet_ == i & group_ ==j]$family_]
    }

    # on alignment
    dstart = 1
    dend = dfrequ

    # top alignment
    if (falign == 'top'){
      dstart = kmax - dfrequ + 1
      dend = kmax
    }

    # center alignment
    if (falign %in% c( 'center', 'centre')){
      dl = ((kmax - dfrequ) / 2) %>% floor
      dstart = kmax - dfrequ - dl + 1
      dend = kmax - dl
    }

    # get facet tab
    dftab = cbind(ktab[c(dstart:dend)],
                  ftab)

    tab = rbind(tab, dftab)
  }

  # create legend table
  tab_leg = dtab[!group_ %>% duplicated()]
  dnb = tab_leg %>% nrow
  dfac = tab_leg[dnb, facet_]
  tab_leg[, x := max(tab$x) + 2][, y := dnb %>% seq][, facet := dfac]


  d1 = tab[, .(.N), by=.(facet, group, color)]

  # order and factors
  tab[, facet := facet %>% factor(levels = levels_facet)]
  tab[, group := group %>% factor(levels = levels_group)]

  fplot = ggplot(data = tab) +
    geom_point(data=tab, aes(x, y, color = group), size=7, alpha=0)  +
    geom_text(aes(x, y, label=label, family=family, color = group), size=fsize, show.legend = FALSE) +
    #geom_text(aes(x, y, label=flabel, family=ffamily, color = group), size=fsize) +

    theme_void(base_family = ffont)  +
    theme(
      legend.title = element_blank(),
      legend.text=element_text(size=flegend_text_size),
      panel.spacing = unit(facet_margin, "lines"),  # facet - distance
      strip.text = element_text(hjust = 0.5, size=facet_text_size, color=facet_text_color),
      legend.position = legend_pos,
      plot.margin = margin(r=foutermargin_right, l=foutermargin_left, unit= "cm"),
    ) +
    coord_equal(clip = 'off') +
    xlim(1-fchart_xy_margin, max(tab$x) + fchart_xy_margin) +
    ylim(1-fmargin_bottom, max(tab$y) + fmargin_top) +
    # scale_color_identity(guide = "legend") +
    scale_color_manual(breaks = dtab$group_,  values = dtab$color_)+
    guides(colour = guide_legend(override.aes = list(size = flegend_symbol_size, alpha = 1))) +
    #annotate('text', 7, 1, label = flabel, family=ffamily, size = 25) +
    #scale_colour_identity() +
    facet_wrap(.~facet, ncol = facet_ncol, strip.position=facet_title_position)
  if (flegend == 'label'){
    fplot = fplot +
      #annotate('text', x = 7, y=2, label='sdsgsdg')
      geom_text(data = tab_leg, aes(x=x, y=y+1, label=label_,  family=family_, color=group_), size = flabel_legend_size, hjust=0) +
      geom_text(data = tab_leg, aes(x=x+1, y=y+1, label=group_), size = flabel_legend_size, color=flabel_legend_color, hjust=0)
  }

  return (fplot)
}
