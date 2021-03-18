#' Interactive Line and Scatter Plot
#'
#' @param dp List of parameters
#'
#' \itemize{
#' \item tab: table to be plotted, already aggregated
#' \item xval = 'x': column name with x-values
#' \item yval = 'y': column name with y-values
#' \item group = 'group': column name for grouping (not needed)
#' \item color = NULL: color vector
#' \item pointsize = 8: symbol size
#' \item linesize = 2: line width of line plot
#' \item title = NULL: chart title
#' \item title_left = margin_left: left position of title
#' \item title_top  = 'auto': top position of title
#' \item title_size = 18: text size of title
#' \item title_color = axis_title_color: text color of title
#' \item text_format = '': text format is '' (standard), 'percent', or 'euro'
#' \item show_label = FALSE: show text labels (TRUE/FALSE)
#' \item label_color = '#A9A9A9': color of text labels
#' \item label_size = 10: size of text labels
#' \item ax_label_color = '#606060': color of axes text labels
#' \item ax_label_size = 10: size of axes text labels
#' \item xaxis_title = xval: name of x-axis title (string), set NA to remove it
#' \item yaxis_title = yval: name of y-axis title (string), set NA to remove it
#' \item axis_title_color = '#606060': color of axes titles
#' \item axis_title_size = 15: size of axes titles
#' \item show_xgrid = FALSE: show x-grid (TRUE/FALSE)
#' \item show_ygrid = FALSE: show y-grid (TRUE/FALSE)
#' \item margin_left = '10%': chart margin left
#' \item margin_right = '10%': chart margin right
#' \item margin_top = 60: chart margin top
#' \item margin_bottom = 60: chart margin bottom
#' \item legend = FALSE: show (TRUE/FALSE) the legend
#' \item legend_left = 'auto': legend position left
#' \item legend_right = 'auto': legend position right
#' \item legend_top = 'auto': legend position top
#' \item legend_bottom = 'auto': legend position bottom
#' \item legend_orient = 'horizontal': legend orientation (horizontal or vertical)
#' \item chart_width = NULL: chart width (css units)
#' \item chart_height = NULL: chart height (css units)
#' \item x_min = 'dataMin': min. value on x-axis
#' \item x_max = NULL: max. value on x-axis
#' \item y_min = NULL: min. value on y-axis
#' \item y_max = NULL: max. value on y-axis
#' }
#'
#'
#' @return an echarts4r line and scatter plot
#' @export
#'
#' @import echarts4r
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{plot_ecbar(dp)}
#' \dontrun{
#' dp = list(NULL)
#' dp$tab = tab
#' dp$xval = 'x'
#' dp$yval = 'y'
#' dp$group = 'group'
#' dp$color = c('#b4943e', '#777acd', '#60a862', '#c45ca2', '#cb5a4c')
#' dp$pointsize = 8
#' dp$linesize = 2
#' dp$title = NULL
#' dp$text_format = ''
#' dp$show_label = FALSE
#' dp$label_color = '#A9A9A9'
#' dp$label_size = 10
#' dp$ax_label_color = '#606060'
#' dp$ax_label_size = 10
#' dp$xaxis_title = NULL
#' dp$yaxis_title = NULL
#' dp$axis_title_color = '#606060'
#' dp$axis_title_size = 15
#' dp$show_xgrid = FALSE
#' dp$show_ygrid = FALSE
#' dp$margin_left = '10%'
#' dp$margin_right = '10%'
#' dp$margin_top = 60
#' dp$margin_bottom = 60
#' dp$legend = FALSE
#' dp$legend_left = 'auto'
#' dp$legend_right = 'auto'
#' dp$legend_top = 'auto'
#' dp$legend_bottom = 'auto'
#' dp$legend_orient = 'horizontal'
#' dp$chart_width = NULL
#' dp$chart_height = NULL
#' fplot = plot_ecscatter(dp)
#' fplot
#' }
#'
plot_ecscatter = function(dp = NULL){

  color = NULL
  group = NULL

  fgrey = '#A9A9A9'    # light grey
  fgrid_size = 0.25   # width of grid line

  # helper function
  # assign dval1 (TRUE) or dval2 (FALSE)
  df_assign = function(dlist, dval1, dval2){
    if (dval1 %in% names(dlist)){
      return(dp[[dval1]])
    } else{
      return(dval2)
    }
  }

  # data is needed
  if (!'tab' %in% names(dp)){
    print('Please, provide data')
    return (NULL)
  }

  tab  = dp$tab %>% copy %>% setDT   # data

  xval = ifelse('xval' %in% names(dp), dp$xval, 'x')  # x-col
  yval = ifelse('yval' %in% names(dp), dp$yval, 'y')  # y-col
  # group
  group = ifelse('group' %in% names(dp), dp$group, 'group')  # y-col
  if (!group %in% names(tab)){
    tab[, group := 'Wert']
  }

  # chart size
  chart_width = df_assign(dp, 'chart_width', NULL)
  chart_height = df_assign(dp, 'chart_height', NULL)

  # axes limits
  x_min = df_assign(dp, 'x_min', 'dataMin')
  x_max = df_assign(dp, 'x_max', NULL)
  y_min = df_assign(dp, 'y_min', NULL)
  y_max = df_assign(dp, 'y_max', NULL)

  # legend position and orientation
  legend_left = df_assign(dp, 'legend_left', 'auto')
  legend_right = df_assign(dp, 'legend_right', 'auto')
  legend_top = df_assign(dp, 'legend_top', 'auto')
  legend_bottom = df_assign(dp, 'legend_bottom', 'auto')
  legend_orient = df_assign(dp, 'legend_orient', 'horizontal') # or vertical

  # symbol size and line size
  pointsize  = ifelse('pointsize' %in% names(dp), dp$pointsize, 8)
  linesize   = ifelse('linesize'  %in% names(dp), dp$linesize,  2)

  # grid margins
  margin_left   = ifelse('margin_left'  %in% names(dp), dp$margin_left,  '10%')  # x-col
  margin_right  = ifelse('margin_right' %in% names(dp), dp$margin_right, '10%')  # y-col
  margin_top    = ifelse('margin_top' %in% names(dp), dp$margin_top, 60)  # y-col
  margin_bottom = ifelse('margin_bottom' %in% names(dp), dp$margin_bottom, 60)  # y-col

  # text format
  text_format = ifelse('text_format' %in% names(dp), dp$text_format, '')

  # text label
  show_label  = ifelse('show_label' %in% names(dp), dp$show_label, FALSE)
  label_color = ifelse('label_color' %in% names(dp), dp$label_color, '#A9A9A9')
  label_size  = ifelse('label_size'  %in% names(dp), dp$label_size, 10)

  # axis labels
  ax_label_color = ifelse('ax_label_color' %in% names(dp), dp$ax_label_color, '#606060')
  ax_label_size  = ifelse('ax_label_size'  %in% names(dp), dp$ax_label_size, 10)

  # axis names ect
  xaxis_title  = ifelse('xaxis_title'  %in% names(dp), dp$xaxis_title, xval)
  yaxis_title  = ifelse('yaxis_title'  %in% names(dp), dp$yaxis_title, yval)
  axis_title_size  = ifelse('axis_title_size'  %in% names(dp), dp$axis_title_size, 15)
  axis_title_color  = ifelse('axis_title_color'  %in% names(dp), dp$axis_title_color, '#606060')

  #title
  title_left = df_assign(dp, 'title_left', margin_left)
  title_top  = df_assign(dp, 'title_top',  'auto')
  title_size = df_assign(dp, 'title_size',  18)
  title_color = df_assign(dp, 'title_color', axis_title_color)

  # axis grid
  show_xgrid = ifelse('show_xgrid'  %in% names(dp), dp$show_xgrid, FALSE)
  show_ygrid = ifelse('show_ygrid'  %in% names(dp), dp$show_ygrid, FALSE)

  # legend
  show_legend = ifelse('show_legend'  %in% names(dp), dp$show_legend, FALSE)


  names(tab)[names(tab) == xval] = 'xval'
  names(tab)[names(tab) == yval] = 'yval'
  names(tab)[names(tab) == group] = 'group'

  # text and axis tooltip format
  js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[1]);
    return f;}"

  js_axisform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params);
    return f;}"

  js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[0]);
    let g = Intl.NumberFormat('de-DE').format(params.value[1]);
    let h = params.seriesName;
    return h + '<br>' +'x = ' + f + '<br>'  +'y = ' + g;}"

  if (text_format == 'percent'){
    tab[, yval := yval * 100]
    js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[1]);
    return f+'%';}"

    js_axisform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params);
    return f+'%';}"

    js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[0]);
    let g = Intl.NumberFormat('de-DE').format(params.value[1]);
    let h = params.seriesName;
    return h + '<br>' +'x = ' + f + '<br>'  +'y = ' + g + '%';}"
  }

  if (text_format == 'euro'){
    js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[1]);
    return f+'\u20AC';}"

    js_axisform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params);
    return f+'\u20AC';}"

    js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[0]);
    let g = Intl.NumberFormat('de-DE').format(params.value[1]);
    let h = params.seriesName;
    return h + '<br>' +'x = ' + f + '<br>'  +'y = ' + g + '\u20AC';}"
  }


  fplot = tab %>%
    group_by(group) %>%
    e_charts(xval, width = chart_width, height = chart_height) %>%
    e_grid(left = margin_left, right = margin_right, top = margin_top, bottom = margin_bottom) %>%
    #e_scatter(yval, symbolSize = pointsize) %>%
    e_line(yval, symbolSize = pointsize, symbol = 'circle', lineStyle = list(width=linesize)) %>%
    # e_color(color = c('green', 'blue', 'grey')) %>%
    # e_add("itemStyle", 'color') %>%
    e_labels(show=show_label, position = 'right', fontSize = label_size, color=label_color,
             formatter = htmlwidgets::JS(js_numform)) %>%
    e_x_axis(
      name = xaxis_title,
      nameLocation='middle',
      nameGap=30,
      nameTextStyle = list(color = axis_title_color, fontSize = axis_title_size),
      axisLine=list(show=FALSE),
      splitLine=list(show=show_xgrid, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      axisLabel = list(color=ax_label_color, fontSize=ax_label_size, formatter = htmlwidgets::JS("function (value) {var f= Intl.NumberFormat('de-DE').format(value); return f;}")),
      axisTick = list(show=FALSE),
      min = x_min,
      max = x_max
      #min = 'dataMin'
    ) %>%
    e_y_axis(
      name = yaxis_title,
      nameTextStyle = list(color = axis_title_color, fontSize = axis_title_size),
      axisLine=list(show=FALSE),
      splitLine=list(show=show_ygrid, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      axisLabel = list(color=ax_label_color, fontSize=ax_label_size, formatter = htmlwidgets::JS(js_axisform)),
      axisTick = list(show=FALSE),
      min = y_min,
      max = y_max
    ) %>%
    # e_x_axis(type = "category", axisLabel = list(fontSize=x_label_size, color=x_label_color)) %>%
    # e_y_axis(show=FALSE) %>%
    #e_flip_coords() %>%
    #e_text_style(fontSize = 30, color='blue') %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    #  e_toolbox_feature(feature = "dataView") %>%
    e_tooltip(formatter = htmlwidgets::JS(js_ttform))

  # Color
  if ('color' %in% names(dp)){
    fplot = fplot %>% e_color(color = dp$color)
  }

  # Title
  if ('title' %in% names(dp)){
    fplot = fplot %>% e_title(dp$title,
                              left = title_left,
                              top = title_top,
                              textStyle = list(
                                color = title_color,
                                fontSize = title_size
                              )
    )
  }
  # Legend
  if ('legend' %in% names(dp)){
    fplot = fplot %>% e_legend(show = dp$legend,
                               right = legend_right,
                               left = legend_left,
                               bottom = legend_bottom,
                               top = legend_top,
                               orient = legend_orient)
  } else {
    fplot = fplot %>% e_legend(show = FALSE)
  }

  return(fplot)

}
