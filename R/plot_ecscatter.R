#' Interactive Bar Plot
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
#' \item title: chart title
#' \item text_format = '': text format is '' (standard), 'percent', or 'euro'
#' \item show_legend = FALSE: show legend (TRUE/FALSE)
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
#' \item margin_right = '12': chart margin right
#' }
#'
#'
#' @return an echarts4r bar plot
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
#' dp$show_legend = FALSE
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
#' dp$margin_right = '12'
#' fplot = plot_ecscatter(dp)
#' fplot
#' }
#'
plot_ecscatter = function(dp = NULL){

  color = NULL
  group = NULL

  fgrey = '#A9A9A9'    # light grey
  fgrid_size = 0.25   # width of grid line

  # data is needed
  if (!'data' %in% names(dp)){
    print('Please, provide data')
    return (NULL)
  }

  tab  = dp$data %>% copy %>% setDT   # data

  xval = ifelse('xval' %in% names(dp), dp$xval, 'x')  # x-col
  yval = ifelse('yval' %in% names(dp), dp$yval, 'y')  # y-col
  # group
  group = ifelse('group' %in% names(dp), dp$group, 'group')  # y-col
  if (!group %in% names(tab)){
    tab[, group := 'Wert']
  }

  # symbol size and line size
  pointsize  = ifelse('pointsize' %in% names(dp), dp$pointsize, 8)
  linesize   = ifelse('linesize'  %in% names(dp), dp$linesize,  2)

  # grid margins
  margin_left  = ifelse('margin_left'  %in% names(dp), dp$margin_left,  '10%')  # x-col
  margin_right = ifelse('margin_right' %in% names(dp), dp$margin_right, '12')  # y-col
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
    e_charts(xval) %>%
    e_grid(left = margin_left, right = margin_right) %>%
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
      min = 'dataMin'
    ) %>%
    e_y_axis(
      name = yaxis_title,
      nameTextStyle = list(color = axis_title_color, fontSize = axis_title_size),
      axisLine=list(show=FALSE),
      splitLine=list(show=show_ygrid, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      axisLabel = list(color=ax_label_color, fontSize=ax_label_size, formatter = htmlwidgets::JS(js_axisform)),
      axisTick = list(show=FALSE)
    ) %>%
    e_legend(show = show_legend, right = '10%') %>%
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
    fplot = fplot %>% e_title(dp$title, left = margin_left)
  }

  return(fplot)

}
