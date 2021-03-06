#' Interactive Bar Plot
#'
#' @param dp List of parameters
#'
#' \itemize{
#' \item tab: table to be plotted, already aggregated
#' \item xval = 'x': column name with x-values
#' \item yval = 'y': column name with y-values
#' \item group = 'group': column name for grouping (not needed)
#' \item color = 'color': column name with color-values
#' \item label_position = 'right': position of text labels ('none', 'top',
#' 'left', 'right', 'bottom', 'inside', 'insideLeft', 'insideRight',
#' 'insideTop', 'insideBottom', 'insideTopLeft', 'insideBottomLeft',
#' 'insideTopRight', 'insideBottomRight')
#' \item label_color = '#A9A9A9': color of text labels
#' \item x_label_color = '#606060': color of x-axis text labels
#' \item label_size = 10: size of text labels
#' \item x_label_size = 15: size of x-axis text labels
#' \item margin_left = '10%': chart margin left
#' \item margin_right = '10%': chart margin right
#' \item margin_top = 60: chart margin top
#' \item margin_bottom = 60: chart margin bottom
#' \item text_format = '': text format is '' (standard), 'percent', or 'euro'
#' \item chart_width = NULL: chart width (css units)
#' \item chart_height = NULL: chart height (css units)
#' \item legend = FALSE: show (TRUE/FALSE) the legend
#' \item legend_left = 'auto': legend position left
#' \item legend_right = 'auto': legend position right
#' \item legend_top = 'auto': legend position top
#' \item legend_bottom = 'auto': legend position bottom
#' \item legend_orient = 'horizontal': legend orientation (horizontal or vertical)
#' \item title = NULL: chart title
#' \item title_left = margin_left: left position of title
#' \item title_top  = 'auto': top position of title
#' \item title_size = 18: text size of title
#' \item title_color = x_label_color: text color of title
#' \item y_min = NULL: min. value on y-axis
#' \item y_max = NULL: max. value on y-axis
#' \item show_yaxis = FALSE: show y-axis (TRUE/FALSE)
#' \item yaxis_title  = yval: name of y-axis (title)
#' \item yaxis_title_size  = 15: text size of y-axis title
#' \item yaxis_title_color  = '#606060': text color of yaxis title
#' \item show_ygrid = FALSE: show y-grid
#' \item ygrid_color = fgrey: color of y-grid
#' \item y_label_color = '#606060'): color of y-axis text labels
#' \item y_label_size  = 10: size of y-axis text labels
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
#' dp$color = 'color'
#' dp$text_format = ''
#' dp$label_color = '#A9A9A9'
#' dp$label_size = 10
#' dp$x_label_color = '#606060'
#' dp$x_label_size = 15
#' dp$margin_left = '10%'
#' dp$margin_right = '10%'
#' dp$margin_top = 60
#' dp$margin_bottom = 60
#' dp$title = 'Chart'
#' dp$chart_width = NULL
#' dp$chart_height = NULL
#' dp$legend = FALSE
#' dp$legend_left = 'auto'
#' dp$legend_right = 'auto'
#' dp$legend_top = 'auto'
#' dp$legend_bottom = 'auto'
#' dp$legend_orient = 'horizontal'
#' fplot = plot_ecbar(dp)
#' fplot
#' }
#'
plot_ecbar = function(dp = NULL){

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
  # color
  col  = ifelse('color' %in% names(dp), dp$color, 'color')  # color-col
  # add color col
  if (!col %in% names(tab)){
    tab[, color := '#da9e92']
  }



  # chart size
  chart_width = df_assign(dp, 'chart_width', NULL)
  chart_height = df_assign(dp, 'chart_height', NULL)

  # axes limits
  y_min = df_assign(dp, 'y_min', NULL)
  y_max = df_assign(dp, 'y_max', NULL)

  # legend position and orientation
  legend_left = df_assign(dp, 'legend_left', 'auto')
  legend_right = df_assign(dp, 'legend_right', 'auto')
  legend_top = df_assign(dp, 'legend_top', 'auto')
  legend_bottom = df_assign(dp, 'legend_bottom', 'auto')
  legend_orient = df_assign(dp, 'legend_orient', 'horizontal') # or vertical

  # labels
  label_position = df_assign(dp, 'label_position', 'right')
  show_labels = TRUE
  if (label_position == 'none'){
    show_labels = FALSE
  }

  # grid margins
  margin_left   = ifelse('margin_left'  %in% names(dp), dp$margin_left,  '10%')  # x-col
  margin_right  = ifelse('margin_right' %in% names(dp), dp$margin_right, '10%')  # y-col
  margin_top    = ifelse('margin_top' %in% names(dp), dp$margin_top, 60)  # y-col
  margin_bottom = ifelse('margin_bottom' %in% names(dp), dp$margin_bottom, 60)  # y-col
  # text format
  text_format = ifelse('text_format' %in% names(dp), dp$text_format, '')
  # label color
  label_color = ifelse('label_color' %in% names(dp), dp$label_color, '#A9A9A9')
  label_size  = ifelse('label_size'  %in% names(dp), dp$label_size, 10)
  x_label_color = ifelse('x_label_color' %in% names(dp), dp$x_label_color, '#606060')
  x_label_size  = ifelse('x_label_size'  %in% names(dp), dp$x_label_size, 15)

  #title
  title_left = df_assign(dp, 'title_left', margin_left)
  title_top  = df_assign(dp, 'title_top',  'auto')
  title_size = df_assign(dp, 'title_size',  18)
  title_color = df_assign(dp, 'title_color', x_label_color)

  # y-axis
  show_yaxis = df_assign(dp, 'show_yaxis', FALSE)
  yaxis_title  = df_assign(dp, 'yaxis_title', yval)
  yaxis_title_size  = df_assign(dp, 'yaxis_title_size', 15)
  yaxis_title_color  = df_assign(dp, 'yaxis_title_color', '#606060')
  show_ygrid = df_assign(dp, 'show_ygrid', FALSE)
  ygrid_color = df_assign(dp, 'ygrid_color', fgrey)
  y_label_color = df_assign(dp, 'ax_label_color', '#606060')
  y_label_size  = df_assign(dp, 'ax_label_size', 10)



  names(tab)[names(tab) == xval] = 'xval'
  names(tab)[names(tab) == yval] = 'yval'
  names(tab)[names(tab) == col] = 'color'
  names(tab)[names(tab) == group] = 'group'

  # text and tooltip format
  js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[0]);
    return f;}"

  js_axisform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params);
    return f;}"

  js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[0]);
    let g = params.name;
    let h = params.seriesName;
    return g + '<br>' +h+': '+f;}"

  if (text_format == 'percent'){
    tab[, yval := yval * 100]
    js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[0]);
    return f+'%';}"

    js_axisform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params);
    return f+'%';}"

    js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[0]);
    let g = params.name;
    let h = params.seriesName;
    return g + '<br>' +h+': '+f+'%';}"
  }

  if (text_format == 'euro'){
    js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[0]);
    return f+'\u20AC';}"

    js_axisform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params);
    return f+'\u20AC';}"

    js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[0]);
    let g = params.name;
    let h = params.seriesName;
    return g + '<br>' +h+': '+f+'\u20AC';}"
  }


  fplot = tab %>%
    group_by(group) %>%
    e_charts(xval, width = chart_width, height = chart_height) %>%
    e_grid(left = margin_left, right = margin_right, top = margin_top, bottom = margin_bottom) %>%
    e_bar(yval) %>%
    e_color(color = tab$color %>% unique) %>%
    e_add("itemStyle", color) %>%
    e_labels(show = show_labels,
             position = label_position,
             fontSize = label_size,
             color=label_color,
             formatter = htmlwidgets::JS(js_numform)) %>%
    e_x_axis(type = "category",
             axisLabel = list(fontSize=x_label_size, color=x_label_color)) %>%
    e_y_axis(
      show = show_yaxis,
      name = yaxis_title,
      nameLocation='middle',
      nameGap=30,
      nameTextStyle = list(color = yaxis_title_color, fontSize = yaxis_title_size),
      axisLine=list(show=FALSE),
      splitLine=list(show=show_ygrid, lineStyle = list(color=ygrid_color, opacity=1, width=fgrid_size)),
      axisLabel = list(color=y_label_color, fontSize=y_label_size, formatter = htmlwidgets::JS(js_axisform)),
      axisTick = list(show=FALSE),
      min = y_min,
      max = y_max,
      z = 5
    ) %>%
    #e_y_axis(show=FALSE,
    #         min = y_min,
    #         max = y_max) %>%
    e_flip_coords() %>%
    #e_text_style(fontSize = 30, color='blue') %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    #  e_toolbox_feature(feature = "dataView") %>%
    e_tooltip(formatter = htmlwidgets::JS(js_ttform))

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
