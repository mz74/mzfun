#' Interactive Heatmap Plot
#'
#' @param dp List of parameters
#'
#'\itemize{
#' \item tab: table to be plotted, already aggregated
#' \item xval = 'x': column name with x-values, categorical or integer values
#' \item yval = 'y': column name with y-values, categorical or integer values
#' \item zval = 'z': column name with z-values, numerical values
#' \item color = 'c('#fef8cb', '#ba8d7e'): color vector defining color palette
#' \item title: chart title
#' \item text_format = '': text format is '' (standard), 'percent', or 'euro'
#' \item show_label = TRUE: show text labels (TRUE/FALSE)
#' \item label_color = '#606060': color of text labels
#' \item label_size = 10: size of text labels
#' \item x_label_color = '#606060': color of x-axis text labels
#' \item x_label_size = 15: size of x-axis text labels
#' \item y_label_color = '#606060': color of y-axis text labels
#' \item y_label_size = 15: size of y-axis text labels
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
#' \item chart_width = NULL: chart width (css units)
#' \item chart_height = NULL: chart height (css units)
#' }
#'
#'
#'
#' @return an echarts4r heatmap plot
#' @export
#'
#' @import echarts4r
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{plot_echeatmap(dp)}
#' \dontrun{
#' dp = list(NULL)
#' dp$tab = tab
#' dp$xval = 'x'
#' dp$yval = 'y'
#' dp$zval = 'z'
#' dp$color = 'c('#fef8cb', '#ba8d7e')
#' dp$text_format = ''
#' dp$show_label = TRUE
#' dp$label_color = '#606060'
#' dp$label_size = 10
#' dp$x_label_color = '#606060'
#' dp$x_label_size = 15
#' dp$y_label_color = '#606060'
#' dp$y_label_size = 15
#' dp$xaxis_title = xval
#' dp$yaxis_title = yval
#' dp$axis_title_color = '#606060'
#' dp$axis_title_size = 15
#' dp$show_xgrid = FALSE
#' dp$show_ygrid = FALSE
#' dp$margin_left = '10%'
#' dp$margin_right = '10%'
#' dp$margin_top = 60
#' dp$margin_bottom = 60
#' dp$title = 'Chart'
#' dp$chart_width = NULL
#' dp$chart_height = NULL
#' fplot = plot_echeatmap(dp)
#' fplot
#' }
#'
plot_echeatmap = function(dp = NULL){

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
  zval = ifelse('zval' %in% names(dp), dp$zval, 'z')  # z-col
  # colors
  dcol = c('#fef8cb', '#ba8d7e')
  if ('colors' %in% names(dp)){
    dcol = dp$colors
  }

  # chart size
  chart_width = df_assign(dp, 'chart_width', NULL)
  chart_height = df_assign(dp, 'chart_height', NULL)


  # grid margins
  margin_left   = ifelse('margin_left'  %in% names(dp), dp$margin_left,  '10%')  # x-col
  margin_right  = ifelse('margin_right' %in% names(dp), dp$margin_right, '10%')  # y-col
  margin_top    = ifelse('margin_top' %in% names(dp), dp$margin_top, 60)  # y-col
  margin_bottom = ifelse('margin_bottom' %in% names(dp), dp$margin_bottom, 60)  # y-col

  # text format
  text_format = ifelse('text_format' %in% names(dp), dp$text_format, '')
  # label
  show_label  = ifelse('show_label'  %in% names(dp), dp$show_label, TRUE)
  label_color = ifelse('label_color' %in% names(dp), dp$label_color, '#606060')
  label_size  = ifelse('label_size'  %in% names(dp), dp$label_size, 10)

  # axis labels
  x_label_color = ifelse('x_label_color' %in% names(dp), dp$x_label_color, '#606060')
  x_label_size  = ifelse('x_label_size'  %in% names(dp), dp$x_label_size, 15)

  y_label_color = ifelse('ax_label_color' %in% names(dp), dp$ax_label_color, '#606060')
  y_label_size  = ifelse('ax_label_size'  %in% names(dp), dp$ax_label_size, 15)

  # axis names ect
  xaxis_title  = ifelse('xaxis_title'  %in% names(dp), dp$xaxis_title, xval)
  yaxis_title  = ifelse('yaxis_title'  %in% names(dp), dp$yaxis_title, yval)
  axis_title_size  = ifelse('axis_title_size'  %in% names(dp), dp$axis_title_size, 15)
  axis_title_color  = ifelse('axis_title_color'  %in% names(dp), dp$axis_title_color, '#606060')

  # axis grid
  show_xgrid = ifelse('show_xgrid'  %in% names(dp), dp$show_xgrid, FALSE)
  show_ygrid = ifelse('show_ygrid'  %in% names(dp), dp$show_ygrid, FALSE)

  names(tab)[names(tab) == xval] = 'xval'
  names(tab)[names(tab) == yval] = 'yval'
  names(tab)[names(tab) == zval] = 'zval'

  # text and tooltip format
  js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[2]);
    return f;}"

  js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[2]);
    let g = params.value[1];
    let h = params.value[0];
    return g + '<br>' +h+': '+f;}"

  if (text_format == 'percent'){
    tab[, zval := zval * 100]
    js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[2]);
    return f+'%';}"

    js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[2]);
    let g = params.value[1];
    let h = params.value[0];
    return g + '<br>' +h+': '+f+'%';}"
  }

  if (text_format == 'euro'){
    js_numform = "function (params) {
    let f= Intl.NumberFormat('de-DE').format(params.value[2]);
    return f+'\u20AC';}"

    js_ttform = "function (params) {
    let f = Intl.NumberFormat('de-DE').format(params.value[2]);
      let g = params.value[1];
    let h = params.value[0];
    return g + '<br>' +h+': '+f+'\u20AC';}"
  }

  fplot = tab %>%
    e_charts(xval, width = chart_width, height = chart_height) %>%
    e_heatmap(yval, zval) %>%
    e_visual_map(zval,
                 show =FALSE,
                 inRange = list(
                   color = as.list(dcol)
                 ),
                 outOfRange = list(
                   color = list('grey')
                 )
    ) %>%
    e_labels(
      show = show_label,
      position = 'inside',
             fontSize = label_size,
             color=label_color,
             formatter = htmlwidgets::JS(js_numform)) %>%
    e_grid(left = margin_left, right = margin_right, top = margin_top, bottom = margin_bottom) %>%
    e_x_axis(
      type = "category",
      name = xaxis_title,
      nameLocation='middle',
      nameGap=30,
      nameTextStyle = list(color = axis_title_color, fontSize = axis_title_size),
      axisLine=list(show=FALSE),
      splitLine=list(show=show_xgrid, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      axisLabel = list(color=x_label_color, fontSize=x_label_size),
      axisTick = list(show=FALSE)
    ) %>%
    e_y_axis(
      type = "category",
      name = yaxis_title,
      nameLocation='end',
      nameGap=15,
      nameTextStyle = list(color = axis_title_color, fontSize = axis_title_size),
      axisLine=list(show=FALSE),
      splitLine=list(show=show_ygrid, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      axisLabel = list(color=y_label_color, fontSize=y_label_size),
      axisTick = list(show=FALSE)
    ) %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    e_tooltip(formatter = htmlwidgets::JS(js_ttform)) #%>%

     # Title
     if ('title' %in% names(dp)){
       fplot = fplot %>% e_title(dp$title, left = margin_left)
     }

  return(fplot)
}
