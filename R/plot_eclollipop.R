#' Interactive Lollipop Plot
#'
#' @param dp List of parameters
#'
#' \itemize{
#' \item tab: table to be plotted, already aggregated
#' \item xval = 'x': column name with x-values, categorical or integer values
#' \item yval = 'y': column name with y-values, numerical values
#' \item color = 'color': column name with colors
#' \item pointsize = 8: symbol size
#' \item linesize = 2: line width of line plot
#' \item title: chart title
#' \item text_format = '': text format is '' (standard), 'percent', or 'euro'
#' \item show_label = TRUE: show text labels (TRUE/FALSE)
#' \item label_color = '#A9A9A9': color of text labels
#' \item label_size = 10: size of text labels
#' \item label_position = 'out': position of text labels ('out', 'left', 'right', 'inside', 'top', 'bottom')
#' \item x_label_color = '#606060': color of x-axes text labels
#' \item x_label_size = 15: size of x-axes text labels
#' \item y_label_color = '#606060': color of y-axes text labels
#' \item y_label_size = 10: size of y-axes text labels
#' \item xaxis_title = '': name of x-axis title (string), set NA to remove it
#' \item show_xaxis_line = FALSE: show (vertical) x-axis line
#' \item show_yaxis = FALSE: show (horizontal) y-axis
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
#' }
#'
#'
#' @return an echarts4r lollipop plot
#' @export
#'
#' @import echarts4r
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{plot_eclollipop(dp)}
#' \dontrun{
#' dp = list(NULL)
#' dp$tab = tab
#' dp$xval = 'x'
#' dp$yval = 'y'
#' dp$color = 'color'
#' dp$pointsize = 8
#' dp$linesize = 2
#' dp$title = NULL
#' dp$text_format = ''
#' dp$show_label = FALSE
#' dp$label_color = '#A9A9A9'
#' dp$label_size = 10
#' dp$label_position = 'out'
#' dp$x_label_color = '#606060'
#' dp$x_label_size = 15
#' dp$y_label_color = '#606060'
#' dp$y_label_size = 10
#' dp$xaxis_title = NULL
#' dp$show_xaxis_line = FALSE
#' dp$show_yaxis = FALSE
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
#' fplot = plot_lollipop(dp)
#' fplot
#' }
#'
plot_eclollipop = function(dp = NULL){

  color = NULL
  key = NULL
  dyval = NULL
  yval1 = NULL
  yval2 = NULL

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

  # color
  col  = ifelse('color' %in% names(dp), dp$color, 'color')  # color-col
  # add color col
  if (!col %in% names(tab)){
    tab[, color := '#da9e92']
  }

  # chart size
  chart_width = df_assign(dp, 'chart_width', NULL)
  chart_height = df_assign(dp, 'chart_height', NULL)

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
  # label
  show_label  = ifelse('show_label'  %in% names(dp), dp$show_label, TRUE)
  label_color = ifelse('label_color' %in% names(dp), dp$label_color, '#A9A9A9')
  label_size  = ifelse('label_size'  %in% names(dp), dp$label_size, 10)
  # positions: out, left, right, inside, top, bottom
  label_position = ifelse('label_position' %in% names(dp), dp$label_position, 'out')
  # label position details
  if (label_position == 'out'){
    label_pos1 = 'right'  # positive values
    label_pos2 = 'left'   # negative values
  } else {
    label_pos1 = label_position
    label_pos2 = label_position
  }

  x_label_color = ifelse('x_label_color' %in% names(dp), dp$x_label_color, '#606060')
  x_label_size  = ifelse('x_label_size'  %in% names(dp), dp$x_label_size, 15)

  # axis labels
  y_label_color = ifelse('ax_label_color' %in% names(dp), dp$ax_label_color, '#606060')
  y_label_size  = ifelse('ax_label_size'  %in% names(dp), dp$ax_label_size, 10)

  # axis names ect
  xaxis_title  = ifelse('xaxis_title'  %in% names(dp), dp$xaxis_title, '')
  yaxis_title  = ifelse('yaxis_title'  %in% names(dp), dp$yaxis_title, yval)
  axis_title_size  = ifelse('axis_title_size'  %in% names(dp), dp$axis_title_size, 15)
  axis_title_color  = ifelse('axis_title_color'  %in% names(dp), dp$axis_title_color, '#606060')
  show_xaxis_line = ifelse('show_xaxis_line'  %in% names(dp), dp$show_xaxis_line, FALSE)
  show_yaxis = ifelse('show_yaxis'  %in% names(dp), dp$show_yaxis, FALSE)


  # axis grid
  show_xgrid = ifelse('show_xgrid'  %in% names(dp), dp$show_xgrid, FALSE)
  show_ygrid = ifelse('show_ygrid'  %in% names(dp), dp$show_ygrid, FALSE)

  names(tab)[names(tab) == xval] = 'xval'
  names(tab)[names(tab) == yval] = 'yval'
  names(tab)[names(tab) == col] = 'color'

  # prepare table for lollipop
  dt1 = (tab %>% copy)[, key := paste0('k', (.N %>% seq))][, dyval := yval %>% copy]
  dt2 = (tab %>% copy)[, key := paste0('k', (.N %>% seq))][, dyval := 0][, yval := NA]
  tab2 = rbind(dt1, dt2)

  # pos vs negative values
  tab2[yval >= 0, yval1 := yval]
  tab2[yval < 0, yval2 := yval]

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
    return g + '<br>' +'Wert: '+f;}"

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
    return g + '<br>' +'Wert: '+f+'%';}"
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
    return g + '<br>' +'Wert: '+f+'\u20AC';}"
  }


  fplot = tab2 %>%
    group_by(key) %>%
    e_charts(xval, width = chart_width, height = chart_height) %>%
    e_grid(left = margin_left, right = margin_right, top = margin_top, bottom = margin_bottom) %>%
    e_line(dyval, symbolSize = 0, lineStyle = list(width=linesize)) %>%
    # e_bar(yval,
    #       barWidth = 2,
    #       barCategoryGap = '100%',
    #       barGap = '-100%') %>%
    e_scatter(yval1,
              symbolSize = pointsize,
              itemStyle = list(opacity=1),
              label = list(show=show_label,
                           position = label_pos1,
                           fontSize = label_size,
                           color=label_color,
                           formatter = htmlwidgets::JS(js_numform)
                           )) %>%
    e_scatter(yval2,
              symbolSize = pointsize,
              itemStyle = list(opacity=1),
              label = list(show=show_label,
                           position = label_pos2,
                           fontSize = label_size,
                           color=label_color,
                           formatter = htmlwidgets::JS(js_numform)
              )) %>%
   # e_scatter(yval2, symbolSize = pointsize, itemStyle = list(opacity=1)) %>%
    #e_bar(yval) %>%
    e_color(color = tab$color) %>%
    #e_add("itemStyle", opacity=1) %>%
 #   e_labels(fontSize = label_size, color=label_color,
#             formatter = htmlwidgets::JS(js_numform)) %>%
    # e_add('label', position) %>%
    e_x_axis(
      type = "category",
      name = xaxis_title,
      nameLocation='end',
      nameTextStyle = list(color = axis_title_color, fontSize = axis_title_size),
      axisLine=list(show=show_xaxis_line, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      splitLine=list(show=show_xgrid, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      axisLabel = list(color=x_label_color, fontSize=x_label_size),
      axisTick = list(show=FALSE)
    ) %>%
    e_y_axis(
      show = show_yaxis,
      name = yaxis_title,
      nameLocation='middle',
      nameGap=30,
      nameTextStyle = list(color = axis_title_color, fontSize = axis_title_size),
      axisLine=list(show=FALSE),
      splitLine=list(show=show_ygrid, lineStyle = list(color=fgrey, opacity=1, width=fgrid_size)),
      axisLabel = list(color=y_label_color, fontSize=y_label_size, formatter = htmlwidgets::JS(js_axisform)),
      axisTick = list(show=FALSE)
    ) %>%

    e_flip_coords() %>%
    #e_text_style(fontSize = 30, color='blue') %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    #  e_toolbox_feature(feature = "dataView") %>%
    e_tooltip(formatter = htmlwidgets::JS(js_ttform))

  # Title
  if ('title' %in% names(dp)){
    fplot = fplot %>% e_title(dp$title, left = margin_left)
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
