#' Interactive Dot or Cleveland Plot
#'
#' @param dp List of parameters
#'
#' \itemize{
#' \item tab: table to be plotted, already aggregated
#' \item xval = 'x': column name with x-values
#' \item yval = 'y': column name with y-values
#' \item group = 'group': column name with group values
#' \item color = 'color': column name with colors
#' \item pointsize = 8: symbol size
#' \item linesize = 2: line width of line plot
#' \item show_line = TRUE: show horizontal lines (TRUE/FALSE)
#' \item line_color = '#A9A9A9': color of horizontal lines
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
#' \item margin_right = '12': chart margin right
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
#' \dontrun{plot_ecdot(dp)}
#' \dontrun{
#' dp = list(NULL)
#' dp$tab = tab
#' dp$xval = 'x'
#' dp$yval = 'y'
#' dp$group = 'group'
#' dp$color = 'color'
#' dp$pointsize = 8
#' dp$linesize = 2
#' dp$show_line = TRUE
#' dp$line_color = '#A9A9A9'
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
#' dp$margin_right = '12'
#' fplot = plot_dot(dp)
#' fplot
#' }
#'
plot_ecdot = function(dp = NULL){

  color = NULL
  key   = NULL
  dyval = NULL
  yval1 = NULL
  yval2 = NULL
  id    = NULL

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

  # symbol size and line size
  pointsize  = ifelse('pointsize' %in% names(dp), dp$pointsize, 8)
  linesize   = ifelse('linesize'  %in% names(dp), dp$linesize,  2)

  # show lines
  show_line  = ifelse('show_line'  %in% names(dp), dp$show_line, TRUE)
  line_color = ifelse('line_color' %in% names(dp), dp$line_color, '#A9A9A9')

  # grid margins
  margin_left  = ifelse('margin_left'  %in% names(dp), dp$margin_left,  '10%')  # x-col
  margin_right = ifelse('margin_right' %in% names(dp), dp$margin_right, '12')  # y-col
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
  names(tab)[names(tab) == group] = 'group'


  # prepare tab
  tab[, id := .N %>% seq]
  tab = tab[order(yval)]
  tab[!xval %>% duplicated(), yval2 := yval]
  tab[yval2 %>% is.na, yval1 := yval]
  tab = tab[order(id)]

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
    return g + '<br>' +h+': '+f+'\u20AC';}"
  }

  test_line <- list(
    list(
      coord = c(0.1, 0.1)
    ),
    list(
      coord = c(5000, 1500)
    )
  )



  fplot = tab %>%
    group_by(group) %>%
    e_charts(xval) %>%
    # e_mark_line(data = list(xAxis = 'liberal')) %>%
    e_grid(left = margin_left, right = margin_right) %>%
    e_scatter(yval,
              symbolSize = pointsize,
              itemStyle = list(opacity=1),
              z = 5,
              zlevel = 5
    ) %>%
    e_add("itemStyle", color) %>%
    e_color(color = tab[!group %>% duplicated]$color) %>%
    # e_line(yval,
    #        symbolSize = 0,
    #        seriesLayoutBy = 'column',
    #        lineStyle = list(width=linesize, color=fgrey)) %>%

    e_scatter(yval1,
              symbolSize = 1e-6,
              itemStyle = list(opacity=1),
              label = list(show=show_label,
                           position = label_pos1,
                           fontSize = label_size,
                           color=label_color,
                           distance = pointsize/2+5,
                           formatter = htmlwidgets::JS(js_numform)
              )) %>%
    e_scatter(yval2,
              symbolSize = 1e-6,
              itemStyle = list(opacity=1),
              label = list(show=show_label,
                           position = label_pos2,
                           fontSize = label_size,
                           color=label_color,
                           distance = pointsize/2+5,
                           formatter = htmlwidgets::JS(js_numform)
              )) %>%
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
      axisTick = list(show=FALSE)#,
      #min = 'dataMin'
    ) %>%

    e_flip_coords() %>%
    e_legend(show = TRUE) %>%
    #e_text_style(fontSize = 30, color='blue') %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    #  e_toolbox_feature(feature = "dataView") %>%
    e_tooltip(formatter = htmlwidgets::JS(js_ttform)) #%>%
  #e_mark_line(data = list(yAxis = 'traditionell'), title = "Speed Limit") %>%
  #e_mark_line(data = test_line)

  # Title
  if ('title' %in% names(dp)){
    fplot = fplot %>% e_title(dp$title, left = margin_left)
  }

  # plot the lines
  if (show_line == TRUE){
    dgroup = tab$xval %>% unique
    for (i in dgroup){
      dtab = tab[xval %in% i]
      dmin = dtab$yval %>% min(na.rm = TRUE) + pointsize
      dmax = dtab$yval %>% max(na.rm = TRUE) - pointsize
      dline = list(
        list(
          coord = c(dmin, i)
        ),
        list(
          coord = c(dmax, i)
        )
      )
      fplot = fplot %>%
        e_mark_line(data = dline,
                    silent = TRUE,
                    symbol = 'none',
               #     zlevel = 5,
                #    z = 5,
                #    Z = 5,
                    lineStyle = list(type = 'solid',
                                     color = line_color,
                                     width = linesize))
    }
  }

  return(fplot)

}
