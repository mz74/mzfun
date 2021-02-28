library(devtools)

#use_package('ggplot2')
#use_package('echarts4r')
#use_package('magrittr')
#use_package('htmlwidgets')
document()
load_all()
check()
install()

#use_r('add_table_bottom')
#use_r('add_table_right')
#use_r('explore_table')
#use_r('plot_ecbar')
#use_r('plot_ecdots')
use_r('plot_heatmap')

# change readme.rmd and then
#build_readme()

# test 'add_table_bottom'
library(data.table)
library(magrittr)
library(ggplot2)
library(echarts4r)
library(mzfun)

# heatmap

v <- paste(LETTERS[1:10], '- datetime')
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE),
  y = sample(v, 300, replace = TRUE),
  z = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) %>%
  dplyr::group_by(x, y) %>%
  dplyr::summarise(z = sum(z)) %>%
  dplyr::ungroup()
#> `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.

matrix %>% setDT
matrix[, z := z %>% round(0)]
dp = list(NULL)
dp$data = matrix
dp$xval = 'x'
dp$yval = 'y'
dp$zval = 'z'
dp$colors = c('#fef8cb', '#985b5a')
dp$label_color = 'black'
dp$xaxis_title = 'Title from x'
dp$yaxis_title = 'Title from y'
mp = plot_echeatmap(dp)
mp


matrix %>%
  e_charts(x) %>%
  e_heatmap(y, z) %>%
  e_visual_map(z) %>%
  e_title("Heatmap")

# add_table_right
d1 = mtcars
d2 = iris

tab2 = (d2 %>% setDT)[, .(mean1 = mean(Petal.Width)), by=.(Species)]
tab2[, color2 := 'grey'][1, color2 := 'blue']
dt1 = (tab2 %>% copy)[, key := paste0('k', (.N %>% seq))][, mean2 := mean1 %>% copy]
dp = list(NULL)
dp$data = tab2
dp$xval = 'Species'
dp$yval = 'mean1'
dp$color = 'color2'
dp$text_format = 'euro'
dp$margin_left = '12%'
dp$pointsize = 35
dp$linesize = 5
mp = plot_eclollipop(dp)
mp


tab3 = (d2 %>% setDT)[, id := .N %>% seq]
dcols = names(tab3) %>% setdiff('Species')
tab3[, c(dcols) := lapply(.SD, as.numeric), .SDcols = c(dcols)]
dp = list(NULL)
dp$data = tab3
dp$xval = 'id'
dp$yval = 'Petal.Length'
dp$group = 'Species'
dp$text_format = ''
dp$margin_right = '12%'
dp$legend = TRUE
#dp$xaxis_title = 'Holla'
dp$yaxis_title = NA
dp$show_xgrid = TRUE
dp$show_ygrid = TRUE
dp$title = 'ein neues Chart'
dp$color = c('#b4943e', '#777acd', '#60a862', '#c45ca2', '#cb5a4c')
mp = plot_ecscatter(dp)
mp

locales = system("locale -a", intern = TRUE) %>% data.table()

d3 = '/home/mz/mz/datascience/20_create_data/test_data.txt' %>% fread(colClasses = 'character')
#d33 = d3[, .(Anzahl = .N), by=.(Milieu, Wohnart)]
d33 = d3[, .(Anzahl = .N), by=.(Alter)]
d33[, color := 'grey'][1, color := 'blue']
#d33[Alter == '11', Anzahl := -545]
d33 = d33[order(Alter %>% as.numeric)]
dp = list(NULL)
dp$data = d33
dp$xval = 'Alter'
dp$yval = 'Anzahl'
#dp$group = NULL
#dp$color = 'color2'
#dp$text_format = 'euro'
dp$margin_left = '100'
dp$title = 'Chart'
dp$legend = 'blue'
dp$label_color = NULL
dp$label_size = '10'
dp$show_label = FALSE
dp$pointsize = 10
dp$label_position = 'out'
dp$show_ygrid = TRUE
dp$show_xaxis_line = TRUE
dp$show_yaxis = TRUE
dp$xaxis_title = 'Altersklasse'

#mp = plot_ecbar(dp)
mp = plot_eclollipop(dp)
mp


# dot plot
d3 = '/home/mz/mz/datascience/20_create_data/test_data.txt' %>% fread(colClasses = 'character')
d33 = d3[, .(Gehalt = Gehalt %>% as.numeric %>% mean), by=.(Altersklasse, Milieu)]
d33[, color := 'grey'][Altersklasse == '5', color := 'blue']
dp = list(NULL)
dp$data = d33
dp$xval = 'Milieu'
dp$yval = 'Gehalt'
dp$group = 'Altersklasse'
dp$color = 'color'
dp$pointsize = 15
dp$text_format = 'euro'
#dp$line_color = 'green'
dp$linesize = 0.1
#dp$show_line = FALSE
dp$label_position = 'top'

mp = plot_ecdot(dp)
mp


for (i in names(d3)){
  d3[sample(c(1:.N), 1000), c(i) := NA]
  d3[sample(c(1:.N), 1000), c(i) := '']
}
d3 = d3[, id := id %>% as.numeric()]
d33 = d3[, id := id / 15000]

# test explore_table
ftable = d33 %>% copy
ftarget = 'Anrede'
fassign_classes = TRUE
fprm_na = FALSE
fpmax_numlevels = 20

et1 = explore_table(copy(d33), ftarget = 'Anrede', fassign_classes = TRUE,
                    fadd_tables = TRUE, fpmax_numlevels = 20, fprm_na = FALSE,
                    fadd_plots = TRUE, fptext_labels = TRUE)
et1$plot
et1$table

et1 = explore_table(copy(d3), ftarget = 'Anrede', fassign_classes = TRUE, fadd_tables = TRUE, fadd_plots = TRUE, fpmax_numlevels = 20, fprm_na = FALSE, fpmax_faclevels = 50)

et2 = explore_table(copy(d2), ftarget = 'Species', fadd_plots = TRUE, fmax_numlevels = 20)
et3 = explore_table(copy(d1), ftarget = NA, fadd_plots = TRUE, fmax_numlevels = 20)

d3 = add_table_right(ftable=d2, colname='Gesamt', ffun = sum)
d4 = add_table_right(ftable=d2, colname='Gesamt', ffun = mean, fcols = names(d2)[c(1:2)])


# add_table_bottom
d1 = iris
d2 = add_table_bottom(d1)
d3 = add_table_bottom(d1, ffun = mean)
d3 = add_table_bottom(d1, ffun = mean, fname = c(Species = 'Gesamt'))
class(d1)
class(d2)

d1[0]

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x)
colSums(x)
x
colSums(d1)


stringi::stri_escape_unicode('ä')
print(' - Bedingte H\u00e4ufigkeit je Segment')

# about signigy
library(data.table)
b1 = data.table(A = c(0.3472343, 0.00234))
b1[, b2 := signif(b1, 3)]
b1[, b3 := as.character(b2)]




