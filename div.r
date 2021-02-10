library(devtools)

#use_package('ggplot2')
document()
load_all()
check()
install()

#use_r('add_table_bottom')
#use_r('add_table_right')
#use_r('explore_table')



# change readme.rmd and then
#build_readme()

# test 'add_table_bottom'
library(data.table)
library(magrittr)
library(ggplot2)
library(mzfun)

# add_table_right
d1 = mtcars
d2 = iris
d3 = '/home/mz/mz/datascience/20_create_data/test_data.txt' %>% fread(colClasses = 'character')
for (i in names(d3)){
  d3[sample(c(1:.N), 1000), c(i) := NA]
  d3[sample(c(1:.N), 1000), c(i) := '']
}

# test explore_table
ftable = d3 %>% copy
ftarget = 'Anrede'
fassign_classes = TRUE
fprm_na = FALSE

et1 = explore_table(copy(d3), ftarget = 'Anrede', fassign_classes = TRUE,
                    fadd_tables = TRUE, fpmax_numlevels = 20, fprm_na = FALSE)
et1$summary
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
