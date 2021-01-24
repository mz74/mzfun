library(devtools)

use_r('add_table_bottom')
use_r('add_table_right')
document()
load_all()
check()
install()


# test 'add_table_bottom'
library(data.table)
library(magrittr)

# add_table_right
d1 = mtcars
d2 = iris
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
