library(devtools)

use_r('add_table_bottom')
document()
load_all()
check()
install()


# test 'add_table_bottom'
library(data.table)
library(magrittr)

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
