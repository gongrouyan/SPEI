library(readxl)
library(data.table)
library(lubridate)
library(magrittr)


df <- read_xls("INPUT/gd_water.xls") %>% data.table()
df[, date := make_date(year, month, 1)]
df <- reorder_name(df, c("year", "month", "date"))

library(foreach)
library(iterators)

sitenames <- colnames(df)[-(1:3)]
lst = foreach(sitename = sitenames, i = icount()) %do% {
  r = sgi_site(df, sitename = sitename, scale=3, na.rm = TRUE)
}

write_fig(expression({
    par(mfrow = c(2, 4))
    temp = foreach(r = lst, sitename = sitenames, i = icount(1)) %do% {
      plot_sgi(r)
      title(sitename)
    }
}), "sgi_zhanj.pdf", 10, 5)

d = r %>% {data.table(date =date(.$data), data = .$data, fit = .$fitted)}
