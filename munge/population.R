load.project()


event.date <- c("2010-05-25", "2010-09-10", "2011-05-13", "2012-03-28", "2013-03-07",    
                "2014-02-13", "2010-06-11", "2010-09-10", "2011-05-13", "2012-03-28",
                "2013-03-07", "2014-02-13")
variable   <- c("neck.bmd", "neck.bmd", "neck.bmd", "neck.bmd", "neck.bmd", "neck.bmd",
                "wbody.bmd", "wbody.bmd", "wbody.bmd", "wbody.bmd", "wbody.bmd", "wbody.bmd")
value      <- c(0.7490, 0.7615, 0.7900, 0.7730, NA, 0.7420, 1.0520, 1.0665, 1.0760,
                1.0870, NA, 1.0550)
## Bind into a data frame
df <- data.frame(event.date, variable, value)
rm(event.date, variable, value)
## Convert date
df$event.date <- as.Date(df$event.date)
## Load libraries
library(magrittr)
library(xts)
library(zoo)


tmp <- df %>%
  group_by(variable) %>%
  arrange(variable, event.date) %>%
  mutate(time=seq(1,n())) %>%
  mutate(ip.value=approx(time,value,time)$y) %>%
  select(-time)
