trn <- LGA_trends
names(trn) <- trn[3,]
trn <- trn[-c(1:3), -c(8:10)]

trn <- trn %>%
  filter(`Offence type` == "Domestic violence related assault") %>%
  gather(period, value, 3:7)

latest <- trn %>%
  filter(period == "Apr 2019 - Mar 2020")
