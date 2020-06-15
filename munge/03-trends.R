trn <- LGA_trends
names(trn) <- trn[3,]
trn <- trn[-c(1:3), -c(8:10)]

tmp <- trn %>%
  filter(`Offence type` == "Domestic violence related assault") %>%
  gather(period, value, 3:7)

