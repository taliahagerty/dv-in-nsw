trn <- LGA_trends
names(trn) <- trn[3,]
trn <- trn[-c(1:3), -c(8:10)]

trn <- trn %>%
  filter(`Offence type` == "Domestic violence related assault") %>%
  gather(period, value, 3:7) %>%
 mutate(value = as.numeric(value)) %>%
  rename(lga = `Local Government Area`) %>%
  mutate(lga = gsub("Cootamundra-Gundagai", "Cootamundra-Gundagai Regional", lga)) %>%
  mutate(lga = gsub("Greater Hume Shire", "Greater Hume", lga)) %>%
  mutate(lga = gsub("Glen Innes Severn", "Glen Innes Severn Shire", lga)) %>%
  mutate(lga = gsub("Upper Hunter Shire", "Upper Hunter", lga)) %>%
  mutate(lga = gsub("Warrumbungle Shire", "Warrumbungle", lga))
  
  
  
  
  

latest <- trn %>%
  filter(period == "Apr 2019 - Mar 2020") %>%
  arrange(-value) 
