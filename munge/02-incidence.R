
df <- RCI_offencebymonth 
names(df) <- tolower(names(df))

df <- df %>%
  filter(subcategory == "Domestic violence related assault") %>%
  select(-subcategory, -offence.category ) %>%
  gather(date, value, -lga) %>%
  separate(date, into = c("month", "year"), sep = "\\.") %>%
  mutate(year = as.numeric(ifelse(year %in% c("19", "20"), paste0("20", year), year)),
         indicator = "incidents") %>%
  filter(year %in% c(2011:2020))


tmp <- df %>%
  left_join(pop, by = c("lga", "year")) %>%
  mutate(value = value / (annual.pop / 10000)) %>%
  filter(annual.pop > 10000)
