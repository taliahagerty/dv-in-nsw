
# For map: raw incidents by lga

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


# For trend chart: monthly rates, nationwide

nsw <- pop %>%
  filter(lga == "State total") %>%
  select(-lga)

tmp <- incidents.nsw[, c(3, 7:309)] %>%
  filter(Subcategory == "Domestic violence related assault") %>%
  gather(date, value, -1) %>%
  separate(date, into = c("month", "year"), sep = "\\.") %>%
  mutate(year = as.numeric(ifelse(year %in% c("19", "20"), paste0("20", year), year)),
         indicator = "incidents") %>%
  filter(year %in% c(2011:2020)) %>%
  left_join(nsw, by = c("year")) %>%
  mutate(value = value / (annual.pop / 100000)) %>%
  mutate(indicator = "rate") %>%
  select(-annual.pop) %>%
  unite(monthyear, month, year, sep=" ", remove=FALSE, na.rm=FALSE) %>%
  mutate(monthyear = as_date(as.yearmon(monthyear))) %>%
  arrange(monthyear) 
  
  
  
  mutate(date = as.Date(paste(tmp$year,tmp$month, "01", sep="-"), format = "%Y-%b-%d") ) %>%
  mutate(month = month(date),
         year= year(date),
         date = format.Date(date, format = "%m-%y"))


# %>%
#   arrange(race, year, month) %>%
#   select(date, race, n) %>% 
#   distinct() %>%
#   filter(race %in% c("A", "W", "H", "B")) %>%
#   mutate(race = recode(race, "A" = "Asian",
#                        "B" = "Black",
#                        "W" = "White",
#                        "H" = "Hispanic")) 


ggplot(tmp, aes(x = monthyear, y = value, group = 1)) +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y = "Rate per 100,000", x = "") +
  geom_smooth(se = F) 
  
  
  scale_color_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  # theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Race")
© 2020 GitHub, Inc.

















# compare change

chg <- df %>%
  # filter()
  group_by(lga, indicator, year) %>%
  summarise(value )
  filter(year %in% c(min(year), max(year))) %>%
  arrange(lga, indicator, year) %>%
  group_by(lga, indicator) %>%
  mutate(diff = value - lag(value),
         change = (diff/lag(value))*100)



key <- top_n(x, 5, pctchg) %>% pull(pctchg)
pos <- which(x$pctchg %in% key)
x <- x[pos, ] %>%
  arrange(-pctchg)