
# Raw incidents by lga

df <- RCI_offencebymonth 
names(df) <- tolower(names(df))

time.series <- c(min(pop$year):year(now()))

df <- df %>% 
  mutate(subcategory = gsub("\\*", "", trimws(subcategory))) %>% 
  mutate(subcategory = ifelse(subcategory %in% "", offence.category, subcategory)) %>%
  filter(subcategory == "Domestic violence related assault") %>%
  select(-subcategory, -offence.category ) %>%
  gather(date, value, -lga) %>%
  separate(date, into = c("month", "year"), sep = "\\.") %>%
  mutate(year = as.numeric(ifelse(year %in% c("19", "20"), paste0("20", year), year)),
         indicator = "incidents") %>%
  filter(year %in% time.series)


  

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
  filter(year %in% c(2015:2020)) %>%
  left_join(nsw, by = c("year")) %>%
  mutate(value = value / (annual.pop / 100000)) %>%
  mutate(indicator = "rate") %>%
  select(-annual.pop) %>%
  unite(monthyear, month, year, sep=" ", remove=FALSE, na.rm=FALSE) %>%
  mutate(monthyear = as_date(as.yearmon(monthyear))) %>%
  arrange(monthyear) 

# %>%
#   mutate(mavg = SMA(value, n=4)) %>%
#   select(-Subcategory, -indicator) %>%
#   gather(indicator, value, 4:5)



ggplot(tmp, aes(x = monthyear, y = value, group = indicator)) +
  geom_line() +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 25)) + 
  labs(y = "Monthly rate per 100,000 people", x = "") +
  geom_smooth(se = F, span = 0.4) 



# 
# # compare change
# 
# chg <- df %>%
#   # filter()
#   group_by(lga, indicator, year) %>%
#   summarise(value )
#   filter(year %in% c(min(year), max(year))) %>%
#   arrange(lga, indicator, year) %>%
#   group_by(lga, indicator) %>%
#   mutate(diff = value - lag(value),
#          change = (diff/lag(value))*100)
# 
# 
# 
# key <- top_n(x, 5, pctchg) %>% pull(pctchg)
# pos <- which(x$pctchg %in% key)
# x <- x[pos, ] %>%
#   arrange(-pctchg)