# population data for all LGAs and all year
# approx() use based on https://stackoverflow.com/questions/27920690/linear-interpolation-using-dplyr

pop <- dem_pop_proj_age_snap 
names(pop) <- pop[3,]


lga <- c("Alls", 
             "Gundagai", 
             "Botany Bay", 
             "Rockdale", 
             "Western Plains Regional")

crime.name <- c("State total", 
                "Cootamundra-Gundagai", 
                "Bayside", 
                "Bayside", 
                "Dubbo Regional")

fix <- data.frame(lga, crime.name)

pop <- pop[-c(1:3),-c(2:3)] %>%
  gather(year, value, 3:10) %>%
  mutate(age = trimws(` Age (years)`),
         year = as.numeric(gsub("\\*", "", trimws(year))),
         value = as.numeric(gsub(",", "", value)),
         lga = gsub(" LGA", "", trimws(` Local Government Areas`))) %>%
  # note that "All LGAs" becomes "Alls"
  filter(age == "All ages",
         year %in% c(2011:2021),
         complete.cases(.)) %>%
  distinct() %>%
  select(lga, year, value) %>%
  group_by(lga) %>%
  complete(year = full_seq(2011:2021, 1)) %>%
  mutate(annual.pop = approx(year,value,year)$y) %>%
  ungroup() %>%
  left_join(fix) %>%
  mutate(lga = ifelse(lga %in% fix$lga, crime.name, lga)) %>%
  select(-value, - crime.name)
