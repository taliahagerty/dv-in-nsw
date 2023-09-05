# population data for all LGAs and all year
# approx() use based on https://stackoverflow.com/questions/27920690/linear-interpolation-using-dplyr

pop <- dem_pop_proj_age_snap 
names(pop) <- as.character(pop[3,])


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
  pivot_longer(3:10, names_to = "year", values_to = "value") %>%
  mutate(age = trimws(` Age (years)`),
         year = as.numeric(gsub("\\*", "", trimws(year))),
         value = as.numeric(gsub(",", "", value)),
         lga = gsub(" LGA", "", trimws(` Local Government Areas`))) %>%
  # note that "All LGAs" becomes "Alls"
  filter(age == "All ages",
         complete.cases(.)) %>%
  distinct() %>%
  select(lga, year, value) %>%
  group_by(lga) %>%
  complete(year = full_seq(min(year):max(year), 1)) %>%
  mutate(annual.pop = round(approx(year,value,year)$y)) %>%
  ungroup() %>%
  left_join(fix) %>%
  mutate(lga = ifelse(lga %in% fix$lga, crime.name, lga)) %>%
  select(-value, - crime.name)
