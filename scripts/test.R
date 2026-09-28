clean <- import(here("data", "backup", "carehome_clean1.rds"), 
                trust = T)
x <- clean %>% 
  mutate(dayhour = dayonset + starthour,
         buffet_datetime = ymd_h("2026-09-13 14h"),
         incubation = dayhour - buffet_datetime

         )
  
  mutate(dayhour = str_glue("{dayonset}, {starthour}", .na=""),
         dayhour2 = paste0(dayonset, " ", starthour),
         
         dateformat = dmy_h(dayhour,  truncated = 2),
         dateformat2 = dmy_h(dayhour2))

clean <- clean %>% 
    mutate(onset_datetime = dayonset + starthour) %>% 
  mutate(buffet_datetime = ymd_h("2026-09-13 14h"),
         incubation = as.numeric(onset_datetime - buffet_datetime))
  
clean %>% 
  # filter(!is.na(incubation),
  #        incubation >= 12 & incubation <= 72) %>% 
  ggplot(aes(x = incubation)) +
  geom_histogram(binwidth = 6) +
  scale_x_continuous(breaks = seq(min(clean$incubation, na.rm = T), max(clean$incubation, na.rm = T), 12))
