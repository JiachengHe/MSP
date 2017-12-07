df %>% 
  mutate(female = (SEX==2), 
         college = (as.numeric(EDUCA) %in% 5:6),
         work = (as.numeric(EMPLOY) %in% 1:2)) %>% 
  filter(as.numeric(as.character(event_time)) >= 0) %>% 
  group_by(treated, post_treatment) %>% 
  summarize(MEDCOST = mean(MEDCOST, na.rm = TRUE),
            female = mean(female, na.rm = TRUE), 
            college = mean(college, na.rm = TRUE),
            work = mean(work, na.rm = TRUE))
