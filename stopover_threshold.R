t <- fall_classified %>% 
  filter(step_state == 3) #%>% 
  pull(step) %>% 
  quantile(probs = 0.95, na.rm = TRUE)
  
  # 95% 
  # 2.41869 km

  
rbind(fall_classified, spring_female_classified, spring_male_classified) %>% 
  filter(step_state == 3) %>% 
  pull(step) %>% 
  quantile(probs = 0.99, na.rm = TRUE)
    
# 99% 
# 6.678688 
  
