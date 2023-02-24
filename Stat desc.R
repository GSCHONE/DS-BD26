#Stat desc PL

library(tidyverse)
library(xlsx)

#Match PL
colnames(Match_PL)

result <-  Match_PL %>% 
     mutate(result = home_team_goal-away_team_goal) %>% 
     group_by(home_team_goal, away_team_goal, result) %>% 
     summarise(n = n()) %>% 
     arrange(result) 

write.csv(result,"result.CSV")








