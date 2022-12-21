library(tidyverse)
library(tidytext)

library(wordcloud2)

cpp <- read_csv("C:\\Users\\Zoey Lu\\OneDrive - Delaware Education\\Data Requests\\Educator Preparation\\SLPP\\Monthly Questionnaire Results\\Candidate's Performance & Preparedness (Responses) - Form Responses 1.csv")

q1 <-  cpp %>% select(program, q1)
q2 <-  cpp %>% select(program, q2)
q3 <-  cpp %>% select(program, q3)
q4 <-  cpp %>% select(program, q4)

q1_wrd <-  q1%>% 
  unnest_tokens(word, q1) %>%
  anti_join(stop_words) %>% drop_na(word)

q2_wrd <-  q2%>% 
  unnest_tokens(word, q2) %>%
  anti_join(stop_words)%>% drop_na(word)

q3_wrd <-  q3%>% 
  unnest_tokens(word, q3) %>%
  anti_join(stop_words)%>% drop_na(word)

q4_wrd <-  q4%>% 
  unnest_tokens(word, q4) %>%
  anti_join(stop_words)%>% drop_na(word)
  

count_q1 <-  q1_wrd %>% 
  group_by(program)%>%
  count(word) %>%
  arrange(desc(n)) 
  
count_q2 <-  q2_wrd %>% 
  group_by(program)%>%
  count(word) %>%
  arrange(desc(n)) 

count_q3 <-  q3_wrd %>% 
  group_by(program)%>%
  count(word) %>%
  arrange(desc(n)) 

count_q4 <-  q4_wrd %>% 
  group_by(program)%>%
  count(word) %>%
  arrange(desc(n)) 

count_bind <-  rbind(count_q1, count_q2, count_q3, count_q4)

regroup <-  aggregate(n ~ word, data=count_bind, FUN=sum)
  
wordcloud2(regroup, size = 0.7, shape = 'star')


