library(unvotes)
library(dplyr)
library(magrittr)
library(MASS)
votes<-un_votes
un_roll_calls
joined <- un_votes %>% inner_join(un_roll_calls, by = "rcid")
un_agree<-function(db,country1, country2, abstain = TRUE)
{
  c1 <- db %>% 
    filter(country == country1) %>%
    mutate("vote2" = vote)
  c2 <- db %>% 
    filter(country == country2)
  j <- c1 %>% select(vote2, rcid) %>%
    left_join(., c2 %>% select(rcid, vote), by = "rcid") %>%
    filter(!is.na(vote))
  if(!abstain){
    j <- j %>% 
      filter(vote != "abstain") %>%
      filter(vote2 != "abstain")
  }
  j <- j %>% 
    mutate("agree" = ifelse(vote2 == as.character(vote), 1, 0))
  sum(j$agree)/length(j$agree)*100
}
un_agree(joined, "Armenia", "Azerbaijan")
un_agree(votes, "Armenia", "Russian Federation")
un_agree(votes, "Armenia", "Turkey")
un_agree(votes, "Russian Federation", "Azerbaijan")
un_agree(votes, "Armenia", "United States")
##world and Armenia
wa<- function(db, country_pivot)
  {
  v <- NULL
  for(i in 1:length(unique(db$country))){
    country <- unique(db$country)[i]
    p <- un_agree(db = votes, 
                  country1 = country_pivot, 
                  country2 = unique(db$country)[i])
    d <- data.frame("country" = country, "p" = p)
    v <- rbind.data.frame(v, d)}
  return(v)
}
wa(votes, "Armenia") %>% 
  arrange(desc(p)) %>%
  filter(country != "Armenia") %>% # obviously p=100
  head()
wa(votes, "Armenia") %>% 
  arrange(desc(p)) %>%
  filter(country != "Armenia") %>%
  tail()

##abstentions
abstentions <- votes %>% 
  mutate("abstain" = ifelse(as.character(vote) == "abstain", 1, 0)) %>%
  group_by(country) %>% 
  summarise("prct_abstentions" = sum(abstain)/n_distinct(rcid)) %>%
  arrange(desc(prct_abstentions))
abstentions[1:10, ] %>% as.data.frame
abstentions[47,]
##joining all the data

##Armenia's behavior by presidents
joined %<>% mutate("President" = ifelse(date >=  '1992-01-01' & 
                                      date <= '1998-02-03', "Ter-Petrosyan", 
                                    ifelse(date >= '1998-02-04' & 
                                             date <= '2008-04-09', "Kocharyan",
                                           ifelse(date >= '2008-04-10' & 
                                                    date <='2018-11-30', "Sargsyan",
                                                         "No President"))
                                           ))
un_agree(joined %>% filter(President =="Sargsyan"), 
         "Armenia", "Azerbaijan")
wa(joined, "Armenia") %>% 
  arrange(desc(p)) %>%
  filter(President=="Kocharyan") %>%
  head()
#######################
joined %>%
  filter(country == "Armenia") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  group_by(year = year(date), issue) %>%
  summarize(votes = n(),
            percent_yes = mean(vote == "yes")) %>%
  filter(votes > 5) %>%
  ggplot(aes(year, percent_yes)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ issue)  ##function year not found

###desriptive statistics
summary(joined)
summary(joined$vote)
cor(data[,c()])
tbl<-table(joined$country, joined$vote)
chisq.test(tbl) ##p<2.2e-16 thus dependent
tbl
armenia<-joined%>%filter(country=="Armenia")
tbl1<-table(armenia$country, armenia$President)
chisq.test(tbl1) ##dependent

