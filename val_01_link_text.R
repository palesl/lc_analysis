# this script links the text measure with the output of the main model...

# there is no relationship at all. these are just random numbers...


set.seed(123)

library(tidyverse)

# reading in text measure...
flan_out <- read_csv("data/text/text_05_flan_recommend_full.csv")


#applying text responses to parliament for linkage...
parls<-ausPH::getParliaments()|>select(PID,DateElection, ParliamentEnd)|>
  mutate(DateElection=year(DateElection),
         ParliamentEnd=year(ParliamentEnd)
  )|>
  filter(!is.na(ParliamentEnd))


years<-mapply(seq, parls$DateElection, parls$ParliamentEnd)

years <- data.frame(
  parliament = unlist(lapply(seq_along(years), function(x) rep(x, length(years[[x]])))),
  year = unlist(years)
)


flan_out$year<-str_extract_all(flan_out$href,"\\/\\d{4}\\/")|>
  str_remove_all("\\/")

flan_out$year<-as.integer(flan_out$year)

flan_linked<- years|>left_join(flan_out)

# removing blank parliaments (1901-1970s)

flan_linked<-flan_linked|>
  filter(!is.na(text))

# summarizing text responses to the act parliament level using a count and average...

flan_sum<- flan_linked|>
  mutate(yes_dummy=case_when(flan_class_rec=="No"~0,flan_class_rec=="Yes"~1))|>
  group_by(austlii_id, parliament)|>
  summarise(n_refs = n(),
            flan_count = sum(yes_dummy),
            flan_mean = mean(yes_dummy),
            flan_sd = sd(yes_dummy)
  )


#linking austlii id back to central id...

austlii_linked <- readRDS("data/text/text_01_austlii_linked.rds")|>
  select(id, austlii_id)

out_flan_sum<-flan_sum|>left_join(austlii_linked)|>
  select(id, austlii_id, parliament:flan_sd)

ggplot(out_flan_sum, aes(parliament, flan_count))+
  geom_jitter(alpha=0.1,height=5)

names(out_flan_sum)[1]<-'principal_id'

# linking to model data...

model_data <- readRDS("data/amends/amends_02_model_outputs.rds")


text_verif_merged<-out_flan_sum|> left_join(model_data)|>select(-principal_name, -subject)

name_id<-bind_cols(principal_name=model_data$principal_name,
                   subject=model_data$subject,
                   principal_id=model_data$principal_id)|>distinct()

text_verif_merged<-
  text_verif_merged|>left_join(name_id)

# zero out NAs

text_verif_merged$multiple_amends[is.na(text_verif_merged$multiple_amends)]<-0
text_verif_merged$wt_number_enactments[is.na(text_verif_merged$wt_number_enactments)]<-0

# visualising relationship between

acts25<-sample(text_verif_merged$principal_name[!is.na(text_verif_merged$principal_name)],25)

text_verif_merged|>group_by(principal_name)|>
  add_count(principal_id)|>filter(principal_name%in%acts25)|>
  ggplot()+
  geom_line(aes(parliament,flan_mean))+
  geom_point(aes(parliament,flan_mean))+
  geom_segment(aes(x=parliament,y=0,
                   xend=parliament, yend=multiple_amends+0.01),
               col="red")+
  facet_wrap(facets= ~ principal_name, nrow = 5, ncol=5)

text_verif_merged|>group_by(parliament,subject)|>
  summarise(mean_flan = mean(flan_mean),
            mean_amend = mean(multiple_amends),
            mean_enactments= mean(wt_number_enactments))|>
  ggplot()+
  geom_line(aes(parliament,mean_flan))+
  geom_point(aes(parliament,mean_flan))+
  geom_point(aes(parliament,mean_amend), col='red')+
  geom_line(aes(parliament,mean_amend), col='red')+
  facet_wrap(facets= ~ subject, nrow = 5, ncol=5)




by_subject<-text_verif_merged|>group_by(parliament,subject)|>
  summarise(mean_flan = mean(flan_mean),
            mean_amend = mean(multiple_amends),
            mean_enactments= mean(wt_number_enactments),
            mean_surprise = mean(error_OLS))

by_subject|>
  ggplot()+
  geom_line(aes(parliament,mean_flan))+
  geom_point(aes(parliament,mean_flan))+
  geom_point(aes(parliament,mean_enactments), col='blue')+
  geom_line(aes(parliament,mean_enactments), col='blue')+
  facet_wrap(facets= ~ subject, nrow = 9, ncol=3)+
  theme_bw()+
  ylab("prop. calls for amendment in ALRC", col="blue")




lm(data=by_subject,
   mean_enactments ~ mean_flan)|>
  summary()

lm(data=by_subject,
   mean_amend ~ mean_flan)|>
  summary()

lm(data=by_subject,
   mean_surprise ~ mean_flan)|>
  summary()


by_subject|>
  ggplot()+
  geom_point(aes(mean_flan, mean_amend))+
  geom_smooth(aes(mean_flan, mean_amend),se=F)+
  facet_wrap(~subject)


## now with sentiment...

text_04_valence_sentiment <- readRDS("data/text/text_04_valence_sentiment.rds")



