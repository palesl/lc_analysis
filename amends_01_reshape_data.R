library(tidyverse);library(ausleg)


# get acts...
acts <- ausleg::alrc_as_made(leg_type="Acts")
acts$parliament<-as.numeric(acts$parliament)

# get amendments...
amendments<- ausleg::alrc_relationship_data()

#renaming for clarity, and removing old vars
amendments$principal_name<-amendments$toName
amendments$principal_id<-amendments$toId

amendments$amending_name<-amendments$fromName
amendments$amending_id<-amendments$fromId

amendments<-amendments|>select(principal_name,
                               principal_id,
                               amending_name,
                               amending_id)



# adjoining
amendments<-amendments|>left_join(acts|>select(parliament, id, assent),
                                  by=join_by(amending_id==id))

# removing legislative instruments

amendments<-amendments[!is.na(amendments$parliament),]

amendments<-amendments|>group_by(amending_id)|>
  mutate(wt_amendment=1/n())|>ungroup()

# creating a list of acts which have had at least one amendment
# (and capturing the number of amendments in one parliament...)

amend_counts<-amendments|>group_by(principal_id,parliament)|>
  summarise(principal_name=principal_name[1],
            number_enactments=n(),
            wt_number_enactments=sum(wt_amendment),
            amending_act_names=paste(amending_name, collapse  =";  "),
            date_first_assent = min(assent)
  )



#adding amend counts to principal acts

leg_compilations <- ausleg::alrc_as_made(leg_type="Acts", prin_amend = "Principal")|>
  select(name,id, parliament,assent)|>
  rename("principal_name"=name,
         "principal_id"=id,
         ) |>
  mutate_at('parliament', as.numeric)|>
  full_join(amend_counts)|>
  mutate(date_first_assent = case_when(!is.na(date_first_assent)~date_first_assent,
                                       is.na(date_first_assent)~assent))|>
  select(-assent)


leg_compilations<-leg_compilations|>
  group_by(principal_id)|>
  mutate(number_enactments = case_when(parliament==min(parliament) & number_enactments>=1 ~ number_enactments+1,
                                       is.na(number_enactments)~1, number_enactments>=1 ~ number_enactments))|>
  mutate(multiple_amends = case_when(number_enactments==1~0, number_enactments>=2~1),
         wt_multiple_amends = case_when(multiple_amends==0~1, multiple_amends==1~wt_number_enactments))




# adding context for the model...

# principal act context...

model_set<-leg_compilations|>left_join(acts|>
                                         filter(amendingPrincipal=="Principal")|>
                                         select(id,subject,administrator, legPages, assent),
                                       by=join_by(principal_id==id)
                                       )|>
  mutate(administrator = case_when(is.na(administrator)~"Unknown",
                                   !is.na(administrator)~administrator),
         subject = case_when(is.na(subject)~"Unknown",
                                   !is.na(subject)~subject))|>
  arrange(principal_id,parliament)|>
  group_by(principal_id)|>
  mutate(prin_parliament= min(parliament),
         wt_number_enactments = case_when(is.na(wt_number_enactments)~1,
                                          !is.na(wt_number_enactments)~wt_number_enactments),
         cum_amendments = cumsum(wt_number_enactments)-wt_number_enactments)


# removing acts that are not principal acts..

model_set<- model_set|>
  filter(!is.na(assent))

# adding time left in the parliament...

parldates<-ausPH::getParliaments()|>mutate(parliament=PID)|>
  select(parliament, DateOpening)|>
  mutate(dateExpiry = DateOpening+lubridate::years(3))

model_set<-model_set|>left_join(parldates)|>
  mutate(parl_time_left = dateExpiry - date_first_assent)|>
  select(-dateExpiry, -DateOpening)

# outputting the data

saveRDS(model_set,'data/amends/amends_01_model_data.rds')
