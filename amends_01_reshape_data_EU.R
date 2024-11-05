library(tidyverse);library(evoeu);library(eurlex)


# get acts and their amendments...
eur_leg <- evoeu::nodes|>filter(node_type=="Directives"|
                                  node_type=="Regulations")|>
  select(-key_id)



eur_amends<-evoeu::edges|>filter(edge_type=="Changes text in clause of"|
                                 edge_type=="Replaces clause of"|
                                 edge_type=="Adds text to clause of"|
                                 edge_type=="Inserts new clause in"|
                                 edge_type=="Repeals all or part of")|>
  select(-key_id)


eur_leg<- eur_leg|>left_join(eur_amends, by=join_by('celex'=='incoming_celex'))

# adding date_amendment

eur_leg_amends<-evoeu::nodes|>filter(node_type=="Directives"|
                                       node_type=="Regulations")|>
  select(celex,date)

names(eur_leg_amends)[2]<-'amend_date'

eur_leg<-eur_leg|>left_join(eur_leg_amends, by=join_by('outgoing_celex'=='celex'))

# adding expiry dates...

query_directive<-elx_make_query(resource_type = c("directive"),
                            include_date_endvalid=T)

end_dates_directive<-elx_run_query(query_directive)

query_regulation<-elx_make_query(resource_type = c("regulation"),
                                include_date_endvalid=T)

end_dates_regulation<-elx_run_query(query_regulation)


end_dates<-bind_rows(end_dates_directive,end_dates_regulation)

# removing invalid dates
end_dates$dateendvalid[end_dates$dateendvalid=="1002-02-02"]<-NA

end_dates$dateendvalid<-as.Date(end_dates$dateendvalid)

end_dates_latest<-end_dates|>filter(!is.na(dateendvalid))|>
  group_by(celex)|>
  mutate(max_end_date=max(dateendvalid, na.rm = T))|>
  summarise(max_end_date=max_end_date[1])


# adjoining
eur_leg<-eur_leg|>left_join(end_dates_latest)

# grouping by year

eur_leg$repeal_year<-as.character(eur_leg$max_end_date)|>
  substr(1,4)|>as.numeric()

eur_leg$repeal_year[is.na(eur_leg$repeal_year)]<-9999

eur_leg$amend_year<-as.character(eur_leg$amend_date)|>
  substr(1,4)|>as.numeric()

eur_leg$enacted_year<-as.character(eur_leg$date)|>
  substr(1,4)|>as.numeric()


# simplifying data

amends <-eur_leg|>select(celex,enacted_year,amend_year,repeal_year)|>
  filter(!is.na(amend_year))


enacts<-eur_leg|>select(celex,enacted_year, repeal_year)|>distinct()

enacts$amend_year<-enacts$enacted_year

#enacts and amends

pre_dat<-bind_rows(enacts,amends)
pre_dat$count<-1

# pivot_wide

wide_dat<-pre_dat|>arrange(amend_year)|>
  pivot_wider(names_from = amend_year,
                     values_from = count,
                     names_prefix = 'y',
                     values_fn=sum,
                     values_fill = 0)|>
  arrange(celex)

# NA out years where the act did not exist

for (i in 1958:2015){
  varname<-paste0('y',i)
  col<-wide_dat[names(wide_dat)==varname]
  names(col)<-'col'

  col<-col$col

  # na out cols before enactment
  col[i < wide_dat$enacted_year] <-NA

  #na out cols after repeal
  col[i > wide_dat$repeal_year] <-NA


  wide_dat[names(wide_dat)==varname]<-col
}


# summarising new Acts and Amendments (n), and Repeals (p)

on_books<-pre_dat|>arrange(amend_year)|>
  pivot_wider(names_from = amend_year,
              values_from = count,
              names_prefix = 'y',
              values_fn=sum,
              values_fill = 0)|>
  arrange(celex)

for (i in 1958:2015){
  varname<-paste0('y',i)
  col<-on_books[names(on_books)==varname]
  names(col)<-'col'

  col<-col$col

  # na out cols before enactment
  col[i < on_books$enacted_year] <-NA

  #na out cols after repeal
  col[i >= on_books$repeal_year] <-NA


  on_books[names(on_books)==varname]<-col
}

on_books<-on_books|>select(y1958:y2015)

on_books[on_books>=0]<-1
on_books[is.na(on_books)]<-0



n <-long_dat|>group_by(year)|>summarise(on_books=n())

n<-bind_rows(year=1958:2015,on_books=colSums(on_books))




long_dat<-pivot_longer(wide_dat,
             cols = starts_with('y'),
             names_to = 'year',
             names_prefix = "y",
             values_to = 'enactments',
             values_drop_na = TRUE)





p <-long_dat|>group_by(year, enactments)|>
  summarise(n_enactments=n())

p$year<-as.numeric(p$year)
out<-p|>left_join(n)



new_acts<-long_dat|>filter(enacted_year==year)|>group_by(year)|>distinct()|>
  summarise(new_acts=n())

repeals<-long_dat|>filter(repeal_year==year)|>group_by(year)|>distinct()|>
  summarise(repealed_acts=n())

enactments<-long_dat|>group_by(year)|>
  summarise(n_enactments=sum(enactments))


sum<-new_acts|>left_join(repeals)|>left_join(enactments)
sum[is.na(sum)]<-0

sum$amendments<-sum$n_enactments-sum$new_acts

sum$on_books<-cumsum(sum$new_acts-sum$repealed_acts)

sum

# outputting the data

saveRDS(out,'data/amends/amends_01_model_data_EU.rds')
saveRDS(sum,'data/amends/amends_01_sum_data_EU.rds')
