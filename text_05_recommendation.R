

# testing out the python script...

tic()
system("python3 text_05_recommendation_test.py")
toc()


## now running human verification...


# now running the whole thing...
#
# tic()
# system("python3 text_05_recommendation_full.py")
# toc()


# now testing to see what validation looks like...

library(readr)
full <- read_csv("data/text/text_05_flan_recommend_full.csv")

# now to export a table which looks at the basic classification.

tab_flan<- table(full$flan_class_rec)

# send to appendix...

saveRDS(tab_flan,"tables_figures/appendix/text_05_tab_flan.rds")


# since it is a rare event it makes sense to randomly select a
# 50/50 sample and verify this way, as well as taking the usual random sample...

yes_sample_50<-sample(which(full$flan_class_rec=='Yes'), 50, replace = F)

no_sample_50<-sample(which(full$flan_class_rec=='No'), 50, replace = F)

sample_5050<-full[c(yes_sample_50, no_sample_50),]

random_sample<-full[sample(1:nrow(full), 100, replace=F),]


table(random_sample$flan_class_rec)
table(sample_5050$flan_class_rec)

# putting together and exporting for manual classification...

sample_5050$fifty50<-"Yes"
random_sample$fifty50<-"No"

text_05_manual_class<-bind_rows(sample_5050,random_sample)

write_csv(text_05_manual_class, "data/text/text_05_manual_class.csv")


# reading in the resulting manual coding...

coded<-read_csv("data/text/text_05_manual_class_coded.csv")


# reliability of random sample...

coded_random<-coded[coded$fifty50=="No",]


pct_agreement<-100*sum(coded_random$flan_class_rec==coded_random$human_rec)/nrow(coded_random)
saveRDS(pct_agreement,"tables_figures/appendix/text_05_random_pct_agreement.rds")
pct_agreement

Cohens_kappa<-irr::kappa2(ratings = coded_random[,c(4,6)])
saveRDS(Cohens_kappa,"tables_figures/appendix/text_05_random_cohens_kappa.rds")
Cohens_kappa

iir_contingency <-table(coded_random[,c(4,6)])
saveRDS(iir_contingency,"tables_figures/appendix/text_05_random_irr_contingency.rds")
iir_contingency

# reliability of 50/50 sample...

coded_50 <-coded[coded$fifty50=="Yes",]

pct_agreement<-100*sum(coded_50$flan_class_rec==coded_50$human_rec)/nrow(coded_50)
saveRDS(pct_agreement,"tables_figures/appendix/text_05_fifty_pct_agreement.rds")
pct_agreement

Cohens_kappa<-irr::kappa2(ratings = coded_50[,c(4,6)])
saveRDS(Cohens_kappa,"tables_figures/appendix/text_05_fifty_cohens_kappa.rds")
Cohens_kappa

iir_contingency <-table(coded_50[,c(4,6)])
saveRDS(iir_contingency,"tables_figures/appendix/text_05_fifty_irr_contingency.rds")
iir_contingency
