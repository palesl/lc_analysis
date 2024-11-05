# Show agreement rates for the thresholds, justify choice.

library(tidyverse)
library(rvest)
library(httr)
library(tictoc)
library(foreach)
library(doParallel)
library(stringr)
library(ausPH)
library(ausleg)
library(xml2)
library(DataEditR)


# testing for baseline relevance to the act itself

scraped_references<-readRDS("data/text/text_02_scraped_references.rds")


#inputting

set.seed(1234)

rows<-sample(1:nrow(scraped_references), 100)

sample_for_coding<-scraped_references[rows,]

sample_for_coding$intent_to_evaluate_0 <-F
sample_for_coding$intent_to_evaluate_1 <-F
sample_for_coding$intent_to_evaluate_2 <-F


#write.csv(sample_for_coding,"data/text/text_03_intent_human.csv")

# inputting human relevance codings...

text_03_intent_human <- read.csv("data/text/text_03_intent_human.csv")


text_03_intent_human$intent_to_evaluate_0[text_03_intent_human$intent_to_evaluate_0==T]<-1
text_03_intent_human$intent_to_evaluate_0[text_03_intent_human$intent_to_evaluate_0==FALSE]<-0

text_03_intent_human$intent_to_evaluate_1[text_03_intent_human$intent_to_evaluate_1==T]<-1
text_03_intent_human$intent_to_evaluate_1[text_03_intent_human$intent_to_evaluate_1==FALSE]<-0

text_03_intent_human$intent_to_evaluate_2[text_03_intent_human$intent_to_evaluate_2==T]<-1
text_03_intent_human$intent_to_evaluate_2[text_03_intent_human$intent_to_evaluate_2==FALSE]<-0

#appendix outputs...


#what percentage of passages are evaluating the law in some way?
text_03_intent_evaluate_human_rate_by_threshold<-
  text_03_intent_human|>
  summarise(rate_evaluative_0=100*mean(intent_to_evaluate_0),
            rate_evaluative_1=100*mean(intent_to_evaluate_1),
            rate_evaluative_2=100*mean(intent_to_evaluate_2))

saveRDS(text_03_intent_evaluate_human_rate_by_threshold,
        "tables_figures/appendix/text_03_intent_evaluate_human_rate_by_threshold.rds")



#how reliable was my own coding across the 0,1,2 sentence windows?



cor_vars<-text_03_intent_human|>select(-(austlii_id:href)) |>
  cor()

saveRDS(cor_vars, "tables_figures/appendix/text_03_intent_evaluate_human_rate_within_class_corr.rds")

# using flan to code for us...

# reducing size of sample text to zero window, removing unnecessary characters


text_03_intent_human$text<-
  text_03_intent_human$plus_minus_0 |>
  str_replace_all('\n',' ')|>
  str_squish()

text_03_intent_human$text<-
  substr(text_03_intent_human$text,start=1, stop=512)



hist<-hist(text_03_intent_human$text|>nchar(),
     main = paste("Histogram of character length: specific reference"),
     xlab="character length: specific reference")

saveRDS(hist,"tables_figures/appendix/text_03_nchar_flan_hist.rds")


#adding a reference to the law for flan before the passage of text to classify...

text_03_intent_human$law<-
  text_03_intent_human$text|>
  str_remove_all("\\].*")|>
  str_extract_all("\\[.*", simplify = T)

text_03_intent_human$law<-
  paste0(text_03_intent_human$law, "]")



saveRDS(text_03_intent_human,"data/text/text_03_intent_human_flan_test.rds")

#running the python code...


tic()
system("python3 text_03_intent_to_evaluate_test.py")
toc()


# evaluating inter rater reliability for the intent to evaluate task...

flan_output <- read.csv("data/text/text_03_flan_test.csv")

flan_output$intent_to_evaluate_0[flan_output$intent_to_evaluate_0==1]<-'Yes'
flan_output$intent_to_evaluate_0[flan_output$intent_to_evaluate_0==0]<-'No'



pct_agreement<-100*sum(flan_output$intent_to_evaluate==flan_output$flan_class)/nrow(flan_output)
saveRDS(pct_agreement,"tables_figures/appendix/text_03_pct_agreement.rds")
pct_agreement

Cohens_kappa<-irr::kappa2(ratings = flan_output[,3:4])
saveRDS(Cohens_kappa,"tables_figures/appendix/text_03_cohens_kappa.rds")
Cohens_kappa

iir_contingency <-table(flan_output[,3:4])
saveRDS(iir_contingency,"tables_figures/appendix/text_03_irr_contingency.rds")
iir_contingency # note there is a high sensitivity so we don't do too much harm...





# now running the whole thing...

text_03_intent_full<-scraped_references|>select(austlii_id, href,
                                                     plus_minus_0)



text_03_intent_full$text<-
  text_03_intent_full$plus_minus_0 |>
  str_replace_all('\n',' ')|>
  str_squish()

text_03_intent_full$text<-
  substr(text_03_intent_full$text,start=1, stop=512)



hist<-hist(text_03_intent_full$text|>nchar(),
           main = paste("Histogram of character length: specific reference (full sample)"),
           xlab="character length: specific reference")

saveRDS(hist,"tables_figures/appendix/text_03_nchar_flan_hist_full.rds")


#adding a reference to the law for flan before the passage of text to classify...

text_03_intent_full$law<-
  text_03_intent_full$text|>
  str_remove_all("\\].*")|>
  str_extract_all("\\[.*", simplify = T)

text_03_intent_full$law<-
  paste0(text_03_intent_full$law, "]")



saveRDS(text_03_intent_full,"data/text/text_03_intent_flan_full.rds")


#running the full thing on the system
# WARNING DO NOT UNCOMMENT UNLESS YOU WANT TO RUN THE WHOLE THING

tic()
system("python3 text_03_intent_to_evaluate_full.py")
toc()
