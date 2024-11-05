# Use quanteda’s sentiment dictionary, output data, histograms over time/type of law


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


# reading in data

flan_output <- read.csv("data/text/text_03_flan_full.csv")

text_data<-flan_output|>filter(flan_class=="Yes")

# setting up quanteda

library(quanteda)
library(quanteda.sentiment)

afinn <- read.delim(system.file("extdata/afinn/AFINN-111.txt",
                                package = "quanteda.sentiment"),
                    header = FALSE, col.names = c("word", "valence"))
head(afinn)

data_dictionary_afinn <- dictionary(list(afinn = afinn$word))
valence(data_dictionary_afinn) <- list(afinn = afinn$valence)
data_dictionary_afinn


sentiment_valence <-textstat_valence(tokens(text_data$text), data_dictionary_afinn) |>
  select(sentiment)|>
  rename("sentiment_valence"= sentiment)


out_text<-cbind(text_data,sentiment_valence)

saveRDS(out_text, 'data/text/text_04_valence_sentiment.rds')

# outputs for the appendix

text_01_austlii_linked <- readRDS("data/text/text_01_austlii_linked.rds")

valence_app<-out_text|>left_join(text_01_austlii_linked)

by_subject<-valence_app|>group_by(subject)|>
  summarise(mean_valence=mean(sentiment_valence),
            sd_valence =sd(sentiment_valence))

saveRDS(by_subject,"tables_figures/appendix/text_04_valence_by_subject.rds")

valence_app$decade<-paste0(substr(as.character(valence_app$assent),1,3),"0s")

by_decade<-valence_app|>group_by(decade)|>
  summarise(mean_valence=mean(sentiment_valence),
            sd_valence =sd(sentiment_valence))

saveRDS(by_decade,"tables_figures/appendix/text_04_valence_by_decade.rds")
