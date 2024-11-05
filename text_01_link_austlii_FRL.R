## get acts and link them to their relevant ID in austlii

# devtools::install_github('palesl/ausleg')
# devtools::install_github('palesl/ausPH')


library(pacman)
p_load(tidyverse, rvest, httr, tictoc,foreach, doParallel, stringr, ausPH, ausleg)


# new script


list_numbered<-list()
for(year in 1901:2023){
  hrefs<-names<-NULL
  print(year)
  numbered_url<-paste0('http://www8.austlii.edu.au/cgi-bin/viewtoc/au/legis/cth/num_act/', year)
  hrefs<-numbered_url |>read_html()|> html_elements(paste0("#", year," a")) |>html_attr("href")
  names<-numbered_url |>read_html()|> html_elements(paste0("#", year," a")) |>html_text()
  list_numbered[[year-1900]]<-bind_cols(names=names,hrefs=hrefs)
}

numbered_total<-bind_rows(list_numbered)

#bridging over
bridge<-ausleg::alrc_as_made(prin_amend = "Principal", leg_type = "Acts")|>select(modernName,
                                                                                  id, assent,
                                                                                  subject, administrator,
                                                                                  numberYear)
bridge$numberYear<- gsub(" as made", "", bridge$numberYear)
numbered_total$numberYear <-gsub("NO\\. ", "No. ", numbered_total$names)
numbered_total$numberYear <-gsub(".*No\\. ", "Act No. ", numbered_total$numberYear)
numbered_total$numberYear <-gsub("\\,", " of", numbered_total$numberYear)
numbered_total$numberYear <-gsub("\\)", "", numbered_total$numberYear)
numbered_total$numberYear <-gsub("[0-9]{4} [0-9]{4}", "\\1", numbered_total$numberYear)
numbered_total$numberYear <-gsub("Á", "", numbered_total$numberYear)


numbered_total$austlii_id<-gsub(".*num_act\\/", "", numbered_total$hrefs)
numbered_total$austlii_id<-gsub("\\/", "", numbered_total$austlii_id)


austlii_linked_acts<-bridge|>left_join(numbered_total)|>select(modernName,
                                                        id, assent,
                                                        subject, administrator,
                                                        austlii_id)


saveRDS(austlii_linked_acts,"data/text/text_01_austlii_linked.rds")

