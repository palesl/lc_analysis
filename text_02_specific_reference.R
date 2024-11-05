# Automate Austlii reference search in law review and ALRC databases –
# download passages surrounding reference to law, output data.
# Appendix: show coverage through time and area of law.


# libraries

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
library(lubridate)




# data goes in
text_01_austlii_linked <- readRDS("data/text/text_01_austlii_linked.rds")


# removing dead entries (for now)
search_list<-text_01_austlii_linked$austlii_id

search_list<-search_list[!is.na(search_list)]
search_list<-search_list[search_list!=""]

#Building search url


url_pre_journals<- "http://www.austlii.edu.au/cgi-bin/sinosrch.cgi?meta=;mask_path=;method=auto;query="
url_post_journals<- ";view=relevance-collapse;collapse-level=2&mask_path=au/journals"
url_search_list_journals <- paste0(url_pre_journals,search_list,url_post_journals)|>as.data.frame()
names(url_search_list_journals)[1]<-"url_search_list"
url_search_list_journals$source<-"journals"
url_search_list_journals$austlii_id<-search_list

url_pre_alrc<-'http://www.austlii.edu.au/cgi-bin/sinosrch.cgi?search-filter=on&search-text=on&query='
url_post_alrc<-"&mask_path=au%2Fother%2Flawreform%2FAdminRC&mask_path=au%2Fother%2Flawreform%2FACTCLRC&mask_path=au%2Fother%2Flawreform%2FACTLRAC&mask_path=au%2Fother%2Flawreform%2FACTLRC&mask_path=au%2Fother%2Flawreform%2FALRC&mask_path=au%2Fother%2Flawreform%2FALRCBP&mask_path=au%2Fother%2Flawreform%2FALRCCP&mask_path=au%2Fother%2Flawreform%2FALRCCPS&mask_path=au%2Fother%2Flawreform%2FALRCDP&mask_path=au%2Fother%2Flawreform%2FALRCDRP&mask_path=au%2Fother%2Flawreform%2FALRCIntroP&mask_path=au%2Fother%2Flawreform%2FALRCIP&mask_path=au%2Fother%2Flawreform%2FCLRC&mask_path=au%2Fother%2Flawreform%2FNSWLRC&mask_path=au%2Fother%2Flawreform%2FNSWLRCCP&mask_path=au%2Fother%2Flawreform%2FNSWLRCDP&mask_path=au%2Fother%2Flawreform%2FNSWLRCIP&mask_path=au%2Fother%2Flawreform%2FNSWLRCQP&mask_path=au%2Fother%2Flawreform%2FNTLRC&mask_path=au%2Fother%2Flawreform%2FQLRC&mask_path=au%2Fother%2Flawreform%2FQLRCMP&mask_path=au%2Fother%2Flawreform%2FQLRCWP&mask_path=au%2Fother%2Flawreform%2FTASLRI&mask_path=au%2Fother%2Flawreform%2FTASLRIIP&mask_path=au%2Fother%2Flawreform%2FTASLRIRP&mask_path=au%2Fother%2Flawreform%2FVLRC&mask_path=au%2Fother%2Flawreform%2FVLRCCP&mask_path=au%2Fother%2Flawreform%2FVLRCDP&mask_path=au%2Fother%2Flawreform%2FVLRCIP&mask_path=au%2Fother%2Flawreform%2FVLRCInfoP&mask_path=au%2Fother%2Flawreform%2FVLRCInterimRp&mask_path=au%2Fother%2Flawreform%2FVLRCOP&mask_path=au%2Fother%2Flawreform%2FVLRCPP&mask_path=au%2Fother%2Flawreform%2FVicLRCmr&mask_path=au%2Fother%2Flawreform%2FVicLRCmrWP&mask_path=au%2Fother%2Flawreform%2FVicLRComm&mask_path=nz%2Fother%2Flawreform%2FNZIPACRp&mask_path=nz%2Fother%2Flawreform%2FNZLCR&mask_path=nz%2Fother%2Flawreform%2FNZLCIP&mask_path=nz%2Fother%2Flawreform%2FNZLCMBP&mask_path=nz%2Fother%2Flawreform%2FNZLCMP&mask_path=nz%2Fother%2Flawreform%2FNZLCOP&mask_path=nz%2Fother%2Flawreform%2FNZLCPP&mask_path=nz%2Fother%2Flawreform%2FNZLCSP&mask_path=nz%2Fother%2Flawreform%2FNZAHLRCom&mask_path=nz%2Fother%2Flawreform%2FNZALLRCom&mask_path=nz%2Fother%2Flawreform%2FNZCCLRCom&mask_path=nz%2Fother%2Flawreform%2FNZCLRCom&mask_path=nz%2Fother%2Flawreform%2FNZELRCom&mask_path=nz%2Fother%2Flawreform%2FNZFamJustRef&mask_path=nz%2Fother%2Flawreform%2FNZPenalPP&mask_path=nz%2Fother%2Flawreform%2FNZPPLRCom&mask_path=nz%2Fother%2Flawreform%2FNZPELRCom&mask_path=nz%2Fother%2Flawreform%2FNZPALRCom&mask_path=nz%2Fother%2Flawreform%2FNZRMLawRef&mask_path=nz%2Fother%2Flawreform%2FNZSSWLRCom&mask_path=nz%2Fother%2Flawreform%2FNZTGLRCom"
url_search_list_alrc <- paste0(url_pre_alrc,search_list,url_post_alrc)|>as.data.frame()
names(url_search_list_alrc)[1]<-"url_search_list"
url_search_list_alrc$source<-"alrc"
url_search_list_alrc$austlii_id<-search_list



url_search_list<-bind_rows(url_search_list_journals,url_search_list_alrc)

#function to give list addresses

retrieve_articles<-function(i){
  print(paste0('Row ', i))
  try(
    articles <- url_search_list$url_search_list[i] |>
      read_html()|>
      html_elements(".right , #page-main :nth-child(6) a")|>
      html_attr("href")
  )
  try(articles<-articles[str_detect(articles,pattern="bin/viewdoc")])
  try(articles<-articles[!is.na(articles)])
  try(bind_cols(href=articles, austlii_id=url_search_list$austlii_id[i]))
}

# paralell webscrape

registerDoParallel(cores=10)
tic()
austlii_articles<-foreach(i=1:nrow(url_search_list), .combine=rbind)%dopar% {retrieve_articles(i)}
toc()

saveRDS(austlii_articles, "data/text/text_02_austlii_articles.rds")


#### OUTPUTTING SOME STUFF FOR THE APPENDIX ####



df_included<-data.frame(austlii_id=unique(austlii_articles$austlii_id),
                        included=1)

text_01_austlii_linked<-text_01_austlii_linked|>left_join(df_included)

text_01_austlii_linked$included[is.na(text_01_austlii_linked$included)]<-0

text_01_austlii_linked$decade<-as.character(year(text_01_austlii_linked$assent))

substring(text_01_austlii_linked$decade, 4,5)<-"0"
text_01_austlii_linked$decade<-paste0(text_01_austlii_linked$decade,'s')

# table 1 coverage by decade

text_02_coverage_by_decade<-text_01_austlii_linked|>group_by(decade)|>
  summarise(pct_included=100*mean(included))

saveRDS(text_02_coverage_by_decade, "tables_figures/appendix/text_02_coverage_by_decade.rds")

# table 2 coverage by topic

text_02_coverage_by_subject<-text_01_austlii_linked|>group_by(subject)|>
  summarise(pct_included=100*mean(included))

saveRDS(text_02_coverage_by_subject, "tables_figures/appendix/text_02_coverage_by_subject.rds")



# Now to retrieve the text of interest...

austlii_articles$url<-paste0("http://www.austlii.edu.au",austlii_articles$href)

scrape_references<-function(i){
  #print(paste0('Row ', i))
  try(
    references <- austlii_articles$url[i] |>
      read_xml(as_html = T)
  )
  if(!is.na(references)){
    old<-xml_find_all(references,paste0("//b//*[contains(@href,'",austlii_articles$austlii_id[i],"')]"))

    new<-old[grepl("Act",old|>html_text(),ignore.case = T)]
    new<-new[nchar(new|>html_text())==max(nchar(new|>html_text()))][1]

    xml_replace(old, new)

    if(length(new)>0){

      tag<-html_text(new)|>
        stringr::str_replace_all("\n|\t", " ")

      tag_square<-paste0('[',tag,']')
      tag_square<-tag_square|>str_remove_all("\\.")

      references_text<-references|>html_text()|>
        str_replace_all("\\[", "\\(")|>str_replace_all("\\]",'\\)')|>
        stringr::str_replace_all("\n|\t", " ")


      references_text<-gsub(tag,tag_square, references_text, fixed = T)

      #sentences

      references_sentences<-references_text|>str_split_1(pattern="\\.")

      index<-grep(tag_square,references_sentences, fixed = T)

      out<-data.frame(austlii_id = rep(austlii_articles$austlii_id[i], length(index)),
                      plus_minus_0=NA,
                      plus_minus_1=NA,
                      plus_minus_2=NA)

      for(k in 1:length(index)){
        text<-paste(references_sentences[index[k]], collapse = ". ")
        out$plus_minus_0[k]<-text
      }

      for(k in 1:length(index)){
        plus_minus_1<-(index[k]-1):(index[k]+1)
        if(min(plus_minus_1)<1){plus_minus_1[1]<-1}
        text<-paste(references_sentences[plus_minus_1], collapse = ". ")
        out$plus_minus_1[k]<-text
      }

      for(k in 1:length(index)){
        plus_minus_2<-(index[k]-2):(index[k]+2)
        if(min(plus_minus_2)<1){plus_minus_2[1]<-1}
        text<-paste(references_sentences[plus_minus_2], collapse = ". ")
        out$plus_minus_2[k]<-text
      }

      out$href<-austlii_articles$href[i]

      return(out)
    }

  }
}

# # testing
# j<-1
# listtest<-list()
# tic()
# for(i in 7980:8507){
#   print(i)
#
#   listtest[[j]]<-scrape_references(i)
#   j<-j+1
# }
# outtest<-bind_rows(listtest)
# toc()
#

# parallell webscrape

tic()
scraped_references<-foreach(i=1:8507, .combine = rbind)%dopar% {scrape_references(i)}
toc()

saveRDS(scraped_references, "data/text/text_02_scraped_references.rds")



