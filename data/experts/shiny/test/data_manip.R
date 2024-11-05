# data

acts <- readRDS("s01_austlii_linked.rds")

# randomise the order of the acts.

set.seed(123)

acts<-acts[sample(1:nrow(acts)), ]

# restricting to acts whose subject we have information...

acts<-acts[!is.na(acts$subject),]



#anchor_acts
anchor_acts <- acts[acts$modernName=="Corporations Act 2001"|
                      acts$modernName=="Social Security Act 1991"|
                      acts$modernName=="Migration Act 1958"|
                      acts$modernName=="Family Law Act 1975"|
                      acts$modernName=="Freedom of Information Act 1982"
                    ,]

#removing anchors from the main list

acts <- acts[acts$modernName!="Corporations Act 2001"|
               acts$modernName!="Social Security Act 1991"|
               acts$modernName!="Migration Act 1958"|
               acts$modernName!="Family Law Act 1975"|
               acts$modernName!="Freedom of Information Act 1982"
             ,]


# restricting only to acts within the last 40 years


acts<- acts[acts$assent>=as.Date('1983-01-01'),]
