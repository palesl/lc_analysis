
library(tidyverse)



# calculating the expected number of repeat enactments (simple binomial approach)
calculate_prob_distribution <- function(n_acts, amendments) {

  n<-amendments
  p<-1/n_acts
  probabilities <- numeric(n + 1)

  for (k in 0:n) {
    probabilities[k+1]<- choose(n + k -1, k)*p^k*(1-p)^(n-k)
  }

  return(probabilities)
}

# Example usage
n_acts <- 4000  # total number of acts on the books
amendments<- 300 # Number of amendments made

# Calculate the probability distribution
prob_distribution <- calculate_prob_distribution(n_acts, amendments)

probs<-bind_cols(k=0:amendments, pr = prob_distribution)
probs$F<-cumsum(probs$pr)


pbinom(q=0:amendments,size=amendments,prob=1/n_acts)

# Print the results


ggplot(probs)+geom_col( aes(x=k,y=pr ))

# a really simple test using new acts, amendments, and repeals per parliament...

acts_on_the_books <- ausleg::alrc_as_made(prin_amend="Principal", leg_type = "Acts")

# new acts
new_acts<-acts_on_the_books|>group_by(parliament)|>
  summarise(new_acts=n())|>
  mutate(parliament=as.numeric(parliament))


# repeals
acts_on_the_books$repeal_parl<-NA
parls<-ausPH::getParliaments()
for(i in 1:nrow(parls)){
  acts_on_the_books$repeal_parl[acts_on_the_books$repealDate>parls$DateElection[i]&
                                  acts_on_the_books$repealDate<parls$ParliamentEnd[i]]<-i
}

acts_on_the_books$repeal_parl[is.na(acts_on_the_books$repeal_parl)]<-50
repeals<-acts_on_the_books|>group_by(repeal_parl)|>
  summarise(repeals=n())|>
  rename(parliament="repeal_parl")

# amendments

amendments<-amend_counts|>
  group_by(parliament)|>
  summarise(amendments=sum(number_enactments))|>
  mutate(parliament=as.numeric(parliament))

# joining

acts_for_test<- new_acts|>left_join(repeals)|>left_join(amendments)|>
  arrange(parliament)


acts_for_test$repeals[is.na(acts_for_test$repeals)]<-0

# creating running total

acts_for_test$on_books<-cumsum(acts_for_test$new_acts - acts_for_test$repeals)

# adding new_acts to amendments

enactments<-model_set|>group_by(parliament)|>
  summarise(enactments=sum(number_enactments))

acts_for_test$enactments<-enactments$enactments

# creating theoretical binomial distributions...
dists<-bind_cols(parliament=NA, k = NA, Pr = NA,)

for(i in 1:47){
  K<-0:acts_for_test$enactments[i]
  probs<-dbinom(x=0:acts_for_test$enactments[i],
                size=acts_for_test$enactments[i],
                prob=1/acts_for_test$on_books[i])
  parl<-rep(i, length(K))
  appender<-bind_cols(parliament=parl, k=K, Pr=probs)

  dists<-bind_rows(dists, appender)

}
dists<-dists[!is.na(dists$parliament),]
dists<-dists[dists$parliament!=47,]

dists|>ggplot(aes(x=k,y=Pr ))+geom_line( ) + facet_wrap(~parliament)

# now to calculate observed binomials

dists_observed<-bind_cols(parliament=NA, k = NA, Pr = NA,)

for(i in 1:47){
  K <-data.frame(K=0:acts_for_test$enactments[i] )


  probs_table<-  model_set$number_enactments[model_set$parliament==i] |>table()|>
    as.data.frame()

  probs_table$Var1<-probs_table$Var1|>as.character()|>as.numeric()


  K<-K|>left_join(probs_table, by=join_by("K"=="Var1"))
  K$Freq[is.na(K$Freq)]<-0
  K$Freq[K$K==0]<-acts_for_test$on_books[i] - sum(K$Freq)
  K$Freq[K$Freq<0]<-0

  K$probs<-K$Freq/sum(K$Freq)

  parl<-rep(i, nrow(K))

  appender<-bind_cols(parliament=parl, k=K$K, Pr=K$probs)

  dists_observed<-bind_rows(dists_observed, appender)

}
dists_observed<-dists_observed[!is.na(dists_observed$parliament),]
dists_observed<-dists_observed[dists_observed$parliament!=47,]

dists_observed|>ggplot(aes(x=k,y=Pr ))+geom_line( ) + facet_wrap(~parliament)

## comparing variances of expected and observed distributions

dists_observed_sum<-dists_observed|>group_by(parliament)|>
  summarise(obs_var=sum(Pr*k*(1-Pr)),
            obs_mean=sum(Pr*k),
            n=n())

dists_sum<-dists|>group_by(parliament)|>
  summarise(exp_var=sum(Pr*k*(1-Pr)),
            exp_mean=sum(Pr*k))

dists_sum<-dists_sum|>left_join(dists_observed_sum)

dists_sum$delta_mean<-dists_sum$obs_mean-dists_sum$exp_mean


plot0<-ggplot(dists_sum)+geom_line(aes(parliament, exp_var))+
  geom_line(aes(parliament, obs_var), col="red")+
  annotate(geom='text',label= expression(paste(italic(Var),"[",italic(X),"]"," ",(Expected))), x=21.2,y=.1, col= "black")+
  annotate(geom='text',label=expression(paste(italic(Var),"[",italic(X),"]"," ",(Observed))), x=21,y=.9, col='red')+
  geom_vline(xintercept = 1, lty=2,col='grey40')+
  annotate(geom='text',label="converging", angle=90,x=2,y=1.3, col='grey40')+
  geom_vline(xintercept = 4, lty=2,col='grey40')+
  annotate(geom='text',label="binomial variance", angle=90,x=5,y=1.3, col='grey40')+
  geom_vline(xintercept = 31, lty=2,col='grey40')+
  annotate(geom='text',label="overdispersion", angle=90,x=32,y=1.3, col='grey40')+
  theme_minimal()+ylab(expression(paste(italic(Var),"[",italic(X),"]")))+xlab("Parliament")+
  scale_x_continuous(breaks = seq(1,46, 5))+
  scale_y_continuous(limits=c(0,2))

plot0 # interesting overdispersion from parliament #30 ()

ggsave(plot0,file="tables_figures/manuscript/plot_var_3_4.pdf", width = 4, height = 3)


# check that nothing is going on with the mean (should be tautologically equal)
plot0mean<-ggplot(dists_sum)+geom_line(aes(parliament, exp_mean),linewidth = 2)+
  geom_line(aes(parliament, obs_mean), col="red")+
  annotate(geom='text',label= expression(paste(italic(E),"[",italic(X),"]"," ", (Expected))), x=23,y=.85, col= "black")+
  annotate(geom='text',label= expression(paste(italic(E),"[",italic(X),"]"," ", (Observed))), x=23,y=0.1, col='red')+
  theme_minimal()+ylab(expression(paste(italic(E),"[",italic(X),"]")))+xlab("Parliament")+
  scale_x_continuous(breaks = seq(1,46, 5))+
  scale_y_continuous(limits=c(0,2))

plot0mean

ggsave(plot0mean,file="tables_figures/manuscript/plot_means_3_4.pdf", width = 4, height = 3)


dists_sum$delta_var <- dists_sum$obs_var-dists_sum$exp_var
dists_sum$delta_mean <- dists_sum$obs_mean-dists_sum$exp_mean

cor.test(dists_sum$exp_var, dists_sum$obs_var)


plot0_scatter_mean<- ggplot(dists_sum, aes(exp_mean, obs_mean, col=parliament))+
  geom_abline(slope = 1, lwd=.25) + geom_point() +theme_minimal()+
  ylab(expression(paste(italic(E),"[",italic(X),"]"," ",(Observed))))+
  xlab(expression(paste(italic(E),"[",italic(X),"]"," ",(Expected))))+
  labs(col='Parliament')+
  scale_y_continuous(limits=c(0,2))+
  scale_x_continuous(limits=c(0,2))


plot0_scatter_mean

ggsave(plot0_scatter_mean,file="tables_figures/manuscript/plot_mean_scatter.pdf", width = 4, height = 3)


plot0_scatter_var<- ggplot(dists_sum, aes(exp_var, obs_var, col=parliament))+
  geom_abline(slope = 1, lwd=.25) + geom_point() +theme_minimal()+
  xlab(expression(paste(italic(Var),"[",italic(X),"]"," ",(Expected))))+
  ylab(expression(paste(italic(Var),"[",italic(X),"]"," ",(Observed))))+
  labs(col='Parliament')+scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(0,2))


plot0_scatter_var

ggsave(plot0_scatter_var,file="tables_figures/manuscript/plot_var_scatter.pdf", width = 4, height = 3)


# testing for over-dispersion using simulation using the ratio of the variances.

set.seed(123)

sim_vars_test<-bind_cols(parliament=1:46,
                         obs_ratio =dists_sum$obs_var/dists_sum$exp_var ,
                         pctile_0_95 = rep(NA,46),
                         pctile_0_99 = rep(NA,46))

for(i in 1:46){

  bucket<-bind_cols(parliament=rep(i,1000), sim_var_ratio=rep(NA,1000))
  for(j in 1:1000){
    probs<-rbinom(n=acts_for_test$on_books[i],
                  size=acts_for_test$enactments[i],
                  prob=1/acts_for_test$on_books[i])



    probs_table<-  probs |>table()|>
      as.data.frame()

    probs_table$probs<-probs_table$probs|>as.character()|>as.numeric()

    K <-data.frame(K=0:acts_for_test$enactments[i] )
    K<-K|>left_join(probs_table, by=join_by("K"=="probs"))
    K$Freq[is.na(K$Freq)]<-0
    K$Pr<-K$Freq/sum(K$Freq)

    exp_var<-sum(K$Pr*K$K*(1-K$Pr))


    bucket$sim_var_ratio[j] <- exp_var/dists_sum$exp_var[i]
  }

  sim_vars_test$pctile_0_95[i] <- quantile(bucket$sim_var_ratio, .95)
  sim_vars_test$pctile_0_99[i] <- quantile(bucket$sim_var_ratio, .99)

}

sim_vars_test$overdispersed95<-sim_vars_test$obs_ratio>sim_vars_test$pctile_0_95
sim_vars_test$overdispersed99<-sim_vars_test$obs_ratio>sim_vars_test$pctile_0_99

#looking at the number of expected and observed K >= 5 in a given term, over time

exp_pr_five_or_more<-dists[dists$k>=5,]|>
  group_by(parliament)|>
  summarise(exp_pr_5_or_more=sum(Pr))


dists_sum<-dists_sum|>left_join(exp_pr_five_or_more)


obs_pr_five_or_more<-dists_observed[dists_observed$k>=5,]|>
  group_by(parliament)|>
  summarise(obs_pr_5_or_more=sum(Pr))


dists_sum<-dists_sum|>left_join(obs_pr_five_or_more)


# visualising k >= 5

plot1_five<-dists_sum|>
  ggplot()+
  geom_line(aes(parliament,exp_pr_5_or_more))+
  geom_line(aes(parliament,obs_pr_5_or_more),col='red')+
  ylab(expression(P(X >= 5)))+
  xlab("Parliament")+theme_minimal()+
  scale_x_continuous(breaks = seq(1,46, 5))+
  annotate(geom='text',label="Expected", x=20,y=-.005, col= "black")+
  annotate(geom='text',label="Observed", x=20,y=.02, col='red')+
  geom_vline(xintercept = 31, lty=2,col='grey40')+
  annotate(geom='text',label="overdispersion", angle=90,x=32,y=0.08, col='grey40')
plot1_five


#looking at the number of expected and observed K = 0 in a given term, over time

exp_pr_zero<-dists[dists$k==0,]|>
  group_by(parliament)|>
  summarise(exp_pr_zero=sum(Pr))


dists_sum<-dists_sum|>left_join(exp_pr_zero)


obs_pr_zero<-dists_observed[dists_observed$k==0,]|>
  group_by(parliament)|>
  summarise(obs_pr_zero=sum(Pr))


dists_sum<-dists_sum|>left_join(obs_pr_zero)


# visualising k = 0

plot1_zero<-dists_sum|>
  ggplot()+
  geom_line(aes(parliament,exp_pr_zero))+
  geom_line(aes(parliament,obs_pr_zero),col='red')+
  ylab(expression(P(X == 0)))+
  xlab("Parliament")+theme_minimal()+
  scale_x_continuous(breaks = seq(1,46, 5))+
  scale_y_continuous(limits = c(0,1.15))+
  annotate(geom='text',label="Expected", x=40,y=.1, col= "black")+
  annotate(geom='text',label="Observed", x=40,y=.81, col='red')+
  geom_vline(xintercept = 31, lty=2,col='grey40')+
  annotate(geom='text',label="overdispersion", angle=90,x=32,y=0.9, col='grey40')
plot1_zero


# tracking the monster acts...

model_set<-model_set|>left_join(ausPH::getParliaments()|>select(PID, DateOpening), by=join_by('parliament'=="PID"))

plot_2_monster<-model_set|>
  group_by(principal_name)|>
  mutate(sum_amendments=sum(number_enactments))|>
  filter(sum_amendments>193)|>filter(parliament<47)|>
  ggplot()+
  geom_line(aes(DateOpening,number_enactments))+
  facet_wrap(~principal_name, ncol=2)+ theme_minimal()+
  ylab("Amendments per term")+xlab("Parliament")+
  scale_x_date(breaks = seq(as.Date("1900/1/1"), as.Date("2022/1/1"), "10 years"), date_labels = "%Y")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank())

plot_2_monster

ggsave(plot_2_monster,file="tables_figures/manuscript/plot_monster_acts.pdf", width = 8, height = 4)



# plot4<-pr_two_or_more|>
#   ggplot(aes(pr_2_or_more,pr_multiple_enactments))+
#   geom_point() +
#   xlab(expression(paste(P(X >= 2))))+
#   ylab("Observed")+theme_minimal()+ geom_abline(slope=1)+
#   scale_x_continuous(limits=c(0,.6),breaks = seq(0,.6, .1))+
#   scale_y_continuous(limits=c(0,.6),breaks = seq(0,.6, .1))+
#   annotate(geom='text',label=expression(y==x), x=.5,y=.55, hjust="left" )+
#   annotate(geom='text',label=expression(paste(rho==0.94,", ", p<0.001)), x=.13,y=.37, hjust="right" )+
#   annotate(geom='text',label=expression(paste(Var[Observed]==0.008)), x=.13,y=.32, hjust="right" )+
#   annotate(geom='text',label=expression(paste(Var[P(X >= 2)]==0.023)), x=.13,y=.27, hjust="right" )
# plot4
#
#
# cor.test(pr_two_or_more$pr_multiple_enactments,pr_two_or_more$pr_2_or_more)
#
# var( pr_two_or_more$pr_2_or_more)
# var(pr_two_or_more$pr_multiple_enactments )
#
#
#
# # now for deviance...
#
# pr_two_or_more$delta<-pr_two_or_more$pr_multiple_enactments-pr_two_or_more$pr_2_or_more
#
# pr_two_or_more<-pr_two_or_more|>left_join(parls|>select(PID,FQName), by=join_by(parliament==PID))|>
#   arrange(delta)
#
#
#
# pr_two_or_more|>
#   ggplot(aes(y=reorder(FQName, delta ),x=delta))+
#   geom_vline(xintercept = 0, col="grey")+
#   geom_point()+
#   xlab(expression(paste(observed - P(X >= 2))))+
#   ylab("Parliament")+theme_minimal()
#
#
#
#
# plot2<-pr_two_or_more|>
#   ggplot()+
#   geom_line(aes(parliament,delta))+
#   ylab(expression(paste(observed - P(X >= 2))))+
#   xlab("Parliament")+theme_minimal()+ geom_hline(yintercept=0, lty=2)
# plot2
#
# plot3<- pr_two_or_more|>
#   ggplot()+
#   geom_histogram(aes( delta), bins=20)+
#   geom_density(aes( delta))+
#   xlab(expression(paste(observed - P(X >= 2))))+
#   theme_minimal()+ geom_vline(xintercept=0, lty=2)
# plot3

# # what about when we account for weighted amendments (that's better)...
#
# weighting<- amend_counts|>select(parliament, wt_number_enactments)
#
# weighting<-weighting|>group_by(parliament)|>summarise(wt_n_multiple_amendments= sum(wt_number_enactments>=2),
#                                                       wt_pr_multiple_amendments= mean(wt_number_enactments>=2))
#
# pr_two_or_more<-pr_two_or_more|>left_join(weighting)
#
# pr_two_or_more|>
#   ggplot()+
#   geom_line(aes(parliament,n_2_or_more))+
#   geom_line(aes(parliament,wt_n_multiple_amendments),col='red')+
#   scale_x_continuous(breaks = seq(0,46, 2))+
#   ylab("N. Acts with Multiple Amendments")+
#   xlab("Parliament")+theme_minimal()
#
# pr_two_or_more|>left_join(weighting)|>
#   ggplot()+
#   geom_line(aes(parliament,pr_2_or_more))+
#   geom_line(aes(parliament,wt_pr_multiple_amendments),col='red')+
#   ylab("Prop. Acts with Multiple Amendments")+
#   xlab("Parliament")+theme_minimal()+
#   scale_y_continuous(limits = c(0,1))+
#   scale_x_continuous(breaks = seq(0,46, 2))


#
# plot0_1<-dists_sum|>
#   ggplot()+
#   geom_line(aes(parliament,delta))+
#   ylab(expression(paste(observed - expected)))+
#   xlab("Parliament")+theme_minimal()+ geom_hline(yintercept=0, lty=2)
# plot0_1
#


## now to look at the poisson binomial version of this...



