# apply simple OLS model and check for predictive accuracy

model_data <- readRDS("data/amends/amends_01_model_data.rds")

model_data$parl_time_left_years<-as.numeric(model_data$parl_time_left/365)


# MLMM with grouping
library(lme4)




# weighted model

wt_binomial<-glmer(data=model_data,
                   multiple_amends ~ log(legPages)+ cum_amendments+parl_time_left_years+
                      (1|subject)+(1|parliament),
                   weights = wt_multiple_amends,
                   family = 'binomial')

wt_binomial|>summary()

predictions_logit_MLM<-boot::inv.logit(predict(wt_binomial))

predictions50_logit_MLM<-round(predictions_logit_MLM)

reality<-wt_binomial@frame$multiple_amends

table(predictions50_logit_MLM,reality)

error_logit_MLM <-reality-predictions_logit_MLM

error_weight_logit_MLM<-model_data$wt_multiple_amends-predictions_logit_MLM


error_logit_MLM_pearson<-residuals(wt_binomial,type="pearson", scaled=TRUE)

parliament<-wt_binomial@frame$parliament


pct_correct_logit_MLM<-(100*sum(predictions50_logit_MLM==reality)/length(reality))


# simpler OLS version

wt_OLS<-lm(data=model_data,
           multiple_amends ~ log(legPages)+ cum_amendments+parl_time_left_years+
              subject +as.factor(parliament),
           weights = wt_multiple_amends)

summary(wt_OLS)



predictions_OLS<- predict(wt_OLS)

predictions_OLS_50<-round(predictions_OLS)
predictions_OLS_50[predictions_OLS_50>1]<-1

table(predictions_OLS_50, reality)

error_OLS <-reality-predictions_OLS
error_weight_OLS<-model_data$wt_multiple_amends-predictions_OLS


parliament<-wt_binomial@frame$parliament


pct_correct_OLS<-(100*sum(predictions_OLS_50==reality)/length(reality))


# comparing errors with OLS and logit MLM

OLS_logit_gg_error_compare<-ggplot(ggplot_data,aes(error_OLS, error_logit_MLM))+
   geom_point(alpha=0.1)+theme_bw()+
   geom_text(data = data.frame(x = 0.36, y = -0.42,
                               label = "rho = 0.96"),
             mapping = aes(x = x, y = y, label = label),
             inherit.aes = FALSE)

cor.test(error_OLS, error_logit_MLM)


#plotting glmer

ggplot_data<-cbind(parliament,error_logit_MLM)|>as.data.frame()


ggplot(ggplot_data,aes(parliament, error_logit_MLM))+geom_point(alpha=0.1)+geom_smooth()
#ggplot(ggplot_data,aes(parliament, error_weight_logit_MLM))+geom_point(alpha=0.1)+geom_smooth()


ggplot(ggplot_data,aes(parliament, predictions))+geom_point(alpha=0.1)+geom_smooth()

# saving model outputs

model_outputs <-bind_cols(model_data,
                          predictions_OLS=predictions_OLS,
                          predictions_logit_MLM=predictions_logit_MLM,
                          predictions_OLS_50=predictions_OLS_50,
                          predictions50_logit_MLM=predictions50_logit_MLM,
                          error_OLS=error_OLS,
                          error_logit_MLM=error_logit_MLM,
                          error_logit_MLM_pearson=error_logit_MLM_pearson,
                          error_weight_OLS=error_weight_OLS,
                          error_weight_logit_MLM=error_weight_logit_MLM
)

saveRDS(model_outputs,'data/amends/amends_02_model_outputs.rds')

hist(model_outputs$error_logit_MLM_pearson, breaks=100)
