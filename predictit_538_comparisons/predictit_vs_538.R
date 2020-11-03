library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(rjson)
library(reshape2)

#load / transform data 
data = read.csv('C:/Users/Larry/Downloads/website_preds_11_01_2020.csv')
data[,'lower_house_party'] = unlist(lapply(data[,'Lower.House'], function(x) strsplit(x, ' ')[[1]][1]))
data[,'upper_house_party'] = unlist(lapply(data[,'Upper.House'], function(x) strsplit(x, ' ')[[1]][1]))
data[,'governors'] = 'R'
data[which(data[,'Governors'] == 'Democratic'),'governors'] = 'D'
data = mutate(data, diff_538_predictict = fivethirtyeight - predictit)
data = mutate(data, diff_538_predictict_pct = fivethirtyeight / predictit)

data = mutate(data, diff_economist_predictict = fivethirtyeight - predictit)
data[which(data[,'State'] == 'ND'),'economist'] = 1


#set colors 
mycolors = c('blue','red','purple','black')
names(mycolors) = c("D","R","Coalition","NA")
colScale <- scale_colour_manual(name = "grp",values = mycolors)

#order state by difference
data[,'State'] = factor(data[,'State'], data[,'State'][order(data[,'diff_538_predictict'])])

#dc isn't a state 
state_data = subset(data, State !='DC')

by_governor = ggplot(state_data, aes(x = State, y = diff_538_predictict, fill = governors))+geom_bar(stat = 'identity')+
  scale_colour_manual(name = "governors",values = mycolors,aesthetics = c("colour", "fill"))+
  theme_minimal()+
  ylab("P(Trump) Diff")+
  xlab('State')+
  ggtitle("Difference in (predictit-538) Trump Probabilities colored by Governors")+ theme(legend.position="none")+
  theme(text = element_text(size = 15))
by_upper_house = ggplot(state_data, aes(x = State, y = diff_538_predictict, fill = upper_house_party))+geom_bar(stat = 'identity')+
  scale_colour_manual(name = "upper_house_party",values = mycolors,aesthetics = c("colour", "fill"))+
  theme_minimal()+
  ylab("P(Trump) Diff")+
  xlab('State')+
  ggtitle("Difference in (predictit-538) Trump Probabilities colored by Upper House")+ theme(legend.position="none")+
  theme(text = element_text(size = 15))

by_lower_house = ggplot(state_data, aes(x = State, y = diff_538_predictict, fill = lower_house_party))+geom_bar(stat = 'identity')+
  scale_colour_manual(name = "lower_house_party",values = mycolors,aesthetics = c("colour", "fill"))+
  theme_minimal()+
  ylab("P(Trump) Diff")+
  xlab('State')+
  ggtitle("Difference in (predictit-538) Trump Probabilities colored by Lower House")+ theme(legend.position="bottom")+
  theme(text = element_text(size = 15))



discrepency_by_govt_controls = ggarrange(by_governor, by_upper_house, by_lower_house,
                    ncol = 1, nrow = 3)
discrepency_by_govt_controls


by_upper_house_scat = ggplot(state_data, aes(x = 100-fivethirtyeight, y = diff_538_predictict, 
                                             color = upper_house_party,label = State))+
  #geom_point()+
  scale_colour_manual(name = "upper_house_party",values = mycolors,aesthetics = c("colour", "fill"))+
  theme_minimal()+
  ylab("P(Trump) Predictit - Five Thirty Eight")+
  xlab('P(Trump) Five Thirty Eight')+
  ggtitle("Differences in Predictions vs 538 Trump Probabilities Colored by Upper House Party")+ theme(legend.position="bottom")+
  geom_text_repel() + 
  theme(text = element_text(size = 15))
  




by_upper_house_scat_economist = ggplot(state_data, aes(x = 100-economist, y = diff_economist_predictict, 
                                             color = upper_house_party,label = State))+
  #geom_point()+
  scale_colour_manual(name = "upper_house_party",values = mycolors,aesthetics = c("colour", "fill"))+
  theme_minimal()+
  ylab("P(Trump) Predictit - Economist")+
  xlab('P(Trump) Economist')+
  ggtitle("Differences in Predictions vs Economist Trump Probabilities Colored by Upper House Party")+ theme(legend.position="bottom")+
  geom_text_repel() + 
  theme(text = element_text(size = 15))

## Update Analysis

#look at 2016 fivethirtyeight results
fte_2016<- fromJSON( file = "fte_2016.json")
state = unlist(lapply(fte_2016, function(x) x$state))
p_trump = unlist(lapply(fte_2016, function(x) x$latest$R$models$plus$winprob))
forecast_trump = unlist(lapply(fte_2016, function(x) x$latest$R$models$plus$forecast))
forecast_clinton = unlist(lapply(fte_2016, function(x) x$latest$D$models$plus$forecast))
fte_2016_df = data.frame(state, p_trump, forecast_trump, forecast_clinton)

results_2016 = read.csv("C:/Users/Larry/Downloads/2016_results.csv")
results_2016 = results_2016[,c('STATE',"STATE.ABBREVIATION","LAST.NAME","GENERAL.RESULTS")]
results_2016 = results_2016[which(results_2016[,'LAST.NAME'] %in% c('Trump',"Clinton")),]
results_2016[,'GENERAL.RESULTS'] = as.numeric(gsub(',','',results_2016[,'GENERAL.RESULTS']))
results_2016 = aggregate(GENERAL.RESULTS ~ STATE.ABBREVIATION+LAST.NAME, FUN = sum, data = results_2016)


results_2016_wide <- dcast( results_2016, STATE.ABBREVIATION ~ LAST.NAME, value.var="GENERAL.RESULTS", sum)
colnames(results_2016_wide)[1] = 'state'
results_preds_2016 = merge(results_2016_wide, fte_2016_df, by = 'state')
results_preds_2016 = mutate(results_preds_2016, clinton_trump_forecast_ratio = forecast_trump/(forecast_clinton+forecast_trump),
                            clinton_trump_actual_ratio = Trump/(Trump+Clinton))
results_preds_2016 = mutate(results_preds_2016, resid_log_odds = clinton_trump_actual_ratio/(1-clinton_trump_actual_ratio) -clinton_trump_forecast_ratio/(1-clinton_trump_forecast_ratio))
results_preds_2016 = mutate(results_preds_2016, diff_preds = clinton_trump_actual_ratio-clinton_trump_forecast_ratio)
results_preds_2016 = mutate(results_preds_2016, agreed = ((clinton_trump_actual_ratio>.5)== (clinton_trump_forecast_ratio>.5))*1 )
results_preds_2016[,'correct_prediction'] = 'correct'
results_preds_2016[which(results_preds_2016[,'agreed']==0),'correct_prediction'] = 'false'

colnames(results_preds_2016)[1] = 'State'
all_data = merge(results_preds_2016, state_data, by = 'State')

ggplot(all_data, aes(x = clinton_trump_forecast_ratio ,y = diff_preds, label = State))+geom_text_repel()

prev_538_resids_on_predictit_diff = ggplot(all_data, aes(x = diff_preds, y = diff_538_predictict, colour = correct_prediction,
                                            label = State))+
  #geom_point()+
  #scale_colour_manual(name = "upper_house_party",values = mycolors,aesthetics = c("colour", "fill"))+
  theme_minimal()+
  ylab("P(Trump) Predictit - Five Thirty Eight")+
  xlab('2016 Actual-Expected of Trump/(Trump+Clinton) Votes')+
  ggtitle("Differences in Predictions vs 2016 residuals Colored by 538 Correctly Classified")+ theme(legend.position="bottom")+
  geom_text_repel() + 
  theme(text = element_text(size = 15))
prev_538_resids_on_predictit_diff


reg = lm(diff_538_predictict ~ upper_house_party+fivethirtyeight + diff_preds  , data = all_data)
summary(reg)
