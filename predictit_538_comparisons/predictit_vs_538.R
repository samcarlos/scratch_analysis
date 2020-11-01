library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)

#load / transform data 
data = read.csv('website_preds_11_01_2020.csv')
data[,'lower_house_party'] = unlist(lapply(data[,'Lower.House'], function(x) strsplit(x, ' ')[[1]][1]))
data[,'upper_house_party'] = unlist(lapply(data[,'Upper.House'], function(x) strsplit(x, ' ')[[1]][1]))
data[,'governors'] = 'R'
data[which(data[,'Governors'] == 'Democratic'),'governors'] = 'D'
data = mutate(data, diff_538_predictict = fivethirtyeight - predictit)



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
  
