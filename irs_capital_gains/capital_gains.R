library(dplyr)
library(ggplot2)

capital_gains = read.csv('C:/Users/Larry/Downloads/capital_gains.csv')
capital_gains_meta = read.csv('C:/Users/Larry/Downloads/capital_gains_meta.csv')
colnames(capital_gains_meta)[1] = 'type'
capital_gains[,'Type'] = tolower(capital_gains[,'Type'])
capital_gains[,'type'] = trimws(capital_gains[,'type'])
capital_gains_meta[,'type'] = trimws(capital_gains_meta[,'type'])

capital_gains = merge(capital_gains, capital_gains_meta, on = 'type')

##capital_gains[which(capital_gains[,'Type'] == 'losses'),'Gain'] = -capital_gains[which(capital_gains[,'Type'] == 'losses'),'Gain']
#capital_gains[which(capital_gains[,'Type'] == 'losses'),'Basis'] = -capital_gains[which(capital_gains[,'Type'] == 'losses'),'Basis']
#capital_gains[which(capital_gains[,'Type'] == 'losses'),'Sales.price'] = -capital_gains[which(capital_gains[,'Type'] == 'losses'),'Sales.price']

capital_gains_r = capital_gains %>% 
  group_by(type, start, end, Year) %>% 
  summarise(sum_sales_price = sum(Sales.price), sum_basis = sum(Basis))%>%
  mutate(gains = sum_sales_price - sum_basis, return = (sum_sales_price - sum_basis)/sum_basis, midpoint_time = (start+end)/2)%>%
  mutate(arr = (sum_sales_price / sum_basis)^(1/midpoint_time) - 1, purchased_time = Year-midpoint_time) %>%
  as.data.frame()

ggplot(subset(capital_gains_r, Year == 2002), aes(x = start, y = gains))+geom_point()
ggplot(subset(capital_gains_r, Year == 2002), aes(x = start, y = return))+geom_point()
ggplot(subset(capital_gains_r, Year == 2007), aes(x = start, y = return))+geom_point()

capital_gains_r
ggplot(capital_gains_r, aes(x = purchased_time, y = arr, group = as.factor(start), colour = as.factor(start)))+geom_point()+
  geom_line()+facet_grid(start~.)
capital_gains_r[,'purchased_time_floor'] = floor(capital_gains_r[,'purchased_time'])

ggplot(subset(capital_gains_r, purchased_time_floor>1994& midpoint_time <5), aes(x = midpoint_time, y = arr, group = as.factor(purchased_time_floor), colour = as.factor(purchased_time_floor)))+geom_point()+
  geom_line()+geom_smooth(se = FALSE)+facet_grid(purchased_time_floor~.)

ggplot(subset(capital_gains_r, purchased_time_floor>1994& midpoint_time <5 &midpoint_time > 1), aes(x = midpoint_time, y = arr, group = as.factor(purchased_time_floor), colour = as.factor(purchased_time_floor)))+geom_point()+
  geom_line()+facet_grid(purchased_time_floor~.,scales = 'free_y')

ggplot(subset(capital_gains_r,purchased_time == 2000), aes(x = midpoint_time, y = arr, group = as.factor(start), colour = as.factor(start)))+geom_point()+
  geom_line()

capital_gains_r_1 = na.omit(capital_gains_r)[,c('arr','purchased_time','Year', 'start')]
capital_gains_r_1$group = 1:nrow(capital_gains_r_1)

ggplot(capital_gains_r_1, aes(x = purchased_time, y = Year, colour = arr))+geom_point()

c_gains_1 = capital_gains_r_1[,c('arr','purchased_time','group')]
c_gains_2 = capital_gains_r_1[,c('arr','Year','group')]
colnames(c_gains_2 ) = c('arr','year','group')
colnames(c_gains_1 ) = c('arr','year','group')
ggplot(subset(rbind(c_gains_1, c_gains_2), arr<1), aes(x = year, y= arr, group = as.factor(group), colour = as.factor(group)))+
  geom_line()


head(capital_gains_r)

capital_gains_2 = aggregate(Sales.price    -  Basis ~ Year + type+start+end, data = capital_gains, FUN = sum)
capital_basis = aggregate( Basis ~ Year + type+start+end, data = capital_gains, FUN = sum)

capital_gains_2[,'returns'] = capital_gains_2[,'Sales.price - Basis']/capital_basis[,'Basis']

capital_gains_2 = aggregate(returns ~ Year + type, data = capital_gains, FUN = sum)


library(ggplot2)

ggplot(subset(capital_gains_2), aes(x = Year, y = returns))+geom_point()+geom_line()+facet_wrap(.~type, scales = 'free_y')



ggplot(subset(capial_gains_2, type == 'Under 1 month'),
       aes(x = Year, y = Gain))+geom_point()+geom_line()

subset(capital_gains,  Type == "losses" & type == 'Under 1 month')
