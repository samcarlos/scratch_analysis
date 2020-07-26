#idea was to scrape bat prices for caymans and see how good a model could predict it. 
#initial results were promising (.95 r^2). However, model was predicting differences between gt4
#and other non-special caymans. removing gt4's yielded an r^2 closer to .65. Not bad but still alot of
#variation in estimates vs actuals. Most likely due to features not encoded (color, carbon brakes, etc)
#might look into how 'reserve not met' can be used to predict prices...

require(rvest)

#function definition
html_text_collapse <- function(x, trim = FALSE, collapse = "\n"){
  UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = "\n"){
  vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}

library(rvest)
url = 'https://bringatrailer.com/porsche/porsche/?pagedl=100'

webpage <- read_html(url)
faddress<-html_nodes(webpage, 'a') %>%
  html_attrs()

faddress<-html_nodes(webpage, 'a') %>%
  html_attrs()

get_webpages = function(url){
  
  webpage <- read_html(url)
  faddress<-html_nodes(webpage, 'a') %>%
    html_attrs()
  return(unlist(faddress))
}

webpages = lapply(1:400, function(x) get_webpages(paste0('https://bringatrailer.com/porsche/porsche/?pagedl=',x)))
webpages_unlist = unique(unlist(webpages))

porsche_listings = webpages_unlist[grep('listing/', webpages_unlist)]


get_bat_listing_info = function(url){
  print(url)
  webpage = read_html(url)
  meta_info = html_nodes(webpage, '.listing-essentials')%>%
    html_text_collapse.xml_node()
  results = html_nodes(webpage, '.hide-mobile-inline span')%>% html_text()
  text = html_nodes(webpage, '.post-excerpt p')%>% html_text()
  Sys.sleep(2)
  return(list(results = results, text = text, meta_info = meta_info))
  
}

listings_993 = porsche_listings[grep('-993',porsche_listings)]
listings_964 = porsche_listings[grep('-964',porsche_listings)]
https://bringatrailer.com/listing/196-porsche-993-c4/
cayman_listings = porsche_listings[grep('cayman',porsche_listings)]
all_results = list()

for(x in 1:length(cayman_listings)){
  
  all_results[[x]] = get_bat_listing_info(cayman_listings[x])
}

clean_auction_results = function(x){
  if(length(x) <1){return(NULL)}
  temp = data.frame('sold' = x[[1]]=='Sold For', 'price' = x[[2]], date = x[[4]])
  return(temp)
}

cayman_auction_results = lapply(all_results, function(x) clean_auction_results(x$results))

cayman_text = lapply(all_results, function(x) paste(x$text, collapse = ' '))

clean_meta_data = function(x, key_words){
  x = tolower(x)
  key_words = tolower(key_words)
  new_data = lapply(key_words, function(q) x[grep(q,x)[1]])
  new_data[new_data =="character(0)"] = "NA"
  new_data = data.frame(t((unlist(new_data))))
  colnames(new_data) = key_words

    return(new_data)
}

get_variant = function(x){
  key_words = c('-s','-gt4','-r')
  x = tolower(x)
  key_words = tolower(key_words)
  new_data = lapply(key_words, function(q) x[grep(q,x)[1]])
  new_data[new_data =="character(0)"] = "NA"
  new_data = data.frame(t((unlist(new_data))))
  colnames(new_data) = key_words
  
  return(new_data)
}


key_words = c('miles','flat','speed','Porsche Cayman','owner')
cayman_meta = lapply(all_results, function(x) (clean_meta_data(strsplit(x$meta_info, '\n')[[1]], key_words)))


clean_meta_data(cayman_meta[[4]][[1]], key_words)
cayman_meta = do.call(rbind,cayman_meta )
cayman_meta[,'miles'] = gsub(',','',cayman_meta[,'miles'])
cayman_meta[,'miles'] = gsub('k','000',cayman_meta[,'miles'])
cayman_meta[,'miles_2'] = extract_numeric(cayman_meta[,'miles'])
cayman_meta_1 = gsub('https://bringatrailer.com/listing/','',cayman_listings)

cayman_year = unlist(lapply(strsplit(cayman_meta_1, '-'), function(x) x[[1]]))
cayman_year = unlist(lapply(strsplit(cayman_meta_1, '-'), function(x) x[[3]]))

cayman_year = lapply(cayman_meta_1, function(x)strsplit(x, '-')[[1]][[1]])
cayman_year = as.numeric(unlist(cayman_year))
cayman_variant = 1-is.na(do.call(rbind,lapply(cayman_meta_1, get_variant)))*1

cayman_auction_results = do.call(rbind,cayman_auction_results)
cayman_auction_results[,'price'] = gsub(',','',cayman_auction_results[,'price'])
cayman_auction_results[,'price'] = gsub('\\$','',cayman_auction_results[,'price'])

cayman_auction_results[,'date'] = as.Date(cayman_auction_results[,'date'], '%m/%d/%y')

initial_model_df = cbind(cayman_auction_results, cayman_variant[-c(1:2),], cayman_meta[-c(1:2),c('miles_2', 'speed')], year = cayman_year[-c(1:2)], listing = cayman_listings[-c(1,2)])
initial_model_df
initial_model_df[,'price'] = as.numeric(initial_model_df[,'price'])
initial_model_df[,'speed_1'] = gsub('five','5',initial_model_df[,'speed'])
initial_model_df[,'speed_1'] = gsub('six','6',initial_model_df[,'speed_1'])
initial_model_df[,'gears_5'] = 0
initial_model_df[grep('5',initial_model_df[,'speed_1']),'gears_5'] = 1
initial_model_df[,'gears_6'] = 0
initial_model_df[grep('6',initial_model_df[,'speed_1']),'gears_6'] = 1

initial_model_df[,'gears_7'] = 0
initial_model_df[grep('7',initial_model_df[,'speed_1']),'gears_7'] = 1

initial_model_df[,'tiptronic'] = 0
initial_model_df[grep('tiptronic',initial_model_df[,'speed_1']),'tiptronic'] = 1

initial_model_df[,'pdk'] = 0
initial_model_df[grep('pdk',initial_model_df[,'speed_1']),'pdk'] = 1
colnames(initial_model_df) = gsub('-','',colnames(initial_model_df))


save(initial_model_df,  file = 'C:/Users/Larry/Downloads/cayman_bat.rdata')
reg_1 = lm(price ~ s+gt4+r+miles_2+gears_5+gears_7+tiptronic+pdk, data = initial_model_df)
summary(reg_1)
pairs(initial_model_df[,c('price','miles_2','s')])


random_sample = sample(nrow(initial_model_df), 20)

library(mgcv)
reg_2 = gam(price ~ s+gt4+r+s(miles_2)+gears_5+tiptronic+pdk+as.factor(year), data = initial_model_df[-random_sample,])
preds = predict(reg_2, initial_model_df[random_sample,])

oos_preds_vs_actual = lm(initial_model_df[random_sample,'price'] ~ preds)

plot(ureg_2)
summary(reg_2)

##subset to non gt r cars 
normal_caymans = subset(initial_model_df, gt4==0 & r==0 )
normal_caymans_na_omit= na.omit(normal_caymans)
reg_3 = gam(price ~ s+s(miles_2)+gears_5+tiptronic+pdk+as.factor(year)+s(as.numeric(date)), data = normal_caymans_na_omit)
summary(reg_3)
plot(predict(reg_3), normal_caymans_na_omit[,'price'])

reg_4 = gam(price ~ sold+s+miles_2+gears_5+tiptronic+pdk+as.factor(year)+s(as.numeric(date)), data = normal_caymans_na_omit)
summary(reg_4)


normal_caymans_1 = normal_caymans
normal_caymans_1 = rbind( normal_caymans_1[1,],normal_caymans_1)
normal_caymans_1[1,'date'] = as.Date('2020-07-04')
normal_caymans_1[1,'miles_2'] = 60000
predict(reg_3, normal_caymans_1[1,])
head(normal_caymans_1)



reg_sold = gam((sold==TRUE)*1 ~ price+ s+miles_2+gears_5+tiptronic+pdk+as.factor(year)+s(as.numeric(date)), data = normal_caymans_na_omit)
summary(reg_sold)
