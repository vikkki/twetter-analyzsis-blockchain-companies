# the csv is in ./data_sentiment_10.26/
setwd("/home/xiaofan/Documents/INF550/finalpro/")

library(dplyr)

#======================= import sentiment data =======================#
keywords.list <- c("binance","bitcoincom","bitmain","bitpay","bitstamp","blockchain","coinbase","kraken","BCH","shapeshift","xapo")
i = 1

file.names  = paste("./data_sentiment_11.28/" , keywords.list[i] , ".csv", sep = "")

data.names = paste(keywords.list[i],"_data",sep = "")
assign(data.names, read.csv(file.names, row.names=NULL,sep = ","))


two_cols = get(data.names)

two_cols$keyword = rep(keywords.list[i],nrow(get(data.names)))
two_cols$sentiment_score = as.numeric(as.character(two_cols$sentiment_score))# remove NaN rows
two_cols = two_cols[!is.na(two_cols$sentiment_score),]
assign(data.names, two_cols)

data.long = get(data.names) # create the long matrix for ggplot2 and then add the following data to it

for (i in c(2:length(keywords.list))) {
  file.names  = paste("./data_sentiment_11.28/" , keywords.list[i] , ".csv", sep = "")
  data.names = paste(keywords.list[i],"_data",sep = "")
  assign(data.names, read.csv(file.names, row.names=NULL, sep = ","))
  two_cols = get(data.names)
  two_cols$keyword = rep(keywords.list[i],nrow(get(data.names)))
  two_cols$sentiment_score = as.numeric(as.character(two_cols$sentiment_score))
  two_cols = two_cols[!is.na(two_cols$sentiment_score),]
  assign(data.names, two_cols)
  
  data.long = rbind(data.long, get(data.names))
}

#================== add sentiment section =================#
library(RColorBrewer)
library(scales)
library(ggplot2)
data.long$sentiment_group = ifelse(data.long$sentiment_score>0.00001,1,ifelse(data.long$sentiment_score< -0.00001,-1,0))

sentiment.bar = ggplot(data.long,aes(x = keyword))+
  geom_bar(aes(fill=factor(sentiment_group,levels = c(0,-1,1))),position = "fill")+
  coord_polar(theta = "x")+
  scale_fill_manual(values = hue_pal()(4)[c(3,1,2)])

sentiment.bar
#================== filter out the rows with sentiment score of 0 =================#

# convert factor colunms to numeric
data.long$sentiment_score = as.numeric(as.character(data.long$sentiment_score))
data.long$followers = as.numeric(as.character(data.long$followers))
data.long$follows = as.numeric(as.character(data.long$follows))
data.long$repost_count = as.numeric(as.character(data.long$repost_count))


# remove rows contains NAN
data.long = data.long[!is.na(data.long$followers),]

# filter out the rows with sentiment score of 0
data.long.filted = subset(data.long, abs(sentiment_score)>0.0001 )


# make the bar plot of sentiment
library(Rmisc) # to use summarySE function


data.long.CI = summarySE(data.long.filted, measurevar = "sentiment_score" , groupvar = "keyword", na.rm = TRUE)

#================= bar plot of sentiment of companies ===================#

bar_plot = ggplot(data.long.CI,aes(x = keyword, y = sentiment_score)) +
  geom_bar(position = position_dodge() , stat = "identity") +
  geom_errorbar(aes(ymin = sentiment_score - se, ymax = sentiment_score + se),
                position = position_dodge(0.9),
                width=.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

bar_plot

#=================== bar plot of followers of companies ===================#
data.long.followers.CI = summarySE(data.long.filted, measurevar = "followers" , groupvar = "keyword", na.rm = TRUE)
bar_plot_follower = ggplot(data.long.followers.CI,aes(x = keyword, y = followers)) +
  geom_bar(position = position_dodge() , stat = "identity") +
  geom_errorbar(aes(ymin = followers - se, ymax = followers + se),
                position = position_dodge(0.9),
                width=.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

bar_plot_follower

#=================== bar plot of repost of companies ===================#
library(plotrix)

data.long.repost.CI = summarySE(data.long.filted, measurevar = "repost_count" , groupvar = "keyword", na.rm = TRUE)
bar_plot_repost = ggplot(data.long.repost.CI[data.long.repost.CI$keyword != "kraken",],aes(x = keyword, y = repost_count)) +
  geom_bar(position = position_dodge() , stat = "identity") +
  #scale_fill_manual(values=c("grey2","grey20")) +
  #scale_x_discrete(limits=c("6","7","8","9","10")) +
  geom_errorbar(aes(ymin = repost_count - se, ymax = repost_count + se),
                position = position_dodge(0.9),
                width=.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


bar_plot_repost


#=================== scatter plot of sentiment and followers ===================#

scatter.sentiment_followers  = ggplot(data.long.filted[data.long.filted$keyword != "shapeshift",], aes(x = sentiment_score, y = followers, color = keyword)) +
  geom_point(alpha = 0.5)

scatter.sentiment_followers

scatter.company.sentiment_followers  = ggplot(data.long.filted, aes(x = sentiment_score, y = followers, color = keyword)) +
  geom_point(alpha = 0.5,size = 1)+
  facet_grid(keyword~. , scale = "free")
scatter.company.sentiment_followers


#=================== scatter plot of sentiment and repost ===================#

scatter.sentiment_repost  = ggplot(data.long.filted[data.long.filted$keyword != "kraken",], aes(x = sentiment_score, y = repost_count, color = keyword)) +
  geom_point(alpha = 0.5,size = 2)

scatter.sentiment_repost


scatter.company.sentiment_repost  = ggplot(data.long.filted, aes(x = sentiment_score, y = repost_count, color = keyword)) +
  geom_point(alpha = 0.5,size = 1)+
  facet_grid(keyword~. , scale = "free")
scatter.company.sentiment_repost

#=================== histograms ===================#
sentiments = as.data.frame(data.long.filted$sentiment_score)
colnames(sentiments) = "sentiments"

hist.sentiment <- ggplot(data =sentiments, aes(sentiments,..density..))+
  geom_histogram() +
  geom_line(stat = 'density')+
  geom_vline(aes(xintercept = mean(data.long.filted$sentiment_score)), color = "red",linetype = "dashed" ,size = 1)

hist.sentiment

# sub hist for companies

hist.company.histogram <- ggplot(data.long.filted, aes(sentiment_score, color = keyword))+
  geom_histogram() +
  facet_grid(keyword ~ .)
  geom_vline(data = data.long.CI, aes(xintercept = sentiment_score,color = keyword), color = "red",linetype = "dashed")


  hist.company.histogram

# repost hist
library(scales)

repost = as.data.frame(data.long.filted$repost_count)
colnames(repost) = "repost"
hist.repost <- ggplot(data = data.long.filted, aes(repost_count))+
  geom_histogram()+
  scale_y_continuous(trans = log10_trans())

hist.repost

#===================== date and time segmentation and formating =======================#

data.long.filted$date = rep("",nrow(data.long.filted))
data.long.filted$time = rep("",nrow(data.long.filted))

sp.date = strsplit(as.character(data.long.filted$created_time)," ")

for (j in c(1:nrow(data.long.filted))) {
  data.long.filted$date[j] = sp.date[[j]][1]
  data.long.filted$time[j] = sp.date[[j]][2]
}

date.long.CI = summarySE(data.long.filted, measurevar = "sentiment_score" , groupvar = c("date","keyword"), na.rm = TRUE)
# save to fix the missing value
date.sub.CI  = read.csv("./sentiment_time.csv",sep = ",")# read the manully fixed sentiment trend

#correct the date order
date.sub.CI$date = factor(date.sub.CI$date,ordered = TRUE, levels = c("2018/10/26" ,"2018/10/27", "2018/10/28" ,"2018/10/29" ,"2018/10/30" ,"2018/10/31",
                                                                      "2018/11/1" , "2018/11/2" ,"2018/11/3" , "2018/11/4" , "2018/11/5", "2018/11/6",
                                                                      "2018/11/7" , "2018/11/8",  "2018/11/9", "2018/11/10", "2018/11/11", "2018/11/12",
                                                                      "2018/11/13" ,"2018/11/14" ,"2018/11/15", "2018/11/16" ,"2018/11/17" ,"2018/11/18",
                                                                      "2018/11/19", "2018/11/20", "2018/11/21" ,"2018/11/22" ,"2018/11/23", "2018/11/24",
                                                                      "2018/11/25" ,"2018/11/26"))


date_plot_company_trend = ggplot(date.sub.CI,aes(x = date, y = sentiment_score, colour = keyword,group = keyword)) +
  geom_line(size = 0.75,alpha = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

date_plot_company_trend
# facet
date_plot_company_trendfacet = ggplot(date.sub.CI,aes(x = date, y = sentiment_score, colour = keyword,group = keyword)) +
  geom_line(size = 0.75,alpha = 0.5)+
  facet_grid(keyword~.)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

date_plot_company_trendfacet






# BTC prize
library(readxl)
prize <- read_excel("BTC.xlsx", sheet = "BTC")
BTC = prize[1:6,]
ETH = prize[7:12,]
BCH = prize[13:18,]


line_plot1 = ggplot(BTC, aes(x = Date, y = High)) + geom_line(size = 2) + ggtitle("BTC")
line_plot2 = ggplot(ETH, aes(x = Date, y = High)) + geom_line(size = 2) + ggtitle("ETC")
line_plot3 = ggplot(BCH, aes(x = Date, y = High)) + geom_line(size = 2) + ggtitle("BCH")


multiplot(line_plot1,line_plot2,line_plot3,cols = 3)
