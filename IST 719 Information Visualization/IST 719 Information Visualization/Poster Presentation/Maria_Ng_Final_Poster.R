

####Data Description:

#The data is downloaded from from CoinMarketCap.com.The data capture the six month of 
#2021 cryptocurrency trends.   

fname <- file.choose()

coin <- read.csv(file = fname
                 , header = TRUE
                 , stringsAsFactors = FALSE)

fname <- file.choose()

holders <- read.csv(file = fname
                 , header = TRUE
                 , stringsAsFactors = FALSE)

#### The data structure output:
str (coin)
str(holders)
summary(coin)


#To determine if this data set suitable for this project. The dataset has 9 columns 
#and 3084 rows would score: (9 * 4) * (3084/100) =  1110.24

(11 * 4) * (3491/100)

##################
# Color
##################

library(RColorBrewer)

##################
# Time Series, scatter and Bar plots (Figure #1, 2, & 3)
##################

library(ggplot2)

theme_set(theme_minimal())

str(coin)
coin$Date<-as.Date(coin$Date)
str(coin)

ggplot(coin) + aes(x = Date, y = Open, color = Name) + 
  (scale_x_date(date_breaks = "1 month", date_labels = "%m")) +
  scale_y_continuous(trans='log10') +
  geom_line(size = 1) +
  ggtitle("Crypto Token Trendings From June to November 2021") +
  xlab(" Month") +
  ylab("Dollar") 

ggplot(coin) + aes(x = Market.Cap, y = Volume, color = Name) + 
  geom_point(size=3, shape=16) +
  ggtitle("Crypto Token Trendings From June to November 2021") +
  xlab(" Market.Cap") +
  ylab("Volume") 

ggplot(holders) + aes(x= Crypto.Type, y = Holders, fill = Name) + 
  geom_bar(stat = "identity") +
  ggtitle("Number of Crypto Holders in 2021") +
  scale_colour_brewer(palette = "Set2")  +
  coord_flip()


######################
# Treemap Figure 4
######################

library(treemap)

treemap(coin, index = c("Crypto.Type", "Name")
        , vSize = "Volume"
        , vColor = "Market.Cap"
        , type = "index"
        , fontsize.labels = 15
        , palette = brewer.pal(6, "Set2"))

######################
# Memes Figure #5
######################
meme<-subset(coin, Crypto.Type == "Memes")
meme
ggplot(meme) + aes(x = Date, y = Market.Cap, color = Name) + 
  (scale_x_date(date_breaks = "1 month", date_labels = "%m")) +
  geom_line() +
  #geom_point() +
  ggtitle("Meme Crypto Token Market.Cap Trendings From June to November 2021") +
  xlab(" Month") +
  ylab("Dollar") +
  scale_y_continuous(trans='log10') +
  scale_colour_brewer(palette = "Set2")

######################
# Shib vs. Doge Figure 6 & 7
######################
shib<-subset(coin, Symbol == "SHIB")
doge<-subset(coin, Symbol == "DOGE")

inu<-rbind(shib, doge)
inu

ggplot(inu, aes(x=Volume, color = Name)) +
  geom_density() +
  ggtitle("Volume Traded of Shib vs.Doge") +
  xlab("Volume") +
  xlim(c(0, 4e+10)) 
   

#Shib
ggplot(shib) + aes(x = Date, y = Open) + 
  (scale_x_date(date_breaks = "1 month", date_labels = "%m")) +
  geom_line(color="8D6DF4") +
  #geom_point(color = '8D6DF4') +
  ylim(c(0, 0.0001)) +
  ggtitle("Shib Token") +
  xlab(" Month") +
  ylab("Dollar") +
  scale_y_continuous(trans='log10')



