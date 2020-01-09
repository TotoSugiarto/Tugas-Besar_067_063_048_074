#set and get location
getwd()
setwd("C:/Users/ASUS/Downloads/Data")
#library
install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")
install.packages("ggplot2")
library(ggplot2)
library(cluster)
library(factoextra)
library(magrittr)
library(dplyr)
install.packages('plotly')
install.packages('hrbrthemes')
install.packages('tidyverse')
library(plotly)
library(hrbrthemes)
library(tidyverse)
#get dataset
df <- read.csv("datamining.csv", sep = ",")
#////////////////////////////////Pertahun
by_years<- df %>% group_by(year)
g <- ggplot(data=summarise(.data=by_years, Jumlah = sum(number)),aes(x=year, y = Jumlah )) + geom_line() + geom_point() + theme_minimal()                                                                   
ggplotly(g,tooltip="Fire Cases Reported")
#//////////////////////////////////////////
#/////////////////per state
by_states <- df %>% group_by(state)
ggplot(data=by_states,aes(x=state,y=number)) + geom_col(aes(fill=state)) + coord_flip(
) + theme_minimal() +  theme(legend.position = "none") 
#///////////////////////////
#/kmeannnn

data.numerik<-by_states[2:4]
data.numerik
data.stds<-scale(data.numerik)
data.stds
fviz_nbclust(data.stds, kmeans, method = "silhouette")
Clustering=kmeans(data.stds,centers=3,nstart=25)
Clustering
fviz_cluster(Clustering, geom = "point", data = data.stds)+ggtitle("k=3")

final=data.frame(df, Clustering$cluster)
View(final)