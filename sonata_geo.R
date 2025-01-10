
library(tidyverse)
library(rstan)
library(reshape)
library(scales)

thefts=read.csv("civic_west_south_compare_w_2023_7-29-24.csv")



lam2=as.matrix(thefts[1:48,3:26])
y <- seq(2000,2023,1)
x <- thefts[1:48,2]
colnames(lam2) <- y
rownames(lam2) <- x
df <- melt(lam2)
colnames(df) <- c("y", "x", "thefts")

lam2=as.matrix(thefts[49:96,3:26])
y <- seq(2000,2023,1)
x <- thefts[49:96,2]
colnames(lam2) <- y
rownames(lam2) <- x
df2 <- melt(lam2)
colnames(df2) <- c("y", "x", "thefts")

lam2=as.matrix(thefts[97:144,3:26])
y <- seq(2000,2023,1)
x <- thefts[97:144,2]
colnames(lam2) <- y
rownames(lam2) <- x
df3<- melt(lam2)
colnames(df3) <- c("y", "x", "thefts")

ggplot(df, aes(x = x, y = y, fill = thefts)) +geom_tile()+
  scale_fill_gradient2( low = "blue", high = "red")+theme_minimal()
ggplot(df2, aes(x = x, y = y, fill = thefts)) +geom_tile()+
  scale_fill_gradient2( low = "blue", high = "red")+theme_minimal()
ggplot(df3, aes(x = x, y = y, fill = thefts)) +geom_tile()+
  scale_fill_gradient2( low = "blue", high = "red")+theme_minimal()

df$time=df$x-df$y
df$type="all"
df2$time=df2$x-df2$y
df2$type="west"
df3$time=df3$x-df3$y
df3$type="south"

dfc=df %>% filter(time>=0) %>% group_by(time) %>% summarize(thefts=mean(thefts))
dfc2=df2 %>% filter(time>=0) %>% group_by(time) %>% summarize(thefts=mean(thefts))
dfc3=df3 %>% filter(time>=0) %>% group_by(time) %>% summarize(thefts=mean(thefts))

dfc2$p=dfc2$thefts/(dfc$thefts+.001)
dfc3$p=dfc3$thefts/(dfc$thefts+.001)
dfc2$type="west"
dfc2$time=dfc2$time+15
dfc3$type="south"
dfcombine=rbind(dfc2,dfc3)
dfcombine = dfcombine %>% filter(time<40)
dfcombine %>% ggplot(aes(x=time,y=p,color=type,group=type))+geom_point()+theme_minimal()



model=loess(p~time,data=dfcombine,span=.65)
dfcombine$predict=predict(model,dfcombine)

dfplot=dfcombine
names(dfcombine)[4]="bureau"
dfcombine$time[dfcombine$bureau=="west"]=dfcombine$time[dfcombine$bureau=="west"]-15
dfcombine = dfcombine[order(dfcombine$time),]
p=dfcombine %>% ggplot()+geom_point(aes(x=time,y=p,color=bureau,group=bureau))+
  geom_line(aes(x=time,y=predict,group=bureau),color="black")+theme_minimal()+
  xlab("age of car")
ggsave("west2south.pdf",p,width=7,height=4)
write.csv(dfcombine[,c("time","predict","bureau")],"multiplier.csv",row.names=F)

forecast=read.csv("forecast.csv")
rownames=forecast$X
forecast$X=NULL
nr=nrow(forecast)
nc=ncol(forecast)
forecast_south=forecast
forecast_west=forecast

for(i in 1:nr){
  for(j in 1:nc){
    ts=(j-i+24)
    if(ts>=0 & ts<40){
      forecast_south[i,j]=predict(model,ts)*forecast[i,j]
    
    }
    if(ts>=40){
      forecast_south[i,j]=max(.0584-(ts-39)*.016,0)*forecast[i,j]

    }
    ts=(j-i+24+15)
    if(ts>=0 & ts<40){
      forecast_west[i,j]=predict(model,ts)*forecast[i,j]
    }
    if(ts>=40){
      forecast_west[i,j]=max(.0584-(ts-39)*.016,0)*forecast[i,j]
    }
  }
}

row.names(forecast_south)=rownames
row.names(forecast_west)=rownames
write.csv(forecast_south,"forecast_south.csv")
write.csv(forecast_west,"forecast_west.csv")
