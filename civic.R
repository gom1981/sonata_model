
library(tidyverse)
library(rstan)

thefts=read.csv("civic_thefts_by_year.csv")

CarMat=as.matrix(thefts[1:48,3:26])

model <- suppressMessages(stan_model("civic.stan"))

fit_cars <- sampling(model,
                       data = list(N=48,M=24,thefts=CarMat),
                       iter = 2000,
                       chains = 1)

pars=c("a","b","c","d","bump","carseed")

print(fit_cars, pars = pars)


lam=matrix(0,48,24)
a=summary(fit_cars,pars = "a")$summary[1]
b=summary(fit_cars,pars = "b")$summary[1]
c=summary(fit_cars,pars = "c")$summary[1]
d=summary(fit_cars,pars = "d")$summary[1]
bump=summary(fit_cars,pars = "bump")$summary[1]
carseed=summary(fit_cars,pars = "carseed")$summary[1]
for (i in 1:48){
  for(j in 1:24){
    lam[i,j]=0.0
    if((i-j)<26){
      if(i<25){
        lam[i,j]=i**d*carseed/(1.0+a*exp(b*((j-i+25.0)/25.0)**c))
      }else{
        lam[i,j]=i**d*bump*carseed/(1.0+a*exp(b*((j-i+25.0)/25.0)**c))
      }
    }
  }
}

lam2=lam[,1:24]
y <- seq(2000,2023,1)
x <- thefts[1:48,2]
colnames(lam2) <- y
rownames(lam2) <- x

# Transform the matrix in long format
df <- melt(lam2)
colnames(df) <- c("y", "x", "thefts")

ggplot(df, aes(x = x, y = y, fill = thefts)) +geom_tile()+
  scale_fill_gradient2( low = "blue", high = "red")+theme_minimal()

write.csv(lam2,"model_civic.csv")


library(ggplot2)
library(gridExtra)

myplots = list()

myplot <- function(j){
  
  # Plot one column against another
  p<- ggplot()+geom_line(aes(x=thefts[,2],y=thefts[,j+2]),color="red")+geom_line(aes(x=thefts[,2],y=lam[,j]),color="blue")+xlab("Make Year")+ylab("Thefts")+ggtitle(paste0("Civic Theft Year: ",j+1999))+theme_bw()
}

plist <- lapply(1:24, myplot)
p=grid.arrange(grobs=plist,ncol=5)
ggsave("civic_plot.pdf",p,width=15,height=10)



