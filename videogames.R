vg<- read.csv("~/Desktop/vgsales.csv")

vg$Platform<-as.factor(vg$Platform)
vg<-vg[complete.cases(vg),]
vg$Genre<-as.factor(vg$Genre)
vg$Year<-as.numeric(vg$Year)
vg$Publisher<-as.factor(vg$Publisher)


#to check for repeated observations
library(dplyr)
dim(distinct(vg))
dim(vg)


#EDA
library(ggplot2)
#From 1980-2020
which.max(table(vg$Publisher))#EA has most games 
plot(table(vg$Platform),las=2)#DS,PS2 most platforms 
plot(table(vg$Genre),las=2)#ACTION games made more


cor(vg$Global_Sales,vg$EU_Sales)
cor(vg$Global_Sales,vg$JP_Sales)
cor(vg$Global_Sales,vg$NA_Sales)
cor(vg$Global_Sales,vg$Other_Sales)


g<-ggplot(vg,aes(Platform))+geom_bar(aes(color=Global_Sales))
#global sales is more in DS,PS2,wii,xbox360

a<-ggplot(vg,aes(Genre))+geom_bar(aes(color=Global_Sales))
#action sales is more followed by sports

plot(table(vg$Year))
#more games releases during the 2000s,2007-8

#top 10 publishers
b<-sort(table(vg$Publisher),decreasing = TRUE)
plot(b[1:10],las=2)


#best genre every year
e<-group_by(vg,Year,Genre)
f<-summarise(e,sum(Global_Sales))
j<-top(f,1)
ggplot(data =j,aes(x = Year, y = j$`sum(Global_Sales)`,fill=Genre))+geom_bar(stat = "identity")

#best platform every year
h<-group_by(vg,Year,Platform)
i<-summarise(h,sum(Global_Sales))
ggplot(data =i,aes(x = Year, y = i$`sum(Global_Sales)`,fill=Platform))+geom_line(stat = "identity",aes(color= Platform))

#best game every year
k<-group_by(vg,Year)
m<-top_n(k,1)

ggplot(m,aes(m$Global_Sales,m$Name,fill=Year))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle=45,hjust=1),
                                                                plot.title = element_text(hjust=.5))


#z<-group_by(vg,Genre)
#z<-summarise(z,count=n())
#z<-top_n(z,1)
#ggplot(z,aes(z$Genre,z$count))+geom_bar(stat ="identity")

