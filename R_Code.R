getwd()
setwd("C:/Users/shett/Documents/R")

install.packages("gganimate")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("plotrix")
install.packages("reshape2")
install.packages("caret")
library(corrplot)
library(ggplot2)
library(gganimate)
library(reshape2)
library(plotrix)


#read csv file
a=read.csv("data.csv",header=TRUE,sep=",")
#print csv file
print(a)

#number of rows
nrow(a)
#number of columns
ncol(a)
#get column names
colnames(a)

#structure
str(a)
#summary
summary(a)

#correlation matrix
cord=cor(head(a,100))
print(cord)
#corrplot using correlation matrix
png("corrplot.png",width = 700,height=700)
corrplot(cord, method="square", bg="black",type="full",tl.col="black",
         title="Correlation Plot" ,tl.cex=1)
dev.off()

#occurance of each price range
freq=table(a$price_range)
freq

#pie chart
colors=c("gray52","gray65","black","gray90")
png("pie_chart1.png")
pie1=pie(freq,col=colors,main="Pie chart representing Price Range")
legend("bottomright",c("0(Low Cost)","1(Medium Cost)","2(High Cost)","3(Very High Cost)"),
       cex=0.8,fill=colors,bg="grey")
dev.off()
#3D pie chart
png("3Dpie_chart.png")
piepercent=round(100*freq/sum(freq))
pie3D(freq,labels=piepercent,col=colors,main="3D Pie Chart Representing Price Range",explode=0.05)
legend("topright",c("0(Low Cost)","1(Medium Cost)","2(High Cost)","3(Very High Cost)"),
       cex=0.8,fill=colors,bg="grey")
dev.off()

#scatter plot1
z1=ggplot(a, aes(int_memory,ram ,size =mobile_wt,colour=price_range ))+
  geom_point(alpha = 0.7, show.legend = TRUE)+
  labs(title = "Scatter Plot",x = 'Internal Memory', y = 'Ram')+
  theme_bw()+theme(plot.background=element_rect(fill="grey") )
ggsave('scatter1.png', width=8, height=8)
z1
#animated scatter plot1
q1=ggplot(a, aes(int_memory,ram , size =  mobile_wt, colour =price_range )) +
  geom_point(alpha = 0.7, show.legend = TRUE)  +
  labs(title='Price Range= {frame_time}', x = 'Internal Memory', y = 'Ram')+
  transition_time(price_range) +
  ease_aes('linear')+theme_bw()
animate(q1)
#scatterplot 2
z2=ggplot(a,aes(battery_power,talk_time,size = mobile_wt,colour=price_range ))+
  geom_point(alpha = 0.7, show.legend = TRUE)+facet_wrap(~price_range)+
  theme(plot.background=element_rect(fill="grey"))+
  labs(title = "Scatter Plot",x = 'Battery power ', y = 'Talk Time')
ggsave('scatter2.png', width=8, height=8)
z2
#animate scatter plot 2
q2=ggplot(a,aes(battery_power,talk_time,size=mobile_wt,colour=price_range))+
  geom_point(alpha = 0.7, show.legend = TRUE)+
  facet_wrap(~price_range)+
  labs(title='Touch screen= {frame_time}',x ='Battery Power',y ='Talk Time')+
  transition_time(touch_screen) +
  ease_aes('linear')
animate(q2)

#box plot1
w1=data.frame(a$fc,a$pc,a$sc_w,a$sc_h,a$price_range)
dmelt1<-melt(w1,id="a.price_range",variable="Features")
print(dmelt1)
b1=ggplot(dmelt1,aes(x=Features,y=value,fill=as.factor(a.price_range)))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=45,hjust=1),plot.background=element_rect(fill="grey"))+
  labs(title = "Box Plot")
ggsave('box1.png', width=8, height=8)
b1
#animated blox plot1
animb1 = b1 + transition_time(a.price_range) + labs(title = "price_range: {frame_time}") + ease_aes('linear')
animate(animb1)

#lineplot1
new=data.frame(a$m_dep,a$mobile_wt,a$px_height,a$px_width,a$id)
new1=head(new,50)
m=melt(new1,id="a.id",variable="Features")
print(m)
plot=ggplot(m,aes(x=a.id,y=value,colour=Features))+geom_line()+
  labs(title="Line Plot",x="Id")+theme_classic()+
  theme(panel.background = element_rect(fill="#333333"),plot.background = element_rect(fill="#CCCCCC"))
ggsave('line1.png', width=8, height=8)
plot
#animate line plot
animline=plot+transition_reveal(a.id)+labs(title='id={frame_along}')+
  ease_aes('linear')
animate(animline)

#histogram1
hist1=ggplot(a,aes(x=px_height,fill=as.factor(blue)))+
  geom_histogram(aes(y=..density..),position='identity',alpha=0.5,bins=50)+
  labs(title="Pixel Resolution Height Histogram Plot",x="Pixel Height",y="Density")+
  theme_dark()+ theme(panel.background = element_rect(fill="black"),plot.background = element_rect(fill="#CCCCCC"))
ggsave('hist1.png', width=8, height=8)
hist1
#histogram2
hist2=ggplot(a,aes(x=px_width,fill=as.factor(wifi)))+
  geom_histogram(aes(y=..density..),position='identity',alpha=0.5,bins = 50)+
  geom_density(alpha=0.4)+facet_grid(price_range~.)+
  labs(title="Pixel Resolution Width Histogram Plot",x="Pixel Width",y="Density")+
  theme_bw()+theme(panel.background = element_rect(fill="lightblue"),plot.background = element_rect(fill="#CCCCCC"))
ggsave('hist2.png', width=8, height=8)
hist2





 




