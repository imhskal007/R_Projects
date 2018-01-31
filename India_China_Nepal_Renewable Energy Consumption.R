#Load Data
#renew<-read.csv('C:/Users/email/Documents/540_R/renew1.csv')
renew<-read.csv('/Users/Lux/Desktop/540/Renew1.csv')
#Delete the columns that are unnecessary - Data available from 1990 until 2012 only
renew<-renew[,c(2,35:57)]
#Select NPL(Base country) , CHN and IND 
renew <- subset(renew, renew$Country.Code == "CHN"|renew$Country.Code == "IND"|renew$Country.Code == "NPL")
#Transpose Data
class(renew)
head(renew)
renew1<-(t(renew))
class(renew1) # its in matrix format
renew<-as.data.frame(renew1)
colnames(renew)<-c("China","India","Nepal")
str(renew)
renew =renew[-1,]
# creating a vector for years
Year<-1990:2012
#Adding the years to renew table
renew<-cbind(Year,renew)
# Converting the columns to numeric
#renew$df=as.numeric(as.character(renew$df[1:nrow(renew)]))
renew$China=as.numeric(as.character(renew$China[1:nrow(renew)]))
renew$India=as.numeric(as.character(renew$India[1:nrow(renew)]))
renew$Nepal=as.numeric(as.character(renew$Nepal[1:nrow(renew)]))
summary(renew)
#Mean of Aggregrate country China and India from 1990 until 2012
renew$Mean <- rowMeans(subset(renew, select = c(China, India)), na.rm = TRUE)
#Min between China and India
renew$Min <-with(renew,pmin(renew$China,renew$India))
#Max between China and India 
renew$Max <-with(renew,pmax(renew$China,renew$India))
# Graph1 - Matplot for Nepal and Mean of India and China
par(mar=c(6, 6, 6, 6), xpd=TRUE)
matplot(renew[,1],renew[c("Nepal","Mean")],type='b',col=1:2,pch=1:2,xlab="Year",ylab = "Ren Energy Consumption %",main="Base Country Vs Aggregate Ren Energy ",xlim = c(1990,2012),ylim =c(10,150),lwd=2,cex.axis=1) 
legend("topleft",legend=c("Nepal","Mean IND CHN"),lty=1,pch=1:2,col = 1:2)

# Graph 2 - Matplot for Nepal and Min and Max of India and China
par(mar=c(5, 5, 5, 5), xpd=TRUE)
matplot(renew[,1],renew[c("Nepal","Min","Max")],type='b',col=1:3,pch=1:3,xlab="Year",ylab = "Ren Energy Consumption %",main="Base Country Vs Min , Max ",xlim = c(1990,2012),ylim =c(10,100),lwd=2,cex.axis=1) 
legend("bottomright",inset=c(-0.1,1),legend=c("Nepal","Min_IndCHN","Max_INDCHN"),lty=1,pch=1:3,col = 1:3)

#Graph - 3 Matplot for data of all 3 countries 
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
matplot(renew[,1],renew[,2:4],type='b',col=1:6,pch=21:23,xlab="Year",ylab = "Ren Energy Consumption %",main="Renewable Energy Consumption",xlim = c(1990,2012),ylim =c(10,100),lwd=2,cex.axis=1) 
legend("bottomright",inset=c(-0.1,1),legend=c("China","India","Nepal"),lty=1,pch=21:23,col = 1:6,lwd=2)


# Graph 4 -Matplot for Nepal as in worldbank website
par(mar=c(6, 6, 6, 6), xpd=TRUE)
yticks <- seq(1990, 2014, 2)
xticks<-seq(83,100,1)
matplot(renew[,1],renew[c("Nepal")],type='b',col=1,pch=1,xlab="Year",ylab = "Ren Energy Consumption %",main="Base Country Nepal ",lwd=2,cex.axis=1,axes= FALSE) 
axis(2, at = xticks,labels=xticks,las=2)
axis(side = 1, at = yticks,labels=yticks,las=2)
legend("bottomright",inset=c(-0.1,1),legend=c("Nepal"),lty=1,pch=1:3,col = 1:3)


