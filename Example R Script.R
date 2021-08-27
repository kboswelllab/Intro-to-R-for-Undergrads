# Denotes a comment in the script (not run)

#The swirl package offers step by step demonstrations of how to do many simple commands in R
install.packages('swirl') #installs the package 'swirl'
library(swirl) #opens the package 'swirl' in your R workspace

#Access help page to any function by typing ? in front of the function, such as
?plot



#Define your workspace
getwd() #displays the current working directory
setwd('C:/Users/allwhite/Documents/R/') #sets the working directory to specified folder. Be sure to update the file path to a folder on your machine.


#Import data into R
data<-read.csv('sharks.csv') #open a .csv file with your data. This must be in the folder that you set as your working directory
head(data) #prints the first few rows of your data
attach(data) #identifies each column heading as a variable object in R

#OR enter it directly into R
dive<-c(1:10)
site<-c(1:5,1:5)
sharks<-c(0,2,1,5,3,4,7,10,8,5) #Creates list of values named 'sharks'
system<-c('OC','OC','OC','OC','OC','CCR','CCR','CCR','CCR','CCR',) #Creates list of values named 'CCR'
data<-data.frame(sharks,system,dive,site) #Creates a table with columns 'sharks' and 'system'
head(data) #prints the first few rows of your data


#summarize data and find simple statistics
summary(data) #prints min, max, and mean of each varialbe in dataframe "data"
mean(data$sharks) #calculates mean number of sharks observed in "data"
sd(data$sharks) #calculates standard deviation of sharks observed in "data"



###Plot data###
plot(data$site, data$sharks,main="Sharks per Site",xlab='Site',ylab='Sharks',type='l',col=1) #a simple scatterplot of the number of sharks seen at each site with title and axis labels
#Plot is messy, we want a separate line for OC v. CCR. So we should subset the data first, then plot

#subset data
OC<-subset(data,data$system=='OC') #groups all rows where the value in column headed "system" is 'OC'
CCR<-subset(data,data$system=='CCR') #groups all rows where the value in column headed "system" is 'CCR'


#plot OC and CCR shark sightings as separate lines
plot(OC$site,OC$sharks,type='l',col=2,ylim=c(0,11),xlab='Site',ylab='Sharks',main='Sharks per Site on OC v. CCR',lwd=2,xlim=c(1,5)) #Creates a red line plot of sharks per site on OC and creates y space to add CCR
lines(CCR$site,CCR$sharks,type='l',col=4,lwd=2) #adds a blue line of sharks per site on CCR to the plot above
legend('topright',col=c(2,4),lty=c(1,1),lwd=c(2,2),c('OC','CCR')) #creates a legend for the graph in the top right corner


#Make a barplot with error bars
barplot(height = c(mean(OC$sharks),mean(CCR$sharks)),cex.axis=1.5,cex.lab=1.5,cex.main=1,ylab='Sharks Observed',main='Mean Shark Counts on Open and Closed Circuit',space=0,ylim = c(0,10),angle=c(0,45),density=c(0,1.5),col=1,xaxt='n',las=2) #makes a bar plot with custom axis text size and fill
axis(side=1,at=c(0.50,1.50),labels=c("OC","CCR"),cex.axis=1.5) #creates a custom x-axis
segments(x0=0.50,y0=mean(OC$sharks),x1=0.50,y1=(mean(OC$sharks)+sd(OC$sharks))) #draws an error bar from the mean to the positive standard deviation
segments(x0=1.50,y0=mean(CCR$sharks),x1=1.50,y1=(mean(CCR$sharks)+sd(CCR$sharks)))
segments(x0=0.50,y0=mean(OC$sharks),x1=0.50,y1=(mean(OC$sharks)-sd(OC$sharks))) #draws an error bar from the mean to the negative standard deviation
segments(x0=1.50,y0=mean(CCR$sharks),x1=1.50,y1=(mean(CCR$sharks)-sd(CCR$sharks)))
segments(x0=0.45,y0=(mean(OC$sharks)+sd(OC$sharks)),x1=0.55) #caps the error bar with a short horizontal line
segments(x0=1.45,y0=(mean(CCR$sharks)+sd(CCR$sharks)),x1=1.55)
segments(x0=0.45,y0=(mean(OC$sharks)-sd(OC$sharks)),x1=0.55) #caps the error bar with a short horizontal line
segments(x0=1.45,y0=(mean(CCR$sharks)-sd(CCR$sharks)),x1=1.55)



###Analyze Data###

#Question 1: Is the number of sharks sighted affected by the system used by the diver?
#We want to compare shark sightings between two categories, so we can utilize a two-sample t-test
shark.t.test<-t.test(sharks~system,data=data) #performs a two-sample t-test to compare the mean number of sharks sighted when using each system in the dataset "data"
shark.t.test #prints the significant statistics of the t-test
#To test our null hypothesis that sharks are not affected by the system used by divers, we need to observe the p-value. If this value is less than 0.05, we reject the null hypothesis. If it's larger, we fail to reject and state that there was a significant difference in the number of sharks sighted using OC v. CCR.


#Now lets say we went diving a third time at each site and added those observations using the code below.
data[c(11:15),1]<-c(11:15)
data[c(11:15),2]<-c(1:5)
data[c(11:15),3]<-'OC'
data[c(11:15),4]<-c(3,5,12,4,2)

data
  
#Question 2: Is the number of sharks sighted different among the sites surveyed by the divers?
#We want to compare shark sightings between more than two categories, so we need an analysis of variance (ANOVA).
#plot the mean, min, max, 1st and 3rd quartiles of number of sharks seen at each site
boxplot(sharks~site,data=data)
shark.anova<-aov(sharks~site,data=data) #performs an anova of sharks by site in the dataset "data"
summary(shark.anova) #prints the significant statistics of the anova. Pr(>F) is our p-value, which we interpret the same as we would a t-test. Since this p-value is >0.05, we fail to reject our null hypothesis and state that there is no significant difference in the number of sharks sighted at each location.

#Now we want to know if the presence of sharks was affected by differences in the time of each dive. We'll add the durations of each dive using the code below.
data$time<-c(10,15,12,18,15,17,22,26,23,20,16,14,20,11,8)
data

#Question 3: Is there a relationship between the number of sharks observed and the time spent on each dive?
#We want to observe the relationship between two continuous variables, so we can use a linear model.
#plot the dive time v. number of sharks seen
plot(sharks~time,data=data,xlab='dive time')

shark.lm<-lm(sharks~time,data=data) #fits a linear model to the relationship between sharks and dive time in the dataset "data"
summary(shark.lm) #pints the result of the linear model. Like the t-test and anova, the p-value will tell us whether there is a significant relationship between dive time and number of sharks spotted. The adjusted r-squared value represents the fraction of variance in the data that's explained by the fitted linear model. An r-squared of 1 would mean that 100% of the variance is explained, whereas an r-squared of 0 would mean that non of the variance is explained.

shark.pred<-predict(shark.lm) #predict the trend between the number of sharks sighted and dive time
lines(data$time,shark.pred,col=2) #add a red line of the linear model prediction to your plot




