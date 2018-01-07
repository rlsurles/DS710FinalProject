#load csv files containing tweet dataframe without duplicates gathered in python
gunControl<-read.csv("/Users/becky/Documents/GitHub/ds710fall2017finalprojectproposal/FinalProject/Gun_Control_Total.csv", header=T)
gunRights<-read.csv("/Users/becky/Documents/GitHub/ds710fall2017finalprojectproposal/FinalProject/Gun_Rights_total.csv",header = T)

#filter out rows with missing data
gunControl<-Filter(function(x)!all(is.na(x)),gunControl)
gunControl[gunControl==""]<-NA
row.has.na<-apply(gunControl, 1,function(x){any(is.na(x))})
gunControl.filtered<-gunControl[!row.has.na,]
gunRights<-Filter(function(x)!all(is.na(x)),gunRights)
gunRights[gunRights==""]<-NA
row.has.na2<-apply(gunRights, 1,function(x){any(is.na(x))})
gunRights.filtered<-gunRights[!row.has.na2,]

#assign values to sentiment 
negGC<-gunControl.filtered$Sentiment_Analysis[gunControl.filtered$Sentiment_Analysis==-1]
negGR<-gunRights.filtered$Sentiment_Analysis[gunRights.filtered$Sentiment_Analysis==-1]
posGC<-gunControl.filtered$Sentiment_Analysis[gunControl.filtered$Sentiment_Analysis==1]
posGR<-gunRights.filtered$Sentiment_Analysis[gunRights.filtered$Sentiment_Analysis==1]
neuGC<-gunControl.filtered$Sentiment_Analysis[gunControl.filtered$Sentiment_Analysis==0]
neuGR<-gunRights.filtered$Sentiment_Analysis[gunRights.filtered$Sentiment_Analysis==0]

#find total number in the filtered data and then use table function to get the number of -1,0, and 1 in each
nGC<-length(gunControl.filtered$Sentiment_Analysis)
nGR<-length(gunRights.filtered$Sentiment_Analysis)
negGR<-table(negGR)
neuGR<-table(neuGR)
posGR<-table(posGR)
negGC<-table(negGC)
posGC<-table(posGC)
neuGC<-table(neuGC)

#do a proportion test of sentiments against each other. The values in GR and GC should add up to 1
prop.test(c(negGC,negGR), c(nGC,nGR))
prop.test(c(posGC,posGR), c(nGC, nGR))
prop.test(c(neuGC,neuGR), c(nGC,nGR))

#add proportion of pos values to the Pew dataset I gathered from http://www.people-press.org/2017/06/22/public-views-about-guns/#total

#load timeseries into R and make a graph of positive tweets of gun control and gun rights over time
opinionOverTime=read.csv("/Users/becky/Documents/GitHub/ds710fall2017finalprojectproposal/FinalProject/timeSeries.csv",header = T)
opinionOverTime$Date<-as.Date(opinionOverTime$Date,format="%m/%d/%y")
matplot(cbind(opinionOverTime$Date,opinionOverTime$Date),cbind(opinionOverTime$Total_Rights,opinionOverTime$Total_Control),type = "l",lty=c(1,2),ylab= "Percent", xlab="Time", main="Positive Public Opinion of Gun Rights and Gun Control over Time",lwd = 2, col = c("black","red"))
legend("topright",lty = c(1,2),col = c("black","red"),legend=c("Total Rights","Total Control"))


#histograms of sentiment analysis in tweets collected in December 2017
hist(gunControl.filtered$Sentiment_Analysis,breaks=c(-1.5,-.5,.5,1.5), freq = F, xlab = "Score", ylab = "Frequency", main = "Sentiment Analysis Scores for Tweets on Gun Control",col = "green")
hist(gunRights.filtered$Sentiment_Analysis,breaks=c(-1.5,-.5,.5,1.5), freq = F, xlab = "Score", ylab = "Frequency", main = "Sentiment Analysis Scores for Tweets on Gun Rights",col = "red")

#are my sentiments equally common within a group?
prop.test(x=opinionOverTime[27:28,2], n=rowSums(opinionOverTime[27:28,2:3]))
table(gunControl.filtered$Sentiment_Analysis)
chisq.test(gunControl.filtered$Sentiment_Analysis)
chisq.test(gunControl.filtered$Sentiment_Analysis,p=c(1/3,1/3,1/3))
table(gunControl$Sentiment_Analysis)
table(gunRights$Sentiment_Analysis)
sentimentCount=table((gunControl$Sentiment_Analysis))
sentimentCountControl=table(gunControl$Sentiment_Analysis)
sentimentCountRights=table(gunRights$Sentiment_Analysis)
chisq.test(sentimentCountControl)
chisq.test(sentimentCountControl,p=c(1/3,1/3,1/3))
chisq.test(sentimentCountRights,p=c(1/3,1/3,1/3))

#1-sample proportion test to compare last 2 timepoints in total rights time series
prop.test(32,83,0.48)
