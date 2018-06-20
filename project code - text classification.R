# first sort out the packages
#setwd("I:/20_newsgroups/20_newsgroups")
# install.packages('tm',dependencies=TRUE)
# install.packages('NLP',dependencies=TRUE)
# install.packages("rmarkdown")
# install.packages("tm.plugin.mail")
# install.packages("wordcloud")
# install.packages("plyr")
#install.packages("caTools")
#install.packages("RSNNS")
#install.packages("dplyr")
# memory.limit(size=6000)
library(tm)
library(tm.plugin.mail)
library(caTools)
library(RSNNS)
library(dplyr)

#hard code
#remove metadata
Remove_metadata <- function(x){
  require(stringr)
  x<-gsub(".*Lines:" ,"",x)
  x<-gsub("\n","",x)
}

#lets have a look at loading all the corpus
news  <- Corpus(DirSource(c("alt.atheism","comp.graphics", "comp.os.ms-windows.misc","comp.sys.ibm.pc.hardware","comp.sys.mac.hardware","comp.windows.x","misc.forsale","rec.autos","rec.motorcycles","rec.sport.baseball","rec.sport.hockey","sci.crypt","sci.electronics","sci.med","sci.space","soc.religion.christian","talk.politics.guns","talk.politics.mideast","talk.politics.misc","talk.religion.misc")))
#copy corpus
newsRaw <- news
#remove all content before (Lines:)
news<- tm_map(news, content_transformer(Remove_metadata))
news <- tm_map(news, tolower)
news <- tm_map(news, removeWords, c("year"))
# do some more pre-processing later...
# Make matrix of how often each word appears in each document
# aka a TDM or DMT (same but collumn switched with rows)
tdm <- DocumentTermMatrix(news,control=list(wordLengths=c(4, 15),stemming=TRUE,tolower=TRUE,removeNumbers = TRUE,removePunctuation = TRUE,stopwords=stopwords("english")))
inspect(tdm)
#Note that these are mostly boring words, because the document frequency is not considered.
#apply ft-idf to take care of that.
# compute the full matrix, then by selecting on only the colunms corresponding to one newsgroup we can look for meaningful classifier terms.
tfidf <- DocumentTermMatrix(news,control=list(weighting=weightTfIdf,wordLengths=c(4, 15),stemming=TRUE,tolower=TRUE,removeNumbers=TRUE,removePunctuation=TRUE,stopwords=stopwords("english")))
#there are 20 news groups/classes
numCase = 20
# delete words that do not appear in more than ~1/3 of all docs in one group (out of 20)
tfidf <- removeSparseTerms(tfidf,(1-0.3/numCase)) 
inspect(tfidf)
#data frame somehow for easier browsing
myFrame <- as.data.frame(as.matrix(tfidf))
#myFrame
# chop it into subjects
alt.atheism <- myFrame[1:1000,]
comp.graphics <- myFrame[1001:2000,]
comp.os.mswindows.misc <- myFrame[2001:3000,]
comp.sys.ibm.pc.hardware <- myFrame[3001:4000,]
comp.sys.mac.hardware <- myFrame[4001:5000,]
comp.windows.x <- myFrame[5001:6000,]
misc.forsale <- myFrame[6001:7000,]
rec.autos <- myFrame[7001:8000,]
rec.motorcycles <- myFrame[8001:9000,]
rec.sport.baseball <- myFrame[9001:10000,]
rec.sport.hockey <- myFrame[10001:11000,]
sci.crypt <- myFrame[11001:12000,]
sci.electronics <- myFrame[12001:13000,]
sci.med <- myFrame[13001:14000,]
sci.space <- myFrame[14001:15000,]
soc.religion.christian <- myFrame[15001:16000,]
talk.politics.guns <- myFrame[16001:17000,]
talk.politics.mideast <- myFrame[17001:18000,]
talk.politics.misc <- myFrame[18001:19000,]
talk.religion.misc <- myFrame[19001:19997,]
head(alt.atheism, n = 2)
# Those are full tfidf tables, we must find the important words
# R collapses the data.frame to a 1dimensional vector unless you tell it not to (and the word labels get dropped)
atheismMean <- data.frame(rowMeans(alt.atheism))
colnames(atheismMean) <- list('Atheism tfidf')
atheismOrder <- with(atheismMean, order(-atheismMean[[1]]))
atheismMean <- atheismMean[atheismOrder, , drop=FALSE] 
print(atheismMean)

spaceMean <- data.frame(rowMeans(sci.space))
colnames(spaceMean) <- list('Space tfidf')
spaceOrder <- with(spaceMean, order(-spaceMean[[1]]))
spaceMean <- spaceMean[spaceOrder, , drop=FALSE]
print(spaceMean)

baseballMean <- data.frame(rowMeans(rec.sport.baseball))
colnames(baseballMean) <- list('Baseball tfidf')
baseballOrder <- with(baseballMean, order(-baseballMean[[1]]))
baseballMean <- baseballMean[baseballOrder, , drop=FALSE]
print(baseballMean)

graphicsMean <- data.frame(rowMeans(comp.graphics))
colnames(graphicsMean) <- list('Graphics tfidf')
graphicsOrder <- with(graphicsMean, order(-graphicsMean[[1]]))
graphicsMean <- graphicsMean[graphicsOrder, , drop=FALSE]
print(graphicsMean)

mswindowsMean <- data.frame(rowMeans(comp.os.mswindows.misc))
colnames(mswindowsMean) <- list('Mswindows tfidf')
mswindowsOrder <- with(mswindowsMean, order(-mswindowsMean[[1]]))
mswindowsMean <- mswindowsMean[mswindowsOrder, , drop=FALSE]
print(mswindowsMean)

ibmMean <- data.frame(rowMeans(comp.sys.ibm.pc.hardware))
colnames(ibmMean) <- list('IBM tfidf')
ibmOrder <- with(ibmMean, order(-ibmMean[[1]]))
ibmMean <- ibmMean[ibmOrder, , drop=FALSE]
print(ibmMean)

macMean <- data.frame(rowMeans(comp.sys.mac.hardware))
colnames(macMean) <- list('MAC tfidf')
macOrder <- with(macMean, order(-macMean[[1]]))
macMean <- macMean[macOrder, , drop=FALSE]
print(macMean)

windowsMean <- data.frame(rowMeans(comp.windows.x))
colnames(windowsMean) <- list('Windows tfidf')
windowsOrder <- with(windowsMean, order(-windowsMean[[1]]))
windowsMean <- windowsMean[windowsOrder, , drop=FALSE]
print(windowsMean)

forsaleMean <- data.frame(rowMeans(misc.forsale))
colnames(forsaleMean) <- list('Forsale tfidf')
forsaleOrder <- with(forsaleMean, order(-forsaleMean[[1]]))
forsaleMean <- forsaleMean[forsaleOrder, , drop=FALSE]
print(forsaleMean)

autosMean <- data.frame(rowMeans(rec.autos))
colnames(autosMean) <- list('Autos tfidf')
autosOrder <- with(autosMean, order(-autosMean[[1]]))
autosMean <- autosMean[autosOrder, , drop=FALSE]
print(autosMean)

motorcyclesMean <- data.frame(rowMeans(rec.motorcycles))
colnames(motorcyclesMean) <- list('Motorcycles tfidf')
motorcyclesOrder <- with(motorcyclesMean, order(-motorcyclesMean[[1]]))
motorcyclesMean <- motorcyclesMean[motorcyclesOrder, , drop=FALSE]
print(motorcyclesMean)

hockeyMean <- data.frame(rowMeans(rec.sport.hockey))
colnames(hockeyMean) <- list('Hockey tfidf')
hockeyOrder <- with(hockeyMean, order(-hockeyMean[[1]]))
hockeyMean <- hockeyMean[hockeyOrder, , drop=FALSE]
print(hockeyMean)

cryptMean <- data.frame(rowMeans(sci.crypt))
colnames(cryptMean) <- list('Crypt tfidf')
cryptOrder <- with(cryptMean, order(-cryptMean[[1]]))
cryptMean <- cryptMean[cryptOrder, , drop=FALSE]
print(cryptMean)

electronicsMean <- data.frame(rowMeans(sci.electronics))
colnames(electronicsMean) <- list('Electronics tfidf')
electronicsOrder <- with(electronicsMean, order(-electronicsMean[[1]]))
electronicsMean <- electronicsMean[electronicsOrder, , drop=FALSE]
print(electronicsMean)

medMean <- data.frame(rowMeans(sci.med))
colnames(medMean) <- list('Med tfidf')
medOrder <- with(medMean, order(-medMean[[1]]))
medMean <- medMean[medOrder, , drop=FALSE]
print(medMean)

christianMean <- data.frame(rowMeans(soc.religion.christian))
colnames(christianMean) <- list('Christian tfidf')
christianOrder <- with(christianMean, order(-christianMean[[1]]))
christianMean <- christianMean[christianOrder, , drop=FALSE]
print(christianMean)

gunsMean <- data.frame(rowMeans(talk.politics.guns))
colnames(gunsMean) <- list('Guns tfidf')
gunsOrder <- with(gunsMean, order(-gunsMean[[1]]))
gunsMean <- gunsMean[gunsOrder, , drop=FALSE]
print(gunsMean)

mideastMean <- data.frame(rowMeans(talk.politics.mideast))
colnames(mideastMean) <- list('Mideast tfidf')
mideastOrder <- with(mideastMean, order(-mideastMean[[1]]))
mideastMean <- mideastMean[mideastOrder, , drop=FALSE]
print(mideastMean)

politicsMean <- data.frame(rowMeans(talk.politics.misc))
colnames(politicsMean) <- list('Politics tfidf')
politicsOrder <- with(politicsMean, order(-politicsMean[[1]]))
politicsMean <- politicsMean[politicsOrder, , drop=FALSE]
print(politicsMean)

religionMean <- data.frame(rowMeans(talk.religion.misc))
colnames(religionMean) <- list('Religion tfidf')
religionOrder <- with(religionMean, order(-religionMean[[1]]))
religionMean <- religionMean[religionOrder, , drop=FALSE]
print(religionMean)

#############################MLP classifier#####################################

trainValues <- myFrame
#add class column
trainValues["Class#"] <- 0
head(trainValues, n = 2)
#now we need to label 1-20 by newsgroup
for (a in 1:19997){
  trainValues[a,"Class#"] <-  floor(a / 1000) + 1
}
trainValues[1002,"Class#"]
dim(trainValues)
trainTargets <- decodeClassLabels(trainValues[,1037])

#sample to shuffle rows so when top 80% is selected for training it includes all classes not just
set.seed(42)
sample_n(trainValues,19997)

#split dataset into traing and test set
trainset <- splitForTrainingAndTest(trainValues, trainTargets, ratio=0.8)
trainset <- normTrainingAndTestSet(trainset)
model <- mlp(trainset$inputsTrain, trainset$targetsTrain, size=5, learnFuncParams=c(0.01), maxit=250, inputsTest=trainset$inputsTest, targetsTest=trainset$targetsTest)

predictTestSet <- predict(model,trainset$inputsTest)

confusionMatrix(trainset$targetsTrain,fitted.values(model))
confusionMatrix(trainset$targetsTest,predictTestSet)

par(mfrow=c(2,2))
plotIterativeError(model)
plotRegressionError(predictTestSet[,2], trainset$targetsTest[,2])
plotROC(fitted.values(model)[,2], trainset$targetsTrain[,2])
plotROC(predictTestSet[,2], trainset$targetsTest[,2])

#confusion matrix with 402040-method
confusionMatrix(trainset$targetsTrain, encodeClassLabels(fitted.values(model),method="402040", l=0.4, h=0.6))


#show detailed information of the model
summary(model)
model
weightMatrix(model)
extractNetInfo(model)







