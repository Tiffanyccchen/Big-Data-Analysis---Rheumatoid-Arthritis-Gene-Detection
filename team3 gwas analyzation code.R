###資料探索-------------------------------------------------------------
require(data.table)
library(ggplot2)
dir <- getwd()
data<-fread(dir + "/data/narac.csv",header=T,sep=",",na.strings=c("?_?","NA"))

##分割檔案----
fwrite(data[,1:9],"/data/datafirst9.csv")
fwrite(data[,10:50],"/data/datars.csv")
fwrite(data[,544928:545089],"/data/datamito.csv")
fwrite(data[:,10:100010], "data1.csv")
fwrite(data[,100010:200010],"data2.csv")
fwrite(data[,200010:300010],"data3.csv")
fwrite(data[,300010:400010],"data4.csv")
fwrite(data[,400010:500010],"data5.csv")
fwrite(data[,500010:544928],"data6.csv")

##資料探索----

#讀檔案
data<-fread(dir + "/data/naracvarsplit/datafirst9.csv",header=T,sep=",")
attach(data)

#了解變數名稱及類型
dim(data)
names<-names(data[,1:9])
names1to10<-names[1:10]
view1<-data[15:20,4]
view1

affection<-data[,2]
sex<-data[,3]
Drb1_1<-data[,4]
Drb1_2<-data[,5]
SENum<-data[,6]
SEStatus<-data[,7]
AntiCCP<-data[,8]

#得到樣本有無染病統計表
table(affection)
prop.table(table(affection))
#得到樣本性別統計表
table(sex)
prop.table(table(sex))

#得到樣本性別與染病交叉表
crosstablesexaffection<-table(data$Sex,data$Affection)
prop.table(crosstablesexaffection, margin = 1)
oddsratio<-(0.5706631*0.3989455)/(0.4293369*0.6010545)

#得到Drb1_1統計表、長條圖
summary(Drb1_1)
table(Drb1_1)

reorder_size <- function(x) {
        factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(data, aes(x = reorder_size(data$DRB1_2))) +
        geom_bar() +
        xlab("DRB1_2") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        geom_text(stat='count',aes(label=..count..),vjust=-1)
geom_text(stat='bin',aes(label=..count..),vjust=-1)

#SENum, SEStatus 與染病交叉表
summary(SENum)
table(SENum)
summary(SEStatus)
table(SEStatus)

crosstablesexaffection<-table(data$SENum,data$Affection)
prop.table(crosstablesexaffection, margin = 1)
oddsratio<-(0.5706631*0.3989455)/(0.4293369*0.6010545)

crosstablesexaffection<-table(data$SEStatus,data$Affection)
prop.table(crosstablesexaffection, margin = 1)
oddsratio<-(0.5706631*0.3989455)/(0.4293369*0.6010545)

#AntiCCP
summary(AntiCCP)
table(AntiCCP)
ls(AntiCCP)
data$AntiCCP<-as.numeric(as.character(AntiCCP))
ggplot(data,aes(x=AntiCCP)) + geom_histogram()+
geom_text(stat='count',aes(label=..count..),vjust=-1)

###1st anayzation------------------------------------------------------------------------------------------
##Read Data----
data1<-fread(dir + "/data/data1.csv",header=T,sep=",",na.strings="?_?")
data2<-fread(dir + "/data/data2.csv",header=T,sep=",",na.strings="?_?")
data3<-fread(dir + "/data/data3.csv",header=T,sep=",",na.strings="?_?")
data4<-fread(dir + "/data/data4.csv",header=T,sep=",",na.strings="?_?")
data5<-fread(dir + "/data/data5.csv",header=T,sep=",",na.strings="?_?")
data6<-fread(dir + "/data/data6.csv",header=T,sep=",",na.strings="?_?")
datafirst9<-fread(dir + "/data/datafirst9.csv",header=T,sep=",",na.strings="?")
datamitoselect<-fread(dir + "/data/datamito.csv",header=T,sep=",",na.strings="?_?")
datatest<-fread(dir + "/datatest.csv",header=T,sep=",")


##刪除missing超過1#snps----
dim(data1)
a1<-which((colSums(is.na(data1[,1:99991]))/2062)>0.01)
length(a1)
a1[1:10]
colSums(is.na(data1[,90000:91000]))/2062
data1<-subset(data1,select=-c(a1))
dim(data1)

dim(data2)
a2<-which((colSums(is.na(data2[,1:100000]))/2062)>0.01)
length(a2)
a2[1:10]
colSums(is.na(data2[,90000:91000]))/2062
data2<-subset(data2,select=-c(a2))
dim(data2)

dim(data3)
a3<-which((colSums(is.na(data3[,1:100000]))/2062)>0.01)
length(a3)
a3[1:10]
colSums(is.na(data3[,90000:91000]))/2062
data3<-subset(data3,select=-c(a3))
dim(data3)

dim(data4)
a4<-which((colSums(is.na(data4[,1:100000]))/2062)>0.01)
length(a4)
a4[1:10]
colSums(is.na(data4[,90000:91000]))/2062
data4<-subset(data4,select=-c(a4))
dim(data4)

dim(data5)
a5<-which((colSums(is.na(data5[,1:100000]))/2062)>0.01)
length(a5)
a5[1:10]
colSums(is.na(data5[,90000:91000]))/2062
data5<-subset(data5,select=-c(a5))
dim(data5)

dim(data6)
a6<-which((colSums(is.na(data6[,1:44927]))/2062)>0.01)
length(a6)
a6[1:10]
colSums(is.na(data6[,90000:91000]))/2062
data6<-subset(data6,select=-c(a6))
dim(data6)

##刪除變異<5%snps----

#得到眾數函數
getmode <- function(v) {
   uniqv <- unique(v)
   a<-uniqv[which.max(tabulate(match(v, uniqv)))]
length(which(v==a))
}

#toyexample
charv <- c("o","it","the","it","it")
length(which(charv=="it"))
length(charv)
result <- getmode(charv)
print(result)

variation<-apply(data1,2,getmode)
length(variation)
b1<-which(variation/2062>0.95)
data1<-subset(data1,select=-c(b1))
dim(data1)

variation2<-apply(data2,2,getmode)
length(variation2)
b2<-which(variation2/2062>0.95)
length(b2)
data2<-subset(data2,select=-c(b2))
dim(data2)

variation3<-apply(data3,2,getmode)
length(variation3)
b3<-which(variation3/2062>0.95)
length(b3)
data3<-subset(data3,select=-c(b3))
dim(data3)

variation4<-apply(data4,2,getmode)
length(variation4)
b4<-which(variation4/2062>0.95)
length(b4)
data4<-subset(data4,select=-c(b4))
dim(data4)

variation5<-apply(data5,2,getmode)
length(variation5)
b5<-which(variation5/2062>0.95)
length(b5)
data5<-subset(data5,select=-c(b5))
dim(data5)

variation6<-apply(data6,2,getmode)
length(variation6)
b6<-which(variation6/2062>0.95)
length(b6)
data6<-subset(data6,select=-c(b6))
dim(data6)

##輸出----
data1snp<-fwrite(data1,"/data/data1.csv")
dim(datasnp1)
data2snp<-fwrite(data2,"/data/data2.csv")
dim(datasnp2)
data3snp<-fwrite(data3,"/data/data3.csv")
dim(datasnp3)
data4snp<-fwrite(data4,"/data/data4.csv")
dim(datasnp4)
data5snp<-fwrite(data5,"/data/data5.csv")
dim(datasnp5)
data6snp<-fwrite(data6,"/data/data6.csv")
dim(datasnp6)

78461+78692+78539+78120+84224+35077+75+1

###2nd analyzation-----------------------------------------------------------------------------
##split data(10000snps/each)----
for(i in 1:43){
       select_idxs <- c(sample(433188-10000*(i-1)))
       aa<-dataselectmerge[,select_idxs,10000))]
       write.csv(aa, file = '/2nd analyze/slice/' + paste0('snp.', i, '.csv'))
       dataselectmerge<-dataselectmerge[,-select_idxs,10000))]
}
dim(dataselectmerge)
write.csv(dataselectmerge, file = '/2nd analyze/slice/' + paste0('snp.', 44, '.csv'))

##read 44 10000snps dataset----
# set working directory
dir<-getwd()
setwd(dir)
rm(list=ls())
 
# list all csv files from the current directory
list.files(pattern="/2nd analyze/slice/.csv$") # use the pattern argument to define a common pattern  for import files with regex. Here: .csv
 
# create a list from these files
list.filenames<-list.files(pattern="/2nd analyze/slice/.csv$")
 
# create an empty list that will serve as a container to receive the incoming files
list.data<-list()
 
# create a loop to read in your data
require(data.table)
for (i in 1:length(list.filenames)){
gc()
list.data[[i]]<-fread(list.filenames[i])
}
gc()

## analyze----
for(i in 1:44){
       gc()
       df1<-list.data[i]
       df1<-as.data.frame(df1)
       df1<-df1[,-c(1)]

       df1<-cbind(datafirst9$Affection,df1)
       df1[] <- lapply(df1, factor) # the "[]" keeps the dataframe structure
       col_names <- names(df1)
       colnames(df1)[1] <- "Affection"
       assign(paste("df",i,sep=""),df1)
       gc()

       # start small, and scale up slowly
       a<-lapply( paste('df', i, sep=''), get )
       a<-data.frame(a)
       gc()
       rf=randomForest(Affection ~ . , data =a,importane = T,proximity = T,ntree=1000)
       assign(paste("rf",i,sep=""), rf)

       nam4 <- paste("rn", i, sep = "")
       assign(nam4,round(importance(rf), 2)[order(round(importance(rf), 2)[,1], decreasing=TRUE),][1:100])
       gc()

       dd<-sapply( paste0('rn', i, sep=''), get )
       b<-names(dd[,1])
       b<-as.vector(b)
       a<-subset(a, select = b)
       dim(a)
       gc()
       write.csv(a, file=paste0(i,'sel' , '.csv'))
}

## plot----
gc()
sample(1:44,4) 

plot(rf7,main="sample.1 10000 snps set")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 

plot(rf7,log="y",main="sample.1 10000 snps set")

par(mar=c(5,4,4,0)) #No margin on the right side
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf7$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(rf7,type=2,main="Variable Importance sample.1 10000 snps")

## read 44 files of first 100 snps----
# set working directory
setwd(getwd())
 
# list all csv files from the current directory
list.files(pattern="/2nd analyze/slice/.csv$") # use the pattern argument to define a common pattern  for import files with regex. Here: .csv
 
# create a list from these files
list.filenames<-list.files(pattern="/2nd analyze/slice/.csv$")
list.filenames
 
# create an empty list that will serve as a container to receive the incoming files
list.data<-list()
 
# create a loop to read in your data
for (i in 1:length(list.filenames)){
       list.data[[i]]<-fread(list.filenames[i])
}
summary(list.data)

## convert into dataframe----
for(i in 1:44){
       gc()
       df<-list.data[i]
       df<-as.data.frame(df)
       assign(paste("df",i,sep=""),df)
}
dim(df1)

## merge data----
select<-as.matrix(rep(1,2062),ncol=1)
select<-data.frame(select)

dim(select)
for(i in 1:44){
       a<-lapply( paste('df', i, sep=''), get )
       a<-data.frame(a)
       a<-a[,-1]
       select<-cbind(select,a)
}
select<-select[,-1]
dim(select)
select<-data.frame(select)
str(select)

gc()

## random forest analyze----
require(randomForest)
rf=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=200,proximity = T,ntree=1000)
rf

## plot----
plot(rf,main="selected snps set")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf, log="y",main="selected snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf,type=2,main="Variable Importance selectedsnps")

## choose variable num.plot----
result <- replicate(50, rfcv(selectsnps[,-c(1)],selectsnps[,1], 
cv.fold=5, scale="log", step=0.5, recursive=FALSE), simplify=FALSE)
error.cv <- sapply(result, "[[", "error.cv")

matplot(result[[1]]$n.var,rowMeans(error.cv), type="o",
lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
xlab="Number of snps", ylab="CV Error",main="select vars/wrapper var. selection",xaxt="n",ylim=c(0,0.4))
axis(side=1, at=c(4387,2194,1097,548,274,137,69,34,17,9,4,2,1))

result[[50]]$error.cv
cvselect<-rep(1,13)
for(i in 1:50){
       cvselect<-cbind(cvselect,result[[i]]$error.cv)
}
dim(cvselect)
cvselect<-data.frame(cvselect)
cvselect<-cvselect[,-c(1)]
write.csv(cvselect,"/2nd analyze//slice/cvselectrow.csv")
cv.select<-fread(dir + "/2nd analyze//slice/cvselect.csv",header=T,sep=",")
cv.select[,1]

cv.selectrow<-fread(dir + "/2nd analyze//slice/cvselectrow.csv",header=T,sep=",")
try<- rev(cv.selectrow)

## Boxplot of cv----
selectvarplot<-boxplot(try, main="snps select by cv.error /50 times each var.num.(wrapper method)", 
  	xlab="Number of snps", ylab="Error Rate",ylim=c(0.2,0.35))

## select first 137 snps plot----
rf=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=1000,proximity = T,ntree=5000)

plot(rf,main="")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf, log="y",main="selected 4400 snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf,type=2,main="Variable Importance selected 4400 snps set")

rn <- round(importance(rf), 2)
snpss<-rn[order(rn[,1], decreasing=T),][1:137]
str(snpss)
rn[1]
name137<-names(snpss)
name137<-as.vector(name137)
str(name137)
select137<-selectsnps[ , which(names(selectsnps) %in% name137)]
select137<-setDF(select137)
dim(select137)
write.csv(select137,"/2nd analyze/slice/select137.csv")

##RF,TREE 137 snps----
#train data
train<-sample(1:2062,1650)
train<-as.vector(train)
str(train)

select137<-cbind(datafirst9$Affection,select137)
select137[] <- lapply(select137, factor) # the "[]" keeps the dataframe structure
col_names <- names(select137)
colnames(select137)[1] <- "Affection"
str(select137)
 
select137train<-select137[c(train),]
dim(select137train)
select137train<-as.vector(
select137train[] <- lapply( select137train, factor) # the "[]" keeps the dataframe structure
col_names <- names(select137train)
colnames(select137train)[1] <- "Affection"
select137test<-select137[-c(train),]

rf137=randomForest(Affection ~ . , data =select137,subset=train,importane = T,do.trace=1000,proximity = T,ntree=5000)
rf137

plot(rf137,main="select 137 snps set")

layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf137, log="y",main="select 137 snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf137$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf137,type=2,main="Variable Importance selected 137 snps set")
prediction <- predict(rf137,select137test)      

#Prediction Table
table<-table(select137test$Affection, prediction, dnn = c('Actual Group','Predicted Group'))
select137test$rightPred <- prediction ==select137test$Affection   
accuracy <- sum(select137test$rightPred)/nrow(select137test)
diag(prop.table(table, 1))

## Decision tree----
install.packages("tree")
library(tree)
require(rpart)

tree<-tree(Affection~.,select137train)

summary(tree)
plot(tree)
text(tree,pretty=0)

tree.1p<-predict(tree,select137test,type="class")

#Prediction Table
table<-table(select137test$Affection,tree.1p, dnn = c('Actual Group','Predicted Group'))
select137test$rightPred <- prediction ==select137test$Affection   
accuracy <- sum(select137test$rightPred)/nrow(select137test)
diag(prop.table(table, 1))
sum(diag(table))/sum(table)

## compare 137 snps at random----
sample<-sample(10:433197,137)
random<-dataselectmerge[,c(sample(10:433197,137))]
random137<-cbind(datafirst9$Affection,random)
random137[] <- lapply(random137, factor) # the "[]" keeps the dataframe structure
col_names <- names(random137)
colnames(random137)[1] <- "Affection"
rfrandom=randomForest(Affection ~ . , data =random137,importane = T,do.trace=1000,proximity = T,ntree=5000)
rfrandom

plot(rfranodm,main="select 137 random snps set")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rfrandom, log="y",main="select 137 random snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfrandom$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rfrandom,type=2,main="Variable Importance selected 137 random snps set")

prediction <- predict(rfrandom,select137test)      

# Prediction Table
table<-table(select137test$Affection, prediction, dnn = c('Actual Group','Predicted Group'))
select137test$rightPred <- prediction ==select137test$Affection   
accuracy <- sum(select137test$rightPred)/nrow(select137test)
diag(prop.table(table, 1))

###3rd analyzation--------------------------------------------------------

##(1)recode snps----
# read 44 10000snps dataset
rm(list=ls())
require(data.table)
library(SNPassoc)

# set working directory
setwd(getwd())

# list all csv files from the current directory
list.files(pattern="/2nd analyze/.csv$") # use the pattern argument to define a common pattern  for import files with regex. Here: .csv
 
# create a list from these files
list.filenames<-list.files(pattern=".csv$")
 
# create an empty list that will serve as a container to receive the incoming files
list.data<-list()
 
# create a loop to read in your data
for (i in 1:length(list.filenames)){
       gc()
       list.data[[i]]<-fread(list.filenames[i])
}
gc()

##analyze----
for(i in 1:44){
       gc()
       sel<-list.data[i]
       sel<-as.data.frame(sel)
       sel<-sel[,-c(1)]
       is.na(sel) <- sel==""
       sel<-cbind(datafirst9$Affection,sel)

       sel[] <- lapply(sel, factor) # the [] keeps the dataframe structure
       col_names <- names(sel)
       colnames(sel)[1] <- "Affection"

       sel[] <- lapply(sel, gsub, pattern = "A_G", replacement = "AG", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "G_A", replacement = "AG", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "G_G", replacement = "GG", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "A_A", replacement = "AA", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "C_T", replacement = "CT", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "T_C", replacement = "TC", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "C_C", replacement = "CC", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "T_T", replacement = "TT", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "A_C", replacement = "AC", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "C_A", replacement = "AC", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "T_G", replacement = "GT", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "G_T", replacement = "GT", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "A_T", replacement = "AT", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "T_A", replacement = "AT", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "C_G", replacement = "CG", fixed = TRUE)
       sel[] <- lapply(sel, gsub, pattern = "G_C", replacement = "CG", fixed = TRUE)

       # Prior the transformation you should exchange 00 or --- to NA with something lik
       sel[ sel == 00 ] <- NA
       sel[ sel == "--" ] <- NA

       # Use the additive function and apply to create a numerical variable.
       mydatnum <-apply(sel,2,additive)
       write.csv(mydatnum, file=paste0('snptran', i, '.csv'))
}

rm(list=ls())
gc()

# set working directory
setwd(getwd())
require(data.table)
 
# list all csv files from the current directory
list.files(pattern="/3rdanalyze/slice/.csv$") # use the pattern argument to define a common pattern  for import files with regex. Here: .csv
 
# create a list from these files
list.filenames<-list.files(pattern="/3rdanalyze/slice/.csv$")
 
# create an empty list that will serve as a container to receive the incoming files
list.data<-list()
 
# create a loop to read in your data
for (i in 1:length(list.filenames)){
       gc()
       list.data[[i]]<-fread(list.filenames[i])
}
gc()

##convert into dataframe----
gc()
library(Amelia)
library(mice)
library(ggplot2)
library(lattice)
library(dplyr)
require(randomForest)

for(i in 10:44){
       gc()
       df1<-list.data[i]
       df1<-as.data.frame(df1)
       df1<-df1[,-c(1,2)]

       ##analyze----
       df1<-cbind(datafirst9$Affection,df1)
       df1[] <- lapply( df1, factor) # the "[]" keeps the dataframe structure
       col_names <- names(df1)
       colnames(df1)[1] <- "Affection"
       assign(paste("df",i,sep=""),df1)
       gc()

       # start small, and scale up slowly
       a<-lapply( paste('df', i, sep=''), get )
       a<-data.frame(a)

       gc()
       rf=randomForest(Affection ~ . , data =df1,importane = T,proximity = T,ntree=1000, na.action=na.roughfix)
       assign(paste("rf",i,sep=""), rf)

       nam4 <- paste("rn", i, sep = "")
       assign(nam4,
       round(importance(rf), 2)[order(round(importance(rf), 2)[,1], decreasing=TRUE),][1:100])

       gc()
       dd<-sapply( paste0('rn', i, sep=''), get )
       b<-names(dd[,1])
       b<-as.vector(b)
       a<-subset(a, select=b)
       dim(a)
       gc()
       write.csv(a, file=paste0(i,'seltran' , '.csv'))
}

##plot----
gc()
plot(rf4,main="sample.4 10000 snps set")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 

plot(rf4,log="y",main="sample.4 10000 snps set")
par(mar=c(5,4,4,0)) #No margin on the right side
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf4$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf4,type=2,main="Variable Importance sample.4 10000 snps")

# my path
setwd(getwd())
datafirst9<-fread(dir + "data/naracvarsplit/datafirst9.csv",header=T,sep=",",na.strings="?")
 
# list all csv files from the current directory
list.files(pattern="/3rdanalyze/slice/.csv$") # use the pattern argument to define a common pattern  for import files with regex. Here: .csv
 
# create a list from these files
list.filenames<-list.files(pattern="/3rdanalyze/slice/.csv$")
 
# create an empty list that will serve as a container to receive the incoming files
list.data<-list()
 
# create a loop to read in your data
require(data.table)
for (i in 1:length(list.filenames)){
       gc()
       list.data[[i]]<-fread(list.filenames[i])
}

gc()

##convert into dataframe----
for(i in 1:44){
       gc()
       df<-list.data[i]
       df<-as.data.frame(df)
       assign(paste("df",i,sep=""),df)
}
dim(df1)

##merge data----
select<-as.matrix(rep(1,2062),ncol=1)
select<-data.frame(select)
dim(select)
for(i in 1:44){
       a<-lapply( paste('df', i, sep=''), get )
       a<-data.frame(a)
       a<-a[,-c(1)]
       select<-cbind(select,a)
}
select<-select[,-1]
dim(select)
select<-data.frame(select)
str(select)

##random forest analyze----
select<-fread(dir + "3rdanalyze/recode/selectsnpstran.csv",header=T,sep=",",na.strings=c("?_?","NA"))
selectsnps<-cbind(datafirst9$Affection,select)
str(selectsnps)
selectsnps<-selectsnps[,-c(2)]
selectsnps[] <- lapply( selectsnps, factor) # the "[]" keeps the dataframe structure
col_names <- names(selectsnps)
colnames(selectsnps)[1] <- "Affection"
gc()

rf=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=1000,proximity = T,ntree=1000,na.action=na.roughfix)
rf

##plot----
plot(rf,main="selected 4400(recode) snps set")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf, log="y",main="selected 4400(recode)snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf,type=2,main="Variable Importance selected 4400 snps")

##rf(origin)compare----
selectori<-fread(dir + "2ndanalyze/slice/selectsnps.csv",header=T,sep=",",na.strings=c("","NA"))
selectori<-cbind(datafirst9$Affection,selectori)
selectori[] <- lapply( selectori, factor) # the "[]" keeps the dataframe structure
col_names <- names(selectori)
colnames(selectori)[1] <- "Affection"
str(selectori)
selectori<-selectori[,-c(2)]

rfori=randomForest(Affection ~ . , data =selectori,importane = T,do.trace=1000,proximity = T,ntree=1000,na.action=na.roughfix)
rfori

##4400 recode----
(21.29+21.44+21.05+20.85+21)/5
(17.42+17.42+16.92+16.42+16.4)/5
(26.61+26.96+26.73+26.96+27.3)/5

##4400 origin----
(21.52+21+21.44+21.34+21.53)/5
(16.92+16.33+17.34+17+17.8)/5
(28+27.42+27.07+27.3+26.7)/5

plot(rfori,main="select 4400 snps set")

layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rfori, log="y",main="select 4400 snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfori$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rfori,type=2,main="Variable Importance selected snps set")

gc()

## choose variable num.plot----
time.temp = proc.time()
result <- replicate(50, rfcv(selectsnps[,-c(1)],selectsnps[,1], 
cv.fold=5, scale="log", step=0.5, recursive=FALSE), simplify=FALSE)
error.cv <- sapply(result, "[[", "error.cv")
proc.time()	- time.temp

matplot(result[[1]]$n.var,rowMeans(error.cv), type="o",
lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
xlab="Number of snps", ylab="CV Error",main="select vars/wrapper var. selection",xaxt="n",ylim=c(0,0.4))
axis(side=1, at=c(4400,2200,1100,550,275,137,69,34,17,9,4,2,1))

result[[50]]$error.cv
cvselect<-rep(1,13)
for(i in 1:50){
       cvselect<-cbind(cvselect,result[[i]]$error.cv)
}
dim(cvselect)
cvselect<-data.frame(cvselect)
cvselect<-cvselect[,-c(1)]
cvselect<-t(cvselect)
write.csv(cvselect,"/3rdanalyze/recode/cvselecttran.csv")
cvselect<-fread(dir+ "/3rdanalyze/recode/cvselecttran.csv",header=T,sep=",")
cvselect[,1]

## Boxplot of cv----
selectvarplot<-boxplot(cvselect, main="snps select by cv.error(recode)/50 times each var.num.(wrapper method)", 
xlab="Number of snps", ylab="Error Rate",ylim=c(0.2,0.35))
axis(side=1, at=c(1,2,4,9,17,34,69,137,275,550,1100,2200,4400))

##choose m----
resultp/2 <- rfcv(selectsnps[,-c(1)],selectsnps[,1], 
cv.fold=5, scale="log", step=0.5,mtry=function(p)(p/2) ,recursive=FALSE)
matplot(result.p/2[[1]]$n.var,cbind(error.cv,cv.slect[,1]), type="o",
lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
xlab="Number of snps", ylab="CV Error",main="select vars/wrapper var. selection",xaxt="n",ylim=c(0,0.4))

##select first 137 snps plot----
rf=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=1000,proximity = T,ntree=5000)
rf

plot(rf,main="")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf, log="y",main="selected 4400 snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf,type=2,main="Variable Importance selected 4400 snps set")

rn <- round(importance(rf), 2)
snpss<-rn[order(rn[,1], decreasing=T),][1:137]
str(snpss)
rn[1]
name137<-names(snpss)
name137<-as.vector(name137)
str(name137)
select137<-selectsnps[ , which(names(selectsnps) %in% name137)]
select137<-setDF(select137)
dim(select137)
write.csv(select137,"select137.csv")

## rf137----
select137<-fread(dir + "/3rdanalyze/recode/select137tran.csv",header=T,sep=",",na.strings=c("?_?","NA"))
select137<-cbind(datafirst9$Affection,select137)
select137[] <- lapply( select137, factor) # the "[]" keeps the dataframe structure
 col_names <- names(select137)
colnames(select137)[1] <- "Affection"
select137<-select137[,-c(2)]
str(select137)
 
rf137=randomForest(Affection ~ . , data =select137,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rf137

plot(rf137,main="select 137 snps(recode) set")

layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf137, log="y",main="select 137 snps(recode) set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf137$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf137,type=2,main="Variable Importance selected 137 snps (recode) set")

## rf(origin)compare-----
select137ori<-fread(dir + "/3rdanalyze/recode/select137tran.csv",header=T,sep=",",na.strings=c("","NA"))
select137ori<-cbind(datafirst9$Affection,select137ori)
select137ori[] <- lapply( select137ori, factor) # the "[]" keeps the dataframe structure
col_names <- names(select137ori)
colnames(select137ori)[1] <- "Affection"
str(select137ori)
select137ori<-select137ori[,-c(2,3)]

rf137ori=randomForest(Affection ~ . , data =select137ori,do.trace=5000,importane = T,proximity = T,ntree=5000,na.action=na.roughfix)
rf137ori

plot(rf137ori,main="select 137 snps set")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf137ori, log="y",main="select 137 snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf137ori$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf137ori,type=2,main="Variable Importance selected 137 snps set")

select137<-fread("/3rdanalyze/recode/select137tran.csv",header=T,sep=",",na.strings="?_?")
str(select137)
dim(select137)
setDT(select137)

for (jj in 1:ncol(select137)) set(select137, i = which(select137[[jj]]=="2"), j = jj, v = 1)
setDF(select137)
str(select137)
select137<-select137[,-1]

selectsnps<-cbind(datafirst9$Affection,select137)
selectsnps[] <- lapply( selectsnps, factor) # the "[]" keeps the dataframe structure
col_names <- names(selectsnps)
colnames(selectsnps)[1] <- "Affection"

gc()

##dominant and recessice model----
select137<-fread("/3rdanalyze/recode/select137tran.csv",header=T,sep=",",na.strings="?_?")
str(select137)
dim(select137)
setDT(select137)

for (jj in 1:ncol(select137)) set(select137, i = which(select137[[jj]]=="0"), j = jj, v = 1)
setDF(select137)
str(select137)
select137<-select137[,-1]
selectsnps<-cbind(datafirst9$Affection,select137)
selectsnps[] <- lapply( selectsnps, factor) # the "[]" keeps the dataframe structure
col_names <- names(selectsnps)
colnames(selectsnps)[1] <- "Affection"

rfdominante=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfdominante1=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfdominante2=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfdominante3=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfdominante4=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)

select4400<-fread("C://Users/user/Desktop/selectsnpstran.csv",header=T,sep=",",na.strings="?_?")
str(select4400)
dim(select4400)
setDT(select4400)
for (jj in 1:ncol(select4400)) set(select4400, i = which(select4400[[jj]]=="2"), j = jj, v = 1)
setDF(select4400)
str(select4400)
select4400<-select4400[,-1]
selectsnps<-cbind(datafirst9$Affection,select4400)
selectsnps[] <- lapply( selectsnps, factor) # the "[]" keeps the dataframe structure
col_names <- names(selectsnps)
colnames(selectsnps)[1] <- "Affection"

rfrecessive=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfrecessive1=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfrecessive2=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfrecessive3=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)
rfrecessive4=randomForest(Affection ~ . , data =selectsnps,importane = T,do.trace=5000,proximity = T,ntree=5000,na.action=na.roughfix)

gc()

##(2.)cluster----
require(ranger)
require(data.table)
dataselectmerge<-fread("/home/team3/dataselectmerge.csv",header=T,sep=",")

a<-which(names(dataselectmerge)=="Affection1")
dataselectmerge<-subset(dataselectmerge,select=-a)
dim(dataselectmerge)
dataselectmerge<-dataselectmerge[,-c(1,3,4,5,6,7,8,9)]
sel<-dataselectmerge

sel[] <- lapply( sel, factor) # the "[]" keeps the dataframe structure
col_names <- names(sel)
colnames(sel)[1] <- "Affection"

rf=ranger(data=sel,num.trees = 5000,mtry=5000,importance="impurity",write.forest=TRUE,probability=FALSE,min.node.size=1,replace=F,dependent.variable.name="Affection")
rf
rf$confusion.matrix
rf$prediction.error

##selectfinalvars----
rn<-round(rf$variable.importance,2)
dim(rn)
str(rn)
rn<-data.frame(as.list(rn))
rn.ranger=sort(rn, decreasing=T)[1:10000]
str(rn.ranger)
dim(rn.ranger)
names.sel=names(rn.ranger)
selectranger<-subset(dataselectmerge,select=c(which(names(dataselectmerge) %in% names.sel)))

dim(dataselectmerge)
dim(selectranger)
write.csv(rn.ranger,"rnranger")
write.csv(names.sel,"namesel")
write.csv(selectranger,"selectranger")

data <- read.table(dir + "3rd analyze/cluster/cluname10000.txt",sep=",",header=T, col.names = c("num", "snp"))
a<-data[,2]
a<-as.vector(a)

require(data.table)
dataselectmerge<-fread(dir + "/data/dataselectmerge.csv",header=T,sep=",")
data10000<-subset(dataselectmerge,select=c(a))
dim(data10000)
data10000[,1]
write.csv(data10000,"/3rd analyze/cluster/clster10000.csv")

## analyze----
require(randomForest)
data <- read.csv(dir + "3rd analyze/clustr/clster10000.csv",sep=",",header=T,na.strings=c(""))
datafirst9<-fread(dir + "data/naracvarsplit/datafirst9.csv",header=T,sep=",")

data<-cbind(datafirst9$Affection,data)
data<-data[,-2]
data[] <- lapply( data, factor) # the "[]" keeps the dataframe structure
col_names <- names(data)
colnames(data)[1] <- "Affection"
str(data)

rf10000<-randomForest(Affection~.,data=data,importance=T,proximity=T,do.trace=200,ntree=1000,na.action=na.roughfix)

plot(rf10000,main="")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf10000, log="y",main="all selected 10000 snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf10000$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf10000,type=2,main="Variable Importance all selected 10000 snps set")

rn <- round(importance(rf10000), 2)
names<-dimnames(rn)[[1]][1:4400]
all4400<-data[ , which(names(data) %in% names)]
all4400<-setDF(all4400)
dim(all4400)
write.csv(all4400,"/3rd analyze/cluster/all4400.csv")

all4400<-cbind(datafirst9$Affection,all4400)
all4400[] <- lapply(all4400, factor) # the "[]" keeps the dataframe structure
col_names <- names(all4400)
colnames(all4400)[1] <- "Affection"
str(all4400)

rf4400<-randomForest(Affection~.,data=all4400,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)
rf4400.1<-randomForest(Affection~.,data=all4400,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)
rf4400.2<-randomForest(Affection~.,data=all4400,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)
rf4400.3<-randomForest(Affection~.,data=all4400,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)
rf4400.4<-randomForest(Affection~.,data=all4400,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)

plot(rf4400,main="")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf4400, log="y",main="all selected 4400 snps set")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf4400$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf4400,type=2,main="Variable Importance all selected 4400 snps set")

###Final analyzation-------------------------------------------------------------
##(1) cluster-additive,dominant-------------------------------------------------
require(ranger)
require(data.table)
dataselectmerge<-fread("/home/team3/dataselectmergetran.csv",header=T,sep=",")
datafirst9<-fread("/home/team3/datafirst9.csv",header=T,sep=",")

a<-which(names(dataselectmerge)=="Affection1")
dataselectmerge<-subset(dataselectmerge,select=-a)
dim(dataselectmerge)
dataselectmerge<-dataselectmerge[,-1]
sel<-cbind(datafirst9,dataselectmerge)

sel[] <- lapply( sel, factor) # the "[]" keeps the dataframe structure
col_names <- names(sel)
colnames(sel)[1] <- "Affection"

rf=ranger(data=sel,num.trees = 5000,mtry=5000,importance="impurity",write.forest=TRUE,probability=FALSE,min.node.size=1,replace=F,dependent.variable.name="Affection")
rf
rf$confusion.matrix
rf$prediction.error

## selectfinalvars----
rn<-round(rf$variable.importance,2)
dim(rn)
str(rn)
rn<-data.frame(as.list(rn))
rn.ranger=sort(rn, decreasing=T)[1:10000]
str(rn.ranger)
dim(rn.ranger)
names.sel=names(rn.ranger)
selectranger<-subset(dataselectmerge,select=c(which(names(dataselectmerge) %in% names.sel)))

dim(dataselectmerge)
dim(selectranger)
write.csv(rn.ranger,"rnranger.csv")
write.csv(names.sel,"namesel.csv")
write.csv(selectranger,"selectranger.csv")

## transfer all data to dominant coding----
dataselectmerge<-fread(dir + "final analyze/cluster additive recessive/dataselectmergetran.csv",header=T,sep=",")
require(data.table)
setDT(dataselectmerge)
for (jj in 1:ncol(dataselectmerge)) set(dataselectmerge, 
i = which(dataselectmerge[[jj]]=="0"), j = jj, v = 1)
setDF(dataselectmerge)

write.csv(dataselectmerge,dir + "final analyze/cluster additive recessive/datarecessive.csv")

## cluster additive recessive 4400 snps select----
require(data.table)

data <- read.table(dir + "final analyze/cluster additive recessive/datarecessive.csv",sep=",",header=T, col.names = c("num", "snp"))
a<-data[,2]
a<-a[1:4400]
a<-as.vector(a)

datarecessive<-fread(dir + "final analyze/cluster additive recessive/datarecessive.csv",header=T,sep=",")
datarecessive<-datarecessive[,-c(1,2)]
datare4400<-subset(datarecessive,select=c(a))
dim(datare4400)
str(datare4400)
names(datare4400)
write.csv(datare4400,"datare4400.csv")

## analyze----
require(randomForest)
data <- read.csv(dir + "final analyze/cluster additive recessive/datare4400.csv",sep=",",header=T)
datafirst9<-fread(dir + "/data/naracvarsplit/datafirst9.csv",header=T,sep=",")

data<-cbind(datafirst9$Affection,data)
str(data)
data<-data[,-2]
data[] <- lapply( data, factor) # the "[]" keeps the dataframe structure
col_names <- names(data)
colnames(data)[1] <- "Affection"
str(data)

rf4400<-randomForest(Affection~.,data=data,importance=T,proximity=T,do.trace=200,ntree=1000,na.action=na.roughfix)

plot(rf4400,main="")
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side

plot(rf4400, log="y",main="all selected 4400 recessive snps set")
par(mar=c(5,0,4,2)) #No margin on the left side

plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf4400$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(rf4400,type=2,main="Variable Importance all selected 4400 recessive snps set")

rf4400.1<-randomForest(Affection~.,data=data,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)
rf4400.2<-randomForest(Affection~.,data=data,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)
rf4400.3<-randomForest(Affection~.,data=data,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)
rf4400.4<-randomForest(Affection~.,data=data,importance=T,proximity=T,do.trace=1000,ntree=1000,na.action=na.roughfix)

(19.59+19.98+19.74+19.93+20.08)/5
(11.3+12.23+11.31+12.31+12.23)/5
(30.8+30.65+31.34+30.41+30.88)/5

###(2)detect interaction term----------------------------------------------------
## read 4400 snps of cluster 4400----
require(data.table)
cluster4400<-fread(dir + "/all4400.csv",header=T,sep=",",na.strings=c("","NA"))
datafirst9<-fread(dir + "/data/naracvarsplit/datafirst9.csv",header=T,sep=",",na.strings="?")

dim(cluster4400)
cluster4400<-cluster4400[,c(2:4401)]
str(cluster4400)

cluster4400<-cbind(datafirst9$Affection,cluster4400)
cluster4400[] <- lapply(cluster4400, factor) # the "[]" keeps the dataframe structure
col_names <- names(cluster4400)
colnames(cluster4400)[1] <- "Affection"

##plot tree to detect interaction term----
require(rpart)

treecluster4400 <- rpart(formula = Affection ~ ., data =cluster4400, method = "class")
plot(treecluster4400)
text(treecluster4400,pretty=0)
plotcp(treecluster4400)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data<-cluster4400
data<-as.data.frame(data)
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- Mode(data[,i])
}
str(data)

## verify interaction term by logistic model----
fitlogit<-glm(Affection~rs2395175*rs9275595+rs2395175*rs660895+rs9275595*rs2306420+rs660895*rs3129941+rs2306420*rs12758854+rs3129941*rs1376679+rs12758854*rs10817045+rs12758854*rs9369084+rs1376679*rs1561505,family=binomial(link=logit),data=data)
summary(fitlogit)
nothing <- glm(Affection~ 1,family=binomial(link=logit),data=data)
summary(nothing)

# forward, backward, stepwise selection
forwards <- step(nothing,scope=list(lower=formula(nothing),upper=formula(fitlogit)), direction="forward")
backwards <-step(fitlogit,trace=0)
stepwise<-step(nothing, list(lower=formula(nothing),upper=formula(fitlogit)),
direction="both",trace=0)

formula(forwards)
formula(backwards)
formula(stepwise)
summary(forwards)
summary(backwards)
summary(stepwise)

#Affection ~ rs2395175 + rs9275595 + rs660895 + rs2306420 + rs3129941 + 
    rs12758854 + rs1376679 + rs10817045 + rs9369084 + rs2395175:rs9275595 + 
    rs2395175:rs660895 + rs3129941:rs1376679 + rs12758854:rs9369084


#fitlogit1<-glm(Affection ~ rs2395175 + rs9275595 + rs660895 + rs2306420 + rs3129941 + 
    rs12758854 + rs1376679 + rs10817045 + rs9369084 + rs2395175:rs9275595 + 
    rs2395175:rs660895 + rs12758854:rs9369084,family=binomial(link=logit),data=data)





summary(fitlogit1)
table(data$rs2395175,data$rs660895)

## show plot----
library(sjPlot)

sjp.int(fitlogit1
, type = "eff", int.term = "rs2395175*rs660895",
moderatorValues = "meansd", swapPredictors = T)
table(data$rs2395175,data$rs660895)

sjp.int(fitlogit1
, type = "eff", int.term = "rs2395175*rs9275595",
moderatorValues = "meansd", swapPredictors = T)

sjp.int(fitlogit1
, type = "eff", int.term = "rs12758854:rs9369084",
moderatorValues = "meansd", swapPredictors = T)

###結論--------------------------------------------------------------
round(prop.table(table(data$rs2395175,data$Affection),1),2)
table(data$Affection,data$rs660895)
round(prop.table(table(data$rs660895,data$Affection),1),2)
table(data$Affection,data$rs6910071)
round(prop.table(table(data$rs6910071,data$Affection),1),2)
table(data$Affection,data$rs2395163)
round(prop.table(table(data$rs2395163,data$Affection),1),2)
table(data$Affection,data$rs3763309)
round(prop.table(table(data$rs3763309,data$Affection),1),2)