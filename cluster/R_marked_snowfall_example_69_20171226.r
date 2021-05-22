# Test snowfall in cluster 69
# Type command: rsh node03 
# Open R 

require(randomForest)
# generate a bigger data
size=1000

#First method with loop size
time.temp = proc.time()
set.seed(100)
sim.airquality=airquality
#for(i in 1:size){
   temp=airquality
   temp[,"Ozone"]=temp[,"Ozone"]+rnorm(length(temp[,"Ozone"]))
   sim.airquality=rbind(sim.airquality, temp)
}
proc.time()	- time.temp
#About 5 sec

# Second method to avoid big loop
time.temp = proc.time()
set.seed(100)
#sim.list=vector("list",6)
for(i in 1:6){sim.list[[i]]=rep(airquality[,i],size)}
sim.air=as.data.frame(do.call(cbind,sim.list))
names(sim.air)=names(airquality)
sim.air[,"Ozone"]=sim.air[,"Ozone"]+rnorm(length(sim.air[,"Ozone"]))
proc.time()	- time.temp
# About 0.05 sec

# Fit the random forest for maxnodes=40 and ntree=30
time.temp = proc.time()
set.seed(131)
#ozone.rf <- randomForest(Ozone ~ ., data=sim.air, mtry=3,
importance=TRUE, na.action=na.omit,maxnodes=40, ntree=30)
proc.time()	- time.temp
# About 5 sec

# Fit the random forest for maxnodes=40 and ntree=300
time.temp = proc.time()
set.seed(131)
#ozone.rf <- randomForest(Ozone ~ ., data=sim.air, mtry=3,
importance=TRUE, na.action=na.omit,maxnodes=40, ntree=300)
proc.time()	- time.temp
# About 44 sec

# Fit the random forest for maxnodes=100 and ntree=300
time.temp = proc.time()
set.seed(131)
#ozone.rf <- randomForest(Ozone ~ ., data=sim.air, mtry=3,
importance=TRUE, na.action=na.omit,maxnodes=100, ntree=300)
proc.time()	- time.temp
# About 60 sec

# Fit the random forest for mtry=2, maxnodes=100 and ntree=300
time.temp = proc.time()
set.seed(131)
#ozone.rf <- randomForest(Ozone ~ ., data=sim.air, mtry=2,
importance=TRUE, na.action=na.omit,maxnodes=100, ntree=300)
proc.time()	- time.temp
# About 49 sec

# work on a set of parameter vectors
rf.out.list=vector("list",8)
#parameter.list=list(p1.v=c(2,50,300), p2.v=c(2,50,500),
                    p3.v=c(2,100,300), p4.v=c(2,100,500),
                    p5.v=c(3,50,300), p6.v=c(3,50,500),
                    p7.v=c(3,100,300), p8.v=c(3,100,500))

# Try 2 parameter vectors
time.temp = proc.time()
#for(i in 1:length(parameter.list[1:2])){
  rf.out.list[[i]]=randomForest(Ozone ~ ., data=sim.air, mtry=parameter.list[[i]][1],
  importance=TRUE, na.action=na.omit,maxnodes=parameter.list[[i]][2], ntree=parameter.list[[i]][3])}
proc.time()	- time.temp
# About 150 sec
rf.out.list[1:2]

# Try 4 parameter vectors
time.temp = proc.time()
#for(i in 1:length(parameter.list[1:4])){
  rf.out.list[[i]]=randomForest(Ozone ~ ., data=sim.air, mtry=parameter.list[[i]][1],
  importance=TRUE, na.action=na.omit,maxnodes=parameter.list[[i]][2], ntree=parameter.list[[i]][3])}
proc.time()	- time.temp
# About 400 sec

# Try all parameter vectors
time.temp = proc.time()
#for(i in 1:length(parameter.list)){
  rf.out.list[[i]]=randomForest(Ozone ~ ., data=sim.air, mtry=parameter.list[[i]][1],
  importance=TRUE, na.action=na.omit,maxnodes=parameter.list[[i]][2], ntree=parameter.list[[i]][3])}
proc.time()	- time.temp
# About 930 sec

#===============================================================================
#===============================================================================
# parallel calculation for parameter selection
#set the host name
# For IP69
HostsNames1 = rep(paste0('node0',3) ,4) #node03, 4 threads
HostsNames2 = rep(paste0('node0',3) ,6) #node03, 6 threads

# For IP39
#HostsNames1 = rep(paste0('node0',5) ,4) #node06, 4 threads
#HostsNames2 = rep(paste0('node0',c(5,6)) ,2) #node05,2 threads, node06,2 threads


#parameter.list=list(p1.v=c(2,50,300), p2.v=c(2,50,500),
                    p3.v=c(2,100,300), p4.v=c(2,100,500),
                    p5.v=c(3,50,300), p6.v=c(3,50,500),
                    p7.v=c(3,100,300), p8.v=c(3,100,500))

parameter.list1=parameter.list[1:2]
parameter.list2=parameter.list[1:4]

require(snowfall)
require(randomForest)

# Generate a bigger data
time.temp = proc.time()
size=1000
set.seed(100)
sim.list=vector("list",6)
#for(i in 1:6){sim.list[[i]]=rep(airquality[,i],size)}
sim.air=as.data.frame(do.call(cbind,sim.list))
names(sim.air)=names(airquality)
sim.air[,"Ozone"]=sim.air[,"Ozone"]+rnorm(length(sim.air[,"Ozone"]))
proc.time()	- time.temp
# About 0.2 sec

# Test 2 parameter vectors
# start the parallel process
sfInit(parallel=TRUE, type="SOCK", socketHosts = HostsNames1)
sfExport("sim.air","parameter.list1")
time.temp = proc.time()
#rf.out.list1=sfLapply(1:length(parameter.list1), function(i){
        require(randomForest)
        out=randomForest(Ozone ~ ., data=sim.air, mtry=parameter.list1[[i]][1],
            importance=TRUE, na.action=na.omit,maxnodes=parameter.list1[[i]][2],
            ntree=parameter.list1[[i]][3])
		    return(out)})
names(rf.out.list1) = names(parameter.list1)
proc.time()	- time.temp
# about 100 sec
sfStop()
# stop the parallel process

rf.out.list1

# Test 4 parameter vectors
# start the parallel process
sfInit(parallel=TRUE, type="SOCK", socketHosts = HostsNames1)
sfExport("sim.air","parameter.list2")
time.temp = proc.time()
#rf.out.list2=sfLapply(1:length(parameter.list2), function(i){
        require(randomForest)
        out=randomForest(Ozone ~ ., data=sim.air, mtry=parameter.list2[[i]][1],
            importance=TRUE, na.action=na.omit,maxnodes=parameter.list2[[i]][2],
            ntree=parameter.list2[[i]][3])
		    return(out)})

names(rf.out.list2) = names(parameter.list2)
proc.time()	- time.temp
# about 186 sec
sfStop()

# Test 8 parameter vectors
# start the parallel process
sfInit(parallel=TRUE, type="SOCK", socketHosts = HostsNames3)
sfExport("sim.air","parameter.list")
time.temp = proc.time()
#rf.out.list2=sfLapply(1:length(parameter.list), function(i){
        require(randomForest)
        out=randomForest(Ozone ~ ., data=sim.air, mtry=parameter.list[[i]][1],
            importance=TRUE, na.action=na.omit,maxnodes=parameter.list[[i]][2],
            ntree=parameter.list[[i]][3])
		    return(out)})
names(rf.out.list2) = names(parameter.list)
proc.time()	- time.temp
# about 180 sec
sfStop()


#===============================================================================
#===============================================================================
# parallel calculation for cross validation
#set the host name
# For IP69
HostsNames1 = rep(paste0('node0',3) ,5) #node03, 5 threads
#HostsNames2 = rep(paste0('node0',3) ,6) #node03, 6 threads

# For IP39
#HostsNames1 = rep(paste0('node0',6) ,4) #node06, 4 threads
#HostsNames2 = rep(paste0('node0',c(5,6)) ,2) #node05,2 threads, node06,2 threads

# Five fold cross validation
sample.size=dim(sim.air)[1]
#index.v=sample(rep(1:5,sample.size/5),sample.size)

# Five fold cross validation for parameter vector  parameter.list1[[1]]
# start the parallel process
sfInit(parallel=TRUE, type="SOCK", socketHosts = HostsNames1)
sfExport("sim.air","parameter.list1","index.v")
time.temp = proc.time()
#rf.out.list1=sfLapply(1:5, function(i){
        require(randomForest)
        out=randomForest(Ozone ~ ., data=sim.air[index.v!=i,], mtry=parameter.list1[[1]][1],
            importance=TRUE, na.action=na.omit,maxnodes=parameter.list1[[1]][2],
            ntree=parameter.list1[[1]][3])
		    return(out)})
names(rf.out.list1) = names(parameter.list1)
proc.time()	- time.temp
# about 62 sec
sfStop()

rf.out.list1

#===============================================================================
#===============================================================================
# Save all results
#save.image("/data9/BDA/teamX/R_snowfall_example_69_20171227.RData")
# Bring back the results
#load("/data9/BDA/teamX/R_snowfall_example_69_20171227.RData") 
