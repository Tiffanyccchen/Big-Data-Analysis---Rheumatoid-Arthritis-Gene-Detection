# Test snowfall in cluster 69
# Type command: rsh node03


require(randomForest)
# generate a bigger data
size=1000


# Second method to avoid big loop
time.temp = proc.time()
set.seed(100)
sim.list=vector("list",6)
for(i in 1:6){sim.list[[i]]=rep(airquality[,i],size)}
sim.air=as.data.frame(do.call(cbind,sim.list))
names(sim.air)=names(airquality)
sim.air[,"Ozone"]=sim.air[,"Ozone"]+rnorm(length(sim.air[,"Ozone"]))
proc.time()	- time.temp
# About 0.05 sec

require(snowfall)

parameter.list=list(p1.v=c(2,50,300), p2.v=c(2,50,500),
                    p3.v=c(2,100,300), p4.v=c(2,100,500),
                    p5.v=c(3,50,300), p6.v=c(3,50,500),
                    p7.v=c(3,100,300), p8.v=c(3,100,500))
parameter.list1=parameter.list[1:2]
parameter.list2=parameter.list[1:4]

# For IP69
HostsNames1 = rep(paste0('node0',3) ,5) #node03, 5 threads
#HostsNames2 = rep(paste0('node0',3) ,6) #node03, 6 threads

# For IP39
#HostsNames1 = rep(paste0('node0',6) ,4) #node06, 4 threads
#HostsNames2 = rep(paste0('node0',c(5,6)) ,2) #node05,2 threads, node06,2 threads

# Five fold cross validation
sample.size=dim(sim.air)[1]
index.v=sample(rep(1:5,sample.size/5),sample.size)

# Five fold cross validation for parameter vector  parameter.list1[[1]]
# start the parallel process
sfInit(parallel=TRUE, type="SOCK", socketHosts = HostsNames1)
sfExport("sim.air","parameter.list1","index.v")
time.temp = proc.time()
rf.out.list1=sfLapply(1:5, function(i){
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

#save.image("/data9/BDA/teamX/R_snowfall_example_69_20171227.RData")