dlock=201
rlock=191
swing=c("OH","CO","FL","IA","MI","NV","NH","VA","WI","NC","PA")
swingev=c(18,9,29,6,16,6,4,13,10,15,20)
swingdprob=c(61,50,30,62,85,69,63,46,68,21,79)/100

simlength=10000
swinglength=length(swing)

isim = rep(0,simlength)
csim=isim


for (i in 1:simlength) {
  corrdraw=runif(1,0,1)
  swingwin=rep(0,swinglength)
  for (j in 1:swinglength) if (corrdraw < swingdprob[j]) swingwin[j]=1
  csim[i]=sum(swingwin * swingev) + dlock
}

for (i in 1:simlength) {
  swingwin=rep(0,swinglength)
  for (j in 1:swinglength) if (runif(1,0,1) < swingdprob[j]) swingwin[j]=1
  isim[i]=sum(swingwin * swingev) + dlock
}

iprobd=0
cprobd=0
for(i in 1:simlength) if(isim[i]>269) iprobd=iprobd+1
for(i in 1:simlength) if(csim[i]>269) cprobd=cprobd+1

iprobd=iprobd/simlength
cprobd=cprobd/simlength

hist(isim)
hist(csim)

ids=read.csv("C:/Documents and Settings/cstumpo/My Documents/cms/pollsim/intrade.csv")
library("MASS")
uri="http://data.intrade.com/graphing/jsp/downloadClosingPrice.jsp?contractId="

nodatad=c(1,2,4,13,17,18,19,25,28,42,45,51) #sd(42) has a price, but no mvmnt
nodatar=c(8,9,21,33,40,46) #vt(46) has a price but no mvmt
rwintrade=sum(ids[nodatad,4])+0
dwintrade=sum(ids[nodatar,4])+0
simids=ids[-c(nodatad,nodatar),]
ds=length(simids[,1])+0

#probmatrix=matrix(rep(rep(0,90),ds), nrow=ds)
#get the data
for(i in 1:ds){ 
  pricehistd<-tail(read.table(paste(uri,simids[i,2], sep=""), header=TRUE, sep=","),90)[5][,1]
  ld=length(pricehistd)+0
  if(ld!=90) pricehistd=c(rep(0,90-ld),pricehistd)
  pricehistr<-tail(read.table(paste(uri,simids[i,3], sep=""), header=TRUE, sep=","),90)[5][,1]
  lr=length(pricehistr)+0
  if(lr!=90) pricehistr=c(rep(0,90-lr),pricehistr)
#file goes from old to new
  temp=pricehistd/(pricehistd+pricehistr)
  probhistd=if(i==1) temp else rbind(probhistd,temp)
}

#parameterize the simulation
returns=probhistd[,-1]
for(j in 1:length(returns[,j])) for(i in 1:length(returns[j,])) returns[j,i]=log(probhistd[j,i]/probhistd[j,i+1])
# just wipe out problematic returns involving 0 price
for (i in 1:length(returns[,1])) returns[i,is.nan(returns[i,])]<-0
for (i in 1:length(returns[,1])) returns[i,is.infinite(returns[i,])]<-0

rcov=returns[,1:length(returns[,1])]
for (i in 1:length(returns[,1])) for (j in 1:length(returns[,1])) rcov[i,j]=cov(returns[i,], returns[j,])

rcor=returns[,1:length(returns[,1])]
for (i in 1:length(returns[,1])) for (j in 1:length(returns[,1])) rcor[i,j]=cor(returns[i,], returns[j,])

volume=rep(0,ds)
for(i in 1:ds) volume[i]=length(which(returns[i,]!=0))

#do the simulation
timeleft=as.numeric(floor(difftime(as.POSIXlt("2012-11-06"),Sys.Date(), units = "days")))
simlength=1000
evd=rep(0,simlength)

for(n in 1:simlength) {
sim=returns[,1:timeleft]
for (i in 1:timeleft) sim[,i]=mvrnorm(1,rep(0,length(returns[,1])), rcov)
sim=exp(sim)
closeprob=as.numeric(probhistd[,90])
fprob=closeprob
for (j in 1:timeleft) fprob=pmin(as.numeric(fprob*sim[,j]),1)
evd[n]=sum(round(fprob) * simids[,4])+dwintrade  #should put in limited fallibility
}

#count the results
chance=0
for (i in 1:simlength) if (evd[i]>269) chance=chance+1
chance=chance/simlength

hist(evd)
expectation=mean(evd)
