setwd("C:/My Projects/Zheng - Poland FaceBook")
require(R.oo)
require(modeest)
require(RecordLinkage)
names<-read.csv("namefbapp2.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(names)
colnames(names)[1]<-"firstname"
#remove the duplicate data
names<-unique(names)
#normalize the name
names$firstname<-trim(tolower(names$firstname))
names$surname<-trim(tolower(names$surname))
names$appfirstname<-trim(tolower(names$appfirstname))
names$appsurname<-trim(tolower(names$appsurname))
colnames(names)
namespos$birthday<-as.Date(namespos$birthday,"%d/%m/%Y")
namespos$lastupdateday<-as.Date(namespos$lastupdateday)
namespos$appDate<-as.Date(namespos$appDate,"%d/%m/%Y")
namespos$appDOB<-as.Date(namespos$appDOB,"%d/%m/%Y")
format(as.numeric(namespos$id),scientific=FALSE)
namespos$idint<-as.numeric(namespos$id)
namespos$idint<-format(namespos$idint,scientific=FALSE)
summary(namespos$idint)
sum(is.na(namespos$appDate))

names$jarofirstname<-jarowinkler(names$firstname,names$appfirstname)
names$jarosurname<-jarowinkler(names$surname,names$appsurname)
names$namesim<-(names$jarofirstname+names$jarosurname)/2
Ecdf(names$namesim,datadensity='density')

friends<-read.csv("friends.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
head(friends)

linknoappid<-read.csv("linknumofappid.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(linknoappid)[1]<-"linknumber"

names$id<-format(names$id,scientific=FALSE)

for(i in seq(1:dim(names)[1]))
{
  print(names$ApplicationID[i])
  if(length(linknoappid$linknumber[linknoappid$ApplicationID==names$ApplicationID[i]])==0)
    names$linknumber[i]<- -1
  else
  names$linknumber[i]<-linknoappid$linknumber[linknoappid$ApplicationID==names$ApplicationID[i]]
}

namespos<-names[which(names$linknumber>-1),]
Ecdf(namespos$namesim,datadensity='density')

namesposnarm<-namespos[complete.cases(namespos),]
namesposnarm$daydiff<-namesposnarm$appDate-namesposnarm$lastupdateday
namesposnarm$daydiff<-as.numeric(namesposnarm$daydiff)
namesposnarm$appfirstname<-trim(tolower(namesposnarm$appfirstname))
namesposnarm$appsurname<-trim(tolower(namesposnarm$appsurname))
Ecdf(namesposnarm$daydiff,datadensity='density')
#store the file containing name similarity and link number into nameslinknumber.csv
write.csv(namespos,file="nameslinknumber.csv")
namesposnarm$dobdiff<-namesposnarm$birthday-namesposnarm$appDOB
Ecdf(namesposnarm$dobdiff,datadensity='density')

namesposnarm$jarofirstname<-jarowinkler(namesposnarm$firstname,namesposnarm$appfirstname)
namesposnarm$jarosurname<-jarowinkler(namesposnarm$surname,namesposnarm$appsurname)
namesposnarm$namesim<-(namesposnarm$jarofirstname+namesposnarm$jarosurname)/2
namesposnarm$bad<-ifelse(namesposnarm$samedob==1,0,1)

namespos$bad<-0
totalbad<- 2986
#morebad<-0;
links<-seq(1,141)
simslevel<-rep(0,141)
lastsim<-0
newsim<-0.0001
simstep<-0.0001
for(i in seq(1,141))
{
   namespos$bad<- ifelse(namespos$linknumber<=i,1,0)
   newsim<-0
#  morebad<-totalbad - sum(namespos$bad)
  while(sum(namespos$bad)<=totalbad)
  {
    lastsim<-newsim
    namespos$bad<-ifelse(namespos$linknumber<=i|namespos$namesim<=newsim,1,0)
    newsim<-newsim+simstep
  }
  simslevel[i]<-lastsim
}

totalbad<-2236
links<-seq(1,28)
simslevel<-rep(0,28)
lastsim<-0
newsim<-0.0001
simstep<-0.0001
for(i in seq(1,28))
{
  namesposnarm$bad<- ifelse(namesposnarm$linknumber<=i|namesposnarm$samedob==0,1,0)
  newsim<-0
  #  morebad<-totalbad - sum(namespos$bad)
  while(sum(namesposnarm$bad)<totalbad)
  {
    lastsim<-newsim
    namesposnarm$bad<-ifelse(namesposnarm$linknumber<=i|namesposnarm$namesim<=newsim|namesposnarm$samedob==0,1,0)
    newsim<-newsim+simstep
  }
  simslevel[i]<-lastsim
}
