library(caret)
#library(car)
library(performanceEstimation)
library(DBI)
library(e1071) 
library(RPostgreSQL)
set.seed(3456)
drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv, host='localhost', port='5432', dbname='grid',
              user='postgres', password='joxman33');


distance <- function(id1,id2) {
  r1 <- dbGetQuery(con,paste("select lat,long from neighbour where towerid =",id1));
  r2 <- dbGetQuery(con,paste("select lat,long from neighbour where towerid =",id2));
  return (sqrt((r1[1,1]-r2[1,1])^2 + (r1[1,2]-r2[1,2])^2))
}

vector <- function(idI,idF) {
  r1 <- dbGetQuery(con,paste("select lat,long from neighbour where towerid = ",idI));
  r2 <- dbGetQuery (con,paste("select lat,long from neighbour where towerid = ",idF));
  relpos = c((r2[1,1]-r1[1,1]),(r2[1,2]-r1[1,2]));
  return(relpos)
}


insertDB <- function(id) {
  d = read.csv(file=paste("WindDesign/Dataset/",id,".csv",sep=""),head=TRUE,sep=",");
 
  for(i in 1:nrow(d)) {
    query = paste("INSERT INTO values VALUES (",id,",",d$windspeed[i], ",",d$ratedpoweroutput[i] ,","
              ,d$SCORE[i] ,",",d$CorrectedScore[i],");",sep="");
    dbSendQuery(con,query);

  }
  print("Done");
}

insertmeteodata <- function() {
  dat = read.csv(file="WindDesign/10minwindcorrected.csv",head=TRUE,sep=",");
  
  for(i in 1:nrow(dat)) {
    query = paste("INSERT INTO meteodata VALUES (",dat$AvgWindSpeed[i],",",dat$AvgWindDirection[i],");",sep="");
    dbSendQuery(con,query);
    
  }
  print("Done");
}
posx <- function(id1,id2) {
  x1 <- getIds(id1)$long;
  x2 <- getIds(id2)$long;
  return(x2-x1);
}

radtodegrees <- function(rads) {
  return(rads*180/pi);
}

vectorialangle <- function(id1,id2) {
  zerovec <- c(1,0);
  vec <- vector(id1,id2);
  vecnorm <- sqrt(vec[1]^2 + vec[2]^2);
  zerovecnorm <- sqrt(zerovec[1]^2 + zerovec[2]);
  angle <- acos((zerovec%*%vec)/(vecnorm *zerovecnorm));
  if(posx(id1,id2)<0 && angle != 0){ 
    angle = - angle +2*pi;
  }
  return (radtodegrees(angle));
}

insertall <- function() {
  insertDB(11159);
  insertDB(11158);
  insertDB(11157);
  insertDB(11156);
  insertDB(11146);
  insertDB(11145);
}

getIds <- function(id) {
  ids = paste("SELECT * FROM neighbour WHERE towerid = ",id,sep="");
  res <- dbGetQuery(con,ids);
  return (res);
}
#edited
selectdata<- function(id) {
  query = paste("SELECT * FROM values WHERE towerid = ",id,sep="");
  values <- dbGetQuery(con,query);
# return (values[1:1000,]);
  return (values);
}

neighbournumber <- function(angle) {
  num = 0;
  if(45<=angle && angle<=135) {
    num = 3;
  } 
  else if(225<=angle && angle<=315){
    num = 4;
  }
  else if(135<angle && angle<225) {
    num = 2;
  }
  else if((0<=angle && angle<45) || (315<angle && angle<=360)) {
    num = 1;
  }
  return (num);
}

windangle <- function(neighbournum,winddir) {
  wangle = 0;
  if(neighbournum==1) {
    wangle = winddir;
  }
  else if(neighbournum==2){
    wangle = (winddir + 180)%%360;
  }
  else if(neighbournum==3){
    wangle = (winddir + 90)%%360;
  }
  else if(neighbournum==4) {
    wangle = (winddir+270)%%360;
  }
  return (wangle);
}
#edited
builddataset <-function(id) {
  values <- getIds(id);
  dataset <- selectdata(values[1]);
  meteodata <- dbGetQuery(con,"SELECT * FROM meteodata");
  winddir <- data.frame(avgwindspeed=numeric());
  size = nrow(meteodata);
  vars<- c("windspeed","ratedpower","score","correctedscore");
  dataset <- dataset[vars];
  
  
#  dataset <- dataset[1:1000,];
 # meteodata <- meteodata[1:1000,];
  
  id2 = 0;
  for(i in 2:5) {
    if(!is.na(values[i])) {
      id2 = values[i];
      sdata <- selectdata(id2);
      sdata <-sdata[vars];
      
  #   sdata <- sdata[1:1000,];
      
      names(sdata) =c(paste("windspeed",i-1,sep=""),paste("ratedpower",i-1,sep="")
                      ,paste("score",i-1,sep=""),paste("correctedscore",i-1,sep="")) ;
      dataset <- cbind(dataset,sdata);
      #1:nrow(meteodata)
      for(x in 1:nrow(meteodata)) {
        windd = meteodata$avgwinddirection[x];
        w = windangle(neighbournumber(vectorialangle(id,id2)),
                      meteodata$avgwinddirection[x]);
        winddir <- rbind(winddir,w);
      }
      names(winddir) = (paste("avgwinddirection",i-1,sep=""));
      dataset <- cbind(dataset,winddir);
      winddir <- data.frame(avgwindspeed=numeric());
    }else {
    d <- data.frame();
    d[,] <- NA
    dataset <-cbind(dataset,d);
    }
  }   
  
  dataset<-cbind(dataset,meteodata);
  return (dataset);
}

lreg <-function(dataset) {
 
  res <- performanceEstimation(
    PredTask(ratedpower~.,dataset,"poweroutputTask"),
    workflowVariants('timeseriesWF',#standardWF
                     learner=c('svm','lm')),
                   # learner.pars=list(cost=c(1,5,10),gamma=c(0.1,0.001))),
    CvSettings(nReps=1,nFolds=10));
  return (res);
}
#powerouput <- lm(dataset$ratedpower~dataset$avgwindspeed + dataset$avgwinddirection + dataset+,data=dataset);
#layout(matrix(c(1,2,3,4),2,2))
#plot(powerouput)

