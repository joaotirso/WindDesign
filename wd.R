library(caret)
#library(car)
library(DBI)
library(RPostgreSQL)
set.seed(3456)
dat = read.csv(file="WindDesign/10minwindcorrected.csv",head=TRUE,sep=",");
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='grid',
                 user='postgres', password='joxman33')



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

#WRITE
insertDB <- function(id) {
  d = read.csv(file=paste("WindDesign/Dataset/",id,".csv",sep=""),head=TRUE,sep=",");
 
  for(i in 1:nrow(d)) {
    query = paste("INSERT INTO values VALUES (",id,",",d$windspeed[i], ",",d$ratedpoweroutput[i] ,","
              ,d$SCORE[i] ,",",d$CorrectedScore[i],");",sep="");
    dbSendQuery(con,query);

  }
  print("Done");
#  dbDisconnect(con);
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

selectdata<- function(id) {
  query = paste("SELECT * FROM values WHERE towerid = ",id,sep="");
  values <- dbGetQuery(con,query);
  return (values);
}

builddataset <-function(id) {
  values <- getIds(id);
  dataset <- selectdata(values[1]);
  for(i in 2:5) {
    if(!is.na(values[i])) {
      dataset <- cbind(dataset,selectdata(values[i]));
    }
  } 
  return (dataset);
}
#powerouput <- lm(dat3$ratedpoweroutput~dat$AvgWindSpeed + dat$AvgWindDirection,data=dat3)
#layout(matrix(c(1,2,3,4),2,2))
#plot(powerouput)

