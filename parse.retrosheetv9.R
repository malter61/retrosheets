##### parse retrosheet is a function written by Jim Albert, and published in his book
##### Exploring Baseball Data with R
##### The function reads and unzips the files from the retrosheets.org site to 
##### be used for play by play analysis.

parse.retrosheet2.pbp = function(season){
  # ADJUSTED FOR MAC -- function will work for WINDOWS and MAC
  # download, unzip, append retrosheet data
  # assume current directory has a folder C:/baseball
  # C:/baseball has two subfolders unzipped and zipped
  # program cwevent.exe is in unzipped folder (for windows)
  
  download.retrosheet <- function(season){
    # get zip file from retrosheet website
    download.file(
      url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep="")
      , destfile=paste("C:/baseball", "/zipped/", season, "eve.zip", sep="")
    )
  }
  unzip.retrosheet <- function(season){
    #unzip retrosheet files
    unzip(paste("C:/baseball", "/zipped/", season, "eve.zip", sep=""), 
          exdir=paste("C:/baseball", "/unzipped", sep=""))
  }
  create.csv.file=function(year){
    # http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent
    # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
    wd = getwd()
    setwd("C:/baseball/unzipped")
    if (.Platform$OS.type == "unix"){
      system(paste(paste("cwevent -y", year, "-f 0-96"), 
                   paste(year,"*.EV*",sep=""),
                   paste("> all", year, ".csv", sep="")))} else {
                     shell(paste(paste("cwevent -y", year, "-f 0-96"), 
                                 paste(year,"*.EV*",sep=""),
                                 paste("> all", year, ".csv", sep="")))              
                   }
    setwd(wd)
  }
  create.csv.roster = function(year){
    # creates a csv file of the rosters
    filenames <- list.files(path = "C:/baseball/unzipped/")
    filenames.roster = 
      subset(filenames, substr(filenames, 4, 11)==paste(year,".ROS",sep=""))
    read.csv2 = function(file)
      read.csv(paste("C:/baseball/unzipped/", file, sep=""),header=FALSE)
    R = do.call("rbind", lapply(filenames.roster, read.csv2))
    names(R)[1:6] = c("Player.ID", "Last.Name", "First.Name", 
                      "Bats", "Pitches", "Team")
    wd = getwd()
    setwd("C:/baseball/unzipped")
    write.csv(R, file=paste("roster", year, ".csv", sep=""))
    setwd(wd)
  }
  cleanup = function(){
    # removes retrosheet files not needed
    wd = getwd()
    setwd("C:/baseball/unzipped")
    if (.Platform$OS.type == "unix"){
      system("rm *.EVN")
      system("rm *.EVA")
      system("rm *.ROS")
      system("rm TEAM*")} else {
        shell("del *.EVN")
        shell("del *.EVA")
        shell("del *.ROS")
        shell("del TEAM*")
      }       
    setwd(wd)
    setwd("C:/baseball/zipped")
    if (.Platform$OS.type == "unix"){
      system("rm *.zip")} else {
        shell("del *.zip")
      }
    setwd(wd)
  }
  download.retrosheet(season)
  unzip.retrosheet(season)
  create.csv.file(season)
  create.csv.roster(season)
  cleanup()
}

download.retrosheet <- function(season){
  # get zip file from retrosheet website
  download.file(
    url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep="")
    , destfile=paste("C:/baseball", "/zipped/", season, "eve.zip", sep="")
  )
}
unzip.retrosheet <- function(season){
  #unzip retrosheet files
  unzip(paste("C:/baseball", "/zipped/", season, "eve.zip", sep=""), 
        exdir=paste("C:/baseball", "/unzipped", sep=""))
}

download.retrosheet(2014)
unzip.retrosheet(2014)


library(Lahman)
library(dplyr)
library(data.table)
####  read all teams' games and rbind then into one df
filenames <- list.files("C:/baseball/unzipped", pattern="*.EV", full.names=TRUE)
filenames <- filenames[grep('2014',filenames)]
count <- 0
for (file in filenames){
  team2014 <- read.csv(file,header=FALSE)
  if(count==0){all2014 <- team2014
  }else {all2014 <- rbind(all2014,team2014)}
  count <- count + 1
}


#### Take the unstructured data allxxxx (xxxx==year) and make a df suitable for R
all2014$V1 <- as.character(all2014$V1)
all2014$V2 <- as.character(all2014$V2)
all2014$V3 <- as.character(all2014$V3)
all2014$V4 <- c(all2014$V1[2:nrow(all2014)],"NA")
all2014$V5 <- c(all2014$V2[2:nrow(all2014)],"NA")
all2014$V6 <- c(all2014$V3[2:nrow(all2014)],"NA")
all2014$V7 <- c(all2014$V1[3:nrow(all2014)],"NA","NA")
all2014$V4 <- ifelse(all2014$V1 %in% c("id","version","info"),"",all2014$V4)
all2014$V5 <- ifelse(all2014$V1 %in% c("id","version","info","data"),"",all2014$V5)
all2014$V6 <- ifelse(all2014$V1 %in% c("id","version","info","data"),"",all2014$V6)
all2014$V7 <- ifelse(all2014$V1 %in% c("id","version","info","start","sub","data"),"",all2014$V7)
all2014 <- all2014[which(all2014$V1 %in% c("id","version","info","start","play","sub","data")),]



start <- Sys.time()
colnames(all2014) <- c('event','inn','half.inn','batter','count','p.seq','play')
all.2014 <- all2014[which(all2014$event=='play'),]
#!all.2014 <- all.2014[which(all.2014$play !='NP'),]
rows <- nrow(all.2014)
all.2014$inn <- as.numeric(all.2014$inn)
all.2014$index <- seq(1:nrow(all.2014))
all.2014$ones <- 1
all.2014$half.inn <- (as.numeric(all.2014$inn))*2 - abs(as.numeric(all.2014$half.inn)-1)
all.2014$half.inn.offset <- c('NA',all.2014$half.inn[1:(nrow(all.2014)-1)]) 
all.2014$new.half.inn <- ifelse(all.2014$half.inn.offset != all.2014$half.inn,1,0)
all.2014$new.half.inn[1]  <- 1
all.2014$X1B <- ifelse(substr(all.2014$play,1,1)=='S' & substr(all.2014$play,1,2) != 'SB',1,0)
all.2014$X2B <- ifelse(substr(all.2014$play,1,1)=='D',1,0)
all.2014$X3B <- ifelse(substr(all.2014$play,1,1)=='T',1,0)
all.2014$HR <- ifelse(substr(all.2014$play,1,2)=='HR',1,0)
all.2014$BB <- ifelse(grepl('BB',all.2014$play) | (grepl('W',all.2014$play) & !grepl('WP',all.2014$play)),1,0)
all.2014$SO <- ifelse(substr(all.2014$play,1,1)=='K',1,0)
all.2014$SH <- ifelse(grepl('SH',all.2014$play),1,0)
all.2014$SF <- ifelse(grepl('SF',all.2014$play),1,0)
all.2014$IW <- ifelse(grepl('IW',all.2014$play),1,0)
all.2014$first <- 0
all.2014$second <- 0
all.2014$third <- 0
all.2014$R <- 0
all.2014$outs <- 0
all.2014$team <- ifelse(all.2014$half.inn %% 2 == 1,'visitors','home')
all.2014$game.num <- 1
start <- Sys.time()
game.num <- 1
all.2014$runs.roi <- 0
all.2014$p.seq <- gsub('+','',all.2014$p.seq)
all.2014$p.seq <- gsub('*','',all.2014$p.seq)
all.2014$p.seq <- gsub('\\.','',all.2014$p.seq)
all.2014$p.seq <- gsub('>','',all.2014$p.seq)
all.2014$pitches.seen <- nchar(all.2014$p.seq)
all.2014$R <- ifelse(grepl('HR',all.2014$play),all.2014$R + 1,all.2014$R)
all.2014$R <- ifelse(grepl('1-H',all.2014$play),all.2014$R + 1,all.2014$R)
all.2014$R <- ifelse(grepl('2-H',all.2014$play),all.2014$R + 1,all.2014$R)
all.2014$R <- ifelse(grepl('3-H',all.2014$play),all.2014$R + 1,all.2014$R)


all.2014$vis.game.score <- ifelse(all.2014$team=='visitors',all.2014$R,0)
all.2014$home.game.score <- ifelse(all.2014$team=='home',all.2014$R,0)
all.2014$vis.game.score <- ifelse(all.2014$half.inn==1 & all.2014$new.half.inn==1,NA,all.2014$vis.game.score)
all.2014$home.game.score <- ifelse(all.2014$half.inn==1 & all.2014$new.half.inn==1,NA,all.2014$home.game.score)
all.2014$vis.game.score <- ave(all.2014$vis.game.score, cumsum(is.na(lag(all.2014$vis.game.score))), FUN = cumsum)
all.2014$home.game.score <- ave(all.2014$home.game.score, cumsum(is.na(lag(all.2014$home.game.score))), FUN = cumsum)
all.2014$run.diff <- abs(all.2014$vis.game.score-all.2014$home.game.score)
all.2014$run.diff <- ifelse(is.na(all.2014$run.diff),0,all.2014$run.diff)
all.2014$vis.game.score <- NULL
all.2014$home.game.score <- NULL

all.2014$vis.inn.score <- ifelse(all.2014$team=='visitors',all.2014$R,0)
all.2014$home.inn.score <- ifelse(all.2014$team=='home',all.2014$R,0)
all.2014$vis.inn.score <- ifelse(all.2014$new.half.inn==1,NA,all.2014$vis.inn.score)
all.2014$home.inn.score <- ifelse(all.2014$new.half.inn==1,NA,all.2014$home.inn.score)
all.2014$vis.inn.score <- ave(all.2014$vis.inn.score, cumsum(is.na(lag(all.2014$vis.inn.score))), FUN = cumsum)
all.2014$home.inn.score <- ave(all.2014$home.inn.score, cumsum(is.na(lag(all.2014$home.inn.score))), FUN = cumsum)
all.2014$vis.inn.score <- ifelse(is.na(all.2014$vis.inn.score),0,all.2014$vis.inn.score)
all.2014$home.inn.score <- ifelse(is.na(all.2014$home.inn.score),0,all.2014$home.inn.score)

all.2014$play.offset <- c('',all.2014$play[1:(rows-1)])
all.2014$outs <- 0
all.2014$outs <- ifelse(substr(all.2014$play.offset,1,1) %in% c('1','2','3','4','5','6','7','8','9') & substr(all.2014$play.offset,2,2) != 'E',1,all.2014$outs)
all.2014$outs <- ifelse((substr(all.2014$play.offset,1,2) %in% c('CS') | grepl('PO',all.2014$play.offset)) & !grepl('E',all.2014$play.offset),1,all.2014$outs)

all.2014$outs <- ifelse(substr(all.2014$play.offset,1,1)== 'K' & !grepl('B-1',all.2014$play.offset) & !grepl('B-2',all.2014$play.offset),
                        all.2014$outs + 1,all.2014$outs)

all.2014$outs <- ifelse(grepl('1X',all.2014$play.offset) & !grepl('E',all.2014$play.offset),all.2014$outs + 1,all.2014$outs)
all.2014$outs <- ifelse(grepl('2X',all.2014$play.offset) & !grepl('E',all.2014$play.offset),all.2014$outs + 1,all.2014$outs)
all.2014$outs <- ifelse(grepl('3X',all.2014$play.offset) & !grepl('E',all.2014$play.offset),all.2014$outs + 1,all.2014$outs)
all.2014$outs <- ifelse((substr(all.2014$play.offset,1,2) %in% c('FC') & grepl('X',all.2014$play.offset)) & !grepl('E',all.2014$play.offset),1,all.2014$outs)
all.2014$outs <- ifelse(grepl('DP',all.2014$play.offset),2,all.2014$outs)

all.2014$outs.offset <- all.2014$outs
all.2014$outs.offset <- ifelse(all.2014$new.half.inn==1,NA,all.2014$outs.offset)
all.2014$outs.offset <- ave(all.2014$outs.offset, cumsum(is.na(lag(all.2014$outs.offset))), FUN = cumsum)
all.2014$outs.offset <- ifelse(is.na(all.2014$outs.offset),0,all.2014$outs.offset)

print(table(all.2014$outs.offset))
all.2014$outs.offset <- ifelse(all.2014$outs.offset>3,3,all.2014$outs.offset)


#### I needed to read in every row sequentially since all action depends on what happened before the at bat.

for(i in 2:rows){
  if(all.2014$half.inn[i] == 1 & all.2014$new.half.inn[i] == 1) {game.num = game.num + 1}
  all.2014$game.num[i] <- game.num

  
  if(i%%10000 == 0){print(i)}
    
  if(((substr(all.2014$play[i-1],1,1) %in% c('S','W') & (substr(all.2014$play[i-1],1,2) != 'SB' & substr(all.2014$play[i-1],1,2) != 'WP')) |
       (substr(all.2014$play[i-1],1,2) %in% c('HP','BB','IW','1E','2E','3E','4E','5E','6E','7E','8E','9E','E1','E2','E3','E4','E5','E6','E7','E8','E9')) | 
        grepl('B-1',all.2014$play[i-1])) & !grepl('BX',all.2014$play[i-1])) 
            {all.2014$first[i]=1}
 
 if ((all.2014$first[i-1]==1) &
       (!grepl('1-',all.2014$play[i-1]) & !grepl('1X',all.2014$play[i-1]) & 
       !(grepl('CS2',all.2014$play[i-1]) & !grepl('E',all.2014$play[i-1])) & 
       !(grepl('PO1',all.2014$play[i-1]) & !grepl('E',all.2014$play[i-1])) &
       !grepl('SB2',all.2014$play[i-1])  & !(grepl('GDP',all.2014$play[i-1]) & grepl('(1)',all.2014$play[i-1])))
 )  {all.2014$first[i]=1}

if(grepl('B-2',all.2014$play[i-1]) | grepl('B-3',all.2014$play[i-1]) | grepl('B-H',all.2014$play[i-1])) {all.2014$first[i]=0} 

  if(((substr(all.2014$play[i-1],1,1) %in% c('D')  | grepl('SB2',all.2014$play[i-1])) & !grepl('BX',all.2014$play[i-1]))  | grepl('-2',all.2014$play[i-1]))  
  {all.2014$second[i]=1}

  if ((all.2014$second[i-1]==1) &
      (!grepl('2-',all.2014$play[i-1]) & !grepl('2X',all.2014$play[i-1]) & 
         !(grepl('CS3',all.2014$play[i-1]) & !grepl('E',all.2014$play[i-1])) & 
         !(grepl('PO2',all.2014$play[i-1]) & !grepl('E',all.2014$play[i-1])) &
         !grepl('SB3',all.2014$play[i-1])  & !(grepl('GDP',all.2014$play[i-1]) & grepl('(2)',all.2014$play[i-1])))
)  {all.2014$second[i]=1}

if(grepl('B-3',all.2014$play[i-1]) | grepl('B-H',all.2014$play[i-1])) {all.2014$second[i]=0}


  if(substr(all.2014$play[i-1],1,1) %in% c('T')  | grepl('SB3',all.2014$play[i-1])| grepl('-3',all.2014$play[i-1])) 
  {all.2014$third[i]=1}

  if ((all.2014$third[i-1]==1) &
      (!grepl('3-',all.2014$play[i-1]) & !grepl('3X',all.2014$play[i-1]) & 
         !(grepl('CSH',all.2014$play[i-1]) & !grepl('E',all.2014$play[i-1])) & 
         !(grepl('PO3',all.2014$play[i-1]) & !grepl('E',all.2014$play[i-1])) &
         !grepl('SBH',all.2014$play[i-1])  & !(grepl('GDP',all.2014$play[i-1]) & grepl('(3)',all.2014$play[i-1])))
)  {all.2014$third[i]=1}

if( grepl('B-H',all.2014$play[i-1])) {all.2014$third[i]=0}

if(all.2014$new.half.inn[i] == 1){all.2014$first[i]=0;all.2014$second[i]=0;all.2014$third[i]=0}

}

master <- read.csv('C:/baseball/Master.csv',header=TRUE,as.is=TRUE)
salaries <- read.csv('C:/baseball/Salaries.csv',header=TRUE,as.is=TRUE)
fielding <- read.csv('C:/baseball/Fielding.csv',header=TRUE,as.is=TRUE)
batting <- read.csv('C:/baseball/batting.csv',header=TRUE,as.is=TRUE)
batting <- batting[which(batting$yearID>2010),]   #### only need last four years

all.2014$GDP <- ifelse(grepl('GDP',all.2014$play),1,0)
all.2014$HBP <- ifelse(grepl('HP',all.2014$play),1,0)
all.2014$IBB <- ifelse(grepl('IW',all.2014$play),1,0)
all.2014$NR <-  (nchar(all.2014$play) - nchar(gsub('NR','',all.2014$play)))/2
all.2014$RBI <- all.2014$R - all.2014$NR
all.2014$year <- 2014
batting.2014 <- batting[which(batting$yearID==2014),]
league.AB <- sum(batting.2014$AB)
league.tb <- sum(batting.2014$H)+sum(batting.2014$X2B)+2*sum(batting.2014$X3B)+3*sum(batting.2014$HR)
league.obp <- (sum(batting.2014$H)+sum(batting.2014$BB)+sum(batting.2014$HBP))/(sum(batting.2014$AB)+sum(batting.2014$SF))
league.slg <- league.tb/league.AB

temp2014 <- all.2014

master <- master[,c(1,23,14,15,2,5,17:22)]
num.chars <- nchar(master$finalGame)
master$fg <- substr(master$finalGame,num.chars-3,num.chars)
master <- master[which(as.numeric(master$fg)>=2014),]
num.chars <- NULL
colnames(master)[2] <- 'batter'
all.2014 <- merge(all.2014,master,by='batter',all.x=TRUE)
all.2014 <- all.2014[order(all.2014$index),]

salaries <- salaries[,c(4,1,5)]
colnames(salaries)[1] <- 'playerID'
salaries <- merge(salaries,master[,c(1,2)],by='playerID',all.x=TRUE)
salaries[2,1] <- 'abrej003' ### to match Lahman 'Master' data

salaries <- salaries[which(salaries$yearID==2014),]
colnames(salaries)[4] <- 'batter'
all.2014 <- merge(all.2014,salaries,by='batter',all.x=TRUE)
all.2014 <- all.2014[order(all.2014$index),]

all.2014$salary <- ifelse(is.na(all.2014$salary),0,all.2014$salary)


fielding <- as.data.table(fielding)
fielding <- fielding[which(fielding$yearID==2014),]
fielding <- setkey(fielding,playerID,G)
fielding <- fielding[unique(playerID),mult='last']
fielding <- as.data.frame(fielding)
fielding <- fielding[,c(1,6)]
fielding <- merge(fielding,Master[,c(1,23)],by='playerID')
colnames(fielding) <- c('playerID','POS','batter')
all.2014 <- merge(all.2014,fielding,by='batter',all.x=TRUE)
all.2014 <- all.2014[order(all.2014$index),]


colnames(all.2014)[41] <- 'player.ID'

all.2014 <- all.2014[which(all.2014$outs.offset<3),]



runs.scored.half.inn <- aggregate(all.2014$R,list(half.inn=all.2014$half.inn,game.num=all.2014$game.num),sum)
colnames(runs.scored.half.inn) <- c('half.inn','game.num','inn.runs')
all.2014 <- merge(all.2014,runs.scored.half.inn,by=c('half.inn','game.num'))
#all.2014 <- all.2014[,c(1:61)]
#colnames(all.2014)[58] <- 'inn.runs'

all.2014 <- all.2014[order(all.2014$index),]
all.2014$runs.roi <- all.2014$inn.runs-all.2014$vis.inn.score-all.2014$home.inn.score+all.2014$R
all.2014$gte.one.run <- pmax(0,pmin(1,all.2014$runs.roi))

all.2014$outs.offset <- as.character(all.2014$outs.offset)
all.2014$state <- as.character(paste(all.2014$first,all.2014$second,all.2014$third,all.2014$outs.offset,sep=''))
runs <- aggregate(all.2014$runs.roi,by=list(all.2014$state),FUN=mean)  #### RUNS.ROI
colnames(runs) <- c('state','runs.potential')
one.run <- aggregate(all.2014$gte.one.run,by=list(all.2014$state),FUN=mean)  #### RUNS.ROI
colnames(one.run) <- c('state','gte.one.run')
all.2014 <- merge(runs,all.2014,by='state')
all.2014 <- all.2014[order(all.2014$index),]
all.2014 <- merge(one.run,all.2014,by='state')
all.2014 <- all.2014[order(all.2014$index),]
runs <- merge(runs,one.run,by='state')
runs$runs.potential <- round(runs$runs.potential,3)
runs$gte.one.run <- round(runs$gte.one.run,3)



all.2014$rp.offset <- c(all.2014$runs.potential[2:nrow(all.2014)],0)
all.2014$new.half.offset <- c(all.2014$new.half.inn[2:nrow(all.2014)],'NA')
all.2014$rp.offset <- ifelse(all.2014$new.half.offset == 1,0,all.2014$rp.offset)
all.2014$rp.offset[1]=all.2014$runs.potential[2]
all.2014$rp.offset <- as.numeric(format(round(all.2014$rp.offset,4),nsmall=4))
all.2014$runs.created <- all.2014$rp.offset - all.2014$runs.potential + all.2014$R

all.2014$runs.potential <- as.numeric(format(round(all.2014$runs.potential,4),nsmall=4))
all.2014$runs.created <- all.2014$rp.offset - all.2014$runs.potential + all.2014$R  
all.2014$runs.created <- as.numeric(format(round(all.2014$runs.created,4),nsmall=4))
colnames(all.2014)[2] <- 'gte.one.run'

temp2014.1 <- all.2014 

all.2014$age <- all.2014$year-all.2014$birthYear
all.2014$years.exp <- all.2014$year-as.numeric(substr(all.2014$debut,1,4))+1
end <- Sys.time()
print(end-start)
print(runs)

plate.appearances <- as.data.frame(table(all.2014$batter))
colnames(plate.appearances) <- c('batter','PA')
all.2014 <- merge(all.2014,plate.appearances,by='batter')
all.2014 <- subset(all.2014,PA>=400)
all.2014 <- all.2014[order(all.2014$index),]

all.2014.400 <- subset(all.2014,!grepl('NP',all.2014$play) & !(grepl('WP',all.2014$play) & !grepl('K',all.2014$play)) & 
                         !grepl('CS',all.2014$play) & !grepl('SB',all.2014$play) & 
                        substr(all.2014$play,1,2) != 'C/' & !grepl('DI',all.2014$play) & !(grepl('PB',all.2014$play) & 
                         !grepl('K',all.2014$play))& !grepl('BK',all.2014$play)& substr(all.2014$play,1,2)!='PO')

all.2014 <- subset(all.2014,!grepl('NP',all.2014$play) & !(grepl('WP',all.2014$play) & !grepl('K',all.2014$play)) & 
                     !grepl('CS',all.2014$play) & !grepl('SB',all.2014$play) & 
                         substr(all.2014$play,1,2) != 'C/' & !grepl('DI',all.2014$play) & !(grepl('PB',all.2014$play) & 
                        !grepl('K',all.2014$play)) & !grepl('BK',all.2014$play) & substr(all.2014$play,1,2)!='PO')




salary.no.na <- all.2014.400[which(all.2014.400$salary != 0),]
mean.salary <- mean(salary.no.na$salary)
sd.salary <- sd(salary.no.na$salary)
all.2014.400$std.sal <- round((all.2014.400$salary-mean.salary)/sd.salary,2)

 
#### This stats function is not necessary for the runs calculations, and is only used to 
#### calculate individual player stats for runs created
stats <- function(player.ID,innings='',run.diff=100){
  p.name <- player.ID
player <- subset(all.2014,batter==player.ID)
if (innings=='early'){player <- subset(player,inn %in% c(1,2,3))
}else if(innings=='middle') {player <- subset(player,inn %in% c(4,5,6))
}else if(innings=='late') {player <- subset(player,inn %in% c(7,8,9))
}else if(innings=='extra') {player <- subset(player,inn > 9)
}else {player <- player}

if (run.diff < 2){player <- subset(player,run.diff<2)
}else if (run.diff < 4) {player <- subset(player,run.diff %in% c(2,3))
}else if (run.diff >= 4 & run.diff < 99) {player <- subset(player,run.diff >= 4)
}else player <- player


player <- subset(player,!grepl('NP',player$play) & !(grepl('WP',player$play) & !grepl('K',player$play)) 
                 & !grepl('CS',player$play) & !grepl('SB',player$play) & 
               substr(player$play,1,2) != 'C/' & !grepl('DI',player$play) & !(grepl('PB',player$play) & !grepl('K',player$play)))
player$runners <- substr(player$state,1,3)
print(table(player$runners))



player.runs <- aggregate(player$runs.created,list(player$runners),FUN=sum)
player.pa <- aggregate(player$runs.created,list(player$runners),FUN=length)
player.runs.potential <- aggregate(player$runs.potential,list(player$runners),FUN=sum)
colnames(player.runs) <- c('runners','runs')
colnames(player.pa) <- c('runners','PA')
colnames(player.runs.potential) <- c('runners','potential')
player.total <- merge(player.pa,player.runs,by='runners')
player.total <- merge(player.total,player.runs.potential,by='runners')
print('')
print(player.total)
print(paste(p.name,' ',sum(player.total$runs),sep=''))
print(paste(p.name,' ',sum(player.total$potential),sep=''))


player.stats <- as.data.frame('')
player.stats$name <- paste(player$nameFirst[1],' ',player$nameLast[1],sep='')
player.stats$Age <- player$age[1]
player.stats$yrs.exp <- player$years.exp[1]
player.stats$PA <- nrow(player)
player.stats$AB <- player.stats$PA - sum(player$BB)- sum(player$HBP) - sum(player$SH) - sum(player$SF)
player.stats$R <- sum(player$R)
player.stats$H <- sum(player$X1B)+sum(player$X2B)+sum(player$X3B)+sum(player$HR)
player.stats$X2B <- sum(player$X2B)
player.stats$X3B <- sum(player$X3B)
player.stats$HR <- sum(player$HR)
player.stats$RBI <- sum(player$RBI)
player.stats$SB <- sum(player$SB)
player.stats$CS <- sum(player$CS)
player.stats$BB <- sum(player$BB)
player.stats$IBB <- sum(player$IW)
player.stats$HBP <- sum(player$HBP)
player.stats$SO <- sum(player$SO)
player.stats$BA <- round(player.stats$H/player.stats$AB,3)
player.stats$OBP <- round((player.stats$H + player.stats$BB + player.stats$HBP)/(player.stats$AB+player.stats$BB+player.stats$HBP+sum(player$SF)),3)
player.stats$SLG <- round((player.stats$H + player.stats$X2B + 2*player.stats$X3B + 3*player.stats$HR)/(player.stats$AB),3)
player.stats$OPS <- round(player.stats$OBP+player.stats$SLG,3)
player.stats$OPSP <- round(100*(player.stats$OBP/league.obp+player.stats$SLG/league.slg-1))
player.stats$TB <- player.stats$H + player.stats$X2B + 2*player.stats$X3B + 3*player.stats$HR
player.stats$GDP <- sum(player$GDP)
player.stats$SH <- sum(player$SH)
player.stats$SF <- sum(player$SF)
player.stats$pitches.seen <- round((sum(player$pitches.seen))/player.stats$PA,3)
player.stats$bats <- player$bats[1]
player.stats$throws <- player$throws[1]
player.stats$pos <- player$POS[nrow(player)]
player.stats <- player.stats[,-1]
print('')
print(player.stats)}

options(scipen=999)
all.2014.400$num.chars <- nchar(all.2014.400$salary)
all.2014.400$salary <- ifelse(all.2014.400$num.chars==8,paste(substr(all.2014.400$salary,1,2),',',substr(all.2014.400$salary,3,5),',',substr(all.2014.400$salary,6,8),sep=''),
                       ifelse(all.2014.400$num.chars==7,paste(substr(all.2014.400$salary,1,1),',',substr(all.2014.400$salary,2,4),',',substr(all.2014.400$salary,5,7),sep=''),
                              paste(substr(all.2014.400$salary,1,3),',',substr(all.2014.400$salary,4,6),sep='')))
all.2014.400$player.salary <- paste(all.2014.400$nameFirst,' ',all.2014.400$nameLast,': ',all.2014.400$salary,sep='')
all.2014$num.chars <- NULL


all.2014.400$name <- paste(all.2014.400$nameFirst,' ',all.2014.400$nameLast,sep='')
all.2014.400 <- all.2014.400[,c("year","batter","name","play","birthYear","birthCountry","inn",
                                "gte.one.run.x","X1B","X2B","X3B","HR","RBI","BB","SO","SH","SF","IW","GDP","HBP","IBB","first",
                                "second","third","R","outs.offset","pitches.seen","run.diff","weight","height","bats","throws","debut",
                                "salary","POS","runs.created","runs.potential","age","years.exp","PA","std.sal","num.chars","player.salary")]
colnames(all.2014.400)[25] <- 'outs'
temp <- all.2014
all.2014$salary <- as.numeric(all.2014$salary)
mean <- mean(all.2014$salary)
std.dev <- sd(all.2014$salary)
all.2014$std.sal <- (all.2014$salary-mean)/std.dev
#all.2014 <- all.2014[,-c(2,5,6,7,9:15,30,31,34:37,41,53,54,55,58:61)]
all.2014$name <- paste(all.2014$nameFirst,all.2014$nameLast,sep=' ')
all.2014$salary <- as.character(all.2014$salary)
all.2014$num.chars <- nchar(all.2014$salary)   #### need this var since 2014-2014 have it
all.2014$salary <- ifelse(all.2014$num.chars==6,paste(substr(all.2014$salary,1,3),',',substr(all.2014$salary,4,6),sep=''),
                   ifelse(all.2014$num.chars==7,paste(substr(all.2014$salary,1,1),',',
                                                      substr(all.2014$salary,2,4),',',substr(all.2014$salary,5,7),sep=''),
                                                paste(substr(all.2014$salary,1,2),',',substr(all.2014$salary,3,5),',',
                                                      substr(all.2014$salary,6,8),sep='')))

all.2014$player.salary <- paste(all.2014$name,': ',all.2014$salary,sep='')
#all.2014 <- all.2014[,c(1:25,28:42)]

all.2014$X <- 0     #### need this var since 2014-2014 have it
all.2014$salary <- ifelse(all.2014$name=='Jose Abreu','7,000,000',all.2014$salary)
all.2014$player.salary <- ifelse(all.2014$name=='Jose Abreu','Jose Abreu: 7,000,000',all.2014$player.salary)  ### his playerID doesn't match the Lahman Salary and Master,
                                                                                                              ### so I had to input his salary by hand
all.2014$salary <- ifelse(all.2014$salary=='0,,','0',all.2014$salary)
all.2014$debut.year <- substr(all.2014$debut,nchar(all.2014$debut)-1,nchar(all.2014$debut))
all.2014$debut.year <- as.numeric(all.2014$debut.year)+2000
all.2014$years.exp <- all.2014$year-all.2014$debut.year+1
all.2014$years.exp <- ifelse(all.2014$years.exp<0,all.2014$years.exp+100,all.2014$years.exp)
all.2014$debut.year <- NULL



all.2014 <- all.2014[,c(5,6,73,1,11,70,47,48,8,3,16:19,42,20,21:24,38,39,40,25:29,
                        32,33,49:53,58,60,65,4,66,67,68,69,71,72)]
all.2014$POS <- ifelse(all.2014$name=='Jose Abreu','1B',ifelse(all.2014$name=='Danny Santana','CF',
                  ifelse(all.2014$name=='Yangervis Solarte','3B',ifelse(all.2014$name=='Rougned Odor','2B',
                  ifelse(all.2014$name=='Ender Inciarte','CF',ifelse(all.2014$name=='Jon Singleton','1B',
                  ifelse(all.2014$name=='David Peralta','RF',ifelse(all.2014$name=='Tommy La Stella','2B',all.2014$POS))))))))
all.2014$player.salary <- paste(all.2014$name,': ',all.2014$salary,sep='')
all.2014 <- all.2014[which(!name %in% c('Yangervis Solarte','Rougned Odor','Danny Santana','Ender Inciarte','Jon Singleton','David Peralta','Tommy La Stella'))]

write.csv(all.2014,file="C:/baseball/all.2014.1.csv")
write.csv(all.2014,file="C:/baseball/all.2014.1.csv")
write.csv(all.2014,file="C:/shiny/tempruns1/all.2014v6.csv")

#### Bunts are not necessary for this analysis
bunt.attempts <- all.years[which(grepl('BP',all.years$play)|grepl('BP',all.years$play)|grepl('BGDP',all.years$play)|grepl('BL',all.years$play)),]
sac.bunt <- all.years[which(grepl('SH',all.years$play)),]
steals2 <- all.years[which(grepl('SB2',all.years$play)),]
cS2 <- all.years[which(grepl('CS2',all.years$play)),]
steals3 <- all.years[which(grepl('SB3',all.years$play)),]
cS3 <- all.years[which(grepl('CS3',all.years$play)),]

all.years <- rbind(all.2014,all.2012,all.2013,all.2014)
table(all.years$state)
