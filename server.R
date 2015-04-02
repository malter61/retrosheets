library(shiny)
require(devtools)
#install.packages('rCharts')
require(rCharts)
library(rCharts)
#require(ramnathv)
require(googleVis)
require(ggplot2)
require(plyr)
require(lubridate)
require(Lahman)
library(Lahman)
#devtools::install_github('rstudio/rscrypt')
#addAuthorizedUser("lighthouse")
options(shiny.maxRequestSize=32*1024^2)

#runs.df <- read.csv('C:/shiny/runs/runs2011_12_13.csv',header=TRUE,as.is=TRUE)
#colnames(runs.df)[9] <- 'Player_Salary'
#baserunning <- read.csv('C:/shiny/runs/baserunning.csv',header=TRUE,as.is=TRUE)
#stats.2011 <- read.csv('C:/shiny/runs4/all.2011.csv',header=TRUE,as.is=TRUE)
#stats.2012 <- read.csv('C:/shiny/runs4/all.2012.csv',header=TRUE,as.is=TRUE)
#stats.2013 <- read.csv('C:/shiny/runs4/all.2013.csv',header=TRUE,as.is=TRUE)
#stats.2014 <- read.csv('C:/shiny/runs4/all.2014v10.csv',header=TRUE,as.is=TRUE)
#state.runs2011.12.13.14 <- read.csv("C:/shiny/runs/state.runs2011_12_13_14.csv",header=TRUE,as.is=TRUE)

picked.off <- paste('https://www.dropbox.com/s/qnb357hivvdg4k0/picked.off.csv?dl=0')
picked <- paste('https://www.dropbox.com/s/c3ss93442kzk8mh/picked.csv?dl=0')
stats.2011 <- paste("https://www.dropbox.com/s/yyl0p0p22r6vwdw/all.2011.csv?dl=0")
stats.2012 <- paste("https://www.dropbox.com/s/ikyxcan4lyvk7nl/all.2012.csv?dl=0")
stats.2013 <- paste("https://www.dropbox.com/s/rrkxbydki64tcya/all.2013.csv?dl=0")
#stats.2014 <- paste("https://www.dropbox.com/s/jp7962kmrkwgnnp/all.2014.1.csv?dl=0")
#stats.2014 <- paste("https://www.dropbox.com/s/niavqxni42gqj90/all.2014v6.csv?dl=0")
#stats.2014 <- paste("https://www.dropbox.com/s/gjphmhrs2sr98s1/all.2014v8.csv?dl=0")
#stats.2014 <- paste("https://www.dropbox.com/s/l4l6y9nlakwxfx3/all.2014v9.csv?dl=0")
#stats.2014 <- paste("https://www.dropbox.com/s/72wu14qezghd2oa/all.2014v10.csv?dl=0")
#stats.2014 <- paste('https://www.dropbox.com/s/y8g4atqcxf0hve2/all.2014v11.csv?dl=0')
stats.2014 <- paste('https://www.dropbox.com/s/k6wr0undfv13orq/all.2014v18.csv?dl=0')
baserunning <- paste("https://www.dropbox.com/s/5p3253rftj23bar/baserunning.csv?dl=0")
#runs.df <- paste("https://www.dropbox.com/s/wupudt2o1rtfmwz/runs2011_12_13_14.csv?dl=0")
#runs.df <- paste("https://www.dropbox.com/s/91sd6fo7uyx50p0/runs2011_12_13_14v5%20.csv?dl=0")
#runs.df <- paste("https://www.dropbox.com/s/x9obvujhl6o18bs/runs2011_12_13_14v6%20.csv?dl=0")
runs.df <- paste("https://www.dropbox.com/s/iemi03bxykq9oiv/runs.df5.csv?dl=0")
state.runs2011.12.13.14 <- paste("https://www.dropbox.com/s/2ajcbrartxt1b1t/state.runs2011_12_13_14.csv?dl=0")
state.runs.all <- paste("https://www.dropbox.com/s/kfbcfwve0ojb6x9/state.runs.csv?dl=0")
second.to.home <- paste("https://www.dropbox.com/s/vidpnaftwgcqtmp/second.home.csv?dl=0")
batting <- paste('https://www.dropbox.com/s/loz7pvr889rrg22/batting.csv?dl=0')

runs.df <- repmis::source_data(runs.df,
                               sep = ",",
                               header = TRUE)

batting <- repmis::source_data(batting,
                                      sep = ",",
                                      header = TRUE)

second.to.home <- repmis::source_data(second.to.home,
                                      sep = ",",
                                      header = TRUE)
picked.off <- repmis::source_data(picked.off, sep = ",", header = TRUE)
picked <- repmis::source_data(picked, sep = ",", header = TRUE)
stats.2014 <- repmis::source_data(stats.2014, sep = ",", header = TRUE)
stats.2011 <- repmis::source_data(stats.2011, sep = ",", header = TRUE)

second.to.home$bases <- as.character(second.to.home$bases)
second.to.home$bases[1:9] <- '01'
second.to.home$bases[10:18] <- '11'


stats.2012 <- repmis::source_data(stats.2012, sep = ",", header = TRUE)
stats.2013 <- repmis::source_data(stats.2013,sep = ",", header = TRUE)

baserunning <- repmis::source_data(baserunning,
                                   sep = ",",
                                   header = TRUE)

state.runs2011.12.13.14 <- repmis::source_data(state.runs2011.12.13.14,
                                               sep = ",",
                                               header = TRUE)

state.runs.all <- repmis::source_data(state.runs.all,
                                      sep = ",",
                                      header = TRUE)

stats.2014$batter <- as.character(stats.2014$batter)
picked.off$batter <- as.character(picked.off$batter)

if(stats.2014$batter[1] %in% picked.off$batter){stats.2014$PA[1] <- stats.2014$PA[1] - picked.off$Freq[which(picked.off$batter==stats.2014$batter[1])]}

colnames(runs.df)[9] <- 'Player_Salary'

state.runs2011.12.13.14$state <- as.character(state.runs2011.12.13.14$state)
state.runs2011.12.13.14$state[1] <- '0000'
state.runs2011.12.13.14$state[2] <- '0001'
state.runs2011.12.13.14$state[3] <- '0002'
state.runs2011.12.13.14$state[4] <- '0010'
state.runs2011.12.13.14$state[5] <- '0011'
state.runs2011.12.13.14$state[6] <- '0012'
state.runs2011.12.13.14$state[7] <- '0100'
state.runs2011.12.13.14$state[8] <- '0101'
state.runs2011.12.13.14$state[9] <- '0102'
state.runs2011.12.13.14$state[10] <- '0110'
state.runs2011.12.13.14$state[11] <- '0111'
state.runs2011.12.13.14$state[12] <- '0112'
state.runs2011.12.13.14$state[13] <- '1000'
state.runs2011.12.13.14$state[14] <- '1001'
state.runs2011.12.13.14$state[15] <- '1002'
state.runs2011.12.13.14$state[16] <- '1010'
state.runs2011.12.13.14$state[17] <- '1011'
state.runs2011.12.13.14$state[18] <- '1012'
state.runs2011.12.13.14$state[19] <- '1100'
state.runs2011.12.13.14$state[20] <- '1101'
state.runs2011.12.13.14$state[21] <- '1102'
state.runs2011.12.13.14$state[22] <- '1110'
state.runs2011.12.13.14$state[23] <- '1111'
state.runs2011.12.13.14$state[24] <- '1112'

colnames(state.runs2011.12.13.14) <- c('state','runs.2011','runs.2012','runs.2013','runs.2014','gte.1.run.2011',
                                       'gte.1.run.2012','gte.1.run.2013','gte.1.run.2014')

batting.2011 <- Batting[which(Batting$yearID==2011),]
league.AB.2011 <- sum(batting.2011$AB)
league.tb.2011 <- sum(batting.2011$H)+sum(batting.2011$X2B)+2*sum(batting.2011$X3B)+3*sum(batting.2011$HR)
league.obp.2011 <- (sum(batting.2011$H)+sum(batting.2011$BB)+sum(batting.2011$HBP))/(sum(batting.2011$AB)+sum(batting.2011$SF))
league.slg.2011 <- league.tb.2011/league.AB.2011

batting.2012 <- Batting[which(Batting$yearID==2012),]
league.AB.2012 <- sum(batting.2012$AB)
league.tb.2012 <- sum(batting.2012$H)+sum(batting.2012$X2B)+2*sum(batting.2012$X3B)+3*sum(batting.2012$HR)
league.obp.2012 <- (sum(batting.2012$H)+sum(batting.2012$BB)+sum(batting.2012$HBP))/(sum(batting.2012$AB)+sum(batting.2012$SF))
league.slg.2012 <- league.tb.2012/league.AB.2012

batting.2013 <- Batting[which(Batting$yearID==2013),]
league.AB.2013 <- sum(batting.2013$AB)
league.tb.2013 <- sum(batting.2013$H)+sum(batting.2013$X2B)+2*sum(batting.2013$X3B)+3*sum(batting.2013$HR)
league.obp.2013 <- (sum(batting.2013$H)+sum(batting.2013$BB)+sum(batting.2013$HBP))/(sum(batting.2013$AB)+sum(batting.2013$SF))
league.slg.2013 <- league.tb.2013/league.AB.2013

batting.2014 <- batting[which(batting$yearID==2014),]
league.AB.2014 <- sum(batting.2014$AB)
league.tb.2014 <- sum(batting.2014$H)+sum(batting.2014$X2B)+2*sum(batting.2014$X3B)+3*sum(batting.2014$HR)
league.obp.2014 <- (sum(batting.2014$H)+sum(batting.2014$BB)+sum(batting.2014$HBP))/(sum(batting.2014$AB)+sum(batting.2014$SF))
league.slg.2014 <- league.tb.2014/league.AB.2014


shinyServer(
  function(input,output){
    
    
    
    output$chart1 <- renderChart({
      
      stats <- runs.df 
      
      #master <- read.csv('C:/baseball/Master.csv',header=TRUE,as.is=TRUE)
      #sal <- read.csv('C:/baseball/Salaries.csv',header=TRUE,as.is=TRUE)
      #master$player <- paste(master$nameFirst,master$nameLast,sep=' ')
      #master <- master[,c(1,23,25)]
      #sal <- sal[,c(1,2,4)]
      #colnames(sal)[1] <- 'year'
      #sal <- subset(sal,year %in% c(2011,2012,2013,2014))
      stats <- subset(stats,year==input$year)
      #sal <- merge(sal,master,by='playerID')
      #sal <- sal[,c(2,3,5)]
      #stats <- merge(stats,sal,by=c('player','year'))
      #colnames(stats)[13] <- 'team'
      if(input$innings=='All game'){stats <- subset(stats,stats$innings=='all')
      }else if (input$innings=='1-3'){stats <- subset(stats,stats$innings=='ear')
      }else if (input$innings=='4-6'){stats <- subset(stats,stats$innings=='mid')
      }else {stats <- subset(stats,stats$innings=='lat')} 
      
      if(input$run.diff=='All'){stats <- subset(stats,stats$run.diff=='all')
      }else if (input$run.diff=='0-1'){stats <- subset(stats,stats$run.diff=='tig')
      }else if (input$run.diff=='2-3'){stats <- subset(stats,stats$run.diff=='med')
      }else{stats <- subset(stats,stats$run.diff=='big')}
      
      if(input$position=='All' | input$year == '2015'){stats <- stats
      }else if (input$position=='C'){stats <- subset(stats,stats$Position=='C')
      }else if (input$position == '1B or DH'){stats <- subset(stats,stats$Position %in% c('1B','DH'))
      }else if (input$position=='2B'){stats <- subset(stats,stats$Position=='2B')
      }else if (input$position=='3B'){stats <- subset(stats,stats$Position=='3B')
      }else if (input$position=='SS'){stats <- subset(stats,stats$Position=='SS')
      }else {stats <- subset(stats,stats$Position %in% c('LF','CF','RF'))}
      
      if(input$team=='All' | input$year == '2015'){stats <- stats
      }else if (input$team=='Dodgers'){stats <- subset(stats,stats$team=='LAN')
      }else if (input$team=='Red Sox'){stats <- subset(stats,stats$team=='BOS')
      }else if (input$team=='Dbacks'){stats <- subset(stats,stats$team=='ARI')
      }else if (input$team=='White Sox'){stats <- subset(stats,stats$team=='CHA')
      }else if (input$team=='Orioles'){stats <- subset(stats,stats$team=='BAL')
      }else if (input$team=='Nationals'){stats <- subset(stats,stats$team=='WAS')
      }else if (input$team=='Marlins'){stats <- subset(stats,stats$team=='MIA')
      }else if (input$team=='Rangers'){stats <- subset(stats,stats$team=='TEX')
      }else if (input$team=='Angels'){stats <- subset(stats,stats$team=='LAA')
      }else if (input$team=="A's"){stats <- subset(stats,stats$team=="OAK")
      }else if (input$team=='Royals'){stats <- subset(stats,stats$team=='KCA')
      }else if (input$team=='Tigers'){stats <- subset(stats,stats$team=='DET')
      }else if (input$team=='Padres'){stats <- subset(stats,stats$team=='SDN')
      }else if (input$team=='Braves'){stats <- subset(stats,stats$team=='ATL')
      }else if (input$team=='Pirates'){stats <- subset(stats,stats$team=='PIT')
      }else if (input$team=='Giants'){stats <- subset(stats,stats$team=='SFG')
      }else if (input$team=='Cubs'){stats <- subset(stats,stats$team=='CHN')
      }else if (input$team=='Brewers'){stats <- subset(stats,stats$team=='MIL')
      }else if (input$team=='Indians'){stats <- subset(stats,stats$team=='CLE')
      }else if (input$team=='Mariners'){stats <- subset(stats,stats$team=='SEA')
      }else if (input$team=='Phillies'){stats <- subset(stats,stats$team=='PHI')
      }else if (input$team=='Rays'){stats <- subset(stats,stats$team=='TBA')
      }else if (input$team=='Reds'){stats <- subset(stats,stats$team=='CIN')
      }else if (input$team=='Yankees'){stats <- subset(stats,stats$team=='NYA')
      }else if (input$team=='Twins'){stats <- subset(stats,stats$team=='MIN')
      }else if (input$team=='Rockies'){stats <- subset(stats,stats$team=='COL')
      }else if (input$team=='Astros'){stats <- subset(stats,stats$team=='HOU')
      }else if (input$team=='Blue Jays'){stats <- subset(stats,stats$team=='TOR')
      }else if (input$team=='Mets'){stats <- subset(stats,stats$team %in% c('NYM','NYN'))
      }else {stats <- subset(stats,stats$team=='SLN')}
      
      
      if(input$bats=='All'| input$year == '2015'){stats <- stats
      }else if (input$bats=='Switch'){stats <- subset(stats,stats$B=='B')
      }else if (input$bats=='L'){stats <- subset(stats,stats$B=='L')
      }else {stats <- subset(stats,stats$B=='R')}
      
      if(input$year != '2015') {stats <- subset(stats,stats$Age>=input$age[1] & stats$Age<=input$age[2])}
      if(input$year != '2015') {stats <- subset(stats,stats$wgt>=input$weight[1] & stats$wgt<=input$weight[2])}
      
      if(input$year != '2015') {runs.created <- rPlot(runs ~ potential ,data=stats,type='point',color='Player_Salary')}
      if(input$year == '2015') {runs.created <- rPlot(runs ~ potential ,data=stats,type='point',color='player')}
      
      runs.created$addParams(dom='chart1')
      runs.created$set(title='Runs created by player vs. potential opportunity')
      return (runs.created)
      
    })
    
    
    output$chart2 <- renderPlot({
      
      runners <- c(rep('1st',3),rep('2nd',3),rep('1st and 2nd',3),rep('1st and 2nd',3),rep('1st and 3rd',3))
      outs <- rep(c(0,1,2),5)
      play <- c(rep('straight steal',6),rep('double steal',3),rep('single steal 3rd',3),rep('single steal 2nd',3))
      state.runs <- state.runs2011.12.13.14
      state.runs$expected.runs <- rowMeans(state.runs[,c(2:5)])
      state.runs$one.run <- rowMeans(state.runs[,c(6:9)])
      prob <- as.numeric(input$success)
      stolen.bases.df <- as.data.frame(cbind(runners,outs,play))
      stolen.bases.df$start.RE <- 0
      stolen.bases.df$end.RE <- 0
      stolen.bases.df$RE.breakeven <- 0
      stolen.bases.df$start.gte.1 <- 0
      stolen.bases.df$end.gte.1 <- 0
      stolen.bases.df$gte.1.breakeven <- 0
      stolen.bases.df$start.RE[1] <- state.runs[13,10]
      stolen.bases.df$start.RE[2] <- state.runs[14,10]
      stolen.bases.df$start.RE[3] <- state.runs[15,10]
      stolen.bases.df$start.RE[4] <- state.runs[7,10]
      stolen.bases.df$start.RE[5] <- state.runs[8,10]
      stolen.bases.df$start.RE[6] <- state.runs[9,10]
      stolen.bases.df$start.RE[7] <- state.runs[19,10]
      stolen.bases.df$start.RE[8] <- state.runs[20,10]
      stolen.bases.df$start.RE[9] <- state.runs[21,10]
      stolen.bases.df$start.RE[10] <- state.runs[19,10]
      stolen.bases.df$start.RE[11] <- state.runs[20,10]
      stolen.bases.df$start.RE[12] <- state.runs[21,10]
      stolen.bases.df$start.RE[13] <- state.runs[16,10]
      stolen.bases.df$start.RE[14] <- state.runs[17,10]
      stolen.bases.df$start.RE[15] <- state.runs[18,10]
      stolen.bases.df$start.gte.1[1] <- state.runs[13,11]
      stolen.bases.df$start.gte.1[2] <- state.runs[14,11]
      stolen.bases.df$start.gte.1[3] <- state.runs[15,11]      
      stolen.bases.df$start.gte.1[4] <- state.runs[7,11]
      stolen.bases.df$start.gte.1[5] <- state.runs[8,11]
      stolen.bases.df$start.gte.1[6] <- state.runs[9,11]      
      stolen.bases.df$start.gte.1[7] <- state.runs[19,11]
      stolen.bases.df$start.gte.1[8] <- state.runs[20,11]
      stolen.bases.df$start.gte.1[9] <- state.runs[21,11]      
      stolen.bases.df$start.gte.1[10] <- state.runs[19,11]
      stolen.bases.df$start.gte.1[11] <- state.runs[20,11]
      stolen.bases.df$start.gte.1[12] <- state.runs[21,11]      
      stolen.bases.df$start.gte.1[13] <- state.runs[16,11]
      stolen.bases.df$start.gte.1[14] <- state.runs[17,11]
      stolen.bases.df$start.gte.1[15] <- state.runs[18,11]
      
      stolen.bases.df$end.RE[1] <- prob*state.runs[7,10]+(1-prob)*state.runs[2,10]
      stolen.bases.df$end.RE[2] <- prob*state.runs[8,10]+(1-prob)*state.runs[3,10]
      stolen.bases.df$end.RE[3] <- prob*state.runs[9,10]+(1-prob)*0
      stolen.bases.df$end.RE[4] <- prob*state.runs[4,10]+(1-prob)*state.runs[2,10]
      stolen.bases.df$end.RE[5] <- prob*state.runs[5,10]+(1-prob)*state.runs[3,10]
      stolen.bases.df$end.RE[6] <- prob*state.runs[6,10]+(1-prob)*0
      stolen.bases.df$end.RE[7] <- prob*state.runs[10,10]+(1-prob)*state.runs[8,10]
      stolen.bases.df$end.RE[8] <- prob*state.runs[11,10]+(1-prob)*state.runs[9,10]
      stolen.bases.df$end.RE[9] <- prob*state.runs[12,10]+(1-prob)*0
      stolen.bases.df$end.RE[10] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
      stolen.bases.df$end.RE[11] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
      stolen.bases.df$end.RE[12] <- prob*state.runs[18,10]+(1-prob)*0
      stolen.bases.df$end.RE[13] <- prob*state.runs[10,10]+(1-prob)*state.runs[5,10]
      stolen.bases.df$end.RE[14] <- prob*state.runs[11,10]+(1-prob)*state.runs[6,10]
      stolen.bases.df$end.RE[15] <- prob*state.runs[12,10]+(1-prob)*0
      
      stolen.bases.df$end.gte.1[1] <- prob*state.runs[7,11]+(1-prob)*state.runs[2,11]
      stolen.bases.df$end.gte.1[2] <- prob*state.runs[8,11]+(1-prob)*state.runs[3,11]
      stolen.bases.df$end.gte.1[3] <- prob*state.runs[9,11]+(1-prob)*0
      stolen.bases.df$end.gte.1[4] <- prob*state.runs[4,11]+(1-prob)*state.runs[2,11]
      stolen.bases.df$end.gte.1[5] <- prob*state.runs[5,11]+(1-prob)*state.runs[3,11]
      stolen.bases.df$end.gte.1[6] <- prob*state.runs[6,11]+(1-prob)*0
      stolen.bases.df$end.gte.1[7] <- prob*state.runs[10,11]+(1-prob)*state.runs[8,11]
      stolen.bases.df$end.gte.1[8] <- prob*state.runs[11,11]+(1-prob)*state.runs[9,11]
      stolen.bases.df$end.gte.1[9] <- prob*state.runs[12,11]+(1-prob)*0
      stolen.bases.df$end.gte.1[10] <- prob*state.runs[16,11]+(1-prob)*state.runs[14,11]
      stolen.bases.df$end.gte.1[11] <- prob*state.runs[17,11]+(1-prob)*state.runs[15,11]
      stolen.bases.df$end.gte.1[12] <- prob*state.runs[18,11]+(1-prob)*0
      stolen.bases.df$end.gte.1[13] <- prob*state.runs[10,11]+(1-prob)*state.runs[5,11]
      stolen.bases.df$end.gte.1[14] <- prob*state.runs[11,11]+(1-prob)*state.runs[6,11]
      stolen.bases.df$end.gte.1[15] <- prob*state.runs[12,11]+(1-prob)*0
      
      re.safe <- c(7,8,9,4,5,6,10,11,12,16,17,18,10,11,12)
      re.out <- c(2,3,0,2,3,0,8,9,0,14,15,0,5,6,0)
      gte.safe <- c(7,8,9,4,5,6,10,11,12,16,17,18,10,11,12)
      gte.out <- c(2,3,0,2,3,0,8,9,0,14,15,0,5,6,0)
      
      for (i in 1:15){
        for(j in c(30:100)){
          p <- j/100
          if(re.out[i]!=0){
            temp <- p*state.runs[re.safe[i],10]+(1-p)*state.runs[re.out[i],10]
          }else {temp <- p*state.runs[re.safe[i],10]}
          
          if(temp>stolen.bases.df$start.RE[i] | j==100){
            stolen.bases.df$RE.breakeven[i] <- p
            break
          }
        }
      }
      
      for (i in 1:15){
        for(j in c(30:100)){
          p <- j/100
          if(gte.out[i]!=0){
            temp <- p*state.runs[gte.safe[i],11]+(1-p)*state.runs[gte.out[i],11]
          }else {temp <- p*state.runs[gte.safe[i],11]}
          
          if(temp>stolen.bases.df$start.gte.1[i] | j==100){
            stolen.bases.df$gte.1.breakeven[i] <- p
            break
          }
        }
      }     
      stolen.bases <- stolen.bases.df[c(1:15),]
      stolen.bases$first <- ifelse(grepl('1st',stolen.bases$runners),1,0)
      stolen.bases$second <- ifelse(grepl('2nd',stolen.bases$runners),1,0)
      stolen.bases$third <- ifelse(grepl('3rd',stolen.bases$runners),1,0)
      stolen.bases$steal.type <- ifelse(grepl('straight',stolen.bases$play),1,ifelse(grepl('double',stolen.bases$play),2,3))
      
      stolen.bases$state <- paste(stolen.bases$first,stolen.bases$second,stolen.bases$third,
                                  as.character(stolen.bases$outs),stolen.bases$steal.type,sep=''  )
      
      stolen.bases <- rbind(stolen.bases,stolen.bases)
      stolen.bases$time <- c(rep('start',15),rep('end',15))
      stolen.bases$runs.expectancy <- 0
      stolen.bases$runs.expectancy[1:15] <- stolen.bases$start.RE[1:15]
      stolen.bases$runs.expectancy[16:30] <- stolen.bases$end.RE[16:30]
      stolen.bases$scoring.prob <- 0
      stolen.bases$scoring.prob[1:15] <- stolen.bases$start.gte.1[1:15]
      stolen.bases$scoring.prob[16:30] <- stolen.bases$end.gte.1[16:30]
      stolen.bases$state <- factor(stolen.bases$state,levels=c('10001','10011','10021','01001','01011','01021',
                                                               '11002','11012','11022','11003','11013','11023','10103','10113','10123'))
      stolen.bases$time <- factor(stolen.bases$time,levels=c('start','end'))
      if (input$view == 'expected runs') {plot <- ggplot(stolen.bases,aes(x=time,y=runs.expectancy))+
                                            geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
                                            scale_size_continuous(guide="none") +
                                            theme(legend.position='none') +
                                            ggtitle  ("Runs expectancies before and after stolen base attempts") + 
                                            xlab("situation and steal type (see explanations below)")
                                          ylab("runs expectency remainder of inning")
                                          
      }else                               {plot <- ggplot(stolen.bases,aes(x=time,y=scoring.prob))+
                                             geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
                                             scale_size_continuous(guide="none") +
                                             theme(legend.position='none') +
                                             ggtitle  ("Probability of scoring at least one run before and after stolen base attempts") + 
                                             xlab("situation and steal type (see explanations below)") + 
                                             ylab("probability of scoring at least one run remainder of inning")}
      
      
      return(plot)
      #if(input$year != '2014') {runs.created <- rPlot(runs ~ potential ,data=stats,type='point',color='Player_Salary')}
      #if(input$year == '2014') {runs.created <- rPlot(runs ~ potential ,data=stats,type='point',color='player')}
      
      #runs.created$addParams(dom='chart2')
      #runs.created$set(title='Runs created by player vs. potential opportunity')
      #return (runs.created)
      
    })
    
    
    
    
    
    
    
    output$player.stats <- renderTable({
      #if (input$year==2011){player <- subset(stats.2011,name==input$name)       
      #}else if (input$year==2012){player <- subset(stats.2012,name==input$name)
      #}else {player <- subset(stats.2013,name==input$name)}
      
      
      if (input$year==2011 & !input$name %in% stats.2011$name){return()}
      if (input$year==2012 & !input$name %in% stats.2012$name){return()}
      if (input$year==2013 & !input$name %in% stats.2013$name){return()}
      if (input$year==2014 & !input$name %in% stats.2014$name){return()}
      
      if (input$year==2011){player <- subset(stats.2011,name==input$name)       
      }else if (input$year==2012){player <- subset(stats.2012,name==input$name)
      }else if (input$year==2013){player <- subset(stats.2013,name==input$name)
      }else {player <- subset(stats.2014,name==input$name)}
      
      
      #player$first <- ifelse(player$first=='x',0,1)
      #player$second <- ifelse(player$second=='x',0,1)
      #player$third <- ifelse(player$third=='x',0,1)
      player$state <- paste(player$first,player$second,player$third,substr(player$state,4,4),sep='')
      
      #player <- subset(player,year==input$year)
      if(input$innings=='All game'){player <- player
      }else if (input$innings=='1-3'){player <- subset(player,player$inn %in% c(1,2,3))
      }else if (input$innings=='4-6'){player <- subset(player,player$inn %in% c(4,5,6))
      }else {player <- subset(player,player$inn > 6)} 
      
      if(input$run.diff=='All'){player <- player
      }else if (input$run.diff=='0-1'){player <- subset(player,player$run.diff %in% c(0,1))
      }else if (input$run.diff=='2-3'){player <- subset(player,player$run.diff %in% c(2,3))
      }else {player <- subset(player,player$run.diff > 3)}
      
      
      player <- subset(player,!grepl('NP',player$play) & !(grepl('WP',player$play) & !grepl('K',player$play)) & 
                         !grepl('CS',player$play) & !grepl('SB',player$play) & substr(player$play,1,2) != 'C/' & 
                         !grepl('DI',player$play) & !(grepl('PB',player$play) & !grepl('K',player$play)))
      
      player$runners <- substr(player$state,1,3)
      
      
      
      player.runs <- aggregate(player$runs.created,list(player$runners),FUN=sum)
      player.pa <- aggregate(player$runs.created,list(player$runners),FUN=length)
      player.runs.potential <- aggregate(player$runs.potential,list(player$runners),FUN=sum)
      colnames(player.runs) <- c('runners','runs')
      colnames(player.pa) <- c('runners','PA')
      colnames(player.runs.potential) <- c('runners','potential')
      player.total <- merge(player.pa,player.runs,by='runners')
      player.total <- merge(player.total,player.runs.potential,by='runners')
      
      
      player.stats <- as.data.frame('')
      player.stats$name <- player$name[1]
      player.stats$Age <- player$age[1]
      player.stats$yrs.exp <- player$years.exp[1]
      player.stats$PA <- nrow(player)
      player.stats$batter <- player$batter[1]
      #if(player.stats$batter %in% picked.off$batter){player.stats$PA <- player.stats$PA - picked.off$Freq[which(picked.off$batter==player.stats$batter)]}
      player.stats <- player.stats[,c(1:5)]
      player.stats$AB <- player.stats$PA - sum(player$BB)- sum(player$HBP) - sum(player$SH) - sum(player$SF)
      #player.stats$R <- sum(player$R)
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
      player.stats$SH <- sum(player$SH)
      player.stats$SF <- sum(player$SF)
      player.stats$BA <- round(player.stats$H/player.stats$AB,3)
      player.stats$OBP <- round((player.stats$H[1] + player.stats$BB[1] + player.stats$HBP[1])/
                                  (player.stats$AB[1]+player.stats$BB[1]+player.stats$HBP[1]+player.stats$SF[1]),3)
      player.stats$SLG <- round((player.stats$H[1] + player.stats$X2B[1] + 2*player.stats$X3B[1] + 3*player.stats$HR[1])/(player.stats$AB[1]),3)
      player.stats$OPS <- round(player.stats$OBP+player.stats$SLG,3)
      
      if(input$year==2011){player.stats$OPSP <- round(100*(player.stats$OBP/league.obp.2011+player.stats$SLG/league.slg.2011-1))
      }else if (input$year==2012){player.stats$OPSP <- round(100*(player.stats$OBP/league.obp.2012+player.stats$SLG/league.slg.2012-1))
                                  #}else if (input$year==2013){player.stats$OPSP <- round(100*(player.stats$OBP/league.obp.2013+player.stats$SLG/league.slg.2013-1))
      }else           {player.stats$OPSP <- round(100*(player.stats$OBP/league.obp.2013+player.stats$SLG/league.slg.2013-1))}
      player.stats$TB <- player.stats$H + player.stats$X2B + 2*player.stats$X3B + 3*player.stats$HR
      player.stats$GDP <- sum(player$GDP)
      player.stats$pitches.seen <- round((sum(player$pitches.seen))/player.stats$PA,3)
      player.stats$bats <- player$bats[1]
      player.stats$throws <- player$throws[1]
      player.stats$pos <- player$POS[nrow(player)]
      player.stats$salary <- player$salary[1]
      player.stats$std.sal <- player$std.sal[1]
      player.stats$salary <- ifelse(nchar(as.character(player.stats$salary))<3,'',player.stats$salary)
      player.stats <- player.stats[,-1] 
      #if(input$year==2014) {player.stats <- player.stats[,-c(2,3,28:32)]}
      player.stats$yrs.exp <- as.numeric(player.stats$yrs.exp)
      player.stats$yrs.exp <- ifelse(player.stats$yrs.exp<0,round(player.stats$yrs.exp+100),round(player.stats$yrs.exp))
      player.stats
      
      
    })
    output$state <- renderTable({
      #state.runs2011.12.13.14 <- state.runs2011.12.13.14[c(1:3,13:15,7:9,19:21,4:6,16:18,10:12,22:24),]    ## reorder rows to get first base on top
      #state.runs <- state.runs2011.12.13.14
      #state.runs$state[1] <- '0000'
      #state.runs$state[2] <- '0001'
      #state.runs$state[3] <- '0002'
      #state.runs$state[7] <- '0100'
      #state.runs$state[8] <- '0101'
      #state.runs$state[9] <- '0102'
      
      #state.runs$state[1] <- '000'
      #state.runs$state[2] <- '100'
      #state.runs$state[3] <- '010'
      #state.runs$state[4] <- '001'
      #state.runs$state[5] <- '110'
      #state.runs$state[6] <- '101'
      #state.runs$state[7] <- '011'
      #state.runs$state[8] <- '111'
      
      
      #state.runs$expected.runs <- rowMeans(state.runs[,c(2:5)])
      #state.runs$one.run <- rowMeans(state.runs[,c(6:9)])                               
      ########state.runs[,c(1,10,11)]
      #colnames(state.runs)[1] <- 'state'
      #colnames(state.runs)[10] <- 'Expected.Runs.ROI'
      #colnames(state.runs)[11] <- 'Prob.Any.Score.ROI'
      #state.runs$run.exp.0outs <- 0
      #state.runs$run.exp.1out <- 0
      #state.runs$run.exp.2outs <- 0
      #state.runs$prob.0outs <- 0
      #state.runs$prob.1out <- 0
      #state.runs$prob.2outs <- 0
      #######state.runs[,c(1,10:17)]
      
      #state.runs$run.exp.0outs[1] <- state.runs$Expected.Runs.ROI[1]
      #state.runs$run.exp.0outs[2] <- state.runs$Expected.Runs.ROI[4]
      #state.runs$run.exp.0outs[3] <- state.runs$Expected.Runs.ROI[7]
      #state.runs$run.exp.0outs[4] <- state.runs$Expected.Runs.ROI[13]
      #state.runs$run.exp.0outs[5] <- state.runs$Expected.Runs.ROI[10]
      #state.runs$run.exp.0outs[6] <- state.runs$Expected.Runs.ROI[16]
      #state.runs$run.exp.0outs[7] <- state.runs$Expected.Runs.ROI[19]
      #state.runs$run.exp.0outs[8] <- state.runs$Expected.Runs.ROI[22]
      #state.runs$run.exp.1out[1] <- state.runs$Expected.Runs.ROI[2]
      #state.runs$run.exp.1out[2] <- state.runs$Expected.Runs.ROI[5]
      #state.runs$run.exp.1out[3] <- state.runs$Expected.Runs.ROI[8]
      #state.runs$run.exp.1out[4] <- state.runs$Expected.Runs.ROI[14]
      #state.runs$run.exp.1out[5] <- state.runs$Expected.Runs.ROI[11]
      #state.runs$run.exp.1out[6] <- state.runs$Expected.Runs.ROI[17]
      #state.runs$run.exp.1out[7] <- state.runs$Expected.Runs.ROI[20]
      #state.runs$run.exp.1out[8] <- state.runs$Expected.Runs.ROI[23]
      #state.runs$run.exp.2outs[1] <- state.runs$Expected.Runs.ROI[3]
      #state.runs$run.exp.2outs[2] <- state.runs$Expected.Runs.ROI[6]
      #state.runs$run.exp.2outs[3] <- state.runs$Expected.Runs.ROI[9]
      #state.runs$run.exp.2outs[4] <- state.runs$Expected.Runs.ROI[15]
      #state.runs$run.exp.2outs[5] <- state.runs$Expected.Runs.ROI[12]
      #state.runs$run.exp.2outs[6] <- state.runs$Expected.Runs.ROI[18]
      #state.runs$run.exp.2outs[7] <- state.runs$Expected.Runs.ROI[21]
      #state.runs$run.exp.2outs[8] <- state.runs$Expected.Runs.ROI[24]
      #state.runs$prob.0outs[1] <- state.runs$Prob.Any.Score.ROI[1]
      #state.runs$prob.0outs[2] <- state.runs$Prob.Any.Score.ROI[4]
      #state.runs$prob.0outs[3] <- state.runs$Prob.Any.Score.ROI[7]
      #state.runs$prob.0outs[4] <- state.runs$Prob.Any.Score.ROI[13]
      #state.runs$prob.0outs[5] <- state.runs$Prob.Any.Score.ROI[10]
      #state.runs$prob.0outs[6] <- state.runs$Prob.Any.Score.ROI[16]
      #state.runs$prob.0outs[7] <- state.runs$Prob.Any.Score.ROI[19]
      #state.runs$prob.0outs[8] <- state.runs$Prob.Any.Score.ROI[22]
      #state.runs$prob.1out[1] <- state.runs$Prob.Any.Score.ROI[2]
      #state.runs$prob.1out[2] <- state.runs$Prob.Any.Score.ROI[5]
      #state.runs$prob.1out[3] <- state.runs$Prob.Any.Score.ROI[8]
      #state.runs$prob.1out[4] <- state.runs$Prob.Any.Score.ROI[14]
      #state.runs$prob.1out[5] <- state.runs$Prob.Any.Score.ROI[11]
      #state.runs$prob.1out[6] <- state.runs$Prob.Any.Score.ROI[17]
      #state.runs$prob.1out[7] <- state.runs$Prob.Any.Score.ROI[20]
      #state.runs$prob.1out[8] <- state.runs$Prob.Any.Score.ROI[23]
      #state.runs$prob.2outs[1] <- state.runs$Prob.Any.Score.ROI[3]
      #state.runs$prob.2outs[2] <- state.runs$Prob.Any.Score.ROI[6]
      #state.runs$prob.2outs[3] <- state.runs$Prob.Any.Score.ROI[9]
      #state.runs$prob.2outs[4] <- state.runs$Prob.Any.Score.ROI[15]
      #state.runs$prob.2outs[5] <- state.runs$Prob.Any.Score.ROI[12]
      #state.runs$prob.2outs[6] <- state.runs$Prob.Any.Score.ROI[18]
      #state.runs$prob.2outs[7] <- state.runs$Prob.Any.Score.ROI[21]
      #state.runs$prob.2outs[8] <- state.runs$Prob.Any.Score.ROI[24]
      #colnames(state.runs)[1] <- 'bases'
      ###########state.runs[c(1:8),c(1,12:17)]
      state.runs <- state.runs.all   ### all of the above was for naught.  I just downloaded a new, cleaner csv file
      state.runs$bases <- c('000','100','010','001','110','101','011','111')
      state.runs
      
    })
    
    output$sac.bunt <- renderTable({
      state.runs <- state.runs.all   ### all of the above was for naught.  I just downloaded a new, cleaner csv file
      state.runs$bases <- c('000','100','010','001','110','101','011','111')
      state.runs[c(2,3,4,5,7),]#,c(1,12:17)]
      
    })
    
    output$sac.bunt.chart <- renderPlot({
      state.runs <- state.runs.all   ### all of the above was for naught.  I just downloaded a new, cleaner csv file
      state.runs$bases <- c('000','100','010','001','110','101','011','111')
      state.runs <- state.runs[c(2,3,4,5,7),]
      state.runs <- rbind(state.runs,state.runs)
      state.runs$outs <- c(rep(0,5),rep(1,5))
      state.runs$state <- paste(state.runs$bases,state.runs$outs,sep='')
      state.runs <- rbind(state.runs,state.runs)
      state.runs$time <- c(rep('start',10),rep('end',10))
      Success <- as.numeric(input$success)
      state.runs$runs.expectancy <- 0
      state.runs$runs.expectancy[1:5] <- state.runs$run.exp.0outs[1:5]
      state.runs$runs.expectancy[6:10] <- state.runs$run.exp.1out[1:5]
      state.runs$runs.expectancy[11:15] <- Success * state.runs$run.exp.1out[2:6] + (1-Success)*state.runs$run.exp.1out[1:5]
      state.runs$runs.expectancy[16:20] <- Success * state.runs$run.exp.2outs[2:6] + (1-Success)*state.runs$run.exp.2outs[1:5]
      state.runs$runs.expectancy[12] <- Success * state.runs$run.exp.1out[3] + (1-Success)*state.runs$run.exp.1out[1]
      state.runs$runs.expectancy[14] <- Success * state.runs$run.exp.1out[5] + (1-Success)*state.runs$run.exp.1out[4]
      state.runs$runs.expectancy[17] <- Success * state.runs$run.exp.2outs[8] + (1-Success)*state.runs$run.exp.2outs[6]
      state.runs$runs.expectancy[19] <- Success * state.runs$run.exp.2outs[10] + (1-Success)*state.runs$run.exp.2outs[9]
      
      state.runs$scoring.prob <- 0
      state.runs$scoring.prob[1:5] <- state.runs$prob.0outs[1:5]
      state.runs$scoring.prob[6:10] <- state.runs$prob.1out[1:5]
      state.runs$scoring.prob[11:15] <- Success * state.runs$prob.1out[2:6] + (1-Success)*state.runs$prob.1out[1:5]
      state.runs$scoring.prob[16:20] <- Success * state.runs$prob.2outs[2:6]+ (1-Success)*state.runs$prob.2outs[1:5]
      state.runs$scoring.prob[12] <- Success * state.runs$prob.1out[3] + (1-Success)*state.runs$prob.1out[1]
      state.runs$scoring.prob[14] <- Success * state.runs$prob.1out[5] + (1-Success)*state.runs$prob.1out[4]
      state.runs$scoring.prob[17] <- Success * state.runs$prob.2outs[8] + (1-Success)*state.runs$prob.2outs[6]
      state.runs$scoring.prob[19] <- Success * state.runs$prob.2outs[10] + (1-Success)*state.runs$prob.2outs[9]
      
      state.runs <- state.runs[c(1,2,4,6,7,9,11,12,14,16,17,19),]
      
      
      state.runs$time <- factor(state.runs$time,levels=c('start','end'))
      
      state.runs$state <- factor(state.runs$state,levels=c('1000','0100','1100','1001','0101','1101'))
      
      if (input$view == 'expected runs') {plot <- ggplot(state.runs,aes(x=time,y=runs.expectancy))+
                                            geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
                                            scale_size_continuous(guide="none") +
                                            theme(legend.position='none') +
                                            ggtitle  ("Runs expectancies before and after sacrifice bunt attempts") + 
                                            xlab("base/outs situation (see explanations below)")
                                          ylab("runs expectancy remainder of inning")
                                          
      }else                               {plot <- ggplot(state.runs,aes(x=time,y=scoring.prob))+
                                             geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
                                             scale_size_continuous(guide="none") +
                                             theme(legend.position='none') +
                                             ggtitle  ("Probability of scoring at least one run before and after sacrifice bunt attempts") + 
                                             xlab("base/outs situation (see explanations below)") + 
                                             ylab("probability of scoring at least one run remainder of inning")}
      
      
      return(plot)
      
    })
    
    output$player.table <- renderTable({
      if (input$year==2011 & !input$name %in% stats.2011$name){return()}
      if (input$year==2012 & !input$name %in% stats.2012$name){return()}
      if (input$year==2013 & !input$name %in% stats.2013$name){return()}
      if (input$year==2014 & !input$name %in% stats.2014$name){return()}
      
      if (input$year==2011){player <- subset(stats.2011,name==input$name)       
      }else if (input$year==2012){player <- subset(stats.2012,name==input$name)
      }else if (input$year==2013){player <- subset(stats.2013,name==input$name)
      }else {player <- subset(stats.2014,name==input$name)}
      
      player$state <- paste(player$first,player$second,player$third,player$outs,sep='')
      
      if(input$innings=='All game'){player <- player
      }else if (input$innings=='1-3'){player <- subset(player,player$inn %in% c(1,2,3))
      }else if (input$innings=='4-6'){player <- subset(player,player$inn %in% c(4,5,6))
      }else {player <- subset(player,player$inn > 6)} 
      
      if(input$run.diff=='All'){player <- player
      }else if (input$run.diff=='0-1'){player <- subset(player,player$run.diff %in% c(0,1))
      }else if (input$run.diff=='2-3'){player <- subset(player,player$run.diff %in% c(2,3))
      }else {player <- subset(player,player$run.diff > 3)}
      
      
      player <- subset(player,!grepl('NP',player$play) & !(grepl('WP',player$play) & !grepl('K',player$play)) & 
                         !grepl('CS',player$play) & !grepl('SB',player$play) & substr(player$play,1,2) != 'C/' & 
                         !grepl('DI',player$play) & !(grepl('PB',player$play) & !grepl('K',player$play)))
      
      
      player$runners <- substr(player$state,1,3)
      #player <- player[,c(1:5)]
      player.runs <- aggregate(player$runs.created,list(player$runners),FUN=sum)
      player.pa <- aggregate(player$runs.created,list(player$runners),FUN=length)
      player.runs.potential <- aggregate(player$runs.potential,list(player$runners),FUN=sum)
      colnames(player.runs) <- c('runners','runs')
      colnames(player.pa) <- c('runners','PA')
      colnames(player.runs.potential) <- c('runners','potential')
      player.total <- merge(player.pa,player.runs,by='runners')
      player.total <- merge(player.total,player.runs.potential,by='runners')
      player.total$PA <- as.numeric(player.total$PA)
      player.total$runs <- as.numeric(player.total$runs)
      player.total$potential <- as.numeric(player.total$potential)
      row <- c('Total',sum(player.total$PA),sum(player.total$runs),sum(player.total$potential))
      player.total <- rbind(player.total,row)
      player.total$runs <- as.numeric(player.total$runs)
      player.total$potential <- as.numeric(player.total$potential)
      player.total$runs <- round(player.total$runs,1)
      player.total$potential <- round(player.total$potential)
      player.total$potential <- as.character(player.total$potential)
      #player.total$potential <- substr(player.total$potential,1,nchar(player.total$potential)-3)
      player.total[9,1] <- paste(input$name,'totals',sep=' ')
      colnames(player.total)[3] <- 'runs.created'
      #picked.off$batter <- as.character(picked.off$batter)
      player$batter <- as.character(player$batter)
      #picked.off$Freq <- as.numeric(picked.off$Freq)
      #player.total$PA <- as.numeric(player.total$PA)
      #player.total$PA <- round(as.numeric(player.total$PA))
      #if(player$batter[1] %in% picked.off$batter){player.total$PA[9] <- player.total$PA[9] - picked.off$Freq[which(picked.off$batter==player$batter[1])]}
      #player.total$PA <- round(player.total$PA)
      player.total
    })
    
    output$tag <- renderTable({
      baserunning <- baserunning[,c(2:11)]
      baserunning$outs <- substr(baserunning$state,4,4)
      baserunning$state <- substr(baserunning$state,1,3)
      baserunning <- baserunning[,c(1,11,2:10)]
      state.runs <- state.runs2011.12.13.14
      state.runs$expected.runs <- rowMeans(state.runs[,c(2:5)])
      state.runs$one.run <- rowMeans(state.runs[,c(6:9)])
      prob <- as.numeric(input$success)
      baserunning$RE.breakeven <- 0
      
      baserunning$RE.attempt[c(1:3)] <- prob*(1+state.runs[2,10])+(1-prob)*state.runs[3,10]
      baserunning$RE.attempt[c(4:6)] <- prob*(1+state.runs[14,10])+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[c(7:9)] <- prob*(1+state.runs[8,10])+(1-prob)*state.runs[9,10]
      baserunning$RE.attempt[c(10:12)] <- prob*(1+state.runs[20,10])+(1-prob)*state.runs[21,10]
      baserunning$RE.attempt[c(13:15)] <- prob*(1+state.runs[3,10])+(1-prob)*0
      baserunning$RE.attempt[c(16:18)] <- prob*(1+state.runs[15,10])+(1-prob)*0
      baserunning$RE.attempt[c(19:21)] <- prob*(1+state.runs[9,10])+(1-prob)*0
      baserunning$RE.attempt[c(21:24)] <- prob*(1+state.runs[21,10])+(1-prob)*0
      
      baserunning$RE.attempt[25] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
      baserunning$RE.attempt[26] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
      baserunning$RE.attempt[27] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
      baserunning$RE.attempt[28] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[29] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[30] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[31] <- prob*state.runs[18,10]+(1-prob)*0
      baserunning$RE.attempt[32] <- prob*state.runs[18,10]+(1-prob)*0
      baserunning$RE.attempt[33] <- prob*state.runs[18,10]+(1-prob)*0
      
      #baserunning[c(1:24),-1]
      baserunning <- baserunning[c(1:24),]
      
      
      re.safe <- c(2,2,2,14,14,14,8,8,8,20,20,20,3,3,3,15,15,15,9,9,9,21,21,21)
      re.out <- c(3,3,3,15,15,15,9,9,9,21,21,21,0,0,0,0,0,0,0,0,0,0,0,0)
      for (i in 1:24){
        for(j in c(30:100)){
          p <- j/100
          if(re.out[i]!=0){
            temp <- p*(1+state.runs[re.safe[i],10])+(1-p)*state.runs[re.out[i],10]
          }else {temp <- p*(1+state.runs[re.safe[i],10])}
          
          if(temp > baserunning$RE.no.attempt[i] | j==100){
            baserunning$RE.breakeven[i] <- p
            break
          }
        }
      }
      colnames(baserunning)[1] <- 'bases'
      baserunning$outs <- c(rep('0',12),rep('1',12))
      baserunning$bases <- rep(c(rep('001',3),rep('101',3),rep('011',3),rep('111',3)),2)
      #colnames(baserunning)[2] <- 'outs.after.catch'
      baserunning$outs <- as.character(c(rep(1,12),rep(2,12)))
      baserunning
    })
    
    
    output$tag_from_third_plot <- renderPlot({
      baserunning <- baserunning[,c(2:11)]
      baserunning$outs <- substr(baserunning$state,4,4)
      baserunning$state <- substr(baserunning$state,1,3)
      baserunning <- baserunning[,c(1,11,2:10)]
      state.runs <- state.runs2011.12.13.14
      state.runs$expected.runs <- rowMeans(state.runs[,c(2:5)])
      state.runs$one.run <- rowMeans(state.runs[,c(6:9)])
      prob <- as.numeric(input$success)
      baserunning$RE.breakeven <- 0
      
      baserunning$RE.attempt[c(1:3)] <- prob*(1+state.runs[2,10])+(1-prob)*state.runs[3,10]
      baserunning$RE.attempt[c(4:6)] <- prob*(1+state.runs[14,10])+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[c(7:9)] <- prob*(1+state.runs[8,10])+(1-prob)*state.runs[9,10]
      baserunning$RE.attempt[c(10:12)] <- prob*(1+state.runs[20,10])+(1-prob)*state.runs[21,10]
      baserunning$RE.attempt[c(13:15)] <- prob*(1+state.runs[3,10])+(1-prob)*0
      baserunning$RE.attempt[c(16:18)] <- prob*(1+state.runs[15,10])+(1-prob)*0
      baserunning$RE.attempt[c(19:21)] <- prob*(1+state.runs[9,10])+(1-prob)*0
      baserunning$RE.attempt[c(21:24)] <- prob*(1+state.runs[21,10])+(1-prob)*0
      
      baserunning$RE.attempt[25] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
      baserunning$RE.attempt[26] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
      baserunning$RE.attempt[27] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
      baserunning$RE.attempt[28] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[29] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[30] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
      baserunning$RE.attempt[31] <- prob*state.runs[18,10]+(1-prob)*0
      baserunning$RE.attempt[32] <- prob*state.runs[18,10]+(1-prob)*0
      baserunning$RE.attempt[33] <- prob*state.runs[18,10]+(1-prob)*0
      
      #baserunning[c(1:24),-1]
      baserunning <- baserunning[c(1:24),]
      
      
      re.safe <- c(2,2,2,14,14,14,8,8,8,20,20,20,3,3,3,15,15,15,9,9,9,21,21,21)
      re.out <- c(3,3,3,15,15,15,9,9,9,21,21,21,0,0,0,0,0,0,0,0,0,0,0,0)
      for (i in 1:24){
        for(j in c(30:100)){
          p <- j/100
          if(re.out[i]!=0){
            temp <- p*(1+state.runs[re.safe[i],10])+(1-p)*state.runs[re.out[i],10]
          }else {temp <- p*(1+state.runs[re.safe[i],10])}
          
          if(temp > baserunning$RE.no.attempt[i] | j==100){
            baserunning$RE.breakeven[i] <- p
            break
          }
        }
      }
      colnames(baserunning)[1] <- 'bases'
      baserunning$outs <- c(rep('0',12),rep('1',12))
      baserunning$bases <- rep(c(rep('001',3),rep('101',3),rep('011',3),rep('111',3)),2)
      #colnames(baserunning)[2] <- 'outs.after.catch'
      baserunning$outs <- as.character(c(rep(1,12),rep(2,12)))
      baserunning <- baserunning[c(1,4,7,10,13,16,19,22),]
      baserunning <- rbind(baserunning,baserunning)
      baserunning$time <- c(rep('hold',8),rep('tag',8))
      baserunning$runs.expectancy <- 0
      baserunning$scoring.prob <- 0
      
      prob <- as.numeric(input$success)
      baserunning$runs.expectancy[1:8] <- baserunning$RE.no.attempt[1:8]
      baserunning$runs.expectancy[9:16] <- baserunning$RE.attempt[1:8]
      baserunning$scoring.prob[1] <- 0.65
      baserunning$scoring.prob[2] <- 0.63
      baserunning$scoring.prob[3] <- 0.67
      baserunning$scoring.prob[4] <- 0.66
      baserunning$scoring.prob[5] <- 0.26
      baserunning$scoring.prob[6] <- 0.28
      baserunning$scoring.prob[7] <- 0.26
      baserunning$scoring.prob[8] <- 0.32
      baserunning$scoring.prob[9] <- prob * 1 + (1-prob)*(0.09)
      baserunning$scoring.prob[10] <- prob * 1 + (1-prob)*(0.14)
      baserunning$scoring.prob[11] <- prob * 1 + (1-prob)*(0.23)
      baserunning$scoring.prob[12] <- prob * 1 + (1-prob)*(0.23)
      baserunning$scoring.prob[13] <- prob * 1 + (1-prob)*(0)
      baserunning$scoring.prob[14] <- prob * 1 + (1-prob)*(0)
      baserunning$scoring.prob[15] <- prob * 1 + (1-prob)*(0)
      baserunning$scoring.prob[16] <- prob * 1 + (1-prob)*(0)
      
      baserunning$time <- factor(baserunning$time,levels=c('hold','tag'))
      baserunning$outs <- as.character(baserunning$outs)
      baserunning$state <- paste(baserunning$bases,baserunning$outs,sep='')
      #baserunning$state <- factor(baserunning$state,levels=c('0011','0012','1011','1012','0111','0112','1111','1112'))
      
      if(input$view == 'expected runs')
        
      {plot <- ggplot(baserunning,aes(x=time,y=runs.expectancy))+
         geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
         scale_size_continuous(guide="none") +
         theme(legend.position='none') +
         ggtitle  ("Runs expectancies for attempting to tag from third on a fly ball vs. holding at third base") + 
         xlab("") +
         ylab("runs expectancy remainder of inning")
       
      }else {plot <- ggplot(baserunning,aes(x=time,y=scoring.prob))+
        geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
        scale_size_continuous(guide="none") +
        theme(legend.position='none') +
        ggtitle  ("Prob of scoring at least one run with tag from third on a fly ball vs. holding at third base") + 
        xlab("") +
        ylab("prob of scoring at least one run over remainder of inning")}
      
    return(plot)
  })




output$first_third <- renderTable({
  baserunning <- baserunning[,c(2:11)]
  baserunning$outs <- substr(baserunning$state,4,4)
  baserunning$state <- substr(baserunning$state,1,3)
  baserunning <- baserunning[,c(1,11,2:10)]
  state.runs <- state.runs2011.12.13.14
  state.runs$expected.runs <- rowMeans(state.runs[,c(2:5)])
  state.runs$one.run <- rowMeans(state.runs[,c(6:9)])
  prob <- as.numeric(input$success)
  baserunning$RE.breakeven <- 0
  
  baserunning$RE.attempt[c(1:3)] <- prob*(1+state.runs[2,10])+(1-prob)*state.runs[3,10]
  baserunning$RE.attempt[c(4:6)] <- prob*(1+state.runs[14,10])+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[c(7:9)] <- prob*(1+state.runs[8,10])+(1-prob)*state.runs[9,10]
  baserunning$RE.attempt[c(10:12)] <- prob*(1+state.runs[20,10])+(1-prob)*state.runs[21,10]
  baserunning$RE.attempt[c(13:15)] <- prob*(1+state.runs[3,10])+(1-prob)*0
  baserunning$RE.attempt[c(16:18)] <- prob*(1+state.runs[15,10])+(1-prob)*0
  baserunning$RE.attempt[c(19:21)] <- prob*(1+state.runs[9,10])+(1-prob)*0
  baserunning$RE.attempt[c(21:24)] <- prob*(1+state.runs[21,10])+(1-prob)*0
  
  baserunning$RE.attempt[25] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
  baserunning$RE.attempt[26] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
  baserunning$RE.attempt[27] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
  baserunning$RE.attempt[28] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[29] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[30] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[31] <- prob*state.runs[18,10]+(1-prob)*0
  baserunning$RE.attempt[32] <- prob*state.runs[18,10]+(1-prob)*0
  baserunning$RE.attempt[33] <- prob*state.runs[18,10]+(1-prob)*0
  
  baserunning <- baserunning[c(25:33),]
  
  
  re.safe <- c(16,16,16,17,17,17,18,18,18)
  re.out <- c(14,14,14,15,15,15,0,0,0)
  for (i in 1:9){
    for(j in c(30:100)){
      p <- j/100
      if(re.out[i]!=0){
        temp <- p*state.runs[re.safe[i],10]+(1-p)*state.runs[re.out[i],10]
      }else {temp <- p*state.runs[re.safe[i],10]}
      
      if(temp > baserunning$RE.no.attempt[i] | j==100){
        baserunning$RE.breakeven[i] <- p
        break
      }
    }
  }
  colnames(baserunning)[1] <- 'bases'
  baserunning
})


output$first_third_plot <- renderPlot({
  baserunning <- baserunning[,c(2:11)]
  baserunning$outs <- substr(baserunning$state,4,4)
  baserunning$state <- substr(baserunning$state,1,3)
  baserunning <- baserunning[,c(1,11,2:10)]
  state.runs <- state.runs2011.12.13.14
  state.runs$expected.runs <- rowMeans(state.runs[,c(2:5)])
  state.runs$one.run <- rowMeans(state.runs[,c(6:9)])
  prob <- as.numeric(input$success)
  baserunning$RE.breakeven <- 0
  
  baserunning$RE.attempt[c(1:3)] <- prob*(1+state.runs[2,10])+(1-prob)*state.runs[3,10]
  baserunning$RE.attempt[c(4:6)] <- prob*(1+state.runs[14,10])+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[c(7:9)] <- prob*(1+state.runs[8,10])+(1-prob)*state.runs[9,10]
  baserunning$RE.attempt[c(10:12)] <- prob*(1+state.runs[20,10])+(1-prob)*state.runs[21,10]
  baserunning$RE.attempt[c(13:15)] <- prob*(1+state.runs[3,10])+(1-prob)*0
  baserunning$RE.attempt[c(16:18)] <- prob*(1+state.runs[15,10])+(1-prob)*0
  baserunning$RE.attempt[c(19:21)] <- prob*(1+state.runs[9,10])+(1-prob)*0
  baserunning$RE.attempt[c(21:24)] <- prob*(1+state.runs[21,10])+(1-prob)*0
  
  baserunning$RE.attempt[25] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
  baserunning$RE.attempt[26] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
  baserunning$RE.attempt[27] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
  baserunning$RE.attempt[28] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[29] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[30] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
  baserunning$RE.attempt[31] <- prob*state.runs[18,10]+(1-prob)*0
  baserunning$RE.attempt[32] <- prob*state.runs[18,10]+(1-prob)*0
  baserunning$RE.attempt[33] <- prob*state.runs[18,10]+(1-prob)*0
  
  baserunning <- baserunning[c(25:33),]
  
  
  re.safe <- c(16,16,16,17,17,17,18,18,18)
  re.out <- c(14,14,14,15,15,15,0,0,0)
  for (i in 1:9){
    for(j in c(30:100)){
      p <- j/100
      if(re.out[i]!=0){
        temp <- p*state.runs[re.safe[i],10]+(1-p)*state.runs[re.out[i],10]
      }else {temp <- p*state.runs[re.safe[i],10]}
      
      if(temp > baserunning$RE.no.attempt[i] | j==100){
        baserunning$RE.breakeven[i] <- p
        break
      }
    }
  }
  colnames(baserunning)[1] <- 'bases'
  baserunning <- baserunning[c(1,4,7),]
  baserunning <- rbind(baserunning,baserunning)
  baserunning$time <- c(rep('hold',3),rep('advance',3))
  baserunning$runs.expectancy <- 0
  baserunning$runs.expectancy[1:3] <- baserunning$RE.no.attempt[1:3]
  baserunning$runs.expectancy[4:6] <- baserunning$RE.attempt[1:3]
  
  prob <- as.numeric(input$success)
  baserunning$scoring.prob <- 0
  baserunning$scoring.prob[1] <- 0.61   ## no attempt, 1st and 2nd, no outs
  baserunning$scoring.prob[2] <- 0.41   ## no attempt, 1st and 2nd, one out
  baserunning$scoring.prob[3] <- 0.23   ## no attempt, 1st and 2nd, two outs
  baserunning$scoring.prob[4] <- prob*(0.85) + (1-prob)*0.28   ## safe 1st and 3rd, no outs or man on first, one out
  baserunning$scoring.prob[5] <- prob*(0.63) + (1-prob)*0.14   ## safe 1st and 3rd, one out or man on first, two outs
  baserunning$scoring.prob[6] <- prob*(0.28) + (1-prob)*0     ## safe 1st and 3rd, two outs or inning over
  
  
  baserunning$time <- factor(baserunning$time,levels=c('hold','advance'))
  baserunning$outs <- as.character(baserunning$outs)
  baserunning$outs <- ifelse(baserunning$outs=='0','0 outs',ifelse(baserunning$outs=='1','1 out','2 outs'))
  baserunning$outs <- factor(baserunning$outs,levels=c('0 outs','1 out','2 outs'))
  
  if(input$view=='expected runs')
    {plot <- ggplot(baserunning,aes(x=time,y=runs.expectancy))+
      geom_bar(aes(fill=time),stat='identity')+facet_grid(.~outs) +
      scale_size_continuous(guide="none") +
      theme(legend.position='none') +
      ggtitle  ("Runs expectancies for advancing to third base on a single vs. holding at second base") + 
      xlab("") +
      ylab("runs expectancy remainder of inning")
  }else{plot <- ggplot(baserunning,aes(x=time,y=scoring.prob))+
          geom_bar(aes(fill=time),stat='identity')+facet_grid(.~outs) +
          scale_size_continuous(guide="none") +
          theme(legend.position='none') +
          ggtitle  ("Scoring probability for advancing to third base on a single vs. holding at second base") + 
          xlab("") +
          ylab("scoring probability remainder of inning")
  }
  return(plot)
})






output$second_home <- renderTable({
  second.to.home  <- second.to.home
  prob <- as.numeric(input$success)
  second.to.home$first <- substr(second.to.home$bases,1,1)
  second.to.home$RE.attempt <- ifelse(second.to.home$outs==0 & second.to.home$first==0,prob*(1+0.85)+
                                        (1-prob)*(0.51),
                                      ifelse(second.to.home$outs==0 & second.to.home$first==1,prob*(1+1.41)+
                                               (1-prob)*(0.87),    
                                             ifelse(second.to.home$outs==1 & second.to.home$first==0,prob*(1+0.51)+
                                                      (1-prob)*(0.23),
                                                    ifelse(second.to.home$outs==1 & second.to.home$first==1,prob*(1+0.87)+
                                                             (1-prob)*(0.43),
                                                           ifelse(second.to.home$outs==2 & second.to.home$first==0,prob*(1+0.23)+
                                                                    (1-prob)*(0),
                                                                  prob*(1+0.43)+
                                                                    (1-prob)*(0))))))
  second.to.home$RE.attempt <- round(second.to.home$RE.attempt,2)
  second.to.home <- second.to.home[,-13]
  second.to.home$RE.breakeven <- c(0.93,0.93,0.93,0.7,0.7,0.7,0.4,0.4,0.4,0.86,0.86,0.86,
                                   0.75,0.75,0.75,0.51,0.51,0.51)
  second.to.home$bases <- paste(second.to.home$bases,'*',sep='')
  second.to.home$play <- paste('S',second.to.home$play,sep='')
  second.to.home$prob.success <- input$success
  second.to.home
})

output$second_home_plot <- renderPlot({
  second.to.home  <- second.to.home
  prob <- as.numeric(input$success)
  second.to.home$first <- substr(second.to.home$bases,1,1)
  second.to.home$RE.attempt <- ifelse(second.to.home$outs==0 & second.to.home$first==0,prob*(1+0.85)+
                                        (1-prob)*(0.51),
                                      ifelse(second.to.home$outs==0 & second.to.home$first==1,prob*(1+1.41)+
                                               (1-prob)*(0.87),    
                                             ifelse(second.to.home$outs==1 & second.to.home$first==0,prob*(1+0.51)+
                                                      (1-prob)*(0.23),
                                                    ifelse(second.to.home$outs==1 & second.to.home$first==1,prob*(1+0.87)+
                                                             (1-prob)*(0.43),
                                                           ifelse(second.to.home$outs==2 & second.to.home$first==0,prob*(1+0.23)+
                                                                    (1-prob)*(0),prob*(1+0.43)+ (1-prob)*(0))))))
  second.to.home$RE.attempt <- round(second.to.home$RE.attempt,2)
  second.to.home <- second.to.home[,-13]
  second.to.home$RE.breakeven <- c(0.93,0.93,0.93,0.7,0.7,0.7,0.4,0.4,0.4,0.86,0.86,0.86,
                                   0.75,0.75,0.75,0.51,0.51,0.51)
  second.to.home$bases <- paste(second.to.home$bases,'*',sep='')
  second.to.home$play <- paste('S',second.to.home$play,sep='')
  second.to.home$prob.success <- input$success
  
  second.to.home <- second.to.home[c(1,4,7,10,13,16),]
  second.to.home <- rbind(second.to.home,second.to.home)
  second.to.home$time <- c(rep('hold',6),rep('advance',6))
  
  second.to.home$runs.expectancy <- 0
  second.to.home$runs.expectancy[1:6] <- second.to.home$RE.no.attempt[1:6]
  second.to.home$runs.expectancy[7:12] <- second.to.home$RE.attempt[1:6]
  
  prob <- as.numeric(input$success)
  second.to.home$scoring.prob <- 0
  second.to.home$scoring.prob <- 0
  second.to.home$scoring.prob[1] <- 0.85   ## no attempt, 1st and 3rd, no outs
  second.to.home$scoring.prob[2] <- 0.63   ## no attempt, 1st and 3rd, one out
  second.to.home$scoring.prob[3] <- 0.28   ## no attempt, 1st and 3rd, two outs
  second.to.home$scoring.prob[4] <- 0.85   ## no attempt, bases loaded, no outs
  second.to.home$scoring.prob[5] <- 0.66   ## no attempt, bases loaded, one out
  second.to.home$scoring.prob[6] <- 0.32   ## no attempt, bases loaded, two outs
  second.to.home$scoring.prob[7] <- prob*(1) + (1-prob)*0.28   ## safe one run and runner on first with no outs or no runs and runner on first one out
  second.to.home$scoring.prob[8] <- prob*(1) + (1-prob)*0.14   ## safe one run and runner on first with one out or out no runs and runner on first two outs
  second.to.home$scoring.prob[9] <- prob*(1) + (1-prob)*0     ## safe one run and runner on first with two outs or inning over
  second.to.home$scoring.prob[10] <- prob*(1) + (1-prob)*0.41     ## safe one run and runners on first and second no outs or out and runners on first and second one out
  second.to.home$scoring.prob[11] <- prob*(1) + (1-prob)*0.23     ## safe one run and runners on first and second one out or out and runners on first and second two outs
  second.to.home$scoring.prob[12] <- prob*(1) + (1-prob)*0     ## safe one run and runners on first and second two outs or out inning over
  
  
  second.to.home$time <- factor(second.to.home$time,levels=c('hold','advance'))
  second.to.home$state <- paste(second.to.home$bases,second.to.home$outs,sep='')
  second.to.home$state <- factor(second.to.home$state,levels=c('01*0','01*1','01*2','11*0','11*1','11*2'))
  
  if (input$view == 'expected runs')
  {plot <- ggplot(second.to.home,aes(x=time,y=runs.expectancy))+
    geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
    scale_size_continuous(guide="none") +
    theme(legend.position='none') +
    ggtitle  ("Runs expectancies for scoring from second on a single vs. holding at third base") + 
    xlab("state") +
    ylab("runs expectancy remainder of inning")
  }else {plot <- ggplot(second.to.home,aes(x=time,y=scoring.prob))+
           geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
           scale_size_continuous(guide="none") +
           theme(legend.position='none') +
           ggtitle  ("Scoring probability for scoring from second on a single vs. holding at third base") + 
           xlab("state") +
           ylab("scoring probability remainder of inning")
        }
  return(plot)
})







output$stolen.bases <- renderTable({
  runners <- c(rep('1st',3),rep('2nd',3),rep('1st and 2nd',3),rep('1st and 2nd',3),rep('1st and 3rd',3))
  outs <- rep(c(0,1,2),5)
  play <- c(rep('straight steal',6),rep('double steal',3),rep('single steal 3rd',3),rep('single steal 2nd',3))
  state.runs <- state.runs2011.12.13.14
  state.runs$expected.runs <- rowMeans(state.runs[,c(2:5)])
  state.runs$one.run <- rowMeans(state.runs[,c(6:9)])
  prob <- as.numeric(input$success)
  stolen.bases.df <- as.data.frame(cbind(runners,outs,play))
  stolen.bases.df$start.RE <- 0
  stolen.bases.df$end.RE <- 0
  stolen.bases.df$RE.breakeven <- 0
  stolen.bases.df$start.gte.1 <- 0
  stolen.bases.df$end.gte.1 <- 0
  stolen.bases.df$gte.1.breakeven <- 0
  stolen.bases.df$start.RE[1] <- state.runs[13,10]
  stolen.bases.df$start.RE[2] <- state.runs[14,10]
  stolen.bases.df$start.RE[3] <- state.runs[15,10]
  stolen.bases.df$start.RE[4] <- state.runs[7,10]
  stolen.bases.df$start.RE[5] <- state.runs[8,10]
  stolen.bases.df$start.RE[6] <- state.runs[9,10]
  stolen.bases.df$start.RE[7] <- state.runs[19,10]
  stolen.bases.df$start.RE[8] <- state.runs[20,10]
  stolen.bases.df$start.RE[9] <- state.runs[21,10]
  stolen.bases.df$start.RE[10] <- state.runs[19,10]
  stolen.bases.df$start.RE[11] <- state.runs[20,10]
  stolen.bases.df$start.RE[12] <- state.runs[21,10]
  stolen.bases.df$start.RE[13] <- state.runs[16,10]
  stolen.bases.df$start.RE[14] <- state.runs[17,10]
  stolen.bases.df$start.RE[15] <- state.runs[18,10]
  stolen.bases.df$start.gte.1[1] <- state.runs[13,11]
  stolen.bases.df$start.gte.1[2] <- state.runs[14,11]
  stolen.bases.df$start.gte.1[3] <- state.runs[15,11]      
  stolen.bases.df$start.gte.1[4] <- state.runs[7,11]
  stolen.bases.df$start.gte.1[5] <- state.runs[8,11]
  stolen.bases.df$start.gte.1[6] <- state.runs[9,11]      
  stolen.bases.df$start.gte.1[7] <- state.runs[19,11]
  stolen.bases.df$start.gte.1[8] <- state.runs[20,11]
  stolen.bases.df$start.gte.1[9] <- state.runs[21,11]      
  stolen.bases.df$start.gte.1[10] <- state.runs[19,11]
  stolen.bases.df$start.gte.1[11] <- state.runs[20,11]
  stolen.bases.df$start.gte.1[12] <- state.runs[21,11]      
  stolen.bases.df$start.gte.1[13] <- state.runs[16,11]
  stolen.bases.df$start.gte.1[14] <- state.runs[17,11]
  stolen.bases.df$start.gte.1[15] <- state.runs[18,11]
  
  stolen.bases.df$end.RE[1] <- prob*state.runs[7,10]+(1-prob)*state.runs[2,10]
  stolen.bases.df$end.RE[2] <- prob*state.runs[8,10]+(1-prob)*state.runs[3,10]
  stolen.bases.df$end.RE[3] <- prob*state.runs[9,10]+(1-prob)*0
  stolen.bases.df$end.RE[4] <- prob*state.runs[4,10]+(1-prob)*state.runs[2,10]
  stolen.bases.df$end.RE[5] <- prob*state.runs[5,10]+(1-prob)*state.runs[3,10]
  stolen.bases.df$end.RE[6] <- prob*state.runs[6,10]+(1-prob)*0
  stolen.bases.df$end.RE[7] <- prob*state.runs[10,10]+(1-prob)*state.runs[8,10]
  stolen.bases.df$end.RE[8] <- prob*state.runs[11,10]+(1-prob)*state.runs[9,10]
  stolen.bases.df$end.RE[9] <- prob*state.runs[12,10]+(1-prob)*0
  stolen.bases.df$end.RE[10] <- prob*state.runs[16,10]+(1-prob)*state.runs[14,10]
  stolen.bases.df$end.RE[11] <- prob*state.runs[17,10]+(1-prob)*state.runs[15,10]
  stolen.bases.df$end.RE[12] <- prob*state.runs[18,10]+(1-prob)*0
  stolen.bases.df$end.RE[13] <- prob*state.runs[10,10]+(1-prob)*state.runs[5,10]
  stolen.bases.df$end.RE[14] <- prob*state.runs[11,10]+(1-prob)*state.runs[6,10]
  stolen.bases.df$end.RE[15] <- prob*state.runs[12,10]+(1-prob)*0
  
  stolen.bases.df$end.gte.1[1] <- prob*state.runs[7,11]+(1-prob)*state.runs[2,11]
  stolen.bases.df$end.gte.1[2] <- prob*state.runs[8,11]+(1-prob)*state.runs[3,11]
  stolen.bases.df$end.gte.1[3] <- prob*state.runs[9,11]+(1-prob)*0
  stolen.bases.df$end.gte.1[4] <- prob*state.runs[4,11]+(1-prob)*state.runs[2,11]
  stolen.bases.df$end.gte.1[5] <- prob*state.runs[5,11]+(1-prob)*state.runs[3,11]
  stolen.bases.df$end.gte.1[6] <- prob*state.runs[6,11]+(1-prob)*0
  stolen.bases.df$end.gte.1[7] <- prob*state.runs[10,11]+(1-prob)*state.runs[8,11]
  stolen.bases.df$end.gte.1[8] <- prob*state.runs[11,11]+(1-prob)*state.runs[9,11]
  stolen.bases.df$end.gte.1[9] <- prob*state.runs[12,11]+(1-prob)*0
  stolen.bases.df$end.gte.1[10] <- prob*state.runs[16,11]+(1-prob)*state.runs[14,11]
  stolen.bases.df$end.gte.1[11] <- prob*state.runs[17,11]+(1-prob)*state.runs[15,11]
  stolen.bases.df$end.gte.1[12] <- prob*state.runs[18,11]+(1-prob)*0
  stolen.bases.df$end.gte.1[13] <- prob*state.runs[10,11]+(1-prob)*state.runs[5,11]
  stolen.bases.df$end.gte.1[14] <- prob*state.runs[11,11]+(1-prob)*state.runs[6,11]
  stolen.bases.df$end.gte.1[15] <- prob*state.runs[12,11]+(1-prob)*0
  
  re.safe <- c(7,8,9,4,5,6,10,11,12,16,17,18,10,11,12)
  re.out <- c(2,3,0,2,3,0,8,9,0,14,15,0,5,6,0)
  gte.safe <- c(7,8,9,4,5,6,10,11,12,16,17,18,10,11,12)
  gte.out <- c(2,3,0,2,3,0,8,9,0,14,15,0,5,6,0)
  
  for (i in 1:15){
    for(j in c(30:100)){
      p <- j/100
      if(re.out[i]!=0){
        temp <- p*state.runs[re.safe[i],10]+(1-p)*state.runs[re.out[i],10]
      }else {temp <- p*state.runs[re.safe[i],10]}
      
      if(temp>stolen.bases.df$start.RE[i] | j==100){
        stolen.bases.df$RE.breakeven[i] <- p
        break
      }
    }
  }
  
  for (i in 1:15){
    for(j in c(30:100)){
      p <- j/100
      if(gte.out[i]!=0){
        temp <- p*state.runs[gte.safe[i],11]+(1-p)*state.runs[gte.out[i],11]
      }else {temp <- p*state.runs[gte.safe[i],11]}
      
      if(temp>stolen.bases.df$start.gte.1[i] | j==100){
        stolen.bases.df$gte.1.breakeven[i] <- p
        break
      }
    }
  }     
  stolen.bases.df[c(1:15),]
})

})
