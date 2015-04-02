library(shiny)
#install.packages("googleVis")
require(rCharts)
#options(RCHART_WIDTH = 800)
#require(ggplot2)
#require(plyr)
#library(Lahman)
#library(ggplot2)
#library(rCharts)
#options(RCHART_LIB = 'polycharts')

shinyUI(pageWithSidebar(
  
  headerPanel(h5("Baseball Runs Potential Analysis by Mark Malter")),
  
  #sidebarLayout(
    sidebarPanel(      
      br(),
      textInput("name","First and last name:",value="Miguel Cabrera"),
      br(),
      sliderInput('success','Base running success rate:', min=0.00, max=1.00,value=c(0.70), step=0.01),
      br(),
      radioButtons('view','Base running graphics view:',c('expected runs','scoring probability'),inline=TRUE),
      br(),
      selectInput('year',"Year:",c(2014,2013,2012,2011),selectize=F,multiple=F),
      br(),
      selectInput("innings","Innings:",c('All game','1-3','4-6','7-extras'),selectize=F,multiple=F),
      br(),
      selectInput("run.diff","Run differential at time of at bat:",c('All','0-1','2-3','4 or more'),selectize=F,multiple=F),
      br(),
      selectInput("position","Position:",
                  c("All","C","1B or DH","2B","3B","SS","OF"),selectize=F,multiple=F),
      br(),
      selectInput('team','Team:',c("All","A's","Angels","Astros","Blue Jays","Braves","Brewers","Cardinals","Cubs","Dbacks",    
                                   "Dodgers","Giants","Indians","Mariners","Marlins","Mets","Nationals","Orioles","Padres",    
                                   "Phillies","Pirates","Rangers","Rays","Red Sox","Reds","Rockies","Royals","Tigers","Twins",     
                                   "White Sox","Yankees"),selectize=F,multiple=F),
      selectInput("bats","Bats:",c("All","R","L","Switch"),selectize=F,multiple=F),

      br(),
      
      sliderInput("age","Age range: (for 'Runs Created' tab)",
                  min=20,max=45,format="###",
                  value=c(20,45),step=1),
      
      
      br(),
      sliderInput("weight","Weight range: (for 'Runs Created' tab)",
                  min=150,max=300,format="###",
                  value=c(150,300),step=10),
            
      br()
         
    ),
  

  
    
    mainPanel(
      tabsetPanel(type="tab",
                  
                  tabPanel("Introduction",HTML("<div><b>Please allow about one minute for the data to download before you move to another tab,
                          unless you want to first just read the explanation summaries.</b><br></br>
                          check out my Baseball wins probability app at <a href= 'https://malter61.shinyapps.io/gamePredictor/'>Baseball Game Probability Calculator</a><br></br> 
                          After reading the book, Analyzing Baseball with R, by Max Marchi and Jim Albert, 
                          I decided to take and expand on some of their ideas relating to runs created.  
                          I downloaded the Retrosheet play-by-play data for every game played in the 2011-2014 seasons in every park. For every play,
                          they list the batterID, pitch sequence, last ball/strike count before the event, and the play.  
                          Some examples from the several hundred thousand play events are:<br></br>  
                          CS2(26), which means 'caught stealing at second base, catcher to shortstop' 
                          <br></br>S9/L.1-H(E9/TH)(UR)(NR);B-3, which means 'line single to right field, 
                          runner on first scores on a throwing error by the right fielder.  Unearned run and no RBI'  <br></br>  
                          S9/G.2-H;1-3(THH), which means 'single to right on the ground, runner
                          on second scores, runner on first takes third on throw home', and 
                          <br></br>8/F., meaning a 'fly out to center'.<br></br>  I wrote some R code to parse through 
                          every play, to track the number of outs on the play in order to know the outs in the inning for each at bat, 
                          to track base runners so I would know which bases were occupied during each at bat, and to track runs scored prior 
                          to the at bat through the remainder of the inning.  That way, I could aggregate and take the mean of the number of runs 
                          scored after every one of the 24 possible bases/outs states in order to get the expected number of runs to be 
                          scored from each state. I also tracked the inning and score and therefore the current run differential for each plate appearance
                          in order to calculate statistics in terms of situation, i.e., who excels in tight games or in late innings 
                          (see the tabs on the left).
   
                          I took Marchi and Albert's idea of runs expectancy 
                          vs. each of the 24 states (see 'Potential Runs by State' tab for a further explanation) 
                          that they had created for the 2002-2003 seasons, and I 
                          created my own for the 2011-2014 seasons. Additionally, I calculated for each state the probability that there
                          will be at least one run scored in the remainder of the inning, which could be valuable in the late
                          innings of a tight game.

                          I merged these files with the Lahman Master, Fielding, and Salaries databases to get a breakdown of variables like salary, position,
                          player age, batting side, and years experience.  <br></br>
                          In the following tab, 'Potential runs by state', I show a table of the runs expectancy and probability of scoring 
                          for each of the 24 states.
                          I then use the table values to calculate the value of each at bat for all players, as we move from one state to the next.
                          <br</br>The following two tabs show in graphical and table form the runs created by players with more than 400 plate 
                          appearances in the selected season.  In these two tabs, and also the 'Player Stats' tab, the user can interactively play
                          with the selections on the left hand side to view stats by year, by portion of game (early, middle, or late innings),
                          by run differential (does the player perform better in tight games than in lopsided ones).
                          In the 'Runs Created by All MLB Players' tab, you can also view the results by selected position of interest.<br></br>
                          There are four tabs showing base running expectancies (base stealing, tagging from third on a fly ball, going first to
                          third, or scoring from second on a single to the outfield.  I show that in general base runners and third base coaches are far too cautious and often 
                          fail to maximize their run scoring potential.  As for stolen bases, a general rule of thumb is that a 70% success rate is 
                          required in order to make the attempt worthwhile, but I break those breakeven levels down by all of the 15 common
                          stealing bases/outs states to show how they differ. I also show the breakeven success rates for going first to third on a 
                          single, and for tagging up from third on a fly ball to the outfield. <br></br>
                          The user can play with the 'Base running success rate' box to the left to see the end.RE (end runs expectancy after the play) 
                          and end.gte.1 (greater than or equal to 1 run at the end of the play) for any base running attempt with a given state. <br></br>
                          Please play around with the app and enjoy.  Feel free to provide feedback to malter61@gmail.com <br></br>
                          Mark
                          </div>")),
                                   
                  tabPanel("Potential runs by bases/outs state",HTML("<div> Runs Expectancy and Probability of Scoring at Least One Run in the 
                            Remainder of the Inning from Given Base/Outs State.  Calculations are based on the four seasons from 2011-2014.  </div>"),
                           tableOutput("state"),
                           HTML
                           ("<div><br></br>
                            run.exp - runs expectancy (RE) over the remainder of the inning from the given state.<br></br>
                            prob - probability of scoring at least one run in the remainder of the inning from the given state.<br></br>
                            For any plate appearance in baseball there are 8 potential base states, 
                            ranging from bases empty to bases loaded.  Combined with the three potential out states (0,1,or 2),
                            there are 24 potential bases/outs states, and each has a unique value for expected runs over the 
                            remainder of the inning, as well as the probability of scoring at least one run in the remainder 
                            of the inning. I examined every
                            plate appearance from the past four MLB seasons (2011-2014), and calculated the 'value' of each state
                            by aggregating and averaging both the runs scored in the remainder of the inning, and calculating
                            the probability that at least one run will be scored (useful information in late innings of tight games).
                            The table is read as follows: <br></br>'bases': the three characters represent whether first, second, or third bases are 
                            occupied, (e.g., '011' means runners on second and third). <br></br>'The values in the rest of the cells represent either the  
                            expected number of runs to be scored in the remainder of the inning or the probability that at least 
                            one run will be scored, since in late innings of tight games it is often more important to focus on scoring 
                            one run than having a big inning. 
                            <br></br>Example: row 2/0 outs (state 1000) shows an RE of 0.85 runs with a probability of 0.42 of scoring at least one 
                            run.  The value of the at bat is calculated by the following formula:<br></br>
                            Runs created (RC) = final state - start state + runs scored on play<br></br>
                            So if the at bat gets to a state of 1001 (RE = 0.51), the at bat had an RC of (0.51 - 0.85 + 0) = -0.34.<br></br>  
                            If the batter doubles in a run,
                            the at bat had an RC of (1.10 - 0.85 + 1) = 1.25<br></br>This method of scoring at bats does a much better job than simply 
                            batting average or on base percentage, since an out where base runners advance is better than one in which there is no advancement.</div>")),
                  

                  tabPanel("Runs Created All Regular MLB Players",
                              HTML("<div>Includes all players with at least 400 plate appearances in the selected year</div>"),
                              showOutput("chart1","polycharts"),
                                HTML("<div><br></br><b>Hold cursor over dot to see player information<br></br>
                                Play with filters at left for more selective viewing.  </b><br></br>
                                This interactive chart takes every MLB player who had at least 400 plate appearances in the chosen season, and 
                                according to the user inputs selected on the left 
                                hand side, aggregates all of his potential for creating runs.  For example, in 2014 the dot in the 
                                top right shows that Mike Trout created 58 runs on 358 potential chances.  
                                The values are the accumulated potentials and runs created from every one of his plate appearances.  
                                <br></br>Example: Mike Trout batted with bases loaded and one out (state 1111), which has a value 
                                of 1.51 runs (see 'Potential Runs by State' tab).  He clears the bases with a double, 
                                going to state 0101, which has a value of 0.66 runs.  The value of his at bat was
                                RC = 0.66 - 1.51 + 3 = 2.15.<br></br>
                                Feel free to play with the filters to see players by position, how they perform in tight games as defined by the 
                                score differential at the time of the at bat, or how they perform in different stages of the game.  
                                You can also filter by age and weight by making use of the slider switches.<br></br>
                                Interestingly, one of my favorite players, Ichiro Suzuki, created -21, -21, and -11 runs 
                                respectively in the 2011, 2012, and 2013
                                seasons, indicating that he was over rated. <br></br>
                                The idea for this plot came from Analyzing Baseball Data with R, by Max Marchi and Jim Albert, chapter 5, 
                                The value of Player Using Runs Expectancy.
                                </div>")),
 
                  
                  tabPanel("Player Runs Table",
                           HTML("<div> <b>Player Runs Created Table: Enter player name in box to the left.</b>  </div>"),
                                tableOutput("player.table"),
                                HTML("<div><b>If you do not see any statistics then your player did not have 400 plate appearances 
                                in your chosen year.  Please choose another year or another player.<br></br>
                                Play with filters at left for more selective viewing.</b><br></br>
                                This view allows you to enter any player by season, 
                                innings, and/or run differential, 
                                and view the plate appearances, potential, and runs created by all eight possible base running states.<br></br>
                                Example: In 2014, Miguel Cabrera created a total of 39.8 runs, 12 each coming with bases empty and with a runner on first.
                                His poorest performance was with runners on first and third (-4.2).   
                                Of his 691 plate appearances, 395 came with the bases empty.  
                                <br></br>Even though he won the triple crown in 2012, he actually created more runs in 2013 (74.7 vs. 47.3), 
                                partly because he was better in 2013 with the bases loaded.
                                <br></br>
                                     </div>")),
                  
                  tabPanel("Stolen Bases Graphic",plotOutput("chart2"),
                                HTML("<div><p><br></br><b>Select 'Base running graphics' view' and 'Base running success rate' on left.</b></p>
                                  <p>Notice how the 'end' values change as you change the 'Base running success rate'</p>
                                  <p>This plot shows the runs expectancy (or probability of scoring at least one run, depending on 'plots view' choice) 
                                  over the remainder of the inning for all possible situations 
                                  before a stolen base attempt, and
                                  given the base running success rate, after a stolen base attempt.</p>
                                  <p><b>States:</b></p>
                                  <p>First character: first base occupied = 1; first base unoccupied = 0</p>
                                  <p>Second character: second base occupied = 1; second base unoccupied = 0</p>
                                  <p>Third character: Third base occupied = 1; third base unoccupied = 0</p>
                                  <p>Fourth character: Number of outs</p>
                                  <p>Fifth character: Steal type: 1 = straight steal, 2 = double steal, 3 = single steal of third base in 1st and 2nd situation</p>
                                  <p><b>Example:</b> state '11012' indicates runners on 1st and 2nd, one out, double steal</p>
                                  <p>On an unsuccessful attempt, I assume the lead runner is thrown out and any trail runner is safe (if he attempted to steal).</p>
                                  </div>")),
                  
                  tabPanel("Stolen Bases Table",
                                HTML("<div> Stolen Bases Table.  Play with the base running success rate box on the left to view 
                                the various expected end states.</div>"),
                                tableOutput("stolen.bases"),
                                HTML("<div>column names:<br></br>
                                start.RE: runs expectancy at the start of the play<br></br>
                                end.RE: runs expectancy after the attempted steal and the expected success rate.<br></br>
                                RE.break.even: the minimum success rate for the play to have a positive expected gain in runs expectancy.<br></br>
                                start.gte.1: probability of scoring greater than or equal to (gte) one run in the remainder of the inning at the 
                                start of the play.<br></br>
                                end.gte.1: probability of scoring at least one run after the play, given the success rate.<br></br>
                                gte.1.breakeven: the minimum success rate for the play to have an increase in the probability of
                                scoring at least one run in the remainder of the inning.<br></br>

                                Play with different values in the 'Base running Success Rate' box to see how the end.RE and end.gte.1 values change.
                                Conventional wisdom says that a base stealer should be successful around 70% of the time in 
                                order for the attempt to be worthwhile.  However, I broke it down to every possible state and calculated the expected 
                                gain/loss from every attempt, based on the expected success rate (play with the user tab on left).  
                                At any given success rates, the end.RE and end.gte.1 are calculated, as well as the breakeven rates. <br></br>
                                Interesting observations: With runners on first and second with one out, a double steal needs a success rate of
                                only 54% to be viable; whereas a single steal of third with first and second and two outs requires 
                                a breakeven of 91% to be viable.  <br></br>
                                Notes: I am assuming that the catcher always tries to get the lead runner in a double steal and that in a 
                                first and third situation, the runner on third does not attempt to score on the throw to second.
                                <br></br></div>")), 
                  
                  tabPanel("Tag From Third Graphic",
                           plotOutput('tag_from_third_plot'),
                           HTML("<div><p><p><br></br><b>Select 'Base running graphics' view' and 'Base running success rate' on left.</b></p>
                               For the 'tag from third' analysis, we're examining all plays which had a runner attempt to score from 
                               third base on a fly ball caught by an outfielder. Since there was just a catch, there will always be either one out 
                               or two outs on the tag play.</p>
                               <p>The runner's decision whether to tag depends on not only the outs, but also on whether there are runners on the other two bases,
                               so we include all possible bases/outs states in our analysis.  Notice the significant difference between the breakeven levels when 
                               the catch is the first out vs. the second out.</p> 
                               
                               <p>Play with the 'base running success rate' to see how the runs expectancies change in the 'advance' columns.
                                Note that the break even success rate is highly dependent on the number of outs, but also depends on whether there 
                                is a trailing runner.</p>
                                <p><b>States:</b></p>
                                <p>First character: first base occupied = 1; first base unoccupied = 0</p>
                                <p>Second character: second base occupied = 1; second base unoccupied = 0</p>
                                <p>Third character: Third base occupied = 1; third base unoccupied = 0</p>
                                <p>Fourth character: Number of outs</p>
                                <p>Example: state '0111' indicates runners on 2nd and third and one out</p>
                                <p>On an unsuccessful attempt, I assume the runner is out at the plate and any other runners hold their bases.  
                                On a successful attempt, I assume the runner scores and any other runners hold their bases.</p>
                                </div>")),
                               
                  
                  tabPanel("Tag From Third",
                                HTML("<div>Analysis of runners tagging from third base on a fly ball. 
                                Play with the base running success rate box on the left to view 
                                the various expected end states.</div>"),
                                tableOutput("tag"),HTML("<div>column names:<br></br>
                                count: count of the number of plays over seasons 2011-2014 <br></br>
                                attempts: Times the runner attempted to score.<br></br>
                                safe: Times the runner attempted to score and was safe.<br></br>
                                out: Times the runner attempted to score and was out.<br></br>
                                RE.no.attempt: The runs expectancy if the runner does not attempt to score.<br></br>
                                RE.attempt: The runs expectancy if the runner does attempt to score.<br></br>
                                RE.break.even: The success rate required for the runner to attempt to score.<br></br>
                                Here we're looking at every play where there was a runner on third with less than two outs, 
                                and a fly ball was caught by an outfielder. <br></br>For example, 495 times there was fly ball to CF 
                                with runners on first and third. On 444 of those plays the runner on third attempted to score.
                                Of those plays, he was safe 429 times.  <br></br>
                                I find it interesting that runners are safe more than 95% of the time on such plays, indicating that they are
                                probably too cautious (tagging only when they're near certain to be safe). By examining the breakeven success rates,
                                we see that in many cases they would have a positive runs created gain with a success rate below 50%, 
                                particularly in one out situations (the catch would be out number two).<br></br>
                                For example, a runner on third with one out.  If he does not attempt to tag, he remains there with 
                                two outs and gives his team only a 0.36 runs expectancy over the remainder of the inning.  If he has greater
                                than a 36% chance of beating the tag, he should go for it.  
                                Given that the success rates are above 95%, this shows that most
                                runners are far too cautious.
                                Incidentally, I first got this thought while watching game 
                                seven of the 2014 world series.  Down by one run with two outs, Alex Gorgon singled to center.  
                                CF Gregor Blanco misplayed the ball into a triple, allowing Gordon to reach third base.  
                                He was held by the coach as the shortstop took the relay throw, leaving a 26% chance of scoring later in the inning 
                                (see the last cell in row four of the 'Potential Runs by state' tab).  Had Gordon tried to score, 
                                he likely would have been thrown out at the plate, but still I wondered if his odds would have been 
                                better (bad throw) than leaving
                                Salvador Perez to bat against Madison Baumgarner. </div>")),
                  
                  tabPanel("First to Third Graphic",
                           plotOutput('first_third_plot'),
                           HTML("<div><p><br></br><b>Select 'Base running graphics' view' and 'Base running success rate' on left.</b></p>
                              For the 'first to third' analysis, we're examining all plays which had a runner attempt to advance 
                              from first to third on a single.  We care only about plays where there was no runner on second 
                               base so we don't corrupt our calculations with runners advancing to third on a throw home. 
                               As for a runner on third to start the play, we assume that he would have already scored and will not influence the 
                               decision of the runner from first base.  So essentially these are all plays with a runner on first, nobody on second,
                               and we don't care about third.</p>
                               <p>Play with the 'base running success rate' to see how the runs expectancies change in the 'advance' columns.
                                Note that the break even success rate is roughly 0.72-0.73 when there are 0 or 1 out, but a much greater 0.89 with two outs.
                                </p>
                                <p>This rebuts the adage of never making the first or third out at third base. That appears to hold true regarding not
                                making the third out, but with no outs it's ok to be more aggressive.</p>
                                <p>I assume that the batter makes no attempt for second base on the throw to third.</p></div>")),
                  
                  tabPanel("First to Third Table",
                                HTML("<div>Analysis of runners attempting to go from first to third on a single. 
                                Play with the base running success rate box on the left to view 
                                the various expected end states.</div>"),
                                tableOutput("first_third"),
                                HTML("<div>column names:<br></br>
                                count: count of the number of plays over seasons 2011-2014 <br></br>
                                attempts: Times the runner on first attempted to reach third base on a single to the outfield.<br></br>
                                safe: Times the runner attempted to go from first to third and was safe.<br></br>
                                out: Times the runner attempted to go from first to third and was out.<br></br>
                                RE.no.attempt: The runs expectancy if the runner does not attempt to go to third base.<br></br>
                                RE.attempt: The runs expectancy if the runner does attempt to score.<br></br>
                                RE.break.even: The success rate required for the runner to attempt to reach third base.<br></br>
                                Here we're looking at every play where there was a runner on first base and a single was hit to the outfield.  I excluded all situations
                                where there was a runner on second because I didn't want a potential play at the plate to skew my calculations.  I don't care whether or 
                                not here was a runner on third because I assume he would have scored and been out of the way of a potential first to third.
                                That is why the bases state always has a 0 for a runner on second and an * for the runner on third.<br></br>
                                There is an old adage in baseball that you should never make the first or third out at third base.  However, here while the data shows that 
                                runners should be cautious in attempting to reach third base with two outs, it's not necessarily true with no outs.  In fact, the breakeven
                                rate with zero outs (73%) is only 1% higher than the rate with one out. Additionally, with zero or one out, if there is greater than about a 73%
                                chance for success, the runner should be aggressive.  Given that the success rates are above 95%, this shows that most
                                runners are too cautious.
                                </div>")),
                  
                  tabPanel("Second to Home Graphic",
                           plotOutput('second_home_plot'),
                           HTML("<div><p><p><br></br><b>Select 'Base running graphics' view' and 'Base running success rate' on left.</b></p>
                               For the 'second to home' analysis, we're examining all plays which had a runner attempt to score from 
                               second base on a single. We assume that any runners on third base before the play would have scored, 
                               so they would have no influence on our runner's decision. However, whether or not there was also a runner on first base
                               does matter, so we examine all cases with a runner on second, with and without a runner on first.</p>
                               <p>Play with the 'base running success rate' to see how the runs expectancies change in the 'advance' columns.
                                Note that the break even success rate is highly dependent on the number of outs, but also depends on whether there 
                                is a trailing runner.</p>
                                <p><b>States:</b></p>
                                <p>First character: first base occupied = 1; first base unoccupied = 0</p>
                                <p>Second character: second base occupied = 1; second base unoccupied = 0</p>
                                <p>Third character: We're not concerned with any runners on third before the play, since we assume they'll score on a single</p>
                                <p>Fourth character: Number of outs</p>
                                <p>Example: state '11*0' indicates runners on first and 2nd and no outs</p>
                                <p>I assume that on the throw home, the runner from first remains on second base and the batter remains at first base.</p>
                                </div>")),

                  
                  tabPanel("Second to Home Table",
                           HTML("<div>Analysis of runners attempting to score from second base on a single. 
                                Play with the base running success rate box on the left to view 
                                the various expected end states.</div>"),
                           tableOutput("second_home"),
                  HTML("<div>column names:<br></br>
                                count: count of the number of plays over seasons 2011-2014 <br></br>
                                attempts: Times the runner attempted to score from second base on a single to the outfield.<br></br>
                                safe: Times the runner attempted to score and was safe.<br></br>
                                out: Times the runner attempted to score and was out.<br></br>
                                RE.no.attempt: The runs expectancy if the runner does not attempt to score.<br></br>
                                RE.attempt: The runs expectancy if the runner does attempt to score.<br></br>
                                RE.break.even: The success rate required for the runner to attempt to score.<br></br>
                                Here we're looking at every play where there was a runner on second base and a single was hit to the outfield.  I ignored whether or
                                not there was a runner on third, since I assume he would have scored and would not interfere with the second base runner's
                                decision to attempt to score.  In order to keep things as simple as possible, I assume that the runner on first will not attempt to 
                                go to third on the throw home, and the batter will not attempt second base on the throw home. <br></br>As was in the case 
                                of tagging up from third base on a fly ball, here we notice that the old adage,
                                'don't ever make the first or second out at the plate' holds well.  The breakeven thresholds drastically decrease as the number of outs increases.
                                A point of interest here is that the success rates appear to be independent of the outs, whereas I would have expected less aggressiveness
                                (fewer attempts) with no outs.  Also, with no outs and no runner on first, the success rates match closely with the breakeven rates, but with one out 
                                or two outs, there is still room for more aggressiveness, since the breakeven rates are lower.
                                </div>")),               
                  
                  tabPanel("Sacrifice Bunt Graphic",plotOutput("sac.bunt.chart"),
                           HTML("<div><p><br></br><b>Select 'Base running graphics view' and 'Base running success rate' on left.</b></p>
                                  <p>Notice how the 'end' values change as you change the 'base running success rate'</p>
                                  <p>This plot shows the runs expectancy (or probability of scoring at least one run, depending on 'plots view' choice) 
                                  over the remainder of the inning for all possible situations 
                                  before a sacrifice bunt attempt, and
                                  given the base running success rate, after a sacrifice bunt attempt.</p>
                                  <p><b>States:</b></p>
                                  <p>First character: first base occupied = 1; first base unoccupied = 0</p>
                                  <p>Second character: second base occupied = 1; second base unoccupied = 0</p>
                                  <p>Third character: Third base = 0 (we're assuming that we'll only bunt with third base unoccupied</p>
                                  <p>Fourth character: Number of outs</p>
                                  <p><b>Example:</b> state '1000' indicates a runner on 1st base with one out</p>
                                  <p>I assume that on any unsuccful sacrifice bunt, the lead runner is thrown out, any other runners advance one base, and the 
                                  batter is safe at first base.  On a successful sacrifice bunt, all runners advance one base and the batter is out at first.</p>
                                  </div>")),
                  
                  tabPanel("Sacrifice Bunt Table",tableOutput('sac.bunt'),
                           HTML("<div><b>Other than with a weak hitting pitcher, sacrifice bunts should ALMOST NEVER
                                be used.</b><br></br>Here we reproduce the pertinent rows of the Potential Runs by State table.
                                The main sacrifice situations are with a runner on first, runners on first and second, or a runner on second, all 
                                with less than two outs.  By viewing the table, we can see that in every one of these situations, even with a 100% success
                                rate, we go to a state with a lower runs expectancy.<br></br>
                                Example: A runner on first and no outs has an RE of 0.85.  A successful sacrifice gets us to a runner on second 
                                with one out, and an RE of only 0.66.  Additionally, the probability of scoring at least on run in the remainder 
                                of the inning is decreased from 0.42 to 0.40.  In fact, there is only one situation on the table
                                where we go to a higher probability with a successful bunt.  With a runner on second and no outs, getting the runner to 
                                third base with one out, increasing the probability of scoring from 0.61 to 0.65.  But even here, this modest increase goes 
                                negative when we account for just a few times where the runner is out at third, getting us to a state with a runner 
                                on first and one out.<br></br>
                                </div>")),
                  
                  tabPanel("Player Stats",tableOutput("player.stats"),
                                HTML("<div><b>If you do not see any statistics then your player did not have 400 plate appearances 
                                in your selected year.  Please choose another year or another player.<br></br></b>Enter any player's name in the text box to the left, select year, 
                                run differential, and innings,
                                and view the player's stats for those selections. This is a good way to see how players perform in late innings vs. the early innings, and 
                                also in close games vs. blow outs.  In addition to the usual statistics, I threw in salary and a newly derived variable, 
                                standardized salary (std. sal), defined as the (player's salary - league mean salary) / league standard deviation salary.  For the 'league', I chose 
                                to include only players with a minimum of 400 plate appearances.  For example, in 2011 Derek Jeter earned $14.7 million dollars, which was 1.54 
                                standard deviations above the mean. 
                                <br></br>
                                </div>"))

      )
    )
                
  ))


