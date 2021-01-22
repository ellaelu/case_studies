df_seeds <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WNCAATourn eySeeds.csv")
df_tour <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WNCAATourn eyCompactResults.csv")
df_reg <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WRegularSea sonCompactResults.csv")
teams <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WTeams.csv")
#WGameCities <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WGameCities. csv")
####################### Data #######################
scores_t <- df_tour$WScore
wins_t <- c(rep("Tournament",length(scores_t))) scores_r <- df_reg$WScore
wins_r <- c(rep("Regular",length(scores_r)))
my_t <- cbind(scores_t,wins_t) my_r <- cbind(scores_r,wins_r) my_df <- rbind(my_t,my_r)
my_df <- as.data.frame(my_df)
colnames(my_df) <- c("Score","Status")
my_df$Score <- as.numeric(as.character(my_df$Score))
ggplot(my_df, aes(Score)) + geom_density(aes(fill=factor(Status)), alpha=0.8) + labs(title="Distribution of Winning Scores",
                                                                                     x="Score",
                                                                                     y = "Density", fill="Status")
scores_t <- df_tour$LScore
loss_t <- c(rep("Tournament",length(scores_t))) scores_r <- df_reg$LScore
loss_r <- c(rep("Regular",length(scores_r)))
my_t <- cbind(scores_t,loss_t) my_r <- cbind(scores_r,loss_r) my_df <- rbind(my_t,my_r)
my_df <- as.data.frame(my_df)
colnames(my_df) <- c("Score","Status")
my_df$Score <- as.numeric(as.character(my_df$Score))
ggplot(my_df, aes(Score)) + geom_density(aes(fill=factor(Status)), alpha=0.8) + labs(title="Distribution of Lossing Scores",
                                                                                     x="Score",
                                                                                     y = "Density", fill="Status")
ggplot(df_tour, aes(WScore)) + geom_histogram(binwidth = 5,
                                              fill = "mediumturquoise", colour = "gray92",
                                              size=1) + # change binwidth
  labs(title="Histogram of Winning Scores", x = "Score",
       y = "Frequency")
ggplot(df_tour, aes(LScore)) + geom_histogram(binwidth = 5,
                                              fill = "mediumturquoise", colour = "gray92",
                                              size=1) + # change binwidth
  labs(title="Histogram of Loosing Scores", x = "Score",
       y = "Frequency")
scores_w <- df_tour$WScore
wins <- c(rep("Winner",length(scores_w))) scores_l <- df_tour$LScore
losses <- c(rep("Loser",length(scores_l)))
my_win <- cbind(scores_w,wins) my_loss <- cbind(scores_l,losses) my_df <- rbind(my_win,my_loss)
my_df <- as.data.frame(my_df)

colnames(my_df) <- c("Score","Status")
my_df$Score <- as.numeric(as.character(my_df$Score))
ggplot(my_df, aes(Status, Score)) + geom_boxplot(varwidth=T, fill="plum") + labs(x="Winner/Loser",
                                                                                 y="Score")
####################### Investigation ####################### 
df_seeds$seed_int <- substr(df_seeds$Seed, 2, 3)
df_seeds$seed_int <- as.numeric(df_seeds$seed_int)
df_seeds <- df_seeds[c("Season","TeamID","seed_int")] df_tour <- df_tour[c("Season","WTeamID","LTeamID")]
df_winseeds <- df_seeds
colnames(df_winseeds) <- c("Season","WTeamID","WSeed")
df_lossseeds <- df_seeds
colnames(df_lossseeds) <- c("Season","LTeamID","LSeed")
df_temp <- merge(df_tour,df_winseeds, by = c("Season","WTeamID")) df_concat <- merge(df_temp, df_lossseeds, by = c("Season","LTeamID")) df_concat$seed_diff <- df_concat$WSeed - df_concat$LSeed head(df_concat)
################ for Data ############### 
ggplot(df_concat, aes(seed_diff)) +
geom_histogram(binwidth = 5,
               fill = "mediumturquoise",
               colour = "gray92",
               size=1) + # change binwidth labs(title="Histogram of Seed Differences",
  x = "Seed Difference", y = "Frequency")
#########################################
# Create training data set w <- df_concat$seed_diff r <- c(rep(1, length(w)))

df_wins <- as.data.frame(cbind(w,r)) colnames(df_wins) <- c("SeedDiff","Result")
l <- -df_concat$seed_diff
r <- c(rep(0,length(l)))
df_losses <- as.data.frame(cbind(l,r)) colnames(df_losses) <- c("SeedDiff","Result")
df_predictions <- as.data.frame(rbind(df_wins,df_losses)) head(df_predictions)
reshape <- list()
index <- 1
for(p in df_predictions$SeedDiff){
  reshape[[index]] <- p
  index <- index + 1 }
X_train <- reshape
Y_train <- df_predictions$Result
train <- as.data.frame(cbind(X_train,Y_train)) train <- train[sample(nrow(train)),]
trainingData <- data.frame(unlist(train$X_train),unlist(train$Y_train)) colnames(trainingData) <- c("X_TRAIN","Y_TRAIN")
logitMod <- glm(Y_TRAIN ~ X_TRAIN, data=trainingData, family=binomial(link="logit"))
summary(logitMod)
#figure 4.1.1
ggplot(trainingData, aes(x=X_TRAIN, y=Y_TRAIN)) +
  geom_point() +
  #geom_abline(intercept = -3.335e-16, slope = -2.625e-01, color = "red") + stat_smooth(method = glm) +
  labs(y="Results",
       x="Seed Diff", title="Logistic Regression")
ncaa.res = resid(logitMod)

trainingData$residuals <- ncaa.res
trainingData$index <- c(1:length(trainingData$X_TRAIN))
#transformed residual plot
ggplot(trainingData, aes(x=index, y=residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") + labs(y="Residual",
                                                   x="Index", title="Residual Plot")
#histogram of transformed residuals ggplot(trainingData, aes(residuals)) +
geom_histogram(binwidth = 1,
               fill = "mediumturquoise",
               colour = "gray92",
               size=1) + # change binwidth labs(title="Histogram of Residuals",
  x = "Residuals", y = "Frequency")
#qqplot of transformed residuals ggplot(trainingData, aes(sample = residuals)) +
stat_qq() +
  stat_qq_line(color = "red") + labs(title = "Normal Q-Q Plot",
                                     x = "Theoretical Quantiles", y = "Sample Quantiles")
#predicted <- predict(logitMod, testData, type="response")
#################### Team Rank Logistic Regression ########################
df_tour <- read.csv("~/Downloads/womens-machine-learning-competition-2019/WDataFiles/WNCAATourn eyCompactResults.csv")
df_temp <- merge(df_tour,df_winseeds, by = c("Season","WTeamID")) df_concat <- merge(df_temp, df_lossseeds, by = c("Season","LTeamID"))
#only tournament wins/losses

scores_w <- df_tour$WScore
teams_w <- df_tour$WTeamID
wins <- c(rep("Winner",length(scores_w))) scores_l <- df_tour$LScore
teams_l <- df_tour$LTeamID
losses <- c(rep("Loser",length(scores_l)))
my_win <- cbind(scores_w,teams_w,wins) my_loss <- cbind(scores_l,teams_l,losses) my_df <- rbind(my_win,my_loss)
my_df <- as.data.frame(my_df)
colnames(my_df) <- c("Score","TeamID","Status") my_df$Score <- as.numeric(as.character(my_df$Score))
df_predictions <- merge(my_df, teams, by = c("TeamID")) df_predictions$Bstatus <- ifelse(df_predictions$Status=="Winner",1,0) head(df_predictions)
team_logit <- glm(Bstatus ~ Score + TeamName, data=df_predictions, family=binomial(link = "logit"))
summary(team_logit)
plot(team_logit)
ggplot(df_predictions, aes(x=Score, y=Bstatus)) +
  geom_point() +
  #geom_abline(intercept = -3.335e-16, slope = -2.625e-01, color = "red") + stat_smooth(method = glm) +
  labs(y="Results",
       x="Score",
       title="Logistic Regression")
team.res = resid(team_logit)
df_predictions$residuals <- team.res
df_predictions$index <- c(1:length(df_predictions$Bstatus)) head(df_predictions)
#transformed residual plot
ggplot(df_predictions, aes(x=index, y=residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  
  labs(y="Residual", x="Index",
       title="Residual Plot")
#histogram of transformed residuals ggplot(df_predictions, aes(residuals)) +
geom_histogram(binwidth = 1,
               fill = "mediumturquoise",
               colour = "gray92",
               size=1) + # change binwidth labs(title="Histogram of Residuals",
  x = "Residuals", y = "Frequency")
#qqplot of transformed residuals ggplot(df_predictions, aes(sample = residuals)) +
stat_qq() +
  stat_qq_line(color = "red") + labs(title = "Normal Q-Q Plot",
                                     x = "Theoretical Quantiles", y = "Sample Quantiles")
#### for data portion ############################### 
ggplot(df_predictions, aes(x=TeamID, y=Score)) +
geom_point(aes(col=Status)) + #xlim(c(0, 0.1)) +
  #ylim(c(0, 500000)) +
  theme_light() +
  theme(panel.grid = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + labs(y="Score",
                                              x="Team ID",
                                              title="Scores per Team") #####################################################

library(ggplot2) #install.packages("randomForest") library(randomForest) #install.packages("forestFloor") library(forestFloor)
library(dplyr)
teams <- read.csv("~/Final Project/Final project dataset/WTeams.csv")
tourney <- read.csv("~/Final Project/Final project dataset/WNCAATourneyDetailedResults.csv") regular <- read.csv("~/Final Project/Final project dataset/WRegularSeasonDetailedResults.csv") regular_results <- read.csv("~/Final Project/Final project dataset/WRegularSeasonDetailedResults.csv")
sub <- read.csv("~/WSampleSubmissionStage1.csv")
seeds <- read.csv("~/Final Project/Final project dataset/WNCAATourneySeeds.csv")
theme_dotplotx <- function() { theme(
  panel.grid.major.x = element_blank() ,
  panel.grid.minor.x = element_blank() ,
  panel.grid.major.y = element_line(color="black", linetype = 3), axis.text.y = element_text(size=rel(1.2)),
  panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"))
}
#changing seed to numeric
seeds$Seed = as.numeric(substring(seeds$Seed,2,4))
#adding team names to datasets tourney <- tourney %>%
left_join(teams, by = c("WTeamID" = "TeamID")) %>% left_join(teams, by = c("LTeamID" = "TeamID"))
tourney <- tourney %>%
  
  rename(WTeamName = TeamName.x, LTeamName = TeamName.y)
seeds <- seeds %>%
  left_join(teams, by = c("TeamID" = "TeamID"))
seeds <- seeds %>% rename(WTeamName = TeamName)
tourney$season_day <- paste(tourney$Season, tourney$DayNum, sep = "_")
regular <- regular %>%
  left_join(teams, by = c("WTeamID" = "TeamID")) %>% left_join(teams, by = c("LTeamID" = "TeamID"))
regular <- regular %>% rename(WTeamName = TeamName.x,
                              LTeamName = TeamName.y)
regular$season_day <- paste(regular$Season, regular$DayNum, sep = "_")
# regular season possesion regular <- regular %>%
mutate(WPoss = WFGA + (WFTA * 0.475) + WTO - WOR, LPoss = LFGA + (LFTA * 0.475) + LTO - LOR)
# Tourney possesion tourney <- tourney %>%
mutate(WPoss = WFGA + (WFTA * 0.475) + WTO - WOR, LPoss = LFGA + (LFTA * 0.475) + LTO - LOR)
###General Plots
#Finals of every season 2010 - 18 ncaa_champs <- tourney %>%
group_by(Season) %>%
  summarise(max_days = max(DayNum)) %>% mutate(season_day = paste(Season, max_days, sep = "_")) %>% left_join(tourney, by = "season_day") %>% ungroup() %>% select(-Season.y) %>%
  
  rename(Season = Season.x)
ncaa_champs <- ncaa_champs %>%
  mutate(season_winner = paste(Season, WTeamID, sep = "_"))
#Winning teams
win_plot <- ncaa_champs %>%
  group_by(WTeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(WTeamName,n), y=n)) +
  geom_bar(stat = "identity", fill = "mediumturquoise", color = "black")+ labs(title = "Most Tourney Wins since 2010", x= "Teams", y= "Titles")
#Runner up teams
lose_plot <- ncaa_champs %>%
  group_by(LTeamName) %>%
  summarise(n = n()) %>% ggplot(aes(x=reorder(LTeamName, n), y=n)) + geom_bar(stat = "identity", fill = "red", colour = "black") + theme_minimal()+
  labs(title = "Most Runnerups since 2010", x= "Runner-up", y= "Titles")
#Seeds that won (Seed of each win) seed_plot <- ncaa_champs %>%
select(Season, TeamID = WTeamID) %>%
  left_join(seeds, by = c("Season", "TeamID")) %>%
  count(Seed, sort = T) %>%
  mutate(Seed = as.character(Seed)) %>%
  ggplot(aes(x= reorder(Seed, n), y= n)) +
  geom_bar(stat = "identity", fill = "mediumturquoise", color = "black", width = 0.1)+ coord_flip() +
  theme(axis.title = element_blank()) +
  labs(title = "Which seeds have won the most titles")
###Working on new datasets
#Seperating matches into winners and losers seperated <- rbind(
regular %>%
  select(Season, DayNum, TeamID=WTeamID, TeamName = WTeamName, Score=WScore,
         OScore=LScore, WLoc, NumOT, Poss=WPoss, FGM=WFGM, FGA=WFGA, FGM3=WFGM3, FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR,
         
         DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OPoss=LPoss, OFGM=LFGM, OFGA=LFGA, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM, OFTA=LFTA, O_OR=LOR, ODR=LDR,
         OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk, OPF=LPF) %>% mutate(Winner=1),
regular %>%
  select(Season, DayNum, TeamID=LTeamID, TeamName = LTeamName, Score=LScore,
         OScore=WScore, WLoc, NumOT, Poss=LPoss, FGM=LFGM, FGA=LFGA, FGM3=LFGM3, FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR,
         DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OPoss=WPoss, OFGM=WFGM, OFGA=WFGA, OFGM3=WFGM3, OFGA3=WFGA3, OFTM=WFTM, OFTA=WFTA, O_OR=WOR, ODR=WDR,
         OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk, OPF=WPF) %>% mutate(Winner=0))
#Summary mean of all stats of teams with win percentages season_summary_mean <-
seperated %>%
  group_by(Season, TeamID, TeamName) %>% summarize(
    GP = n(),
    wins = sum(Winner),
    PtsMean = mean(Score),
    Opp_PtsMean = mean(OScore), PtsMedian = median(Score),
    PosMean = mean(Poss), Opp_PossMean = mean(OPoss), PointsDiffMean = mean(Score - OScore), FGMmean = mean(FGM),
    FGAmean = mean(FGA),
    FGM3mean = mean(FGM3),
    FGA3mean = mean(FGA3),
    FTMmean = mean(FTM),
    FTAmean = mean(FTA),
    ORmean = mean(OR),
    DRmean = mean(DR),
    Astmean = mean(Ast),
    TOmean = mean(TO),
    Stlmean = mean(Stl),
    Blkmean = mean(Blk),
    PFmean = mean(PF),
    
    Opp_FGMmean = mean(OFGM), Opp_FGAmean = mean(OFGA), Opp_FGM3mean = mean(OFGM3), Opp_FGA3mean = mean(OFGA3), Opp_FTMmean = mean(OFTM), Opp_FTAmean = mean(OFTA), Opp_ORmean = mean(O_OR), Opp_DRmean = mean(ODR), Opp_Astmean = mean(OAst), Opp_TOmean = mean(OTO),Seas Opp_Stlmean = mean(OStl), Opp_Blkmean = mean(OBlk), Opp_PFmean = mean(OPF)
  )
season_summary_mean$WinPerc<- season_summary_mean$wins / season_summary_mean$GP
#Summary mean of all stats of teams with win percentages season_summary_total <-
seperated %>%
  group_by(Season, TeamID, TeamName) %>% summarize(
    GP = n(),
    wins = sum(Winner),
    Pts_Total = sum(Score),
    Opp_Pts_Total = sum(OScore), Pos_Total = sum(Poss), Opp_Poss_Total = sum(OPoss), PointsDiff_Total = sum(Score - OScore), FGM_Total = sum(FGM),
    FGA_Total = sum(FGA),
    FGM3_Total = sum(FGM3), FGA3_Total = sum(FGA3),
    FTM_Total = sum(FTM),
    FTA_Total = sum(FTA),
    OR_Total = sum(OR),
    DR_Total = sum(DR),
    Ast_Total = sum(Ast),
    TO_Total = sum(TO),
    
    Stl_Total = sum(Stl), Blk_Total = sum(Blk), PF_Total = sum(PF),
    Opp_FGM_Total = sum(OFGM), Opp_FGA_Total = sum(OFGA), Opp_FGM3_Total = sum(OFGM3), Opp_FGA3_Total = sum(OFGA3), Opp_FTM_Total = sum(OFTM), Opp_FTA_Total = sum(OFTA), Opp_OR_Total = sum(O_OR), Opp_DR_Total = sum(ODR), Opp_Ast_Total = sum(OAst), Opp_TO_Total = sum(OTO), Opp_Stl_Total = sum(OStl), Opp_Blk_Total = sum(OBlk), Opp_PF_Total = sum(OPF)
  )
season_summary_total$WinPerc<- season_summary_total$wins / season_summary_total$GP
season_summary_mean <- season_summary_mean %>% mutate(season_winner = paste(Season, TeamID, sep = "_"),
                                                      win = ifelse(season_winner %in% ncaa_champs$season_winner, "Champs", "Not"))
season_summary_total <- season_summary_total %>% mutate(season_winner = paste(Season, TeamID, sep = "_"),
                                                        win = ifelse(season_winner %in% ncaa_champs$season_winner, "Champs", "Not"))
season_summary_mean <- season_summary_mean %>% left_join(seeds, by = c("Season", "TeamID"))
season_summary_total <- season_summary_total %>% left_join(seeds, by = c("Season", "TeamID"))
#Very strong correlation for Avg points / game Vs. Wins season_summary_mean %>%
ggplot() +
  geom_point(data = subset(season_summary_mean, win == "Not"), aes(x= PtsMean, y= wins), alpha = 0.5, color = "grey") +
  geom_point(data = subset(season_summary_mean, win == "Champs"), aes(x= PtsMean, y= wins), color = "orange") +
  
  geom_text(data = subset(season_summary_mean, win == "Champs"), aes(x= PtsMean, y= wins, label = TeamName), nudge_y = 4, colour = "mediumturquoise") +
  ggtitle("Avg points / game Vs. Wins") + theme_minimal() + theme(legend.position = "none") + facet_wrap(~Season)
season_summary_mean %>%
  ggplot() +
  geom_point(data = subset(season_summary_mean, win == "Not"), aes(x= Seed, y= wins),
             alpha = 0.5, color = "grey") +
  geom_point(data = subset(season_summary_mean, win == "Champs"), aes(x= Seed, y=
                                                                        wins), color = "orange") +
  geom_text(data = subset(season_summary_mean, win == "Champs"), aes(x= Seed, y= wins,
                                                                     label = TeamName), nudge_y = 4, colour = "mediumturquoise") + ggtitle("Seed Vs. Wins") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~Season)
#Very strong correlation for Field Goals Made / game Vs. Wins season_summary_mean %>%
ggplot() +
  geom_point(data = subset(season_summary_mean, win == "Not"), aes(x= FGMmean, y= wins), alpha = 0.5, color = "grey") +
  geom_point(data = subset(season_summary_mean, win == "Champs"), aes(x= FGMmean, y= wins), color = "orange") +
  geom_text(data = subset(season_summary_mean, win == "Champs"), aes(x= FGMmean, y= wins, label = TeamName), nudge_y = 4, colour = "mediumturquoise") +
  ggtitle("Field Goals Made / game Vs. Wins") + theme_minimal() +
  theme(legend.position = "none") + facet_wrap(~Season)
season_summary_mean %>%
  ggplot() +
  geom_point(data = subset(season_summary_mean, win == "Not"), aes(x= Opp_FGMmean, y=
                                                                     wins), alpha = 0.5, color = "grey") +
  geom_point(data = subset(season_summary_mean, win == "Champs"), aes(x=
                                                                        Opp_FGMmean, y= wins), color = "orange") +
  
  geom_text(data = subset(season_summary_mean, win == "Champs"), aes(x= Opp_FGMmean, y= wins, label = TeamName), nudge_y = 4, colour = "mediumturquoise") +
  ggtitle("Opponent Field Goals Made / game Vs. Wins") + theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~Season)
season_summary_mean$Seed[is.na(season_summary_mean$Seed)] <- 0
#Excluding not needed variables for RF ssm <- season_summary_mean
X =
  X = X = X = X = X = X = X = X = X =
  ssm[,!names(ssm) == "WTeamName"]
X[,!names(X) == "wins"] X[,!names(X) == "TeamName"] X[,!names(X) == "Season"] X[,!names(X) == "TeamID"] X[,!names(X) == "GP"] X[,!names(X) == "season_winner"] X[,!names(X) == "win"] X[,!names(X) == "WinPerc"] X[,!names(X) == "PointsDiffMean"]
= randomForest(X,ssm$wins,ntree=1000) plot(rf.y) #The plotted error rate
rf.y rf.y
quantile(rf.y$mse)# Lowest Mean squared Error is 4.057, 50% quantile is 4.07,
#and 75% quantile is 4.10, showing that most mean squared errors are around lowest MSE. #most trees provided an average MSE around the lowest MSE
rf.y$variable.importance
df.rfImportance <- data.frame(variable = names(rf.y$importance[,1]), importance = rf.y$importance[,1])

df.rfImportance <- df.rfImportance[ order(-df.rfImportance[,2]),]
ggplot(df.rfImportance) +
  geom_point(aes(x = importance, y = reorder(variable, importance), size = importance))+ labs(title="Importance of Variables",
                                                                                              x = "Importance",
                                                                                              y = "Variables")+ theme_dotplotx() #theme
importance(rf.y) varImpPlot(rf.y)
#Seed --> PtsMean --> FGMmean --> has the greatest impact in reducing MSE across our trees #node impurity = importance basically
rf.yr = randomForest(X,ssm$wins,sampsize=600,ntree=1000,mtry=4, keep.inbag = T,keep.forest = T)
rf.yr
ff = forestFloor(rf.yr,X,calc_np=T)
Col = fcol(ff,cols=1,outlier.lim = 2.5) plot(ff,col=Col,plot_seq = 1:25, ylim=c(-0.4, 0.4))
importance(rf.yr) varImpPlot(rf.yr)
###Regresson Tree### ###Removing variables the skew the decision tree### 
Y = ssm[,!names(ssm) == "WTeamName"]
Y = Y[,!names(Y) == "TeamName"]
Y = Y[,!names(Y) == "Season"]
Y = Y[,!names(Y) == "TeamID"]
Y = Y[,!names(Y) == "GP"]
Y = Y[,!names(Y) == "season_winner"] Y = Y[,!names(Y) == "win"]
Y = Y[,!names(Y) == "WinPerc"]
Y = Y[,!names(Y) == "PointsDiffMean"]

Y = Y[,!names(Y) == "Seed"]
Y = Y[,!names(Y) == "PtsMean"]
Y = Y[,!names(Y) == "PtsMedian"]
Y = Y[,!names(Y) == "Opp_PtsMean"]
fit <- rpart(wins ~ ., data = Y,method = "anova") #Regression tree plotcp(fit)
plot(fit)
text(fit)
par(mfrow=c(1,2)) # two plots on one page rsq.rpart(fit) # visualize cross-validation results
plot(fit, uniform=TRUE, main="Regression Tree for Wins ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
#Seed --> PtsMean --> FGMmean --> has the greatest impact in reducing MSE across our trees #node impurity = importance basically
rf.yr = randomForest(X,ssm$wins,sampsize=600,ntree=1000,mtry=4, keep.inbag = T,keep.forest = T)
rf.yr
ff = forestFloor(rf.yr,X,calc_np=T)
Col = fcol(ff,cols=1,outlier.lim = 2.5) plot(ff,col=Col,plot_seq = 1:25, ylim=c(-0.4, 0.4))
importance(rf.yr) varImpPlot(rf.yr)
### Kurtosis and Skewness Investigation ### 
library(moments)
kurtosis(df_concat$seed_diff) #2.376826

skewness(df_concat$seed_diff) #
normal_kurtosis=NULL for(i in 1:1000){
  normal_kurtosis[i]=kurtosis(rnorm(nrow(df_concat))) hist(normal_kurtosis)
  ggplot() + aes(normal_kurtosis) +
    geom_histogram(binwidth=0.1, colour="black", fill="mediumturquoise") + labs(title="Simulation of Normal Kurtosis",
                                                                                y = "Frequency",
                                                                                x = "Normal Kurtosis: Seed Differences")
  normal_skewness=NULL for(i in 1:1000){
    normal_skewness[i]=skewness(rnorm(nrow(df_concat)))
    hist(normal_skewness)
    ggplot() + aes(normal_skewness) +
      geom_histogram(binwidth=0.1, colour="black", fill="mediumturquoise") + labs(title="Simulation of Skewness",
                                                                                  y = "Frequency",
                                                                                  x = "Normal Skewness: Seed Differences")
    kurtosis(my_df$Score) #3.421053
    skewness(my_df$Score) #0.3948176
    normal_kurtosis=NULL for(i in 1:1000){
      normal_kurtosis[i]=kurtosis(rnorm(nrow(my_df)))
      hist(normal_kurtosis)
      ggplot() + aes(normal_kurtosis) +
        geom_histogram(binwidth=0.1, colour="black", fill="mediumturquoise") + labs(title="Simulation of Kurtosis",
                                                                                    y = "Frequency",
                                                                                    
                                                                                    x = "Normal Skewness: Win Scores")
      normal_skewness=NULL for(i in 1:1000){
        normal_kurtosis[i]=skewness(rnorm(nrow(my_df)))
        hist(normal_skewness)
        ggplot() + aes(normal_skewness) +
          geom_histogram(binwidth=0.001, colour="black", fill="mediumturquoise") + labs(title="Simulation of Skewness",
                                                                                        y = "Frequency",
                                                                                        x = "Normal Skewness: Win Scores")
        kurtosis(my_df$Score) #3.286252
        skewness(my_df$Score) #0.1899531
        normal_kurtosis=NULL for(i in 1:1000){
          normal_kurtosis[i]=kurtosis(rnorm(nrow(my_df)))
          hist(normal_kurtosis)
          ggplot() + aes(normal_kurtosis) +
            geom_histogram(binwidth=0.01, colour="black", fill="mediumturquoise") + labs(title="Simulation of Kurtosis",
                                                                                         y = "Frequency",
                                                                                         x = "Normal Kurtosis: Loss Scores")
          normal_skewness=NULL for(i in 1:1000){
            normal_skewness[i]=skewness(rnorm(nrow(my_df)))
            hist(normal_skewness)
            ggplot() + aes(normal_skewness) +
              geom_histogram(binwidth=0.01, colour="black", fill="mediumturquoise") + labs(title="Simulation of Skewness",
                                                                                           
                                                                                           y = "Frequency",
                                                                                           x = "Normal Skewness: Loss Scores")
            Decision Classifier
            #Install Python packages
            import numpy as np
            import pandas as pd
            from sklearn.utils import shuffle
            from sklearn.metrics import confusion_matrix
            from sklearn.model_selection import train_test_split from sklearn.model_selection import GridSearchCV from sklearn.tree import DecisionTreeClassifier from sklearn import metrics
            from sklearn.metrics import accuracy_score from sklearn.metrics import classification_report from sklearn.tree import export_graphviz
            from sklearn.externals.six import StringIO
            from IPython.display import Image
            import pydotplus
            import collections
            from sklearn.datasets import load_iris
            from sklearn import tree
            #load data
            seeds = pd.read_csv('WNCAATourneySeeds.csv')
            cresults_tourney = pd.read_csv('WNCAATourneyCompactResults.csv')
            #remove regional abbreviation above seed def seed_to_int(seed):
            #Get just the digits from the seeding. Return as int s_int = int(seed[1:3])
            return s_int
            seeds['seed_int'] = seeds.Seed.apply(seed_to_int) seeds.drop(labels=['Seed'], inplace=True, axis=1) # This is the string label seeds.head()
            #remove extraneous columns
            cresults_tourney.drop(labels=['DayNum', 'WScore', 'LScore', 'WLoc', 'NumOT'], inplace=True, axis=1)
            cresults_tourney.head()
            
            #merge seeds w/ corresponding team ID
            win_seeds = seeds.rename(columns={'TeamID':'WTeamID','seed_int':'WSeed'})
            loss_seeds = seeds.rename(columns={'TeamID':'LTeamID','seed_int':'LSeed'})
            df_temp = pd.merge(left=cresults_tourney,right=win_seeds,how='left',on=['Season','WTeamID']) df_concat = pd.merge(left=df_temp,right=loss_seeds,on=['Season','LTeamID']) df_concat['SeedDiff'] = df_concat.WSeed - df_concat.LSeed
            df_concat.head()
            #make a new df that shows wins and losses, and corresp. seed diff wins = pd.DataFrame()
            wins['SeedDiff'] = df_concat['SeedDiff']
            wins['Result'] = 1
            losses = pd.DataFrame() losses['SeedDiff'] = -df_concat['SeedDiff'] losses['Result'] = 0
            predictions = pd.concat((wins,losses)) predictions.head()
            #divide data to train, test
            X_train = predictions.SeedDiff.values.reshape(-1,1) y_train = predictions.Result.values
            X_train,y_train = shuffle(X_train,y_train)
            #70% train, 30% test
            X_train,X_test,y_train,y_test = train_test_split(X_train,y_train,test_size=0.3,random_state=1)
            #create decision tree classifier object clf = DecisionTreeClassifier()
            #train decision tree classifier clf = clf.fit(X_train,y_train)
            #predict response for test dataset y_pred = clf.predict(X_test) y_pred
            #compute model accuracy print("Accuracy:",metrics.accuracy_score(y_test,y_pred)) Accuracy: 0.7594458438287154
            
            ###Cross-tabulation
            setwd("~/Downloads/WDataFiles")
            Detailed_past_tourney <- read.csv(file = "WNCAATourneyDetailedResults.csv", headers = TRUE)
            filter(Detailed_past_tourney, DayNum == "153") #calls all day 153s, need to pull teamID Seed <- read.csv(file = "WNCAATourneySeeds.csv", header = TRUE)
            test <- Seed
            Detailed_past_tourney$index <- paste(Detailed_past_tourney$Season, Detailed_past_tourney$WTeamID, sep="_")
            test$index <- paste(test$Season, test$TeamID, sep="_")
            #link season and teamid
            output <- merge(Detailed_past_tourney, test, by="index", sort=F) #merge seed data with past tourney
            cross_seed <- output$Seed
            firsthalf <- cross_seed[1:315]
            cross_daynum <- output$DayNum first_daynum <- c(cross_daynum[1:315]) second_seed <- cross_seed[316:567] second_daynum <- c(cross_daynum[316:567]) cross_teamid <- output$index
            cross_teamid1 <- c(cross_teamid[1:315]) cross_teamid2 <- c(cross_teamid[316:567])
            #split data in half because fin4 games on diff days 1:315 and 316:567 #seasons 2010-2014 play on 147/148 and seasons 2015-2018 play on 146/147
            DF <- data.frame(firsthalf, first_daynum)
            DF2 <- data.frame (second_seed, second_daynum) teamid1 <- data.frame(first_daynum, cross_teamid1) teamid2 <- data.frame(second_daynum, cross_teamid2)
            finfour_teamid1 <- subset(teamid1, first_daynum==147 | first_daynum==148) finfour_teamid2 <- subset(teamid2, second_daynum==146 | second_daynum==147) #finfour with winning team id
            finfour1 <- subset(DF, first_daynum==147 | first_daynum==148)
            finfour2 <- subset(DF2, second_daynum==146 | second_daynum==147) #final four teams
            
            sum(finfour1$firsthalf == 'W01') + sum(finfour1$firsthalf == 'X01') + sum(finfour1$firsthalf == 'Y01') + sum(finfour1$firsthalf == 'Z01') + sum(finfour1$firsthalf == 'W02') + sum(finfour1$firsthalf == 'X02') + sum(finfour1$firsthalf == 'Y02') + sum(finfour1$firsthalf == 'Z02')
            sum(finfour2$second_seed == 'W01') + sum(finfour2$second_seed == 'X01') + sum(finfour2$second_seed == 'Y01') + sum(finfour2$second_seed == 'Z01') + sum(finfour2$second_seed == 'W02') + sum(finfour2$second_seed == 'X02') + sum(finfour2$second_seed == 'Y02') + sum(finfour2$second_seed == 'Z02') #sum of 1 and 2 seeds in final four for 2010-2014 = 16
            #sum of 1 and 2 seeds in final four for 2015-2018 = 14
            sum(finfour1$firsthalf == 'W01') + sum(finfour1$firsthalf == 'X01') + sum(finfour1$firsthalf == 'Y01') + sum(finfour1$firsthalf == 'Z01')
            #sum of 1 seeds in 2010-2014 = 12
            sum(finfour1$firsthalf == 'W02') + sum(finfour1$firsthalf == 'X02') + sum(finfour1$firsthalf == 'Y02') + sum(finfour1$firsthalf == 'Z02')
            #sum of 2 seeds in 2010 - 2014 = 4
            sum(finfour2$second_seed == 'W01') + sum(finfour2$second_seed == 'X01') + sum(finfour2$second_seed == 'Y01') + sum(finfour2$second_seed == 'Z01')
            #sum of 1 seeds in 2015-2018 = 11
            sum(finfour2$second_seed == 'W02') + sum(finfour2$second_seed == 'X02') + sum(finfour2$second_seed == 'Y02') + sum(finfour2$second_seed == 'Z02')
            #sum of 2 seeds in 2015-2018 = 3
            sum(finfour2$second_seed == 'W03') + sum(finfour2$second_seed == 'X03') + sum(finfour2$second_seed == 'Y03') + sum(finfour2$second_seed == 'Z03')
            # 3 seeds in second half = 0
            sum(finfour1$firsthalf == 'W03') + sum(finfour1$firsthalf == 'X03') + sum(finfour1$firsthalf == 'Y03') + sum(finfour1$firsthalf == 'Z03')
            # 3 seeds in first half = 1
            sum(finfour1$firsthalf == 'W04') + sum(finfour1$firsthalf == 'X04') + sum(finfour1$firsthalf == 'Y04') + sum(finfour1$firsthalf == 'Z04')
            #sum of 5 seeds in firsthalf = 2
            sum(finfour2$second_seed == 'W04') + sum(finfour2$second_seed == 'X04') + sum(finfour2$second_seed == 'Y04') + sum(finfour2$second_seed == 'Z04')
            #sum of 5 seeds in second half = 1
            sum(finfour1$firsthalf == 'W05') + sum(finfour1$firsthalf == 'X05') + sum(finfour1$firsthalf == 'Y05') + sum(finfour1$firsthalf == 'Z05')
            #sum of 5 seeds in firsthalf = 1
            sum(finfour2$second_seed == 'W05') + sum(finfour2$second_seed == 'X05') + sum(finfour2$second_seed == 'Y05') + sum(finfour2$second_seed == 'Z05')
            #sum of 5 seeds in second half = 0
            
            sum(finfour1$firsthalf == 'W06') + sum(finfour1$firsthalf == 'X06') + sum(finfour1$firsthalf == 'Y06') + sum(finfour1$firsthalf == 'Z06')
            #sum of 5 seeds in firsthalf = 0
            sum(finfour2$second_seed == 'W06') + sum(finfour2$second_seed == 'X06') + sum(finfour2$second_seed == 'Y06') + sum(finfour2$second_seed == 'Z06')
            #sum of 5 seeds in second half = 0
            sum(finfour1$firsthalf == 'W07') + sum(finfour1$firsthalf == 'X07') + sum(finfour1$firsthalf == 'Y07') + sum(finfour1$firsthalf == 'Z07')
            #sum of 5 seeds in firsthalf = 0
            sum(finfour2$second_seed == 'W07') + sum(finfour2$second_seed == 'X07') + sum(finfour2$second_seed == 'Y07') + sum(finfour2$second_seed == 'Z07')
            #sum of 5 seeds in second half = 1
            sum(finfour1$firsthalf == 'W08') + sum(finfour1$firsthalf == 'X08') + sum(finfour1$firsthalf == 'Y08') + sum(finfour1$firsthalf == 'Z08')
            #sum of 5 seeds in firsthalf = 0
            sum(finfour2$second_seed == 'W08') + sum(finfour2$second_seed == 'X08') + sum(finfour2$second_seed == 'Y08') + sum(finfour2$second_seed == 'Z08')
            #sum of 5 seeds in second half = 0
            past_season <- read.csv(file = "WRegularSeasonDetailedResults.csv", header = T) past_season$index <- paste(past_season$Season, past_season$WTeamID, sep="_") test$index <- paste(test$Season, test$TeamID, sep="_")
            #for loop for games won in season
            past_season$losers <- paste(past_season$Season, past_season$LTeamID, sep="_") all_teams <- sort(unique(c(past_season$index,past_season$losers)))
            team_wins <- list() life <- 1
            for(team in all_teams){
              temp <- past_season[which(past_season$index==team),] num_wins <- nrow(temp)
              team_wins[[life]] <- data.frame(teamID = team,
                                              wins = num_wins)
              life <- life + 1 }
            my_df <- team_wins[[1]]
            for(i in 1:length(team_wins)){
              my_df <- rbind(my_df,team_wins[[i]]) }
            
            karissa <- my_df[which(my_df$wins>28),]
            #vector including only the teams that won more than 28 of their games ##total = 36 teams
            ##final_total = 13
            final_total <- 0
            for(team in finfour_teamid1$cross_teamid1){
              temp <- finfour_teamid1[which(karissa$teamID==team),] if(nrow(temp)>0){
                final_total = final_total + 1 }
            }
            #first half of final four teams won over 30 games
            #final_total = 11
            final_total <- 0
            for(team in finfour_teamid2$cross_teamid2){
              temp <- finfour_teamid2[which(karissa$teamID==team),] if(nrow(temp)>0){
                final_total = final_total + 1 }
            }
            #second half final four teams won over 30 games