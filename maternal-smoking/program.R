library(ggplot2)
total <- read.csv("~/Math 189/babies.txt", sep="")
#removing unknown behavior for smoking/not total <- total[which(total$smoke != 9),]
#finding the mean for columns without unknown values g <- total[which(total$gestation<400),]
ges_mean <- as.integer(mean(g$gestation))
h <- total[which(total$height<80),] height_mean <- as.integer(mean(h$height))
w <- total[which(total$weight<300),] weight_mean <- as.integer(mean(w$weight))
a <- total[which(total$age<50),] age_mean <- as.integer(mean(a$age))
#replacing unknown values with the mean values total$gestation[total$gestation > 400] <- ges_mean total$height[total$height > 80] <- height_mean total$weight[total$weight > 300] <- weight_mean total$age[total$age > 50] <- age_mean
#make two separate subsets for smoking/not nonsmoking <- total[which(total$smoke==0),] smoking <- total[which(total$smoke==1),]
#Values for the Data Tables:
#min, 1st q, median, mean, 3rd q, max quantile(nonsmoking$gestation)
#25% & 75% max(nonsmoking$gestation) min(nonsmoking$gestation) median(nonsmoking$gestation) mean(nonsmoking$gestation)
########################### quantile(nonsmoking$smoke) #25% & 75% max(nonsmoking$smoke)
min(nonsmoking$smoke) median(nonsmoking$smoke) mean(nonsmoking$smoke)
########################### quantile(smoking$weight)
#25% & 75% max(smoking$weight) min(smoking$weight) median(smoking$weight) mean(smoking$weight)
#Add column to make smoke categorical (easier to plot)
total$sm <- ifelse(total$smoke==1, "Smoker", "Non-smoker") nonsmoking$sm <- ifelse(nonsmoking$smoke==1, "Smoker", "Non-smoker") smoking$sm <- ifelse(smoking$smoke==1, "Smoker", "Non-smoker")
#Density Plot
k <- ggplot(total, aes(bwt))
k + geom_density(aes(fill=factor(sm)), alpha=0.8) +
  labs(title="Smokers VS Non-smokers", x = "Birthweight (oz)",
       fill="Smoking Status")
#Box Plot
i <- ggplot(total, aes(sm, bwt))
i + geom_boxplot(varwidth=T, fill="plum") +
  geom_hline(aes(yintercept=88, col = "red")) + geom_text(aes(y=85,label = "Low Birthweight", col = "red")) + theme(legend.position="none") +
  labs(title="Box Plot of Babies Birthweights",
       x="Smoking Status", y="Birthweight (oz)")
#Histogram
h <- ggplot(total, aes(bwt))
h + geom_histogram(aes(fill=sm),
                   binwidth = 3,
                   col="black",
                   size=0.1) + # change binwidth
  guides(fill=guide_legend(title="Smoking Status")) +
  
  labs(title="Histogram of Babies Birthweight Distribution", x="Birthweight (oz)")
#Kurtosis and Skewness Analysis and Plots: library(moments)
kurtosis(nonsmoking$bwt) #4.04
kurtosis(smoking$bwt) #2.99
skewness(nonsmoking$bwt) #-0.19
skewness(smoking$bwt) #-0.03
###################plots for kurtosis normal_kurtosis_smoking=NULL
for(i in 1:1000){
  normal_kurtosis_smoking[i]=kurtosis(rnorm(nrow(smoking))) }
hist(normal_kurtosis_smoking)
#Histogram
ggplot() + aes(normal_kurtosis_smoking) +
  geom_histogram(binwidth=0.1, colour="black", fill="mediumturquoise") + labs(title="Simulation of Normal Kurtosis",
                                                                              subtitle = "Actual Kurtosis = 2.99", y = "Frequency",
                                                                              x = "Normal Kurtosis: Smoker")
normal_kurtosis_nonsmoking=NULL for (i in 1:1000){
  normal_kurtosis_nonsmoking[i]=kurtosis(rnorm(nrow(nonsmoking))) }
hist(normal_kurtosis_nonsmoking)

#Histogram
ggplot() + aes(normal_kurtosis_nonsmoking) +
  geom_histogram(binwidth=0.1, colour="black", fill="lightcoral") + labs(title="Simulation of Normal Kurtosis",
                                                                         subtitle = "Actual Kurtosis = 4.04", y = "Frequency",
                                                                         x = "Normal Kurtosis: Non-smoker")
#######################
####################### plots for skewness
normal_skewness_smoking=NULL for (i in 1:1000){
  normal_skewness_smoking[i]=skewness(rnorm(nrow(smoking))) }
hist(normal_skewness_smoking)
#Histogram
ggplot() + aes(normal_skewness_smoking) +
  geom_histogram(binwidth=0.1, colour="black", fill="mediumturquoise") + ylim(0,400) +
  labs(title="Simulation of Normal Skewness",
       subtitle = "Actual Skewness = -0.03", y = "Frequency",
       x = "Normal Skewness: Smoker")
normal_skewness_nonsmoking=NULL for (i in 1:1000){
  normal_skewness_nonsmoking[i]=skewness(rnorm(nrow(nonsmoking))) }
hist(normal_skewness_nonsmoking)
#Histogram
ggplot() + aes(normal_skewness_nonsmoking) +
  geom_histogram(binwidth=0.1, colour="black", fill="lightcoral") + labs(title="Simulation of Normal Skewness",
                                                                         subtitle = "Actual Skewness = -0.19", y = "Frequency",
                                                                         
                                                                         x = "Normal Skewness: Non-smoker")
##################### QQ Plot
ggplot(total, aes(sample = bwt, colour = sm)) + stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Babies Birth Weights",
       subtitle = "Smokers vs. Non-smokers", colour = "Smoking Status")
########################### library(lattice)
qq(sm ~ bwt, aspect = 1, data = total, main = "Comparison of Babies Birth Weights", subset = (sm == "Non-smoker" | sm == "Smoker"),
   col = "forestgreen")
############################
alt <- read.csv("~/Math 189/alt_hyp.txt", sep="\t") alt <- alt[which(alt$Tobacco.Use!="Not Stated"),]
#Due to factors/levels...
dm <- as.character(alt$Delivery.Method) bw <- as.character(alt$Birth.Weight.12) tu <- as.character(alt$Tobacco.Use) count <- as.double(alt$Births)
#condensed dataframe
my_df <- cbind(bw,dm,tu,count) df <- data.frame(my_df)
#filter out rows without valuable info df <- df[which(df$bw!="2"),]
df <- df[which(df$tu!=""),]
df <- df[which(df$dm!="Not Stated"),]
#more issues with factors/levels...

clean_count <- as.character.numeric_version(df$count) clean_count <- as.integer(clean_count)
clean_dm <- as.character(df$dm)
#Stacked bar plot
g <- ggplot(data = df, aes(x = clean_dm, y = log(clean_count))) g + geom_bar(stat = "identity", aes(fill=df$tu), width = 0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + guides(fill=guide_legend(title="Smoking Status")) + labs(title="Stacked Bar Plot on Baby Delivery Method",
                                                                                                                    x = "Delivery Method",
                                                                                                                    y = "Log of Number of Babies")
#idea for other bar plot
#df$bwtu <- paste0(df$bw,", ",df$tu)
#h <- ggplot(data = df, aes(x = clean_dm, y = log(clean_count))) #h + geom_bar(stat = "identity", aes(fill=df$bwtu), width = 0.5) +
# # # # #
theme(axis.text.x = element_text(angle=65, vjust=0.6)) + guides(fill=guide_legend(title="Smoking Status")) + labs(title="Stacked Bar Plot on Baby Delivery Method",
                                                                                                                  x = "Delivery Method",
                                                                                                                  y = "Log of Number of Babies")
######################## Talal
#Generating 1226 samples
boot.population <- rep(smoking$bwt, length.out = 1226) boot2.population <- rep(nonsmoking$bwt, length.out = 1226)
#Loop to generate smoking samples
B = 500 # the number of bootstrap samples we want boot.sample <- array(dim = c(B, 484))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 484, replace = FALSE) }
#Loop to generate non smoking samples
B = 500 # the number of bootstrap samples we want boot.sample <- array(dim = c(B, 742))
for (i in 1:B) {
  
  boot.sample[i, ] <- sample(boot2.population, size = 742, replace = FALSE) }
#histogram plot sample means
nonsmoke.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)
ggplot() + aes(nonsmoke.mean) + geom_histogram(binwidth=0.1, colour="black", fill="lightcoral") + labs(title="Distribution of Sample Means",
                                                                                                       y = "Density",
                                                                                                       x = "Non-Smoker Sample Means")
smoke.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)
ggplot() + aes(smoke.mean) +
  geom_histogram(binwidth=0.2, colour="black", fill="mediumturquoise") + labs(title="Distribution of Sample Means",
                                                                              y = "Density",
                                                                              x = "Smoker Sample Means")
