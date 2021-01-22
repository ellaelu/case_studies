palindromes <- read.csv("~/Downloads/palindromes.txt", sep="") library(ggplot2)
################################## Data ####################################### locations <- palindromes$location
table(locations)
my_bins <- seq(4000, 232000, 4000)
my_counts <- c() for(b in my_bins){
  count <- length(palindromes[which(palindromes$location < b & palindromes$location > b - 4000),])
  my_counts <- c(my_counts,count) }
my_df <- cbind(my_bins,my_counts) my_df <- as.data.frame(my_df)
g <- ggplot(my_df, aes(x=my_bins, y=my_counts)) +
  geom_bar(stat="identity", fill="mediumturquoise") +
  labs(title="Location of Palindromes in consecutive intervals of 4000 base pairs",
       x = "Location on chain of DNA Base pairs",
       y = "Number of palindromes in the interval") + scale_x_discrete(limits = seq(4000, 232000, 12000)) + theme(axis.text.x = element_text(angle=65, vjust=0.6))
g
rand_samples <- sample(1:232000, size = 296, replace = F)
rand_counts <- c() for(b in my_bins){
  count <- length(rand_samples[rand_samples < b & rand_samples > b - 4000])
  rand_counts <- c(rand_counts,count) }
rand_df <- cbind(my_bins,rand_counts) rand_df <- as.data.frame(rand_df)
r <- ggplot(rand_df, aes(x=my_bins, y=rand_counts)) + geom_bar(stat="identity", fill="lightcoral") +
  labs(title="Location of 296 Random Hits in intervals of 4000 (set 1)",
       x = "Locations of hits",
       y = "Number of hits in the interval") + scale_x_discrete(limits = seq(4000, 232000, 12000)) + theme(axis.text.x = element_text(angle=65, vjust=0.6))
r
new_bins <- seq(200, 232000, 200)
new_counts <- c() for(b in new_bins){
  count <- length(palindromes[which(palindromes$location < b & palindromes$location > b - 200),])
  new_counts <- c(new_counts,count) }
new_df <- as.data.frame(cbind(new_bins,new_counts))
g <- ggplot(new_df,aes(new_counts)) + geom_histogram(binwidth = 1, fill = "mediumturquoise",
                                                     col="gray89") +
  expand_limits(x=c(0,14), y=c(0,1000)) + scale_x_discrete(limits = seq(0, 14, 1)) + labs(title="Intervals (length 200) with 0-14 palindromes",
                                                                                          x = "Number of Palindromes",
                                                                                          y = "Number of Intervals") g
rand_samp <- sample(1:232000, size = 296, replace = F)
rand_c <- c() for(b in my_bins){
  count <- length(rand_samp[rand_samp < b & rand_samp > b - 4000])
  rand_c <- c(rand_c,count) }
r_df <- as.data.frame(cbind(my_bins,rand_c)) gr <- ggplot(r_df,aes(rand_c)) +
  
  geom_histogram(binwidth = 1, fill = "lightcoral", col="gray89") +
  expand_limits(x=c(0,14)) +
  scale_x_discrete(limits = seq(0, 14, 1)) +
  labs(title="Intervals (length 4000) containing hits for random numbers (set 1)",
       x = "Number of Hits",
       y = "Number of Intervals") gr
spaces <- c() prev_value <- 0
for(i in 1:length(locations)){
  sp <- locations[i] - prev_value prev_value <- locations[i] spaces <- c(spaces,sp)
}
num_p <- c(1:296)
scatter_df <- as.data.frame(cbind(num_p,spaces))
gg <- ggplot(scatter_df, aes(x=num_p, y=spaces)) + geom_point(colour = "mediumturquoise") +
  geom_abline(intercept = 816.14, slope = -0.2872) + geom_text(aes(x = 250, y = 800, label = "y = -0.2872x + 816.14")) + geom_text(aes(x = 250, y = 400, label = "R^2 = 0.0009")) + ylim(c(0, 6000)) +
  labs(y="Number of Spaces (as DNA base pairs)", x="296 Palindromes",
       title="Spaces between consecutive Palindromes")
gg
rand_spaces <- c() prev_value <- 0
r_loc <- sort(rand_samp)
for(i in 1:length(r_loc)){
  sp <- r_loc[i] - prev_value prev_value <- r_loc[i]
  rand_spaces <- c(rand_spaces,sp)
  
}
num_p <- c(1:296)
r_scatter_df <- as.data.frame(cbind(num_p,rand_spaces))
rgg <- ggplot(r_scatter_df, aes(x=num_p, y=rand_spaces)) + geom_point(colour = "lightcoral") +
  geom_abline(intercept = 714.45, slope = 0.3994) + geom_text(aes(x = 250, y = 800, label = "y = 0.3994x + 714.45")) + geom_text(aes(x = 250, y = 400, label = "R^2 = 0.0019")) + ylim(c(0, 6000)) +
  labs(y="Number of Spaces (as DNA base pairs)", x="Incidence of Spaces",
       title="Spaces between Consecutive Random Hits (set 1)")
rgg
######################### Investigation: Location & Spacing ########################
loc_bins <- seq(10000, 230000, 10000)
loc_counts <- c() for(b in loc_bins){
  count <- length(palindromes[which(palindromes$location < b & palindromes$location > b - 10000),])
  loc_counts <- c(loc_counts,count) }
intervals <- c(1:23)
loc_df <- cbind(loc_bins,loc_counts,intervals) loc_df <- as.data.frame(loc_df)
locg <- ggplot(loc_df, aes(x=intervals, y=loc_counts)) + geom_bar(stat="identity", fill="mediumturquoise") + labs(title="Location of Palindromes in Consecutive Intervals",
                                                                                                                  x = "Intervals",
                                                                                                                  y = "Number of Palindromes in the Interval") + theme(axis.text.x = element_text(angle=65, vjust=0.6))
locg

loc_rand_samples <- sample(1:232000, size = 296, replace = F)
loc_rand_counts <- c() for(b in loc_bins){
  count <- length(loc_rand_samples[loc_rand_samples < b & loc_rand_samples > b - 10000])
  loc_rand_counts <- c(loc_rand_counts,count) }
loc_rand_df <- cbind(loc_bins,loc_rand_counts, intervals) loc_rand_df <- as.data.frame(loc_rand_df)
locr <- ggplot(loc_rand_df, aes(x=intervals, y=loc_rand_counts)) + geom_bar(stat="identity", fill="lightcoral") +
  labs(title="Location of 296 Random Hits in Intervals",
       x = "Locations of Hits",
       y = "Number of Hits in the Interval") + theme(axis.text.x = element_text(angle=65, vjust=0.6))
locr
loc_rand_df$random <- "random"
colnames(loc_rand_df) <- c("loc_bins","loc_counts","intervals","random") loc_df$random <- "non-random"
combined_df <- rbind(loc_rand_df,loc_df)
ggplot(combined_df,aes(x=intervals, y = loc_counts)) +
  geom_bar(stat = "identity",data=subset(combined_df,random == 'random'),
           fill = "lightcoral", alpha = 0.65) +
  geom_bar(stat = "identity",data=subset(combined_df,random == 'non-random'),
           fill = "mediumturquoise", alpha = 0.55) +
  labs(title = "Comparison of Actual vs Random Values in Intervals",
       x = "Intervals",
       y = "Number of Palindromes in Interval")
inv_spaces <- c() prev_value <- 0
for(i in 1:length(locations)){
  sp <- locations[i] - prev_value prev_value <- locations[i] inv_spaces <- c(inv_spaces,sp)
}

num_p <- c(1:296)
#spaces_df <- as.data.frame(cbind(num_p,inv_spaces))
manual_hist <- c()
for(i in c(1000,2000,3000,4000,5000)){
  count <- length(inv_spaces[inv_spaces < i & inv_spaces > i - 1000])
  manual_hist <- c(manual_hist,count) }
manual_intervals <- c(1000,2000,3000,4000,5000)
spaces_df <- as.data.frame(cbind(manual_intervals,manual_hist))
sp <- ggplot(spaces_df, aes(x=manual_intervals, y=manual_hist)) + geom_bar(stat="identity", fill="plum") +
  labs(title="Location of 296 Random Hits in Intervals",
       x = "Locations of Hits",
       y = "Number of Hits in the Interval") + theme(axis.text.x = element_text(angle=65, vjust=0.6))
sp
#hist(inv_spaces) #hist(rexp(296, rate = 0.001))
hist(rexp(296, rate = 0.001),
     col= "lightcoral", #rgb(1,0,0,0.5), xlim=c(0,6000), ylim=c(0,200), main="Overlapping Histogram", xlab="Variable")
     hist(inv_spaces,
          col= rgb(0,1,1,0.5), add=T)
     legend("topright", c("Exponential", "CMV Data"), fill=c("lightcoral", "mediumturquoise"))
     ###### Double Spaces
     d_spaces <- c() prev_value <- 0
     for(i in 2:length(locations)){
       sp <- locations[i] - prev_value
       
       prev_value <- locations[i-1]
       d_spaces <- c(d_spaces,sp) }
     hist(rexp(296, rate = 0.001),
          col= "lightcoral", #rgb(1,0,0,0.5), xlim=c(0,6000), ylim=c(0,150), main="Overlapping Histogram", xlab="Variable")
          hist(d_spaces,
               col= rgb(0,1,1,0.5), add=T)
          legend("topright", c("Exponential", "CMV Data"), fill=c("lightcoral", "mediumturquoise"))
          ######### Triple Spacing
          t_spaces <- c() prev_value <- 0
          for(i in 3:length(locations)){
            sp <- locations[i] - prev_value prev_value <- locations[i-2] t_spaces <- c(t_spaces,sp)
          }
          rgamma(296, rate = 0.001, shape = 1)
          hist(rgamma(296, rate = 0.001, shape = 1),
               col= "lightcoral", #rgb(1,0,0,0.5), xlim=c(0,6000), ylim=c(0,150), main="Overlapping Histogram", xlab="Variable")
               hist(t_spaces,
                    col= rgb(0,1,1,0.5), add=T)
               legend("topright", c("Gamma", "CMV Data"), fill=c("lightcoral", "mediumturquoise"))
               #Emma code portion:
               Palindromes <- read.table(“https://math189.edublogs.org/files/2019/02/hcmv-263hxkx-1qhtfgz.txt”, header = FALSE)
               set.seed(2017)
               n <- 296
               
               Uniform <- runif(n, min = 1, max = 229354)
               hist(Uniform, breaks = 42, probability = TRUE, col = 4, main = "Random Uniform Scatter", xlab = "Bases", ylab = "Density")
               lines(density(sample, adjust = 2), col = 10)
               counts <- as.vector(tab)
               head(counts, 10)
               [1] 7 5 5 9 5 4 7 6 6 9
               hist(counts, breaks = 15, col = rgb(0,0,1,0.5), probability = TRUE, main = "Counts and Poisson Distribution", xlab = "Number of Palindromes in Interval", ylim = c(0,0.2))
               lines(density(counts, adjust = 2), col = rgb(0,0,1,0.9))
               Position <- data[,1]
               k <- 42
               tab <- table(cut(Position, breaks = seq(1, 229354, length.out = k+1), include.lowest = TRUE)) head(tab, 10)
               [1,5.46e+03] (5.46e+03,1.09e+04] (1.09e+04,1.64e+04]
755
(1.64e+04,2.18e+04] (2.18e+04,2.73e+04] (2.73e+04,3.28e+04]
954
(3.28e+04,3.82e+04] (3.82e+04,4.37e+04] (4.37e+04,4.91e+04]
766
(4.91e+04,5.46e+04]
9
P <- rpois(296, lambda = mean(counts))
hist(P, breaks = 15, col = rgb(1,0,0,0.5), probability = TRUE, add = TRUE)
lines(density(Pois, adjust = 2), col = rgb(1,0,0,0.8))
legend(x = 10, y = 0.15, legend = c("CMV Data", "Poisson"), lty = c(1,1), col = c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))
library(lattice)
stripplot(Uniform, pch=16, cex=0.25) stripplot(Position, pch=16, cex=0.25)
####### Alternate Hypothesis
CMV <- read.csv("~/Downloads/CMV.csv", header=FALSE) View(CMV)
positive = mean(CMV$V2)
negative = mean(CMV$V2)
glm(formula = hiv ~ cmv, family = binomial(link = "logit"), data = trainSet)

newdata = data.frame(cmv=1) predict(model,newdata,type="response")
newdata = data.frame(cmv=2) predict(model,newdata,type="response")
newdata = data.frame(cmv=3) predict(model,newdata,type="response")
~~~~~~Talal~~~~~~~~~~~~
  library(ggplot2)
library(plotly)
library(moments)
library(kableExtra)
library(knitr)
options(knitr.table.format = "html") options(kableExtra.latex.load_packages = FALSE)
data <- read.csv("C:/Users/taq19/Downloads/hcmv.txt", sep="") palindromes <- data
expected <- function(c){
  b = length(data$location)/ (N/n) a = (b^c)/(factorial(c)) #5.8 -->
  a * (exp(-b)) }
296 / N/n
#To graph the counts of palindrome hits per interval of 4000: n = 4000
N = 229354
my_bins <- seq(n, N, n) #interval of 5000
a = N/n # #of intervals

my_counts <- c()
for(b in my_bins){
  count <- length(palindromes[which(palindromes$location < b &
                                      palindromes$location > b - n),]) my_counts <- c(my_counts,count)
}
loc_df <- cbind(my_bins,my_counts) loc_df <- as.data.frame(loc_df)
g <- ggplot(loc_df, aes(x=my_bins, y=my_counts)) +
  geom_bar(stat="identity", fill="mediumturquoise") +
  labs(title="Location of Palindromes in consecutive intervals of 4000 base pairs",
       x = "Location on chain of DNA Base pairs",
       y = "Number of palindromes in the interval") + scale_x_discrete(limits = seq(n, N, n*3)) + theme(axis.text.x = element_text(angle=65, vjust=0.6))
g
#To count the observed palindrome hits and the expected value of palindrome hits per 5000 interval
my_palindrome <- seq(0, max(my_counts[]))
expected_values <- cbind()
my_expected <- c() my_observed <- c() for(i in my_palindrome){
  observed <- sum(my_counts == i)
  expect <- expected(i) * a
  my_observed <- c(my_observed, observed)
  my_expected <- c(my_expected, round(expected(i) * a, digits=1))
}

#binding all three columns to form palindrome count table my_df <- cbind(my_palindrome, my_observed, my_expected) my_df <- as.data.frame(my_df)
my_df
graph_df <- as.data.frame(cbind(my_palindrome, my_observed))
#collapsing 0-2 and 9+
site.random.tab <- as.data.frame(matrix(rep(NA, 8*3), 8, 3))
site.random.tab[1, 2:3] <- colSums(my_df[1:3, 2:3]) #0-2 site.random.tab[1,1] <- "0-2"
site.random.tab[2:7,] <- my_df[4:9, 1:3]
site.random.tab[8, 2:3] <- colSums(my_df[10:length(my_df[,2]), 2:3]) #9+ site.random.tab[7,1] <- "9+"
site.random.tab
new_df <- site.random.tab
names(my_df) <- c("Palindrome Counts", "Number Observed", "Number Expected") names(new_df) <- names(my_df)
names(graph_df) <- c("Palindrome Counts", "Number Observed")
graph_df <- graph_df[-c(1),]
new_df %>%
  kable() %>%
  kable_styling("bordered", full_width = F, position = "center")
my_df %>%
  kable() %>%
  kable_styling("bordered", full_width = F, position = "center")
graph_df %>%
  
  kable() %>%
  kable_styling("bordered", full_width = F, position = "center")
my_df
#Test statistic:sna
stats <- ((( (new_df[,2] - new_df[,3])^2) / new_df[,3])) stats[which(!is.finite(stats))] <- 0
stats
sum(stats) #test statistics proves that poisson model is a reasonable fit
pchisq(sum(stats), 7 - 2, lower.tail=FALSE) # chi squared test of test statistic = 0.98 qchisq(0.95, df = 7 - 2)
#Residual plots
Residuals <- (new_df[,2] - new_df[,3]) / sqrt(new_df[3]) Residuals <- round(Residuals, digits = 2)
plot(Residuals[,1], type = 'h', ylab = "standardized residuals", xaxt = "n",xlab = "interval index") axis(1, at=1:8, labels=new_df[1:8,1])
new_observed <- sum (my_df$my_observed[0:3]) new_expected <- sum(my_df$my_expected[0:3])
newRow <- data.frame(my_palindrome = 02, my_observed = new_observed, my_expected = new_expected)
newRow