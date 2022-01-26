library(ggplot2)
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggpubr)
library(moments)
library(corrplot)

#import_data
players <- read.csv("C:\\Add\\Your\\Path\\football_players.csv", 
                        header=TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")

#change players dataframe
players <- data_frame(
  Player = players$Player,
  Origin = players$Origin,
  From_Country = players$From.Country.,
  From_Club = players$From.Club.,
  To_Country = players$To.Country.,
  To_Club = players$To.Club.,
  Position = players$Position,
  Fee_Euro = players$Fee...mln.,
  Age = players$Year - players$Born
  )

#get the summary for everything, in the end not used in the report
summary(data_frame(age=players$Age, Fee_Euro=players$Fee_Euro))

#players divided into positons (with all columns from players dataframe)
keepers = players[players$Position=="Goalkeeper",]
strikers = players[players$Position=="Striker",]
forwards = players[players$Position=="Forward",]
mids = players[players$Position=="Midfielder",]
defenders = players[players$Position=="Defender",]

##creating table of statisctics dividing into positions, calculating stats for age
Name <- c("All", "Forward", "Striker", "Defender", "Goalkeeper", "Midfielder")
Min <- c(min(players$Age), min(forwards$Age), min(strikers$Age), min(defenders$Age), min(keepers$Age), min(mids$Age))
First_Quantile <- c(quantile(players$Age, 0.25), quantile(forwards$Age, 0.25), quantile(strikers$Age, 0.25),
                    quantile(defenders$Age, 0.25), quantile(keepers$Age, 0.25), quantile(mids$Age, 0.25))
Median <- c(median(players$Age), median(forwards$Age), median(strikers$Age), median(defenders$Age), median(keepers$Age), median(mids$Age))
Third_Quantile <- c(quantile(players$Age, 0.75), quantile(forwards$Age, 0.75), quantile(strikers$Age, 0.75),
                    quantile(defenders$Age, 0.75), quantile(keepers$Age, 0.75), quantile(mids$Age, 0.75))
Max <- c(max(players$Age), max(forwards$Age), max(strikers$Age), max(defenders$Age), max(keepers$Age), max(mids$Age))
Mean <- c(mean(players$Age), mean(forwards$Age), mean(strikers$Age), mean(defenders$Age), mean(keepers$Age), mean(mids$Age))
Variance <- c(var(players$Age), var(forwards$Age), var(strikers$Age), var(defenders$Age), var(keepers$Age), var(mids$Age))
Std <- c(sd(players$Age), sd(forwards$Age), sd(strikers$Age), sd(defenders$Age), sd(keepers$Age), sd(mids$Age))
Skewness <- c(skewness(players$Age), skewness(forwards$Age), skewness(strikers$Age), 
              skewness(defenders$Age), skewness(keepers$Age), skewness(mids$Age))
Kurtosis <- c(kurtosis(players$Age), kurtosis(forwards$Age), kurtosis(strikers$Age), 
              kurtosis(defenders$Age), kurtosis(keepers$Age), kurtosis(mids$Age))

AgeStats <- data.frame(Name, Min, First_Quantile, Median, Third_Quantile, Max, Mean, Variance, Std, Skewness, Kurtosis)

##creating table of statistics dividing into positions, calculating stats for fee
Name <- c("All", "Forward", "Striker", "Defender", "Goalkeeper", "Midfielder")
Min <- c(min(players$Fee_Euro), min(forwards$Fee_Euro), min(strikers$Fee_Euro), min(defenders$Fee_Euro), min(keepers$Fee_Euro), min(mids$Fee_Euro))
First_Quantile <- c(quantile(players$Fee_Euro, 0.25), quantile(forwards$Fee_Euro, 0.25), quantile(strikers$Fee_Euro, 0.25),
                    quantile(defenders$Fee_Euro, 0.25), quantile(keepers$Fee_Euro, 0.25), quantile(mids$Fee_Euro, 0.25))
Median <- c(median(players$Fee_Euro), median(forwards$Fee_Euro), median(strikers$Fee_Euro), median(defenders$Fee_Euro), median(keepers$Fee_Euro), median(mids$Fee_Euro))
Third_Quantile <- c(quantile(players$Fee_Euro, 0.75), quantile(forwards$Fee_Euro, 0.75), quantile(strikers$Fee_Euro, 0.75),
                    quantile(defenders$Fee_Euro, 0.75), quantile(keepers$Fee_Euro, 0.75), quantile(mids$Fee_Euro, 0.75))
Max <- c(max(players$Fee_Euro), max(forwards$Fee_Euro), max(strikers$Fee_Euro), max(defenders$Fee_Euro), max(keepers$Fee_Euro), max(mids$Fee_Euro))
Mean <- c(mean(players$Fee_Euro), mean(forwards$Fee_Euro), mean(strikers$Fee_Euro), mean(defenders$Fee_Euro), mean(keepers$Fee_Euro), mean(mids$Fee_Euro))
Variance <- c(var(players$Fee_Euro), var(forwards$Fee_Euro), var(strikers$Fee_Euro), var(defenders$Fee_Euro), var(keepers$Fee_Euro), var(mids$Fee_Euro))
Std <- c(sd(players$Fee_Euro), sd(forwards$Fee_Euro), sd(strikers$Fee_Euro), sd(defenders$Fee_Euro), sd(keepers$Fee_Euro), sd(mids$Fee_Euro))
Skewness <- c(skewness(players$Fee_Euro), skewness(forwards$Fee_Euro), skewness(strikers$Fee_Euro), 
              skewness(defenders$Fee_Euro), skewness(keepers$Fee_Euro), skewness(mids$Fee_Euro))
Kurtosis <- c(kurtosis(players$Fee_Euro), kurtosis(forwards$Fee_Euro), kurtosis(strikers$Fee_Euro), 
              kurtosis(defenders$Fee_Euro), kurtosis(keepers$Fee_Euro), kurtosis(mids$Fee_Euro))

FeeStats <- data.frame(Name, Min, First_Quantile, Median, Third_Quantile, Max, Mean, Variance, Std, Skewness, Kurtosis)

#Get top 10 transfers
head(players[order(players$Fee_Euro, decreasing=T),], n = 10)

#### Box Plot and histogram Per Positon
g1 <- ggplot(players, aes(x=Position, y=Fee_Euro, fill=Position)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4, alpha=0.7) +
  geom_jitter(width=0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", 
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))+
  ggtitle("Positions boxplot")

g2 <- ggplot(players,aes(x=Position)) +
  geom_histogram(fill="sienna1", stat="count", colour = "black", alpha=0.7) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Positions histogram")

ggarrange(g1, g2, ncol=2, nrow=1, font.label=list(size = 10, family='A'))

### Box Plot and histogram Per Country To
g3 <- ggplot(players, aes(x=To_Country, y=Fee_Euro, fill=To_Country)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4, alpha=0.7) +
  geom_jitter(width=0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", 
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))+
  ggtitle("Country to boxplot")

g4 <- ggplot(players, aes(x=To_Country)) +
  geom_histogram(fill="mediumpurple1", stat="count", colour = "black", alpha=0.7) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Country to histogram")

ggarrange(g3, g4, ncol=2, nrow=1, font.label=list(size = 10, family='A'))

### Box Plot and histogram Country From
g5 <- ggplot(players, aes(x=From_Country, y=Fee_Euro, fill=From_Country)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4, alpha=0.7) +
  geom_jitter(width=0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", 
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))+
  ggtitle("Country from boxplot")

g6 <- ggplot(players,aes(x=From_Country)) +
  geom_histogram(fill="turquoise3", stat="count", colour = "black", alpha=0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Country from histogram")

ggarrange(g5, g6, ncol=2, nrow=1, font.label=list(size = 10, family='A'))


### Histogram and boxplot for player origins
g7 <- ggplot(players, aes(x=Origin, y=Fee_Euro, fill=Origin)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4, alpha=0.7) +
  geom_jitter(width=0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", 
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))+
  ggtitle("Players origins boxplot")

g8 <- ggplot(players,aes(x=Origin, fill=Origin)) +
  geom_histogram(fill="darkolivegreen3", stat="count", colour = "black", alpha=0.7) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x = element_blank(), 
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Players origins histogram")

ggarrange(g7, g8, ncol=2, nrow=1, font.label=list(size = 10, family='A'))

### clubs total incomes and outcomes from big transfers (in two different barplots)
### in the end it isn't used in report
g9 <- ggplot(players, aes(x=To_Club, y=Fee_Euro, fill=To_Club)) +
  stat_summary(fun = "mean", geom = "bar", color='black') + 
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Barplot of total costs by club")

g10 <- ggplot(players, aes(From_Club, Fee_Euro, fill=From_Club)) + 
  stat_summary(fun = "mean", geom = "bar", color='black') +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", plot.title = element_text(hjust = 0.5))+
  ggtitle("Barplot of total income by club")

ggarrange(g9, g10, ncol=1, nrow=2, font.label=list(size = 10, family='A'))

### barplot of total incomes-expenses for all mentioned clubs
clubs_from <- data_frame(
  club=players$From_Club,
  money_difference = 0
)
clubs_from <- unique(clubs_from)
clubs_to <- data_frame(
  club=players$To_Club,
  money_difference = 0
)
clubs_to <- unique(clubs_to)

missing_clubs <-  which(!clubs_from$club %in% clubs_to$club) #check which df1 ID's are not in df2, see function is.element()
df.toadd <- clubs_from[missing_clubs,] #define the data frame to add to df2
all_clubs <- rbind(df.toadd, clubs_to) #use rbind to add it


for (row in 1:nrow(players)) {
  fee <- players[row, "Fee_Euro"]
  from_club  <- players[row, "From_Club"]
  to_club  <- players[row, "To_Club"]
  
  from_row <- which(all_clubs==from_club$From_Club)
  all_clubs[from_row, "money_difference"] = all_clubs[from_row, "money_difference"] + fee
  
  to_row <- which(all_clubs==to_club$To_Club)
  all_clubs[to_row, "money_difference"] = all_clubs[to_row, "money_difference"] - fee
  
}

ggplot(all_clubs, aes(reorder(club,money_difference), money_difference, fill=club)) + 
  geom_bar(stat="identity", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "none", 
        plot.title = element_text(hjust = 0.5), axis.title.x = element_blank())+
  ggtitle("Barplot of total incomes minus expenses by club")


### correlation matrix
corrplot(cor(data_frame(Age=players$Age, Fee_Euro=sqrt(players$Fee_Euro))), method = 'number')


### density for age
d1 <- ggplot(players, aes(x=Age, group=Position, fill=Position)) +
  geom_density(adjust=1.5, alpha=.4) + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank())+
  ggtitle("Density for positions when Age is considered")

d2 <- ggplot(players, aes(x=Fee_Euro, group=Position, fill=Position)) +
  geom_density(adjust=1.5, alpha=.4) + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank())+
  ggtitle("Density for positions when Fee_Euro is considered")
ggarrange(d1, d2, ncol=1, nrow=2, font.label=list(size = 10, family='A'))

d3 <- ggplot(players, aes(x=Age)) +
  geom_density(adjust=1.5, alpha=.6, fill = 'firebrick1') + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank())+
  ggtitle("Density for all players when Age is considered")

d4 <- ggplot(players, aes(x=Fee_Euro)) +
  geom_density(adjust=1.5, alpha=.6, fill='darkgoldenrod') + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank())+
  ggtitle("Density for all players when Fee_Euro is considered")
ggarrange(d3, d4, ncol=1, nrow=2, font.label=list(size = 10, family='A'))



### test if ages are normally distributed
shapiro.test(players$Age)

### test if fees are normally distributed
shapiro.test(players$Fee_Euro)
