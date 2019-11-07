#### Reading the data:

library(readr)
dat <- read_csv("Data.csv")

View(head(dat))


################################################
################## Q1: #########################
################################################



library(plyr)
library(ggplot2)

## Subsetting:
dat1 <- dat[dat$Stage == 'Closed Won' & dat$FY == "FY19",]


q1 <- with(dat1, ftable(dat1$`Sub Sector ID`,dat1$`Account Tier`))
q1 <- as.data.frame(q1)
colnames(q1) <- c("SubSector","Tier","Freq")
q1$win_percentage <- round((q1$Freq/sum(q1$Freq)*100),1)
q1$label = paste0(sprintf("%.0f", q1$win_percentage), "%")

q1[q1 == 0] <- NA


plot1 <- ggplot(q1, aes(x = factor(SubSector), y = win_percentage, fill = Tier)) +
          geom_bar(position = position_stack(), stat = "identity", width = .7) +
          geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) 

plot1 <- plot1 + labs(title = "Win (%) by SubSector and Tier (FY19)",
                         y="Win(%)", x = "Sub sector ID")

##########################################################################
dat1 <- dat[dat$Stage == 'Closed Won' & dat$FY == "FY20",]


q1 <- with(dat1, ftable(dat1$`Sub Sector ID`,dat1$`Account Tier`))
q1 <- as.data.frame(q1)
colnames(q1) <- c("SubSector","Tier","Freq")
q1$win_percentage <- round((q1$Freq/sum(q1$Freq)*100),1)
q1$label = paste0(sprintf("%.0f", q1$win_percentage), "%")

q1[q1 == 0] <- NA


plot2 <- ggplot(q1, aes(x = factor(SubSector), y = win_percentage, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) 

plot2 <- plot2 + labs(title = "Win (%) by SubSector and Tier (FY20)",
             y="Win(%)", x = "Sub sector ID")


library("ggpubr")
figure <- ggarrange(plot1,plot2,ncol = 2, nrow = 1)
figure




################################################
################## Q2: #########################
################################################

table(dat$Stage)

#How many opportunities are open (i.e. not closed lost or closed won) by stage and tier..
#e.g. we have x number of opportunities open at stage y for Tier 1



dat2 <- droplevels(dat[!dat$Stage == "Closed Lost",])
dat2 <- droplevels(dat2[!dat2$Stage == "Closed Won",])




q2 <- with(dat2, ftable(dat2$Stage,dat2$`Account Tier`))
q2 <- as.data.frame(q2)
colnames(q2) <- c("Stage","Tier","Freq")
q2[q2 == 0] <- NA
#q1$win_percentage <- round((q1$Freq/sum(q1$Freq)*100),1)
#q2$label = paste0(sprintf("%.0f", q1$win_percentage), "%")

plot3 <- ggplot(q2, aes(x = factor(Stage), y = Freq, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3.5)+
  coord_flip()

plot3 <- plot3 + labs(title = "Open opportunities by stage and tier",
                      y="Frequency", x = "Stage")
plot3


################################################
################## Q3: #########################
################################################
head(dat)

# What is the average order value (ABC) by Tier and Sub sector


