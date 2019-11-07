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

plot1

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

# What is the average order value (ABC) by Tier and Sub sector

q3 <- aggregate(dat$`ABC Amount`,by=list(dat$`Account Tier`,dat$`Sub Sector ID`),FUN=mean,na.rm=TRUE)
colnames(q3) <- c("Tier","SubSector","AverageOrder")
q3$Tier <- as.factor(q3$Tier)
plot4 <- ggplot(q3, aes(x = factor(SubSector), y = AverageOrder, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = round(AverageOrder,1)), position = position_stack(vjust = 0.5), size = 3.5)

plot4 <- plot4 + labs(title = "Average order value (ABC) by Tier and Sub sector",
                      y="Average", x = "Sub sector")
plot4


################################################
################## Q4: #########################
################################################


#What is average sales age by tier and sub sector

q3 <- aggregate(dat$`Sales Age`,by=list(dat$`Account Tier`,dat$`Sub Sector ID`),FUN=mean, na.rm=TRUE)
colnames(q3) <- c("Tier","SubSector","SalesAge")
q3$Tier <- as.factor(q3$Tier)
plot5 <- ggplot(q3, aes(x = factor(SubSector), y = SalesAge, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = round(SalesAge,1)), position = position_stack(vjust = 0.5), size = 3.5)

plot5 <- plot5 + labs(title = "Average sales age by Tier and Sub sector",
                      y="Average", x = "Sub sector")
plot5


################################################
################## Q5: #########################
################################################

# For deals that have closed won what is the average sales age by tier and subsector

dat5 <- dat[dat$Stage == "Closed Won",]
q3 <- aggregate(dat5$`Sales Age`,by=list(dat5$`Account Tier`,dat5$`Sub Sector ID`),FUN=mean, na.rm=TRUE)
colnames(q3) <- c("Tier","SubSector","SalesAge")
q3$Tier <- as.factor(q3$Tier)
plot5 <- ggplot(q3, aes(x = factor(SubSector), y = SalesAge, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = round(SalesAge,1)), position = position_stack(vjust = 0.5), size = 3.5)

plot5 <- plot5 + labs(title = "Average sales age by Tier and Sub sector (Closed won)",
                      y="Average", x = "Sub sector")
plot5

# For deals that have closed lost what is the average sales age by tier and subsector
dat6 <- dat[dat$Stage == "Closed Lost",]
q3 <- aggregate(dat6$`Sales Age`,by=list(dat6$`Account Tier`,dat6$`Sub Sector ID`),FUN=mean, na.rm=TRUE)
colnames(q3) <- c("Tier","SubSector","SalesAge")
q3$Tier <- as.factor(q3$Tier)
plot5 <- ggplot(q3, aes(x = factor(SubSector), y = SalesAge, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = round(SalesAge,1)), position = position_stack(vjust = 0.5), size = 3.5)

plot5 <- plot5 + labs(title = "Average sales age by Tier and Sub sector (Closed won)",
                      y="Average", x = "Sub sector")
plot5

################################################
################## Q6: #########################
################################################





##################### Number of deals closed by tier by opportunity owner #########################


## Subsetting:
dat6 <- dat[dat$Stage == 'Closed Won',]


q1 <- with(dat6, ftable(dat6$`Opportunity Owner ID`,dat6$`Account Tier`))
q1 <- as.data.frame(q1)
colnames(q1) <- c("OpportunityOwner","Tier","Freq")
q1[q1 == 0] <- NA

q1$OpportunityOwner <- factor (q1$OpportunityOwner,
                        levels = c("Person1","Person2","Person6","Person7","Person8","Person9","Person10","Person11","Person13","Person14","Person15","Person16","Person17",
                                   "Person18","Person19","Person20", "Person21", "Person23", "Person24", "Person33"),ordered = TRUE)


plot6a <- ggplot(q1, aes(x = factor(OpportunityOwner), y = Freq, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3)+
  coord_flip()

plot6a <- plot6a + labs(title = "",
                      y="Number of deals closed won", x = "Opportunity owner")
plot6a



#########

dat6 <- dat[dat$Stage == "Closed Lost",]


q1 <- with(dat6, ftable(dat6$`Opportunity Owner ID`,dat6$`Account Tier`))
q1 <- as.data.frame(q1)
colnames(q1) <- c("OpportunityOwner","Tier","Freq")
q1[q1 == 0] <- NA

#nlevels(q1$OpportunityOwner)
q1$OpportunityOwner <- factor (q1$OpportunityOwner,
                               levels = c("Person1","Person2","Person3","Person5","Person6","Person7","Person8","Person9","Person12","Person13","Person14",
                                          "Person16","Person17","Person19","Person21","Person22","Person23","Person24","Person30",
                                          "Person31","Person32","Person33","Person34","Person35","Person36"),ordered = TRUE)


plot6b <- ggplot(q1, aes(x = factor(OpportunityOwner), y = Freq, fill = Tier)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3)+
  coord_flip()

plot6b <- plot6b + labs(title = "",
                        y="Number of deals closed lost", x = "Opportunity owner")
plot6b







library("ggpubr")
figure6 <- ggarrange(plot6a,plot6b,ncol = 1, nrow = 2)

figure6 + labs(title = "Number of deals closed by tier by opportunity owner")


############### Total value (ABC) of deals closed by opportunity owner##################

dat6 <- dat[(dat$Stage %in% c("Closed Lost","Closed Won")),]


q6 <- aggregate(dat6$`ABC Amount`,by=list(dat6$Stage,dat6$`Opportunity Owner ID`),FUN=SUM, na.rm=TRUE)

colnames(q6) <- c("Stage","OpportunityOwner","AverageOrder")

# #q6$OpportunityOwner <- factor (q6$OpportunityOwner,
# #                               levels = c("Person1","Person2","Person3","Person5","Person6","Person7","Person8","Person9","Person12","Person13","Person14",
#                                           "Person16","Person17","Person19","Person21","Person22","Person23","Person24","Person30",
#                                           "Person31","Person32","Person33","Person34","Person35","Person36"),ordered = TRUE)

plot6 <- ggplot(q6, aes(x = factor(OpportunityOwner), y = AverageOrder, fill = Stage)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = round(AverageOrder,1)), position = position_stack(vjust = 0.5), size = 3.5)+
  coord_flip()


plot6 + facet_grid(. ~  Stage)

plot6 <- plot6 + labs(title = "Total value (ABC) of deals closed by opportunity owner",
                      y="Average", x = "Sub sector")
plot5
