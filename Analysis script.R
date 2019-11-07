#### Reading the data:

library(readr)
dat <- read_csv("Data.csv")

View(head(dat))

str(dat)


## Q1:

table(dat$Stage)

dat1 <- dat[dat$Stage == 'Closed Won',]

table(dat1$`Sub Sector ID`,dat1$`Account Tier`)

q1 <- with(dat1, ftable(dat1$`Sub Sector ID`,dat1$`Account Tier`,dat1$FY))
q1 <- as.data.frame(q1)
colnames(q1) <- c("SubSector","Tier","FY","Freq")
q1$win_percentage <- round((q1$Freq/sum(q1$Freq)*100),1)
q1 

library(ggplot2)
ggplot(q1, aes(x = SubSector, y = Tier, fill = win_percentage)) +
  geom_tile() +
  scale_fill_gradientn(name = "", colors = terrain.colors(10)) +
  scale_x_discrete(name = "") +
  scale_y_discrete(name = "")
