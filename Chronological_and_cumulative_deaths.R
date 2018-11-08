install.packages(c("scales", "zoo", "ggplot2", "ggthemes", "plyr", "reshape"))
y <- c("scales", "zoo", "ggplot2", "ggthemes", "plyr", "reshape")
lapply(y, require, character.only = TRUE)

load("~/Downloads/allfronts_battles.csv")
time.days <- as.Date(allfronts$end.date, "%m/%d/%y") - as.Date(allfronts$begin.date, "%m/%d/%y")
time.days[which(time.days<0)] <- 1
dates <- seq.Date(as.Date("1870-1-1"), to = as.Date("1950-1-1"), by = "day")
allfronts$casulties.rate <- allfronts$casulties.numbers/(as.numeric(time.days) + 1)
battle.rate.mat <- matrix(0,length(dates), length(allfronts$begin.date))
time.days <- as.numeric(time.days)

allfronts$begin.date <- as.Date(allfronts$begin.date, "%m/%d/%y")
allfronts$begin.date <- unlist(lapply(allfronts$begin.date, 
  function(x) seq(x, length=2, by="-100 years")[2]))
allfronts$end.date <- as.Date(allfronts$end.date, "%m/%d/%y")
allfronts$end.date <- unlist(lapply(allfronts$end.date, 
  function(x) seq(x, length=2, by="-100 years")[2]))
allfronts$begin.date <- as.Date(allfronts$begin.date)
allfronts$end.date <- as.Date(allfronts$end.date)

for(i in 1:dim(allfronts)[1]){
  battle.rate.mat[which(dates==(allfronts$begin.date)[i])
  :(which(dates==allfronts$begin.date[i]+time.days[i])), i] <- allfronts$casulties.rate[i]
}

agg.battle.rate=aggregate(battle.rate.mat,by=list(as.yearmon(dates)), FUN=sum)
agg.battle.rate[is.na(agg.battle.rate)] <- 0
battle.rate.mat.ww1=agg.battle.rate[which(agg.battle.rate[,1] == "Aug 1914"):
  which(agg.battle.rate[,1] == "Dec 1918"), -1]

battle.rate.mat.ww1.fronts.countries=aggregate(t(battle.rate.mat.ww1), 
  by = list(allfronts$Front,allfronts$ParsedCountry), FUN=sum)

colnames(battle.rate.mat.ww1.fronts.countries)[3:55] <- agg.battle.rate[which(agg.battle.rate[, 1] 
  == "Aug 1914"):which(agg.battle.rate[,1]=="Dec 1918"), 1]

colnames(battle.rate.mat.ww1.fronts.countries)[1:2] <- c("Front", "Country")

battle.rate.mat.ww1.fronts.countries.melt <- melt(battle.rate.mat.ww1.fronts.countries,
  id <- (c("Front", "Country")))

colnames(battle.rate.mat.ww1.fronts.countries.melt)[3:4] <- c("Date", "MonthlyCas")

battle.cumulative <- melt(cbind(battle.rate.mat.ww1.fronts.countries[, c(1,2)], 
  t(apply(battle.rate.mat.ww1.fronts.countries[, -c(1,2)], 1, cumsum))),id=c("Front", "Country"))

colnames(battle.cumulative)[3:4] <- c("Date", "MonthlyCas")

allies <- c("American", "Belgium", "UK", "France", 
  "Italy", "Portugal", "Russian Empire", "Serbia", "Greece", "Romania")

central <- c("Germany", "Austria-Hungary", "Bulgaria", "Ottoman Empire")

is.allies <- battle.rate.mat.ww1.fronts.countries.melt$Country %in% allies

battle.rate.mat.ww1.fronts.countries.melt <- cbind(battle.rate.mat.ww1.fronts.countries.melt,is.allies)

battle.rate.mat.ww1.fronts.countries.melt$dates <- as.Date(as.yearmon(
  as.numeric(as.character(battle.rate.mat.ww1.fronts.countries.melt$Date)))
)

p1 <- ggplot(battle.rate.mat.ww1.fronts.countries.melt, aes(dates, MonthlyCas, group=Country)) + 
  geom_bar(subset=.(is.allies==TRUE),stat="identity",aes(fill=Country), position = "stack") + 
  labs(list(title = "World War One Monthly Casualties by Fronts and Belligerents", 
    x = "Date", y = "Monthly Casualties (Right Side are Allies and Left Side are Central Powers")) + 
  geom_bar(subset=.(is.allies==FALSE), stat="identity", 
    aes(y=MonthlyCas*-1,fill=Country), position="stack") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  facet_grid(.~Front) + 
  coord_flip() 

p1 + scale_y_continuous(labels = comma) # Get rid of the scientific numbering on the axis

battle.cumulative$Date <- as.Date(as.yearmon(as.numeric(as.character(battle.cumulative$Date))))

p2 <- ggplot(battle.cumulative, aes((Date), MonthlyCas, group=Country)) + 
  geom_bar(stat = "identity", aes(fill = Country), position = "stack") + 
  labs(list(title = "World War One Cumulative Casualties by Fronts", 
            x = "Date", y = "Cumulative Casualties")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  facet_grid(.~Front)

p2 + scale_y_continuous(labels = comma) # Get rid of the scientific numbering on the axis
