setwd("G:/My Drive/Fall 22/CMDA 4654/proj 2")
weightlifting <- read.csv("Olympic_Weightlifting_records.csv")

library(ggplot2)
library(stats)
#===============================================================================

weightlifting <- weightlifting[,2:11]
colnames(weightlifting) = c("Athlete", "Bodyweight", "Snatch", "Clean and Jerk",
                            "Total kg", "Ranking", "URL", "Title", "Year",
                            "Gender")
ggplot(data = weightlifting, mapping = aes(x = Bodyweight, y = Snatch, 
                                           color = Gender)) +
  geom_point()

factor(weightlifting$Title)
#corrections
weightlifting$Bodyweight[1351] = 80.55
weightlifting$Bodyweight[1352] = 81.00
weightlifting$Bodyweight[1353] = 80.80
write.csv(weightlifting, "Olympic_Weightlifting_records.csv")
#weightlifting$Bodyweight[1354] =
#weightlifting$Bodyweight[1409] =
#weightlifting$Bodyweight[1410] = 
#weightlifting$Bodyweight[1436] =

for (i in 1:nrow(weightlifting)) {
  if (weightlifting$Snatch[i] == -1) {
      weightlifting$Snatch[i] = 0
  }
  if (weightlifting$Clean.and.Jerk[i] == -1) {
    weightlifting$Clean.and.Jerk[i] = 0
  }
  weightlifting$Total.kg[i] = weightlifting$Snatch[i] +
    weightlifting$Clean.and.Jerk[i]
}

length(which(weightlifting$Snatch == 0))
length(which(weightlifting$Clean.and.Jerk == 0))

weightlifting <-  weightlifting[-which(weightlifting$Clean.and.Jerk == 0),]
weightlifting

men <- (weightlifting[which(weightlifting$Gender == "Men"),])
women <- weightlifting[which(weightlifting$Gender == "Women"),]


ggplot(data = men, mapping = aes(x = Bodyweight, y = Snatch)) +
  geom_point()


library(stats)
smooth.spline(weightlifting$Bodyweight, weightlifting$Snatch)
#=================================================================



players <- read.csv("Data/NBA_players.csv")
pairs(players[,2:10])
ggplot(data = players, mapping = aes(x = FG3A_PER_G, y = FGA_PER_G)) +
  geom_point()
  
ss.fit.1 <- smooth.spline(players$FG_PER_G, players$MP_PER_G)
ss.fit.2 <- smooth.spline(players$FG_PER_G, players$MP_PER_G, lambda = 1)
ss.fit.3 <- smooth.spline(players$FG_PER_G, players$MP_PER_G, lambda = 0.25)

plot(players$FG_PER_G, players$MP_PER_G)
lines(ss.fit.1, col = "red", lwd = 2)
lines(ss.fit.2, col = "blue", lwd = 2)
lines(ss.fit.3, col = "green", lwd = 2)

plot(players$FG_PCT, players$FGA_PER_G)

ss.fit.1 <- smooth.spline(players$FG_PER_G, players$MP_PER_G)
ss.fit.2 <- smooth.spline(players$FG_PER_G, players$MP_PER_G, lambda = 0)
ss.fit.3 <- smooth.spline(players$FG_PER_G, players$MP_PER_G, lambda = 0.25)

#===============================================================================
plot(players$MP_PER_G, players$FG_PER_G) #Obvious upwards trend
plot(players$MP_PER_G, players$FGA_PER_G) #Obvious upwards trend
plot(players$MP_PER_G, players$FG2_PER_G) #upwards trend
plot(players$MP_PER_G, players$FG2A_PER_G) #upwards trend
plot(players$MP_PER_G, players$FT_PER_G) #upwards trend
plot(players$MP_PER_G, players$FTA_PER_G) #upwards trend
plot(players$MP_PER_G, players$DRB_PER_G) #somewhat trend
plot(players$MP_PER_G, players$TRB_PER_G) #somewhat trend (check)
plot(players$MP_PER_G, players$AST_PER_G) #somewhat trend
plot(players$MP_PER_G, players$TOV_PER_G) #trend
plot(players$MP_PER_G, players$PF_PER_G) #somewhat trend (messy trend)
plot(players$MP_PER_G, players$PTS_PER_G) #trend

plot(players$MP_PER_G, players$PF_PER_G) #somewhat trend (check)
ss.fit.1 <- smooth.spline(players$MP_PER_G, players$PF_PER_G, cv=TRUE)
lines(ss.fit.1, col = "blue", lwd = 2)
ss.fit.1$lambda

loess.fit.1 <- loess(players$PF_PER_G ~ players$MP_PER_G, span = 0.5)
loess.fit.2 <- loess(players$PF_PER_G ~ players$MP_PER_G, span = 0.75)
loess.fit.3 <- loess(players$PF_PER_G ~ players$MP_PER_G, span = 0.9)

predicted.loess.1 <- predict(loess.fit.1)
predicted.loess.2 <- predict(loess.fit.2)
predicted.loess.3 <- predict(loess.fit.3)
lines(predicted.loess.1, col = "red")
lines(predicted.loess.2, col = "red")
lines(predicted.loess.3, col = "red")

ggplot(players, mapping = aes(MP_PER_G, PF_PER_G)) +
  geom_point() +
  geom_smooth(method = loess)

ggplot(data = players, mapping = aes(x = MP_PER_G, y = PF_PER_G)) +
  geom_point() +
  geom_line(mapping = 
              aes(x = MP_PER_G, 
                  y = predict(ss.fit.1, players$MP_PER_G)$y,
                  colour="SS"), 
            show.legend = F, 
            linewidth=1) +
  geom_smooth(method = loess, mapping = aes(colour = "LOESS")) +
  ggtitle("Minutes Played Vs. Fouls Automatic Smoothing Splines") +
  ylab("Personal Fouls Per Game") +
  xlab("Minutes Played Per Game") +
  theme_bw() +
  scale_fill_hue(labels = c("Loess", "SS"))

MP_PER_G <- players$MP_PER_G
PF_PER_G <- players$PF_PER_G
