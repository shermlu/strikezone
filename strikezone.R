library(devtools)
install_github("BillPetti/baseballr")
library(baseballr)

players_2018 = read.csv("~/Desktop/2018id.csv", header = TRUE)
pitchers_2018 = players_2018[which(players_2018$mlb_pos == "P"), ]

players_2014 = read.csv("~/Desktop/2014id.csv", header = TRUE)
pitchers_2014 = players_2014[which( (players_2014$ESPN_POS == "SP") |  (players_2014$ESPN_POS == "RP")), ]


data_cols = scrape_statcast_savant_batter(start_date = "2018-03-29", end_date = "2018-03-30", batterid = 592091)

called_pitches_2014 = data.frame(row.names = names(data_cols))
two_strikes = data.frame(row.names = names(data_cols))

# ### 2018 season: March 29 to October 1
# ### for 2018 players
pitchers_2018 = pitchers_2018[142: nrow(pitchers_2018), ]
for (id in pitchers_2018$mlb_id){
  pitcher_data = scrape_statcast_savant_pitcher(start_date = "2018-03-29", end_date = "2018-10-01", pitcherid = id)
  pitcher_data = pitcher_data[which( (pitcher_data$strikes == 2) ), ]
  two_strikes = rbind(two_strikes, pitcher_data)
  Sys.sleep(3)
}
write.csv(two_strikes, file = "~/Desktop/two_strike.csv")

two_strikes = two_strikes[which( (two_strikes$balls != 3) ), ]
two_strikesR = two_strikes[which(two_strikes$stand == "R"), ]
two_strikesL = two_strikes[which(two_strikes$stand == "L"), ]

### 2014 season: March 30 to September 28
### for 2014 players
# for (id in pitchers_2014$MLBAMID){
#   pitcher_data = scrape_statcast_savant_pitcher(start_date = "2014-03-30", end_date = "2014-09-28", pitcherid = id)
#   pitcher_data = pitcher_data[which( (pitcher_data$description == "ball") | (pitcher_data$description == "called_strike") ), ]
#   called_pitches_2014 = rbind(called_pitches_2014, pitcher_data)
# }
# write.csv(called_pitches, file = "~/Desktop/called_pitches_2014.csv")

called_pitches_2014 = read.csv("~/Desktop/called_pitches_2014.csv", header = TRUE)

called_pitches_2014$isStrike = as.numeric(as.logical(called_pitches_2014$description == "called_strike"))
called_pitches$isStrike = as.numeric(as.logical(called_pitches$description == "called_strike"))

called_strikes = called_pitches[which(called_pitches$description == "called_strike"), ]

called_strikesR = called_strikes[which(called_strikes$stand == "R"), ]
called_strikesL = called_strikes[which(called_strikes$stand == "L"), ]

# zone_coords = called_pitches[c("stand", "plate_x", "plate_z", "pfx_x", "pfx_z", "isStrike")]
# zone_coords = na.omit(zone_coords)


library("ggplot2")
library("gridExtra")
##########two strike density plots############
plottwoStrikeR = ggplot(two_strikesR, aes (plate_x, plate_z)) + 
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) + 
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, 
                                               ymin = 1.52, ymax = 3.42), 
            color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "All Pitches with a Two Strike Count to Right Handed Batters", 
       subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")

plottwoStrikeL = ggplot(two_strikesL, aes (plate_x, plate_z)) + 
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) + 
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, 
                                               ymin = 1.52, ymax = 3.42), 
            color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "All Pitches with a Two Strike Count to Left Handed Batters", 
       subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")

grid.arrange(plottwoStrikeR, plottwoStrikeL, ncol = 2, respect = TRUE)

########called strike density plots###############

plotAll = ggplot(called_strikes, aes (plate_x, plate_z)) + 
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) + 
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, 
                                               ymin = 1.52, ymax = 3.42), 
            color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "All Called Strikes", subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")


plotR = ggplot(called_strikesR, aes (plate_x, plate_z)) + 
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42), 
            color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + labs(title = "Called Strikes, Right-handed Batters", 
                                      subtitle = "From catcher's perspective, 2018 season", 
                                      x = "Horizontal Location (feet from middle of home plate)", 
                                      y = "Vertical Location (feet from ground)")


RHB_LHP = called_strikesR[which(called_strikesR$p_throws == "L"), ]
plotRHB_LHP = ggplot(RHB_LHP, aes (plate_x, plate_z)) +
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42), color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "Called Strikes, Right-handed Batters \nagainst Left-handed Pitchers", 
       subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")


RHB_RHP = called_strikesR[which(called_strikesR$p_throws == "R"), ]
plotRHB_RHP = ggplot(RHB_RHP, aes (plate_x, plate_z)) +
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42), color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "Called Strikes, Right-handed Batters \nagainst Right-handed Pitchers", 
       subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")


plotL = ggplot(called_strikesL, aes (plate_x, plate_z)) + stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42), color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "Called Strikes, Left-handed Batters", subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")

LHB_LHP = called_strikesL[which(called_strikesL$p_throws == "L"), ]
plotLHB_LHP = ggplot(LHB_LHP, aes (plate_x, plate_z)) +
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42), color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "Called Strikes, Left-handed Batters \nagainst Left-handed Pitchers", 
       subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")

LHB_RHP = called_strikesL[which(called_strikesL$p_throws == "R"), ]
plotLHB_RHP = ggplot(LHB_RHP, aes (plate_x, plate_z)) +
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
  scale_fill_distiller(palette = "RdYlBu") +
  geom_rect(inherit.aes = FALSE, mapping = aes(xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42), color="black", linetype="dashed", fill = NA) +
  xlim(-1.5, 1.5) + ylim(1, 4) + 
  labs(title = "Called Strikes, Left-handed Batters \nagainst Right-handed Pitchers", 
       subtitle = "From catcher's perspective, 2018 season", 
       x = "Horizontal Location (feet from middle of home plate)", 
       y = "Vertical Location (feet from ground)")

grid.arrange(plotAll, plotR, plotL, ncol = 3, respect = TRUE)
grid.arrange(plotLHB_RHP, plotLHB_LHP, plotRHB_RHP, plotRHB_LHP, ncol = 2, nrow = 2, respect = TRUE)

###contour plots###

library(mgcv)
library(parallel)

cl = makeCluster(detectCores()-1)
cl

myx = matrix(data=seq(from=-1.5, to=1.5, length=100), nrow=100, ncol=100)    
myz = t(matrix(data=seq(from=1,to=4, length=100), nrow=100, ncol=100))    
fitdata = data.frame(plate_x=as.vector(myx), plate_z=as.vector(myz))
set.seed(1)

###2018 contour
fit.All = bam(isStrike ~ s(plate_x, plate_z, k=50), method="GCV.Cp", 
             data=called_pitches, family=binomial(link="logit"), cluster=cl)
mypredict.All = predict(fit.All, fitdata, type="response")
mypredict.All = matrix(mypredict.All, nrow=c(100,100))

pitchesRHB = subset(called_pitches, called_pitches$stand=="R")
fit.R = bam(isStrike ~ s(plate_x, plate_z, k=50), method="GCV.Cp", 
             data=pitchesRHB, family=binomial(link="logit"), cluster=cl)
mypredict.R = predict(fit.R, fitdata, type="response")
mypredict.R = matrix(mypredict.R, nrow=c(100,100))

pitchesLHB = subset(called_pitches, called_pitches$stand=="L")
fit.L = bam(isStrike ~ s(plate_x, plate_z, k=50), method="GCV.Cp", 
             data=pitchesLHB, family=binomial(link="logit"), cluster=cl)
mypredict.L = predict(fit.L, fitdata, type="response")
mypredict.L = matrix(mypredict.L, nrow=c(100,100))

###2014 contour
fit.All2014 = bam(isStrike ~ s(plate_x, plate_z, k=50), method="GCV.Cp", 
                   data=called_pitches_2014, family=binomial(link="logit"), cluster=cl)
mypredict.All2014 = predict(fit.All2014, fitdata, type="response")
mypredict.All2014 = matrix(mypredict.All2014, nrow=c(100,100))

pitchesRHB2014 = called_pitches_2014[which(called_pitches_2014$stand == "R"), ]
pitchesRHB2014 = pitchesRHB2014[c("plate_x", "plate_z", "isStrike")]
pitchesRHB2014 = na.omit(pitchesRHB2014)
fit.R2014 = bam(isStrike ~ s(plate_x, plate_z, k=50), method="GCV.Cp", 
                 data=pitchesRHB2014, family=binomial(link="logit"), cluster=cl)
mypredict.R2014 = predict(fit.R2014, fitdata, type="response")
mypredict.R2014 = matrix(mypredict.R2014, nrow=c(100,100))

pitchesLHB2014 = called_pitches_2014[which(called_pitches_2014$stand == "L"), ]
fit.L2014 = bam(isStrike ~ s(plate_x, plate_z, k=50), method="GCV.Cp", 
                 data=pitchesLHB2014, family=binomial(link="logit"), cluster=cl)
mypredict.L2014 = predict(fit.L2014, fitdata, type="response")
mypredict.L2014 = matrix(mypredict.L2014, nrow=c(100,100))

###plots

par(mfrow=c(1,3))

contour(x=seq(from=-1.5, to=1.5, length=100), y=seq(from=1, to=4, length=100),
        z=mypredict.All, lwd=2, lty="solid", axes=T, levels=.5, labels="", labcex=.1, col="red", 
        main="50% Strike Zone Contour for All Batters", sub = "From catcher's perspective",
        xlab="Horizontal Location (feet from middle of home plate)",
        ylab="Vertical Location (feet from ground)")
contour(x=seq(from=-1.5, to=1.5, length=100), y=seq(from=1, to=4, length=100),
        z=mypredict.All2014, lwd=2, lty="solid", axes=T, levels=.5, labels="", 
        labcex=.1, col="blue", add = TRUE)
rect(-0.83, 1.52, 0.83, 3.42, border="black", lty="dashed")
text(0, 2.5, "> 50% Strike Calls", cex = 1.5)
text(0, 1.25, "< 50% Strike Calls", cex=1.5)


contour(x=seq(from=-1.5, to=1.5, length=100), y=seq(from=1, to=4, length=100),
        z=mypredict.L, lwd=2, lty="solid", axes=T, levels=.5, labels="", labcex=.1, col="red", 
        main="LHB 50% Strike Zone Contour", sub = "From catcher's perspective",
        xlab="Horizontal Location (feet from middle of home plate)",
        ylab="Vertical Location (feet from ground)")
contour(x=seq(from=-1.5, to=1.5, length=100), y=seq(from=1, to=4, length=100),
        z=mypredict.L2014, lwd=2, lty="solid", axes=T, levels=.5, labels="", labcex=.1, col="blue", 
        add = TRUE)
legend("topleft", legend=c("2018", "2014"), col=c("red", "blue"), lty=c("solid", "solid"), 
       x.intersp=.25, bty="n")
rect(-0.83, 1.52, 0.83, 3.42, border="black", lty="dashed")
text(0, 2.5, "> 50% Strike Calls", cex = 1.5)
text(0, 1.25, "< 50% Strike Calls", cex=1.5)


contour(x=seq(from=-1.5, to=1.5, length=100), y=seq(from=1, to=4, length=100),
        z=mypredict.R, lwd=2, lty="solid", axes=T, levels=.5, labels="", labcex=.1, col="red",
        main="RHB 50% Strike Zone Contour", sub = "From catcher's perspective",
        xlab="Horizontal Location (feet from middle of home plate)",
        ylab="Vertical Location (feet from ground)")
contour(x=seq(from=-1.5, to=1.5, length=100), y=seq(from=1, to=4, length=100),
        z=mypredict.R2014, lwd=2, lty="solid", axes=T, levels=.5, labels="", labcex=.1, col="blue",
        add = TRUE)
rect(-0.83, 1.52, 0.83, 3.42, border="black", lty="dashed")
text(0, 2.5, "> 50% Strike Calls", cex = 1.5)
text(0, 1.25, "< 50% Strike Calls", cex=1.5)
