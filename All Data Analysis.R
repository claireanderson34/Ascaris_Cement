####INTRODUCTION####
# Loading
library("readxl")
library("ggplot2")
library("dplyr")
library("effectsize")
library(RColorBrewer)
library(rstatix)
library(broom)
library(car)
library(tidyverse)

####FILE LOADING --------------------------------------------------------------------
#Removal Files
dataR <- read_excel("~/Ascaris Data.xlsx", sheet = "Removal")
dataR <- data.frame(dataR)
dataR$Reduction.Viable <- (dataR$Reduction.Viable*100)

#Survival Files
FWdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival FW")
FWdata <- data.frame(FWdata)
MWdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival MW")
MWdata <- data.frame(MWdata)
FDdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival FD")
FDdata <- data.frame(FDdata)
MDdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival MD")
MDdata <- data.frame(MDdata)
dataS <- rbind(MWdata,FWdata,MDdata,FDdata)
dataS$Temp <- c(rep("Wet",2210),rep("Dry",2210))
dataS$Tile <- c(rep("Fly Ash Mix",1105),rep("Mortar Mix",1105),rep("Fly Ash Mix",1105),rep("Mortar Mix",1105))

#Combined data files
dataS$Exp <- c(rep("Survival",4420))
dataR$Exp <- c(rep("Removal",2482))
dataAll <- rbind(dataS, dataR)



####STATISTICS ------------------------------------------------------------
#Experimental Setup
#Applied
dataApplied <- subset(dataAll, subset = (Sample.Type == "Positive"))
dataApplied <- subset(dataApplied, subset = !(is.na(Total.Egg)))
mean((dataApplied$Total.Egg),na.rm=TRUE)
sd((dataApplied$Total.Egg),na.rm=TRUE)
mean((dataApplied$Viable/dataApplied$Total.Egg),na.rm=TRUE)
sd((dataApplied$Viable/dataApplied$Total.Egg),na.rm=TRUE)
shapiro.test(dataApplied$Total.Egg)

a <- subset(dataApplied, subset = Tile.Type == "Mortar")
b <- subset(dataApplied, subset = Tile.Type == "Fly Ash")
t.test(a$Viable, b$Viable, paired=FALSE)

#Recovered
dataRecovered <- subset(dataAll, subset = !(Sample.Type == "Positive" | Sample.Type == "Wipe" | Sample.Type == "Negative"))
dataRecovered <- subset(dataRecovered, subset = !(is.na(Total.Egg)))
mean((dataRecovered$Total.Egg),na.rm=TRUE)
sd((dataRecovered$Total.Egg),na.rm=TRUE)
shapiro.test(dataRecovered$Total.Egg)

res.aov <- aov(formula= Total.Egg ~ Tile+Temp+Time, data=dataRecovered)
summary(res.aov)
aggregate(dataRecovered$Total.Egg, list(dataRecovered$Temp), FUN=mean)
aggregate(dataRecovered$Total.Egg, list(dataRecovered$Time), FUN=mean)

a <- subset(dataRecovered, subset = Tile == "Mortar Mix")
b <- subset(dataRecovered, subset = Tile == "Fly Ash Mix")
t.test(a$Total.Egg, b$Total.Egg, paired=FALSE)

a <- subset(dataRecovered, subset = Tile == "Mortar Mix" & Exp == "Survival")
b <- subset(dataRecovered, subset = Tile == "Fly Ash Mix" & Exp == "Survival")
t.test(a$Total.Egg, b$Total.Egg, paired=FALSE)

a <- subset(dataRecovered, subset = Tile == "Mortar Mix" & Exp == "Removal")
b <- subset(dataRecovered, subset = Tile == "Fly Ash Mix" & Exp == "Removal")
t.test(a$Total.Egg, b$Total.Egg, paired=FALSE)


#Removal Experiments
dataR1 <- subset(dataR, subset = !(is.na(Reduction.Viable)))
a <- subset(dataR1, subset = Tile.Type == "Mortar")
b <- subset(dataR1, subset = Tile.Type == "Fly Ash")
t.test(a$Reduction.Viable, b$Reduction.Viable, paired=FALSE)
aggregate(100-dataR1$Reduction.Viable, list(dataR1$Tile.Type), FUN=mean)
aggregate(dataR1$Reduction.Viable, list(dataR1$Tile.Type), FUN=sd)

dataR1 <- subset(dataR, subset = !(is.na(Reduction.Total)))
a <- subset(dataR1, subset = Tile.Type == "Mortar")
b <- subset(dataR1, subset = Tile.Type == "Fly Ash")
t.test(a$Reduction.Total, b$Reduction.Total, paired=FALSE)
aggregate(100-(100*dataR1$Reduction.Total), list(dataR1$Tile.Type), FUN=mean)
aggregate(100*dataR1$Reduction.Total, list(dataR1$Tile.Type), FUN=sd)


#Survival Experiments
data1 <- subset(dataS, subset = !is.na(ViablePercent))
data2 <- subset(dataS, subset = !is.na(DevelopedPercent))
a <- subset(data1, subset = Time==0)
b <- subset(data2, subset = Time==0)
aggregate(a$DevelopedPercent, list(a$Temp), FUN=mean)
aggregate(b$ViablePercent, list(b$Temp), FUN=mean)

data1 <- subset(dataS, subset = !is.na(ViablePercent))
data2 <- subset(dataS, subset = !is.na(DevelopedPercent))
a <- subset(data1, subset = Time==28)
b <- subset(data2, subset = Time==28)
aggregate(a$DevelopedPercent, list(a$Temp), FUN=mean)
aggregate(b$ViablePercent, list(b$Temp), FUN=mean)


data1 <- subset(dataS, subset = !is.na(ViablePercent))
a <- subset(data1, subset = Tile == "Mortar Mix" & Time==0)
b <- subset(data1, subset = Tile == "Fly Ash Mix" & Time==0)
t.test(a$DevelopedPercent, b$ViablePercent, paired=FALSE)
a <- subset(data1, subset = Tile == "Mortar Mix" & Time==2)
b <- subset(data1, subset = Tile == "Fly Ash Mix" & Time==2)
t.test(a$DevelopedPercent, b$ViablePercent, paired=FALSE)
a <- subset(data1, subset = Tile == "Mortar Mix" & Time==14)
b <- subset(data1, subset = Tile == "Fly Ash Mix" & Time==14)
t.test(a$DevelopedPercent, b$ViablePercent, paired=FALSE)
a <- subset(data1, subset = Tile == "Mortar Mix" & Time==28)
b <- subset(data1, subset = Tile == "Fly Ash Mix" & Time==28)
t.test(a$DevelopedPercent, b$ViablePercent, paired=FALSE)

data2 <- subset(dataS, subset = !is.na(DevelopedPercent))
a <- subset(data2, subset = Tile == "Mortar Mix" & Time==0)
b <- subset(data2, subset = Tile == "Fly Ash Mix" & Time==0)
t.test(a$DevelopedPercent, b$DevelopedPercent, paired=FALSE)
a <- subset(data2, subset = Tile == "Mortar Mix" & Time==2)
b <- subset(data2, subset = Tile == "Fly Ash Mix" & Time==2)
t.test(a$DevelopedPercent, b$DevelopedPercent, paired=FALSE)
a <- subset(data2, subset = Tile == "Mortar Mix" & Time==14)
b <- subset(data2, subset = Tile == "Fly Ash Mix" & Time==14)
t.test(a$DevelopedPercent, b$DevelopedPercent, paired=FALSE)
a <- subset(data2, subset = Tile == "Mortar Mix" & Time==28)
b <- subset(data2, subset = Tile == "Fly Ash Mix" & Time==28)
t.test(a$DevelopedPercent, b$DevelopedPercent, paired=FALSE)



####PLOTS --------------------------------------------------------------------
#Applied Eggs
dataApplied <- subset(dataAll, subset = (Sample.Type == "Positive"))
dataApplied <- subset(dataApplied, subset = !(is.na(Total.Egg)))
type <- c("Fly Ash Mix" = "OPC Fly Ash Mortar Mix","Mortar Mix" = "OPC Mortar Mix")
ggplot(data=dataApplied, aes(x=Total.Egg))+geom_histogram(bins=10, color='black',fill="gray")+
  theme_bw() +
  xlab("Number of Eggs Applied") +ylab("Count")+
  theme(legend.title=element_blank(),
        strip.text.x = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+ guides(color=guide_legend(ncol=3))+
  facet_wrap(~ Tile,labeller = as_labeller(type))
ggsave("Applied.png",dpi=330, path = "~/", width = 4, height = 3, device='png')

#Recovered Eggs - Total Number
dataRecovered <- subset(dataAll, subset = !(Sample.Type == "Positive" | Sample.Type == "Wipe" | Sample.Type == "Negative"))
dataRecovered <- subset(dataRecovered, subset = !(is.na(Total.Egg)))
type <- c("Fly Ash" = "OPC Fly Ash Mortar Mix Removal Experiments","Mortar" = "OPC Mortar Mix Removal Experiments","Fly Ash Dry"= "OPC Fly Ash Mortar Mix Survival Experiments\nDry Season (15°C, 75% RH)", "Fly Ash Wet" = "OPC Fly Ash Mortar Mix Survival Experiments\nWet Season (34°C, 75% RH)","Mortar Dry"= "OPC Mortar Mix Survival Experiments\nDry Season (15°C, 75% RH)", "Mortar Wet" = "OPC Mortar Mix Survival Experiments\nWet Season (34°C, 75% RH)")
ggplot(data=dataRecovered, aes(x=Total.Egg))+geom_histogram(bins=10, color='black',fill="gray")+
  theme_bw() +
  xlab("Number of Total Eggs Recovered") +ylab("Count")+
  theme(legend.title=element_blank(),
        strip.text.x = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+ guides(color=guide_legend(ncol=3))+
  facet_wrap(~ Tile.Type,labeller = as_labeller(type))
ggsave("Recovered.png",dpi=330, path = "~/", width = 10, height = 7, device='png')


###SURVIVAL SLOPE ANALYSIS --------------------------------------------------------------------
#Survival k values for ANOVA
data1 <- subset(dataS, subset = !is.na(LogRedDevel))
data1$LNDevel <- log(10^data1$LogRedDevel)
data2 <- subset(dataS, subset = !is.na(LogRedViable))
data2$LNViable <- log(10^data2$LogRedViable)

fitted_models <- data1 %>% group_by(Tile,Temp, Trial) %>% do(mod1 = (lm(LNDevel ~ Time, data = .))) %>% ungroup()
model_summary <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                         glance = map(mod1, broom::glance),
                                         augment = map(mod1, broom::augment),
                                         rsq = glance %>% map_dbl('r.squared'),
                                         slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                         int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                         slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                         intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summary$EggType <- c(rep("Developed", 52))

fitted_models <- data2 %>% group_by(Tile,Temp, Trial) %>% do(mod1 = (lm(LNViable ~ Time, data = .))) %>% ungroup()
model_summary2 <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                         glance = map(mod1, broom::glance),
                                         augment = map(mod1, broom::augment),
                                         rsq = glance %>% map_dbl('r.squared'),
                                         slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                         int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                         slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                         intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summary2$EggType <- c(rep("Viable", 52))

model_summary <- rbind(model_summary, model_summary2)
medianK <- median(model_summary$slope)

#Survival k values for table
fitted_models <- data1 %>% group_by(Tile,Temp) %>% do(mod1 = (lm(LNDevel ~ Time, data = .))) %>% ungroup()
model_summary1 <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                         glance = map(mod1, broom::glance),
                                         augment = map(mod1, broom::augment),
                                         rsq = glance %>% map_dbl('r.squared'),
                                         slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                         int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                         slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                         intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summary1$mod1 <- 0
model_summary1$tidy <- 0
model_summary1$glance <- 0
model_summary1$augment <- 0

fitted_models <- data2 %>% group_by(Tile,Temp) %>% do(mod1 = (lm(LNViable ~ Time, data = .))) %>% ungroup()
model_summary3 <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                          glance = map(mod1, broom::glance),
                                          augment = map(mod1, broom::augment),
                                          rsq = glance %>% map_dbl('r.squared'),
                                          slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                          int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                          slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                          intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summary3$mod1 <- 0
model_summary3$tidy <- 0
model_summary3$glance <- 0
model_summary3$augment <- 0

#Survival ANOVA for slope
#ANOVA
res.aov <- aov(formula= slope ~ Tile+Temp+EggType, data=model_summary)
summary(res.aov)
#Survival PostHoc
tukey.n.way<-TukeyHSD(x=res.aov, conf.level=0.95)
tukey.n.way


###SURVIVAL SLOPE ANALYSIS - Corrected for recovery differences --------------------------------------------------------------------
#Percent Recovered
AppliedMortar <- subset(dataApplied, subset = Tile == "Mortar Mix")
meanAppliedMortar <- mean(AppliedMortar$Total.Egg)
AppliedFlyAsh <- subset(dataApplied, subset = Tile == "Fly Ash Mix")
meanAppliedFlyAsh <- mean(AppliedFlyAsh$Total.Egg)

tile_types <- c("Mortar Wet", "Mortar Dry", "Fly Ash Wet", "Fly Ash Dry")
time_points <- c(0, 2, 14, 28)
results <- list()

for (tile in tile_types) {
  for (time in time_points) {
    subset_data <- subset(dataRecovered, Tile.Type == tile & Time == time)
    mean_applied <- ifelse(grepl("Mortar", tile), meanAppliedMortar, meanAppliedFlyAsh)
    mean_recovered <- mean(subset_data$Total.Egg)
    recovered_percent <- mean(subset_data$Total.Egg) / mean_applied
    results <- rbind(results, data.frame(Tile.Type = tile, Time = time, Mean_Applied = mean_applied, Mean_Recovered= mean_recovered, Recovered_Percent = recovered_percent))
  }
}

results_recoveredPercent <- as.data.frame(results)
mean_recoveredPercent <- mean(results_recoveredPercent$Recovered_Percent)
aggregate(results_recoveredPercent$Recovered_Percent, by=list(results_recoveredPercent$Time), FUN=mean)
results_recoveredPercent$Temp <- c(rep("wet", 4), rep("dry", 4),rep("wet", 4), rep("dry", 4))
aggregate(results_recoveredPercent$Recovered_Percent, by=list(results_recoveredPercent$Temp), FUN=mean)

#Survival k values for ANOVA Correction
data1 <- subset(dataS, subset = !is.na(LogRedDevel))
data2 <- subset(dataS, subset = !is.na(LogRedViable))
data1$Devel_Conc <- 10^data1$LogRedDevel
data2$Viable_Conc <- 10^data2$LogRedViable
data1 <- merge(data1, results_recoveredPercent, by = c("Time", "Tile.Type"))
data2 <- merge(data2, results_recoveredPercent, by = c("Time", "Tile.Type"))
data1$Devel_Corrected <- data1$Developed / data1$Recovered_Percent
data2$Viable_Corrected <- data2$Viable / data2$Recovered_Percent

data1 <- data1 %>%
  arrange(Trial, Tile.Type, Time)
data1 <- data1 %>%
  group_by(Trial, Tile.Type) %>%
  mutate(Devel_Corrected_T0 = first(Devel_Corrected),  # Get time 0 value per group
         LNDevel_Corrected = log(Devel_Corrected / Devel_Corrected_T0)) %>%
  ungroup()

data2 <- data2 %>%
  arrange(Trial, Tile.Type, Time)
data2 <- data2 %>%
  group_by(Trial, Tile.Type) %>%
  mutate(Viable_Corrected_T0 = first(Viable_Corrected),  # Get time 0 value per group
         LNViable_Corrected = log(Viable_Corrected / Viable_Corrected_T0)) %>%
  ungroup()

#Models correction
fitted_models <- data1 %>% group_by(Tile,Temp, Trial) %>% do(mod1 = (lm(LNDevel_Corrected ~ Time, data = .))) %>% ungroup()
model_summaryCorrected <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                         glance = map(mod1, broom::glance),
                                         augment = map(mod1, broom::augment),
                                         rsq = glance %>% map_dbl('r.squared'),
                                         slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                         int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                         slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                         intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summaryCorrected$EggType <- c(rep("Developed", 52))

fitted_models <- data2 %>% group_by(Tile,Temp, Trial) %>% do(mod1 = (lm(LNViable_Corrected ~ Time, data = .))) %>% ungroup()
model_summaryCorrected2 <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                          glance = map(mod1, broom::glance),
                                          augment = map(mod1, broom::augment),
                                          rsq = glance %>% map_dbl('r.squared'),
                                          slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                          int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                          slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                          intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summaryCorrected2$EggType <- c(rep("Viable", 52))

model_summaryCorrected <- rbind(model_summaryCorrected, model_summaryCorrected2)
medianKCorrected <- median(model_summaryCorrected$slope)

#Survival k values for table correction
fitted_models <- data1 %>% group_by(Tile,Temp) %>% do(mod1 = (lm(LNDevel_Corrected ~ Time, data = .))) %>% ungroup()
model_summaryCorrected1 <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                          glance = map(mod1, broom::glance),
                                          augment = map(mod1, broom::augment),
                                          rsq = glance %>% map_dbl('r.squared'),
                                          slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                          int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                          slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                          intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summaryCorrected1$mod1 <- 0
model_summaryCorrected1$tidy <- 0
model_summaryCorrected1$glance <- 0
model_summaryCorrected1$augment <- 0

fitted_models <- data2 %>% group_by(Tile,Temp) %>% do(mod1 = (lm(LNViable_Corrected ~ Time, data = .))) %>% ungroup()
model_summaryCorrected3 <-fitted_models %>% mutate(tidy = map(mod1, broom::tidy),
                                          glance = map(mod1, broom::glance),
                                          augment = map(mod1, broom::augment),
                                          rsq = glance %>% map_dbl('r.squared'),
                                          slope = tidy %>% map_dbl(function(x) x$estimate[2]),
                                          int = tidy %>% map_dbl(function(x) x$estimate[1]),
                                          slopePvalue = tidy %>% map_dbl(function(x) x$p.value[2]),
                                          intPvalue = tidy %>% map_dbl(function(x) x$p.value[1])) 
model_summaryCorrected3$mod1 <- 0
model_summaryCorrected3$tidy <- 0
model_summaryCorrected3$glance <- 0
model_summaryCorrected3$augment <- 0

#Survival ANOVA for slope correction
#ANOVA
res.aov <- aov(formula= slope ~ Tile+Temp+EggType, data=model_summaryCorrected)
summary(res.aov)
#Survival PostHoc
tukey.n.way<-TukeyHSD(x=res.aov, conf.level=0.95)
tukey.n.way

#Corrected LN plot vs. unadjusted LN plot
type <- c("LNViable_Corrected" = "Corrected Values", "LNViable"="Unadjusted Values",
  "Fly Ash Dry"= "OPC Fly Ash Mortar Mix Survival Experiments\nDry Season (15°C, 75% RH)", "Fly Ash Wet" = "OPC Fly Ash Mortar Mix Survival Experiments\nWet Season (34°C, 75% RH)","Mortar Dry"= "OPC Mortar Mix Survival Experiments\nDry Season (15°C, 75% RH)", "Mortar Wet" = "OPC Mortar Mix Survival Experiments\nWet Season (34°C, 75% RH)")
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
data2$LNViable <- log(10^data2$LogRedViable)
data2b <- c()
data2b <- pivot_longer(data2, cols=c(LNViable,LNViable_Corrected))
ggplot(data=data2b, aes(x=Time, y=(value), color=as.factor(Trial), group=Trial))+ 
  geom_line()+
  geom_jitter(width=0.1, size=2) +
  theme_bw() +
  xlab("Time (Days)") + ylab(expression("Natural Log Reduction in Viable Eggs"))+
  scale_color_manual(name="Trial", values=getPalette(13))+
  theme(strip.text.x = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "right")+ guides(color=guide_legend(ncol=1, 
                                                              override.aes=list(shape = 15, size=5)))+
  scale_x_continuous(breaks=seq(0,28,7))+
  facet_wrap(~ Tile.Type+name,labeller = as_labeller(type),ncol=2)
ggsave("LNViable_corrected.png",dpi=330, path = "~/", width = 8, height = 10, device='png')



