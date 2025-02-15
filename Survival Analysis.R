####INTRODUCTION####
# Loading
library("readxl")
library("ggplot2")
library("dplyr")
library("effectsize")

#Files
FWdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival FW")
FWdata <- data.frame(FWdata)
FWdata$Tile.Type <- rep("Fly Ash Wet",1105)
MWdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival MW")
MWdata <- data.frame(MWdata)
MWdata$Tile.Type <- rep("Mortar Wet",1105)
FDdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival FD")
FDdata <- data.frame(FDdata)
FDdata$Tile.Type <- rep("Fly Ash Dry",1105)
MDdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Survival MD")
MDdata <- data.frame(MDdata)
MDdata$Tile.Type <- rep("Mortar Dry",1105)
data <- rbind(MWdata,FWdata,MDdata,FDdata)
data$Temp <- c(rep("Wet",2210),rep("Dry",2210))
data$Tile <- c(rep("OPC Fly Ash Mortar",1105),rep("OPC Mortar",1105),rep("OPC Fly Ash Mortar",1105),rep("OPC Mortar",1105))


###Survival Plots --------------------------------
#All developed
library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
data1 <- subset(data, subset = !is.na(DevelopedPercent))
ggplot(data=data1, aes(x=Time, y=DevelopedPercent*100, color=as.factor(Trial), group=Trial))+ 
  geom_line()+
  geom_jitter(width=0.1, size=2) +
  theme_bw() +
  xlab("Time (Days)") + ylab(expression("Percent Developed Eggs of Total Eggs"))+
  scale_color_manual(name="Trial", values=getPalette(13))+
  theme(strip.text.x = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "right")+ guides(color=guide_legend(ncol=1,override.aes=list(shape = 15, size=5)))+
  scale_x_continuous(breaks=seq(0,28,7))+
  facet_wrap(~ Tile.Type,labeller = as_labeller(type))
ggsave("PerDev.png",dpi=330, path = "~/", width = 6, height = 6, device='png')

#All viable
data2 <- subset(data, subset = !is.na(ViablePercent))
ggplot(data=data2, aes(x=Time, y=ViablePercent*100, color=as.factor(Trial), group=Trial))+ 
  geom_line()+
  geom_jitter(width=0.1, size=2) +
  theme_bw() +
  xlab("Time (Days)") + ylab(expression("Percent Viable Eggs of Total Eggs"))+
  scale_color_manual(name="Trial", values=getPalette(13))+
  theme(strip.text.x = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "right")+ guides(color=guide_legend(ncol=1,override.aes=list(shape = 15, size=5)))+
  scale_x_continuous(breaks=seq(0,28,7))+
  facet_wrap(~ Tile.Type,labeller = as_labeller(type))
ggsave("PerVib.png",dpi=330, path = "~/", width = 6, height = 6, device='png')

#Summarized viable
new <- aggregate(data1$ViablePercent, list(data1$Time,data1$Tile,data1$Temp), FUN=sd)
new2 <- aggregate(data1$ViablePercent, list(data1$Time,data1$Tile,data1$Temp), FUN=mean)
new$mean <- new2$x
temp <- c("Dry"= "Dry Season\n(15°C, 75% RH)", "Wet" = "Wet Season\n(34°C, 75% RH)")
ggplot(new, aes(x = Group.1, group = Group.2)) + 
  geom_point(aes(y = mean*100, color = Group.2, shape=Group.2),alpha=.7, size=2.5) + 
  geom_errorbar(aes(y = mean*100, ymin = (mean - x)*100, ymax = (mean + x)*100, color = Group.2), alpha = .7) +
  theme_bw() +
  ylim(c(0,100))+
  xlab("Time (Days)") + ylab(expression("Percent Viable Eggs of Total Eggs (%)"))+
  scale_color_manual(name="Cement-Based Mix Design", values=c("#bb5566","#004488"))+
  scale_fill_manual(name="Cement-Based Mix Design", values=c("#bb5566","#004488"))+
  theme(strip.background =element_rect(fill="gray95"),
        strip.text.x = element_text(size = 9),
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 7),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(0,28,7))+
  facet_wrap(~ Group.3,labeller = as_labeller(temp))
ggsave("PerVib_All.png",dpi=330, path = "~/", width = 6, height = 3, device='png')

#Summarized developed
new <- aggregate(data2$DevelopedPercent, list(data2$Time,data1$Tile,data1$Temp), FUN=sd)
new2 <- aggregate(data2$DevelopedPercent, list(data2$Time,data1$Tile,data1$Temp), FUN=mean)
new$mean <- new2$x
temp <- c("Dry"= "Dry Season\n(15°C, 75% RH)", "Wet" = "Wet Season\n(34°C, 75% RH)")
ggplot(new, aes(x = Group.1, group = Group.2)) + 
  geom_point(aes(y = mean*100, color = Group.2, shape=Group.2), alpha=.7, size=2.5) + 
  geom_errorbar(aes(y = mean*100, ymin = (mean - x)*100, ymax = (mean + x)*100, color = Group.2), alpha = .7) +
  ylim(c(0,100))+
  theme_bw() +
  xlab("Time (Days)") + ylab(expression("Percent Developed Eggs of Total Eggs (%)"))+
  scale_color_manual(name="Cement-Based Mix Design", values=c("#bb5566","#004488"))+
  scale_fill_manual(name="Cement-Based Mix Design", values=c("#bb5566","#004488"))+
  scale_shape_manual(name = "Cement-Based Mix Design", values = c(16, 17)) +
  theme(strip.background =element_rect(fill="gray95"),
        strip.text.x = element_text(size = 9),
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 7),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+ guides(color=guide_legend(ncol=2,override.aes=list(shape = 15, size=5)))+
  scale_x_continuous(breaks=seq(0,28,7))+
  facet_wrap(~ Group.3,labeller = as_labeller(temp))
ggsave("PereDev_All.png",dpi=330, path = "~/", width = 6, height = 3.5, device='png')


###Additional Stats -----------------------
a <- subset(data1, subset = Tile == "OPC Mortar" & Time==28)
b <- subset(data1, subset = Tile == "OPC Fly Ash Mortar" & Time==28)
t.test(a$DevelopedPercent, b$DevelopedPercent, paired=FALSE)

a <- subset(data2, subset = Tile == "OPC Mortar" & Time==28)
b <- subset(data2, subset = Tile == "OPC Fly Ash Mortar" & Time==28)
t.test(a$ViablePercent, b$ViablePercent, paired=FALSE)

a <- subset(data1, subset = Time==28)
b <- subset(data2, subset = Time==28)
aggregate(a$DevelopedPercent, list(a$Temp), FUN=mean)
aggregate(b$ViablePercent, list(b$Temp), FUN=mean)
aggregate(a$DevelopedPercent, list(a$Temp), FUN=sd)
aggregate(b$ViablePercent, list(b$Temp), FUN=sd)


###Lit review graph --------------------------
#Files
Litdata <- read_excel("~/Ascaris Data.xlsx", sheet = "Lit Review")
Litdata <- data.frame(Litdata)
library(RColorBrewer)

log_time_data <- log10(Litdata$Time.to.99..Inactivation)
m <- lm(log_time_data ~ Litdata$Temperature)
intercept <- coef(m)[1]
slope <- coef(m)[2]
r2 <- summary(m)$r.squared  # Calculate R-squared
a <- as.numeric(10^intercept)
b <- as.numeric(slope)
eq <- substitute(italic(y) == a %.% "10"^{b %.% italic(x)}*","~~italic(R)^2~"="~r2, 
                 list(a = format(a, digits = 2), 
                      b = format(b, digits = 2),
                      r2 = format(r2, digits = 3)))
eq <- as.character(as.expression(eq))

g <- ggplot(data=Litdata, aes(x=Temperature...C., y=Time.to.99..Inactivation))+ 
      geom_point(aes(fill=(Matrix....Simplified.), shape=(Study...Simplified.)), size=3,color="black")+
      geom_smooth(method="lm", se=FALSE, color=1) +
      theme_bw() +
      xlab("Temperature (°C)") + ylab(expression("Time to 99% Inactivation (days)"))+
      scale_shape_manual(name="Study", values=c(21,24))+
      scale_fill_manual(name="Matrix", values=c("#ffffff","#ddaa33","#bb5566","#004488"))+
      scale_y_log10()+
      theme(strip.text.x = element_text(size = 10),
            panel.grid.minor = element_blank(),
            legend.position = "right") +
      guides(fill = guide_legend(override.aes = list(shape = 21)), # Ensures the fill legend uses shape 21
        shape = guide_legend())
g <- g + annotate("text",x = c(15), y = c(3), label=(eq), size=3, parse=TRUE)
g 
ggsave("Survival lit review.png",dpi=330, path = "~/", width = 6, height = 5, device='png')


getPalette <- colorRampPalette(brewer.pal(8, "Set1"))
g <- ggplot(data=Litdata, aes(x=Temperature...C., y=Time.to.99..Inactivation))+ 
  geom_point(aes(fill=(Study), shape=(Matrix....Simplified.)), size=3,color="black")+
  geom_smooth(method="lm", formula= (y ~ x), se=FALSE, color=1) +
  theme_bw() +
  xlab("Temperature (°C)") + ylab(expression("Time to 99% Inactivation (days)"))+
  scale_shape_manual(name="Matrix", values=c(21,22,23,24))+
  scale_fill_manual(name="Study", values=getPalette(27))+
  scale_y_log10()+
  theme(strip.text.x = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(shape = 21)), # Ensures the fill legend uses shape 21
         shape = guide_legend())
g <- g + annotate("text",x = c(15), y = c(3), label=(eq), size=3, parse=TRUE)
g 
ggsave("Survival lit review_All.png",dpi=330, path = "~/", width = 9, height = 6, device='png')


#MODEL TESTING
#Exponential model - nls
Litdata <- Litdata[!is.na(Litdata$Time.to.99..Inactivation), ]
Litdata_clean <- Litdata[Litdata$Temperature != 0, ]
exp_model <- nls(Time.to.99..Inactivation ~ a * 10^(b *Temperature...C.), data=Litdata_clean, start = list(a = 4000, b = -0.1))
summary(exp_model)

#Linear model- log-transformed data
log_time_data <- log10(Litdata$Time.to.99..Inactivation)
linear_model <- lm(log_time_data ~ Litdata$Temperature)
summary(linear_model)
intercept <- coef(linear_model)[1]
slope <- coef(linear_model)[2]
a_fitted <- 10^intercept  
b_fitted <- slope

#plot comparing nls vs linear
plot(Litdata$Temperature, Litdata$Time.to.99..Inactivation, xlab = "Temperature (°C)", ylab = "Time to 99% Inactivation (days)", pch = 19)
params <- coef(exp_model)
curve(params["a"] * 10^(params["b"] * x), add = TRUE, col = "red")
t_seq <- seq(0,44, length.out = 105)
linear_fitted <- a_fitted * 10^(b_fitted * t_seq)
lines(t_seq, linear_fitted, col = "blue", lwd = 2)
legend("topright", legend = c("nls fit", "log-transformed fit"), 
       col = c("red", "blue"), lwd = 2)

BIC(linear_model)
BIC(exp_model)
AIC(linear_model)
AIC(exp_model)
