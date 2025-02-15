####INTRODUCTION####
# Loading
library("readxl")
library("ggplot2")
library("dplyr")
library("effectsize")
library(Hmisc)

#Files
data <- read_excel("~/Ascaris Data.xlsx", sheet = "Removal")
data <- data.frame(data)
data <- subset(data, subset = !is.na(Total.Egg))
data <- subset(data, subset = Sample.Type == "Control" | Sample.Type == "Wipe")
data$Reduction.Viable <- (data$Reduction.Viable*100)


###STATISTICS --------------------------------
#Reduction t-test
  #Assumptions
  #t-test
  a <- subset(data, subset = Tile.Type == "Mortar")
  b <- subset(data, subset = Tile.Type == "Fly Ash")
  t.test(a$Reduction.Viable, b$Reduction.Viable, paired=FALSE)

#Data summary (means and SD) - grouped by condition
data$Condition <- paste(data$Tile.Type, data$Sample.Type)
Viable.Mean <- aggregate(100-data$Reduction.Viable, list(data$Condition), FUN=mean)
Viable.SD <- aggregate(data$Reduction.Viable, list(data$Condition), FUN=sd)
data$Viable.SD <- c(rep(Viable.SD$x[3:4], 13), rep(Viable.SD$x[1:2],13),rep(Viable.SD$x[1:2], 13), rep(Viable.SD$x[3:4],13))
data$Viable.Mean <- c(rep(Viable.Mean$x[3:4], 13), rep(Viable.Mean$x[1:2],13),rep(Viable.Mean$x[1:2], 13), rep(Viable.Mean$x[3:4],13))


####PLOTS --------------------------------
data %>% 
  mutate(Tile.Type = case_when(Tile.Type == "Mortar" ~ "OPC Mortar", 
                            TRUE~Tile.Type)) -> data
data %>% 
  mutate(Tile.Type = case_when(Tile.Type == "Fly Ash" ~ "OPC Fly Ash Mortar", 
                               TRUE~Tile.Type)) -> data
#Reduction Plot Points
p <- ggplot(data=data, aes(x=Tile.Type, y=100-Reduction.Viable, color=Tile.Type, fill=Tile.Type))+ 
  geom_violin(alpha=0.2)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.75, alpha=0.7) +
  scale_color_manual(values=c("#bb5566","#004488"))+
  scale_fill_manual(values=c("#bb5566","#004488"))+
  theme_bw() +
  xlab("Cement-Based Mix Design") +ylab("Percent Removal of Viable Eggs (%)")+
  scale_y_continuous(limits = c(0,105))+
  theme(legend.title=element_blank(),
        strip.text.x = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "none")
p+ stat_summary(fun.data=mean_sdl,  
                geom="pointrange", color="black")
ggsave("ReductionPoints0to100.png",dpi=330, path = "~/", width = 4, height = 3, device='png')
