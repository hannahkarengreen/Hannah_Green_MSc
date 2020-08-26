#load packages
library(dplyr)
library(ggplot2)
library(plotrix)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(raster) 
library(sf) 
library(rgeos) 
library(lwgeom)

#set working directory to folder containing code, data, and shape files.
setwd("~/")

#load in data
fish <- read.csv("fish.csv")
fish[,6:ncol(fish)] <- sapply(fish[,6:ncol(fish)], as.numeric)
fish$Year <- as.numeric(format(as.Date(fish$ï..Date, format="%m/%d/%y"), "%Y"))
fish$YearS <- fish$Year - 2006
fish <- as.data.frame(subset(fish, Year != "NA"))
fish$Month <- as.numeric(format(as.Date(fish$ï..Date, format="%m/%d/%y"), "%m"))
fish$CPUE <- fish$Catch.kg.100hrs/12.5
fish <- mutate(fish, CT = ifelse(Craft_Type == "BW", "BW/OM", 
                                 ifelse(Craft_Type == "OM", "BW/OM", Craft_Type)))
fish$Pelagic.kg <- fish$Dolphinfish.kg + fish$Bonito.kg + fish$Rainbow_runner.kg +
                    fish$Sailfish_Marlin.kg + fish$Wahoo.kg + fish$Dogtooth_tuna.kg +
                    fish$Yellowfin_tuna.kg + fish$Skipjack_tuna.kg + fish$Other_tuna.kg
fish$Reef.kg <- fish$Emporers.kg + fish$Snappers.kg + fish$Trevally.kg + fish$Groupers.kg
#the same data frame, but with the Locations separated into separate lines
#from data manipulation in excel. named TOTAL
load("data.Rda")

#descriptive stats: spatial distribution of effort
spatialY <- as.data.frame(
  TOTAL %>%
  group_by(Year, Location) %>%
  summarise(Effort = sum(Fishing_Effort)))
as.data.frame(
  spatialY %>%
  group_by(Location) %>%
  summarise(Mean = mean(Effort),
            sd = sd(Effort)))

spatialM <- as.data.frame(
  TOTAL %>%
    group_by(Year, Month, Location) %>%
    summarise(Effort = sum(Fishing_Effort)))
spatialM <- as.data.frame(
  spatialM %>%
    group_by(Month, Location) %>%
    summarise(Mean = mean(Effort))
)
as.data.frame(
  spatialM %>%
    group_by(Location) %>%
    summarise(Mean2 = mean(Mean),
              SD = sd(Mean))
)


#Figure 1: spatial variation in fishing effort around Diego Garcia

#read in the shape files for DG, OS areas, ABC areas, and the no fishing zone
Chagos <- st_read('Chagos_v6.shp') #DG outline
OS_Areas <- st_read("DG_OS_Areas.shp") #ocean side fishing areas
DG_RA <- st_read("DG_RA.shp") #no-fishing zone 
ABC <- st_read('DG_A.shp') #lagoon side fishing areas
ABC$A <- c('C', 'B', 'A')

#sum fishing effort for each location
Locations <- as.data.frame(TOTAL %>%
                             group_by(Location) %>%
                             summarise(Effort = sum(Fishing_Effort)))

#merge effort data with fishing areas
OS_Efforts <- merge(OS_Areas, Locations, by.x='OS',  
                    by.y = 'Location')
ABC_Efforts <- merge(ABC, Locations, by.x='A',
                     by.y = 'Location')

#plot total fishing effort on map
Totalmap <- ggplot()+
  geom_sf(data=OS_Efforts, aes(fill=Effort), color='grey30')+
  geom_sf(data=DG_RA, fill='grey30')+
  geom_sf(data=Chagos, color='grey10', fill='0')+
  geom_sf(data=ABC_Efforts, aes(fill=Effort), color='grey30')+
  scale_fill_gradient2(low="white", mid="yellow",  high="red2", 
                       midpoint=18851.5 ,limits=c(0, 37703))+
  labs(x="", y="")+
  theme_classic()+
  annotate(geom="text", x=72.36,  y=-7.23,  label="OS1")+
  annotate(geom="text", x=72.35,  y=-7.31,  label="OS2")+
  annotate(geom="text", x=72.37,  y=-7.42,  label="OS3")+
  annotate(geom="text", x=72.5,   y=-7.43,  label="OS4")+
  annotate(geom="text", x=72.515, y=-7.3,   label="OS5")+
  annotate(geom="text", x=72.5,   y=-7.23,  label="OS6")+
  annotate(geom="text", x=72.39,  y=-7.272, label="A")+
  annotate(geom="text", x=72.435, y=-7.29,  label="B")+
  annotate(geom="text", x=72.448, y=-7.33,  label="C")+
  labs(fill="Fishing 
effort (hours)")+
  coord_sf(xlim=c(72.330, 72.54), ylim=c(-7.49, -7.21), expand=FALSE)+
  theme(panel.background = element_rect(fill = "grey10"))

#sum fishing effort by craft type
sLocations <- as.data.frame(TOTAL %>%
                              group_by(Location, CT) %>%
                              summarise(Effort = sum(Fishing_Effort)))

#BW/OM crafts only
BWOMLocations <- subset(sLocations, CT=="BW/OM")
BWOM.OS_Efforts <- merge(OS_Areas, BWOMLocations, by.x='OS',  
                         by.y = 'Location')
BWOM.ABC_Efforts <- merge(ABC, BWOMLocations, by.x='A',
                          by.y = 'Location')
BWOM <- ggplot()+
  geom_sf(data=BWOM.OS_Efforts, aes(fill=Effort), color='grey30')+
  geom_sf(data=DG_RA, fill='grey30')+
  geom_sf(data=Chagos, color='grey10', fill='0')+
  geom_sf(data=BWOM.ABC_Efforts, aes(fill=Effort), color='grey30')+
  scale_fill_gradient2(low="white", mid="yellow",  high="red2", 
                       midpoint = 13636, limits=c(0, 27271.75))+
  theme_classic()+
  annotate(geom="text", x=72.36,  y=-7.23,  label="OS1")+
  annotate(geom="text", x=72.35,  y=-7.31,  label="OS2")+
  annotate(geom="text", x=72.37,  y=-7.42,  label="OS3")+
  annotate(geom="text", x=72.5,   y=-7.43,  label="OS4")+
  annotate(geom="text", x=72.515, y=-7.3,   label="OS5")+
  annotate(geom="text", x=72.5,   y=-7.23,  label="OS6")+
  annotate(geom="text", x=72.39,  y=-7.272, label="A")+
  annotate(geom="text", x=72.435, y=-7.29,  label="B")+
  annotate(geom="text", x=72.448, y=-7.33,  label="C")+
  labs(y="", x="", title="",
       fill="Fishing 
effort (hours)")+
  coord_sf(xlim=c(72.330, 72.54), ylim=c(-7.49, -7.21), expand=FALSE)+
  theme(panel.background = element_rect(fill = "grey10"))

#LCM crafts only
LCMLocations <- subset(sLocations, CT=="LCM")
LCMLocations <- rbind(LCMLocations, c("C", "LCM", 0))
LCMLocations$Effort <- as.numeric(LCMLocations$Effort)
LCM.OS_Efforts <- merge(OS_Areas, LCMLocations, by.x='OS',  
                        by.y = 'Location')
LCM.ABC_Efforts <- merge(ABC, LCMLocations, by.x='A',
                         by.y = 'Location')
LCM <- ggplot()+
  geom_sf(data=LCM.OS_Efforts, aes(fill=Effort), color='grey30')+
  geom_sf(data=DG_RA, fill='grey30')+
  geom_sf(data=Chagos, color='grey10', fill='0')+
  geom_sf(data=LCM.ABC_Efforts, aes(fill=Effort), color='grey30')+
  scale_fill_gradient2(low="white", mid="yellow",  high="red2", 
                       midpoint = 13636, limits=c(0, 27271.75))+
  theme_classic()+
  annotate(geom="text", x=72.36,  y=-7.23,  label="OS1")+
  annotate(geom="text", x=72.35,  y=-7.31,  label="OS2")+
  annotate(geom="text", x=72.37,  y=-7.42,  label="OS3")+
  annotate(geom="text", x=72.5,   y=-7.43,  label="OS4")+
  annotate(geom="text", x=72.515, y=-7.3,   label="OS5")+
  annotate(geom="text", x=72.5,   y=-7.23,  label="OS6")+
  annotate(geom="text", x=72.39,  y=-7.272, label="A")+
  annotate(geom="text", x=72.435, y=-7.29,  label="B")+
  annotate(geom="text", x=72.448, y=-7.33,  label="C")+
  labs(y="", x="", title="Sum of LCM Fishing Effort between 
November 2007 and December 2019",
       fill="Fishing 
effort (hours)")+
  coord_sf(xlim=c(72.330, 72.54), ylim=c(-7.49, -7.21), expand=FALSE)+
  theme(panel.background = element_rect(fill = "grey10"))

#Mako crafts only
MAKOLocations <- subset(sLocations, CT=="MAKO")
MAKOLocations <- rbind(MAKOLocations, 
                       c("OS1", "MAKO", 0), 
                       c("OS2", "MAKO", 0),
                       c("OS5", "MAKO", 0),
                       c("OS6", "MAKO", 0))
MAKOLocations$Effort <- as.numeric(MAKOLocations$Effort)
MAKO.OS_Efforts <- merge(OS_Areas, MAKOLocations, by.x='OS',  
                         by.y = 'Location')
MAKO.ABC_Efforts <- merge(ABC, MAKOLocations, by.x='A',
                          by.y = 'Location')
MAKO <- ggplot()+
  geom_sf(data=MAKO.OS_Efforts, aes(fill=Effort), color='grey30')+
  geom_sf(data=DG_RA, fill='grey30')+
  geom_sf(data=Chagos, color='grey10', fill='0')+
  geom_sf(data=MAKO.ABC_Efforts, aes(fill=Effort), color='grey30')+
  scale_fill_gradient2(low="white", mid="yellow",  high="red2", 
                       midpoint = 13636, limits=c(0, 27271.75))+
  theme_classic()+
  annotate(geom="text", x=72.36,  y=-7.23,  label="OS1")+
  annotate(geom="text", x=72.35,  y=-7.31,  label="OS2")+
  annotate(geom="text", x=72.37,  y=-7.42,  label="OS3")+
  annotate(geom="text", x=72.5,   y=-7.43,  label="OS4")+
  annotate(geom="text", x=72.515, y=-7.3,   label="OS5")+
  annotate(geom="text", x=72.5,   y=-7.23,  label="OS6")+
  annotate(geom="text", x=72.39,  y=-7.272, label="A")+
  annotate(geom="text", x=72.435, y=-7.29,  label="B")+
  annotate(geom="text", x=72.448, y=-7.33,  label="C")+
  labs(y="", x="", title="",
       fill="Fishing 
effort (hours)")+
  coord_sf(xlim=c(72.330, 72.54), ylim=c(-7.49, -7.21), expand=FALSE)+
  theme(panel.background = element_rect(fill = "grey10"))

#plot crafts together
Crafts <- ggarrange(BWOM + labs(title="BW/OM"),
                    LCM + labs(title="LCM"),
                    MAKO + labs(title="Mako"),
                    common.legend = TRUE, legend = "right", nrow=1,
                    labels=c("B", "C", "D"), label.x=.2)
#plot total with crafts
ggarrange(Totalmap, Crafts, nrow=2, heights = c(2, 1.5), labels=c("A", ""),
          label.x = .3)


#Figure 2A: fishing effort (hours) / Year
#sum of fishing effort per year
Effort <- as.data.frame(fish %>%
  group_by(Year) %>%
  summarise(Effort = sum(Fishing_Effort)))
#add a blank space for 2015 (unavailable data)
Effort <- rbind(Effort, c(2015, 0))

Fig2A<- ggplot() + 
  geom_col(data=Effort, aes(x=Year, y=Effort), fill= 'skyblue', colour= 'black',
           width=1)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  labs(y="Fishing effort (hours)")+
  annotate("text", x=2007, y=8138.5, label = "3")+
  annotate("text", x=2008, y=9232.5, label = "4")+
  annotate("text", x=2009, y=23388.0, label = "10")+
  annotate("text", x=2010, y=25415.1, label = "12")+
  annotate("text", x=2011, y=8795.0, label = "5")+
  annotate("text", x=2012, y=17819.35, label = "12")+
  annotate("text", x=2013, y=28176.25, label = "12")+
  annotate("text", x=2014, y=25530, label = "12")+
  annotate("text", x=2015, y=700, label = "0")+
  annotate("text", x=2016, y=17958.75, label = "12")+
  annotate("text", x=2017, y=19103.75, label = "12")+
  annotate("text", x=2018, y=18967.60, label = "12")+
  annotate("text", x=2019, y=17871, label = "12")+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#descriptive stats: annual fishing effort
desc1 <- as.data.frame(fish %>%
                          group_by(Year) %>%
                          summarise(Effort = sum(Fishing_Effort)))
desc1
#mean annual fishing effort across all years
as.data.frame(
  desc1 %>%
  summarise(mean = mean(Effort),
            sd = sd(Effort)))

#Figure 2B: fishing effort (hours) / Year / CT
#group by craft type and year
EffortCT <- as.data.frame(fish %>%
                          group_by(CT, Year) %>%
                          summarise(Effort = sum(Fishing_Effort)))
#add blanks for 2015 (unavailable data)
EffortCT <- rbind(EffortCT, c("BW/OM", 2015, 0))
EffortCT <- rbind(EffortCT, c("LCM", 2015, 0))
EffortCT <- rbind(EffortCT, c("MAKO", 2015, 0))
EffortCT$Effort <- as.numeric(EffortCT$Effort)
Fig2B <- ggplot() + 
  geom_col(data=EffortCT, aes(x=Year, y=Effort, fill=CT), colour= 'black',
           width=.9, position="dodge")+
  scale_x_discrete(name="Year", breaks=seq(2007, 2019, 3))+
  labs(y="Fishing effort (hours)",
       fill= "Craft Type")+
  scale_y_continuous(breaks=seq(0, 16000, 5000))+
  scale_fill_brewer(palette="Set3")+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#mean yearly fishing effort, by craft
#separate out by craft
desc2 <- as.data.frame(fish %>%
                         group_by(CT,Year) %>%
                         summarise(Effort = sum(Fishing_Effort)))
desc2 <- as.data.frame(
  desc2 %>%
    group_by(CT) %>%
    summarise(mean = mean(Effort),
              sd = sd(Effort)))
desc2
as.data.frame(
  desc2 %>%
    summarise(mean2 = mean(mean),
              sd = sd(mean)))

#Figure 2C: fishing effort (hours) / Month
#group by month and year, to get monthly sums of fishing effort
df1 <- as.data.frame(fish %>%
  group_by(Month, Year) %>%
  summarise(Sum = sum(Fishing_Effort)))
#group by Month to find mean monthly effort
EffortM <- as.data.frame( df1 %>%
                            group_by(Month) %>%
                            summarise(Effort = mean(Sum),
                                      sd = sd(Sum)))
#add column Jan-Dec instead of 1-12, in chronological order
EffortM$MonthName <- factor(month.abb[EffortM$Month], levels=month.abb)

Fig2C <-ggplot() + 
  geom_col(data=EffortM, aes(x=MonthName, y=Effort), colour= 'black',
           width=1, fill='skyblue')+
  labs(y="Fishing effort (hours)",
       x="Month")+
  geom_errorbar(data=EffortM, aes(x=Month, ymin=Effort-sd, ymax=Effort+sd),
                width=0.3, size=0.5)+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#descriptive stats: mean monthly fishing effort
desc3 <- as.data.frame(fish %>%
                         group_by(Month, Year) %>%
                         summarise(Effort = sum(Fishing_Effort)))
desc3 <- as.data.frame(
  desc3 %>%
    group_by(Month) %>%
    summarise(mean = mean(Effort),
              sd = sd(Effort)))
desc3
as.data.frame(
  desc3 %>%
    summarise(mean2 = mean(mean),
              sd = sd(mean)))


#Figure 2D: fishing effort (hours) / Month / CT
df2 <- as.data.frame(fish %>%
                       group_by(CT, Month, Year) %>%
                       summarise(Sum = sum(Fishing_Effort)))
EffortMCT <- as.data.frame( df2 %>%
                            group_by(CT, Month) %>%
                            summarise(Effort = mean(Sum),
                                      sd = sd(Sum)))
EffortMCT$MonthName <- factor(month.abb[EffortMCT$Month], levels=month.abb)

Fig2D <- ggplot(data=EffortMCT, aes(x=MonthName, y=Effort, 
                           ymin=Effort-sd, ymax=Effort+sd, 
                          group = CT)) + 
  geom_col(aes(fill=CT), colour= 'black', width=0.9, position="dodge")+
  labs(y="Fishing effort (hours)",
       fill= "Craft Type",
       x= "Month")+
  geom_errorbar(width=0.5, position=position_dodge(width=.9), size=0.5) +
  scale_fill_brewer(palette="Set3")+
  theme_classic()+
  theme(axis.line=element_line(size=.8))  

#descriptive stats: mean monthly fishing effort, by craft
desc4 <- as.data.frame(fish %>%
                         group_by(CT, Month, Year) %>%
                         summarise(Effort = sum(Fishing_Effort)))
desc4 <- as.data.frame(
  desc4 %>%
    group_by(CT, Month) %>%
    summarise(mean = mean(Effort),
              sd = sd(Effort)))
desc4
as.data.frame(
  desc4 %>%
    summarise(mean2 = mean(mean),
              sd = sd(mean)))
as.data.frame(
  desc4 %>%
    group_by(CT) %>%
    summarise(mean2 = mean(mean),
              sd = sd(mean)))

#plot together
Fig2BD <- ggarrange(Fig2B, Fig2D, labels=c("B", "D"),
          common.legend = TRUE, legend = "bottom")
Fig2AC <- ggarrange(Fig2A, Fig2C, labels=c("A", "C"))
ggarrange(Fig2AC, Fig2BD, nrow=2, heights = c(1, 1.2))
#use Export button to Save as Image...

#GAM
library(mgcv)
library(MuMIn)
library(LaplacesDemon)
library(boot)

#total CPUE model
#look at outliers - create boxplot of variables
boxplot(TOTAL$Total_Weight.kg)
boxplot(TOTAL$Fishing_Effort)
boxplot(TOTAL$Dolphinfish.kg)
boxplot(TOTAL$Bonito.kg)
boxplot(TOTAL$Rainbow_runner.kg)
boxplot(TOTAL$Sailfish_Marlin.kg)
boxplot(TOTAL$Wahoo.kg)
boxplot(TOTAL$Dogtooth_tuna.kg)
boxplot(TOTAL$Skipjack_tuna.kg)
boxplot(TOTAL$Yellowfin_tuna.kg)
boxplot(TOTAL$Other_tuna.kg)
boxplot(TOTAL$Emporers.kg)
boxplot(TOTAL$Snappers.kg)
boxplot(TOTAL$Trevally.kg)
boxplot(TOTAL$Groupers.kg)
boxplot(TOTAL$Others.kg)
#outliers seem like biologically plausible, due to the large range of 
#lines used - LCMs can carry up to 30 passengers so will catch many more
#fish per trip than small Makos with only 5 passengers. Check diagnostic plots
#for leverage of these outliers, especially the 300+ Fishing_Effort value.

#homogeneity of variances
var(TOTAL$Total_Weight.kg)
var(TOTAL$Fishing_Effort)
#these variances are homogeneous
var(TOTAL$Year) #modeled with a thin plate regression spline 

#histogram of fishing effort
hist(TOTAL$Fishing_Effort)
#very right skewed 
hist(log(TOTAL$Fishing_Effort))
#modeled as logged offset

#histogram of response variable
hist(TOTAL$Total_Weight.kg)
#many 0s and highly right skewed
table(TOTAL$Total_Weight.kg)
#2638 informative 0s, so need to model catch / no catch and positive values separately

#Year into a sequence from 1 to 13
TOTAL$YearS <- TOTAL$Year - 2006

#turn month into a seasonal curve, by modelling as a sinusoidal function
TOTAL$cmonth <- with(TOTAL, cos(2*pi*Month/12))
TOTAL$smonth <- with(TOTAL, sin(2*pi*Month/12))

#step 1: model catch / no catch
#create column with a 0 if Total_Weight.kg is 0, and a 1 if it is more than 0
TOTAL <- mutate(TOTAL, Presence = ifelse(Total_Weight.kg == 0, 0, 1))
#create global model for total catch presence/absence model, with binomial family
#and logged offset term
Mod1 <- gam(Presence ~ s(YearS) + cmonth + smonth + CT + Location + 
              offset(log(Fishing_Effort)), 
            family = binomial (link = "logit"), na.action="na.fail",
            data=TOTAL)

#dredge for model selection
DredgeMod1 <- dredge(Mod1, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod1

#models are ranked according to the corrected Akaike Information Criterion (AIC).
#out of those with delta <4, the simplest model is selected 
#the first model is the best model 
Model1 <- get.models(DredgeMod1, subset=1)[[1]]
summary.gam(Model1)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model1)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but potentially due to binomail
#distribution so bear in mind when checking
#whether the predicted values are reasonable


#step 2: model positive catch values
#create dataframe that only contains positive values of Total_Weight.kg (only the
#1s from the previous model)
TOTAL2 <- as.data.frame(subset(TOTAL, Total_Weight.kg >0))

#histogram of response variable
hist(TOTAL2$Total_Weight.kg)
#still very right skewed so family=gamma (link= "log") applied
hist(log(TOTAL2$Total_Weight.kg))

#logged offset term in this model too
hist(log(TOTAL2$Fishing_Effort))


Mod2 <- gam(Total_Weight.kg ~ s(YearS) + cmonth + smonth + CT + Location + 
              offset(log(Fishing_Effort)), 
            family = Gamma (link = "log"), na.action="na.fail",
            data=TOTAL2)

#dredge for model selection
DredgeMod2 <- dredge(Mod2, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod2

#the first model is the best model
Model2 <- get.models(DredgeMod2, subset=1)[[1]]
summary.gam(Model2)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model2, old.style=TRUE)
#model required no smoothing parameter selection
#all fantastic residual plots

t <-TOTAL %>%
  group_by(CT) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#central option = BW/OM
t <- TOTAL %>%
  group_by(CT, Location) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "BW/OM")
as.data.frame(arrange(t, mean))
#central option for BW/OM = OS5
t <- TOTAL %>%
  group_by(CT, Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "BW/OM")
t <- subset(t, Location == "OS5")
as.data.frame(arrange(t, mean))
#central option for BW/OM = 3 (March)

#new data frame with a sequence of Year from 0.5 to 13.5 in 1,000 steps, 
#craft type fixed to BW/OM, Location fixed to OS5, month fixed to March,
#and fishing effort fixed to 8h (one day)
newdata_total <- data.frame(YearS = seq(.5,13.5, len=1000),
                            CT = factor("BW/OM", levels = levels(TOTAL$CT)),
                            Location = factor("OS5", levels = levels(TOTAL$Location)),
                            cmonth = cos(2*pi*3/12),
                            smonth = sin(2*pi*3/12),
                            Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model1, newdata_total, se.fit=TRUE)
newdata_total$prob <- inv.logit(preds$fit)
newdata_total$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_total$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict positive catch and multiply with probability of catch
preds2 <- predict(Model2, newdata_total, se.fit=TRUE)
newdata_total$predicted <- exp(preds2$fit)*newdata_total$prob
newdata_total$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_total$lprob
newdata_total$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_total$hprob

#for results: describe peaks in CPUE
#predict CPUE estimates per year
#same new data frame as newdata_total but with year as a sequence from 1 to 13 
#in 13 steps
Year <- data.frame(YearS = seq(1,13, len=13),
                   CT = factor("BW/OM", levels = levels(TOTAL$CT)),
                   Location = factor("OS5", levels = levels(TOTAL$Location)),
                   cmonth = cos(2*pi*3/12),
                   smonth = sin(2*pi*3/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model1, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model2, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#results: one estimate for each year, with SE=1.96*se, to report peaks in CPUE
desc5 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                        SE = Year$predicted - Year$lower))
desc5
desc5 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 3A: total landings and CPUE plot
par(mfrow)
Landings <- as.data.frame(fish %>%
                          group_by(Year) %>%
                          summarise(Catch = sum(Total_Weight.kg),
                                    CPUE = mean(Total_Weight.kg/Fishing_Effort)*8))
Landings <- rbind(Landings, c(2015, 0, 50000))

Fig3A <- ggplot() + 
  geom_col(data=Landings, aes(x=Year, y=Catch), 
           fill= 'lightgreen', colour= 'black',
           width=1)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  labs(y="Total Catch (kg)")+
  geom_ribbon(data=newdata_total, 
              aes(x=YearS+2006, ymin=lower*2000, ymax=higher*2000), 
              fill="darkgreen", alpha=0.6)+
  geom_line(data=newdata_total,
            aes(x=YearS+2006, y=predicted*2000), size=.8)+
  scale_y_continuous(sec.axis = sec_axis(~./2000, name="CPUE"), 
                     limits=c(0, 45000))+
  annotate("text", x=2007, y= 9842, label = "3")+ #annotations to show months' of data for each year
  annotate("text", x=2008, y=13435, label = "4")+
  annotate("text", x=2009, y=41870, label = "10")+
  annotate("text", x=2010, y=44416, label = "12")+
  annotate("text", x=2011, y=21186, label = "5")+
  annotate("text", x=2012, y=23424, label = "12")+
  annotate("text", x=2013, y=31000, label = "12")+
  annotate("text", x=2014, y=25286, label = "12")+
  annotate("text", x=2015, y= 2000, label = "0")+
  annotate("text", x=2016, y=20100, label = "12")+
  annotate("text", x=2017, y=27290, label = "12")+
  annotate("text", x=2018, y=26000, label = "12")+
  annotate("text", x=2019, y=16936, label = "12")+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#check ribbon is reasonable considering nominal (non-standardised CPUEs)
Fig3A+
  geom_point(data=Landings, aes(y=CPUE*2000, x=Year))


#descriptive stats: inter-annual catch trends
#sum of each year, and nominal CPUE to check standardised CPUE is reasonable
desc7 <- as.data.frame(fish %>%
                            group_by(Year) %>%
                            summarise(Catch = sum(Total_Weight.kg),
                                      CPUE = mean(CPUE)))
desc7
#mean annual catch across all years
as.data.frame(
  desc7 %>%
    summarise(mean = mean(Catch),
              sd = sd(Catch)))


#GAM for pelagic catch only

#only include crafts targeting pelagic prey
Pelagic <- subset(TOTAL, CT == "BW/OM")

#histogram of fishing effort
hist(Pelagic$Fishing_Effort)
#very right skewed 
hist(log(Pelagic$Fishing_Effort))
#modeled as logged offset

#histogram of response variable
hist(Pelagic$Pelagic.kg)
#many 0s and highly right skewed
table(Pelagic$Pelagic.kg)
#5961 0s

#step 1: model catch / no catch
Pelagic <- mutate(Pelagic, Presence = ifelse(Pelagic.kg == 0, 0, 1))
Mod3 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location + 
              offset(log(Fishing_Effort)), 
            family = binomial (link = "logit"), na.action="na.fail",
            data=Pelagic)

#dredge for model selection
DredgeMod3 <- dredge(Mod3, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod3

#the first model is the best model
Model3 <- get.models(DredgeMod3, subset=1)[[1]]
summary.gam(Model3)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model3)
#model required no smoothing parameter selection
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but mostly due to binomial 
#distribution, so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Pelagic2 <- as.data.frame(subset(Pelagic, Pelagic.kg >0))

#histogram of fishing effort
hist(Pelagic2$Fishing_Effort)
# right skewed 
hist(log(Pelagic2$Fishing_Effort))
#modeled as logged offset

#histogram of response variable
hist(Pelagic2$Pelagic.kg)

Mod4 <- gam(Pelagic.kg ~ s(YearS) + cmonth + smonth  + Location + 
              offset(log(Fishing_Effort)), 
            family = Gamma (link = "log"), na.action="na.fail",
            data=Pelagic2)

#dredge for model selection
DredgeMod4 <- dredge(Mod4, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod4

#the first model is the best model
Model4 <- get.models(DredgeMod4, subset=1)[[1]]
summary.gam(Model4)
Model4

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model4)
#first plot is not entirely straight but other 
#diagnostic plots look great

#create an empty data frame for predictions based on these models
#selecting parameters to fix at
t <- Pelagic %>%
  group_by(Location) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#central option = OS5
t <- Pelagic %>%
  group_by(Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, Location == "OS5")
as.data.frame(arrange(t, mean))
#central option for BW/OM = 5 (May)
newdata_pel <- data.frame(YearS = seq(.5,13.5, len=1000),
                            Location = factor("OS5", levels = levels(Pelagic$Location)),
                            cmonth = cos(2*pi*5/12),
                            smonth = sin(2*pi*5/12),
                            Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model3, newdata_pel, se.fit=TRUE)
newdata_pel$prob  <- inv.logit(preds$fit)
newdata_pel$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_pel$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model4, newdata_pel, se.fit=TRUE)
newdata_pel$predicted <- exp(preds2$fit)*newdata_pel$prob
newdata_pel$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_pel$lprob
newdata_pel$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_pel$hprob

#predict CPUE estimates per year
Year <- data.frame(YearS = seq(1,13, len=13),
                   Location = factor("OS5", levels = levels(Pelagic$Location)),
                   cmonth = cos(2*pi*5/12),
                   smonth = sin(2*pi*5/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model3, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model4, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#one estimate for each year, with SE=1.96*se
desc6 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                             SE = Year$predicted - Year$lower))
desc6
desc6 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))

#Figure 3B: total landings and CPUE plot
Pelagic3 <- subset(fish, CT == "BW/OM")
Pelagic3 <- subset(Pelagic3, Pelagic.kg != "NA")
Landings_pel <- as.data.frame(Pelagic3 %>%
                            group_by(Year) %>%
                            summarise(Catch = sum(Pelagic.kg),
                                      CPUE = mean(Pelagic.kg/Fishing_Effort)*8))
Landings_pel <- rbind(Landings_pel, c(2015, 0, 50000))

Fig3B <- ggplot() + 
  geom_col(data=Landings_pel, aes(x=Year, y=Catch), 
           fill= 'papayawhip', colour= 'black',
           width=1)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  labs(y="Pelagic Catch (kg)")+
  geom_ribbon(data=newdata_pel, 
              aes(x=YearS+2006, ymin=lower*2000, ymax=higher*2000), 
              fill="orange", alpha=0.6)+
  geom_line(data=newdata_pel,
            aes(x=YearS+2006, y=predicted*2000), size=.8)+
  scale_y_continuous(sec.axis = sec_axis(~./2000, name="CPUE"), 
                     limits=c(0, 45000))+
  annotate("text", x=2007, y= 7226, label = "3")+
  annotate("text", x=2008, y= 9938, label = "4")+
  annotate("text", x=2009, y=31571, label = "10")+
  annotate("text", x=2010, y=30378, label = "12")+
  annotate("text", x=2011, y=16582, label = "5")+
  annotate("text", x=2012, y=11551, label = "12")+
  annotate("text", x=2013, y=13011, label = "12")+
  annotate("text", x=2014, y=10132, label = "12")+
  annotate("text", x=2015, y= 2000, label = "0")+
  annotate("text", x=2016, y= 8706, label = "12")+
  annotate("text", x=2017, y=14516, label = "12")+
  annotate("text", x=2018, y=12781, label = "12")+
  annotate("text", x=2019, y= 9076, label = "12")+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#check ribbon is reasonable considering nominal (non-standardised CPUEs)
Fig3B+
  geom_point(data=Landings_pel, aes(y=CPUE*2000, x=Year))

#descriptive stats: inter-annual catch trends
desc8 <- as.data.frame(Pelagic3 %>%
                         group_by(Year) %>%
                         summarise(Catch = sum(Pelagic.kg),
                                   CPUE = mean(Pelagic.CPUE)*8))
desc8
as.data.frame(
  desc8 %>%
    summarise(mean = mean(Catch),
              sd = sd(Catch)))

#GAM for reef catch only
#only include crafts targeting reef prey
Reef <- subset(TOTAL, CT != "BW/OM")

#histogram of fishing effort
hist(Reef$Fishing_Effort)
#very right skewed 
hist(log(Reef$Fishing_Effort))
#modeled as logged offset

#histogram of response variable
hist(Reef$Reef.kg)
#many 0s and highly right skewed
table(Reef$Reef.kg)
#2227 0s

#step 1: model catch / no catch
Reef <- mutate(Reef, Presence = ifelse(Reef.kg == 0, 0, 1))

Mod5 <- gam(Presence ~ s(YearS) + CT + cmonth + smonth + Location + 
              offset(log(Fishing_Effort)), 
            family = binomial (link = "logit"), na.action="na.fail",
            data=Reef)

#dredge for model selection
DredgeMod5 <- dredge(Mod5, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod5

#the first model is the best model
Model5 <- get.models(DredgeMod5, subset=1)[[1]]
summary.gam(Model5)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model5)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but mostly due to binomial
#distribution of data, so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Reef2 <- as.data.frame(subset(Reef, Reef.kg >0))
#histogram of fishing effort
hist(Reef2$Fishing_Effort)
#very right skewed 
hist(log(Reef2$Fishing_Effort))
#modeled as logged offset

#histogram of response variable
hist(Reef2$Reef.kg)
#family= Gamma (link = "log")
hist(log(Reef2$Reef.kg))

Mod6 <- gam(Reef.kg ~ s(YearS) + cmonth + smonth  + Location + 
              offset(log(Fishing_Effort)), 
            family = Gamma (link = "log"), na.action="na.fail",
            data=Reef2)

#dredge for model selection
DredgeMod6 <- dredge(Mod6, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod6

#the first model is the best model
Model6 <- get.models(DredgeMod6, subset=1)[[1]]
summary.gam(Model6)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model6)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking good

#selecting parameters to fix at
t <- Reef %>%
  group_by(CT) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#LCM is most common by far
t <- Reef %>%
  group_by(CT, Location) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "LCM")
as.data.frame(arrange(t, mean))
#central option = B
t <- Reef %>%
  group_by(CT, Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "LCM")
t <- subset(t, Location == "B")
as.data.frame(arrange(t, mean))
#central option for BW/OM = 11 (Nov)
newdata_reef <- data.frame(YearS = seq(.5,13.5, len=1000),
                          CT = factor("LCM", levels=levels(Reef$Location)),
                          Location = factor("B", levels = levels(Reef$Location)),
                          cmonth = cos(2*pi*11/12),
                          smonth = sin(2*pi*11/12),
                          Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model5, newdata_reef, se.fit=TRUE)
newdata_reef$prob  <- inv.logit(preds$fit)
newdata_reef$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_reef$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model6, newdata_reef, se.fit=TRUE)
newdata_reef$predicted <- exp(preds2$fit)*newdata_reef$prob
newdata_reef$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_reef$lprob
newdata_reef$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_reef$hprob

#predict CPUE estimates per year
Year <- data.frame(YearS = seq(1,13, len=13),
                   CT = factor("LCM", levels=levels(Reef$Location)),
                   Location = factor("B", levels = levels(Reef$Location)),
                   cmonth = cos(2*pi*11/12),
                   smonth = sin(2*pi*11/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model5, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model6, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#one estimate for each year, with SE=1.96*se
desc9 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                             SE = Year$predicted - Year$lower))
desc9
desc9 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 3C: total landings and CPUE plot
Reef3 <- subset(fish, CT != "BW/OM")
Landings_reef <- as.data.frame(Reef3 %>%
                                group_by(Year) %>%
                                summarise(Catch = sum(Reef.kg),
                                          CPUE = mean(Reef.kg/Fishing_Effort)*8))
Landings_reef <- rbind(Landings_reef, c(2015, 0, 50000))

Fig3C <- ggplot() + 
  geom_col(data=Landings_reef, aes(x=Year, y=Catch), 
           fill= 'skyblue', colour= 'black',
           width=1)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  labs(y="Reef Catch (kg)")+
  geom_ribbon(data=newdata_reef, 
              aes(x=YearS+2006, ymin=lower*2000, ymax=higher*2000), 
              fill="skyblue4", alpha=0.6)+
  geom_line(data=newdata_reef,
            aes(x=YearS+2006, y=predicted*2000), size=.8)+
  scale_y_continuous(sec.axis = sec_axis(~./2000, name="CPUE"), 
                     limits=c(0, 45000))+
  annotate("text", x=2007, y= 3429, label = "3")+
  annotate("text", x=2008, y= 4237, label = "4")+
  annotate("text", x=2009, y= 6144, label = "10")+
  annotate("text", x=2010, y= 7520, label = "12")+
  annotate("text", x=2011, y= 4350, label = "5")+
  annotate("text", x=2012, y=10657, label = "12")+
  annotate("text", x=2013, y=16636, label = "12")+
  annotate("text", x=2014, y=13959, label = "12")+
  annotate("text", x=2015, y= 2000, label = "0")+
  annotate("text", x=2016, y=10603, label = "12")+
  annotate("text", x=2017, y=12038, label = "12")+
  annotate("text", x=2018, y=12388, label = "12")+
  annotate("text", x=2019, y=7007, label = "12")+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#check ribbon is reasonable considering nominal (non-standardised CPUEs)
Fig3C+
  geom_point(data=Landings_reef, aes(y=CPUE*2000, x=Year))

#descriptive stats: inter-annual catch trends
desc10 <- as.data.frame(Reef3 %>%
                         group_by(Year) %>%
                         summarise(Catch = sum(Reef.kg),
                                   CPUE = mean(Reef.kg/Fishing_Effort)*8))
desc10
as.data.frame(
  desc10 %>%
    summarise(mean = mean(Catch),
              sd = sd(Catch)))


#plot together
ggarrange(Fig3A, Fig3B, Fig3C, nrow=1, 
          labels = c("A", "B", "C"))
#3 x 13 inches picture


#Species-specific models:

#GAM for yellowfin tuna only
#only include crafts targeting pelagic prey
Yellowfin <- subset(TOTAL, CT == "BW/OM")
#histogram of fishing effort
hist(Yellowfin$Fishing_Effort)
# right skewed 
hist(log(Yellowfin$Fishing_Effort))
#modeled as logged offset

#histogram of response variable
hist(Yellowfin$Yellowfin_tuna.kg)
hist(log(Yellowfin$Yellowfin_tuna.kg))

#step 1: model catch / no catch
Yellowfin <- mutate(Yellowfin, Presence = ifelse(Yellowfin_tuna.kg == 0, 0, 1))
Mod7 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location + 
              offset(log(Fishing_Effort)), 
            family = binomial (link = "logit"), na.action="na.fail",
            data=Yellowfin)

#dredge for model selection
DredgeMod7 <- dredge(Mod7, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod7

#the first model is the best model
Model7 <- get.models(DredgeMod7, subset=1)[[1]]
summary.gam(Model7)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model7)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but mostly due to binomial
#distribution of data so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Yellowfin2 <- as.data.frame(subset(Yellowfin, Yellowfin_tuna.kg >0))
#histogram of fishing effort
hist(Yellowfin2$Fishing_Effort)
# right skewed 
hist(log(Yellowfin2$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Yellowfin2$Yellowfin_tuna.kg)
hist(log(Yellowfin2$Yellowfin_tuna.kg))

Mod8 <- gam(Yellowfin_tuna.kg ~ s(YearS) + cmonth + smonth  + Location + 
              offset(log(Fishing_Effort)), 
            family = Gamma (link = "log"), na.action="na.fail",
            data=Yellowfin2)

#dredge for model selection
DredgeMod8 <- dredge(Mod8, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod8

#the first model is the best model
Model8 <- get.models(DredgeMod8, subset=1)[[1]]
summary.gam(Model8)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model8)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking great 

#selecting parameters values to fix at
t <- Yellowfin %>%
  group_by(Location) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#central option = OS5
t <- Yellowfin %>%
  group_by(Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, Location == "OS5")
as.data.frame(arrange(t, mean))
#central option for OS5 = 3 (Mar)
newdata_Y <- data.frame(YearS = seq(1,13, len=1000),
                          Location = factor("OS5", levels = levels(Yellowfin$Location)),
                          cmonth = cos(2*pi*3/12),
                          smonth = sin(2*pi*3/12),
                          Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model7, newdata_Y, se.fit=TRUE)
newdata_Y$prob  <- inv.logit(preds$fit)
newdata_Y$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_Y$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model8, newdata_Y, se.fit=TRUE)
newdata_Y$predicted <- exp(preds2$fit)*newdata_Y$prob
newdata_Y$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_Y$lprob
newdata_Y$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_Y$hprob

#predict CPUE estimates per year
Year <- data.frame(YearS = seq(1,13, len=13),
                   Location = factor("OS5", levels = levels(Yellowfin$Location)),
                   cmonth = cos(2*pi*3/12),
                   smonth = sin(2*pi*3/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model7, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model8, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#one estimate for each year, with SE=1.96*se
desc11 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                             SE = Year$predicted - Year$lower))
desc11
desc11 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 4A: CPUE plot

Fig4A <- ggplot() + 
  geom_ribbon(data=newdata_Y, 
              aes(x=YearS+2006, ymin=lower, ymax=higher), 
              fill="hotpink")+
  geom_line(data=newdata_Y,
            aes(x=YearS+2006, y=predicted), size=.8)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 12, 2),
                     limits=c(0, 13))+ 
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#seasonal trends in CPUE of yellowfin tuna:
#GAM for yellowfin tuna only
#only include crafts targeting pelagic prey

#step 1: model catch / no catch
Mod9 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location +
              offset(log(Fishing_Effort)), 
            family = binomial (link = "logit"), na.action="na.fail",
            data=Yellowfin)

#dredge for model selection
DredgeMod9 <- dredge(Mod9, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod9

#the first model is the best model
Model9 <- get.models(DredgeMod9, subset=1)[[1]]
summary.gam(Model9)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model9)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but mostly due to binomial
#data (e.g. two normal distributions of residuals) 
#so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Mod10 <- gam(Yellowfin_tuna.kg ~ s(YearS) + cmonth + smonth  + Location +
              offset(log(Fishing_Effort)), 
            family = Gamma (link = "log"), na.action="na.fail",
            data=Yellowfin2)

#dredge for model selection
DredgeMod10 <- dredge(Mod10, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod10

#the first model is the best model
Model10 <- get.models(DredgeMod10, subset=1)[[1]]
summary.gam(Model10)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model10)
#deviance vs quantiles plot close to 1:1 straight line
#other plots look good

#selecting parameters values to fix at
t <- Yellowfin %>%
  group_by(Location) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#central option = OS5
t <- Yellowfin %>%
  group_by(Location, Year) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, Location == "OS5")
as.data.frame(arrange(t, mean))
#central option for OS5 = 2019 (year 13)
newdata_YM <- data.frame(YearS = 13,
                        Location = factor("OS5", levels = levels(Yellowfin$Location)),
                        cmonth = cos(2*pi*seq(1,12, len=1000)/12),
                        smonth = sin(2*pi*seq(1,12, len=1000)/12),
                        Fishing_Effort = 8,
                        Month = seq(1,12, len=1000))

#predict probability of catch
preds <- predict(Model9, newdata_YM, se.fit=TRUE)
newdata_YM$prob  <- inv.logit(preds$fit)
newdata_YM$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_YM$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model10, newdata_YM, se.fit=TRUE)
newdata_YM$predicted <- exp(preds2$fit)*newdata_YM$prob
newdata_YM$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_YM$lprob
newdata_YM$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_YM$hprob

#predict CPUE estimates per month
Month <- data.frame(YearS = 13,
                   Location = factor("OS5", levels = levels(Yellowfin$Location)),
                   cmonth = cos(2*pi*seq(1,12, len=12)/12),
                   smonth = sin(2*pi*seq(1,12, len=12)/12),
                   Fishing_Effort = 8,
                   Month = seq(1,12, len=12))

#predict probability of catch
preds <- predict(Model9, Month, se.fit=TRUE)
Month$prob <- inv.logit(preds$fit)
Month$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Month$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model10, Month, se.fit=TRUE)
Month$predicted <- exp(preds2$fit)*Month$prob
Month$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$lprob
Month$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$hprob

#one estimate for each year, with SE=1.96*se
desc12 <- as.data.frame(cbind(Month = Month$Month, Pred = Month$predicted, 
                              SE = Month$predicted - Month$lower))
desc12
desc12 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 4B: CPUE plot

Fig4B <- ggplot() + 
  geom_ribbon(data=newdata_YM, 
              aes(x=Month, ymin=lower, ymax=higher), 
              fill="hotpink")+
  geom_line(data=newdata_YM,
            aes(x=Month, y=predicted), size=.8)+
  scale_x_continuous(name="Month", breaks=seq(1, 12, 1))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 5, 1),
                     limits=c(0,5))+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#plot Fig4A together (yellowfin tuna trends)
Fig4AB <- plot_grid(Fig4A, Fig4B, align = "v", nrow=2, labels=c("A", "B"))

#GAM for wahoo only
#only include crafts targeting pelagic prey
Wahoo <- subset(TOTAL, CT == "BW/OM")
#histogram of fishing effort
hist(Wahoo$Fishing_Effort)
# right skewed 
hist(log(Wahoo$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Wahoo$Wahoo.kg)
hist(log(Wahoo$Wahoo.kg))

#step 1: model catch / no catch
Wahoo <- mutate(Wahoo, Presence = ifelse(Wahoo.kg == 0, 0, 1))
Mod11 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location +
              offset(log(Fishing_Effort)), 
            family = binomial (link = "logit"), na.action="na.fail",
            data=Wahoo)

#dredge for model selection
DredgeMod11 <- dredge(Mod11, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod11

#the first model is the best model
Model11 <- get.models(DredgeMod11, subset=1)[[1]]
summary.gam(Model11)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model11)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but mostly due to binomial
#distribution so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Wahoo2 <- as.data.frame(subset(Wahoo, Wahoo.kg >0))

#histogram of fishing effort
hist(Wahoo2$Fishing_Effort)
# right skewed 
hist(log(Wahoo2$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Wahoo2$Wahoo.kg)
hist(log(Wahoo2$Wahoo.kg))

Mod12 <- gam(Wahoo.kg ~ s(YearS) + cmonth + smonth  + Location +
              offset(log(Fishing_Effort)), 
            family = Gamma (link = "log"), na.action="na.fail",
            data=Wahoo2)

#dredge for model selection
DredgeMod12 <- dredge(Mod12, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod12

#the first model is the best model
Model12 <- get.models(DredgeMod12, subset=1)[[1]]
summary.gam(Model12)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model12)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking great 

#selecting parameters values to fix at
t <- Wahoo %>%
  group_by(Location) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#central option = OS5
t <- Wahoo %>%
  group_by(Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, Location == "OS5")
as.data.frame(arrange(t, mean))
#central option for OS5 = 3 (Mar)
newdata_W <- data.frame(YearS = seq(1,13, len=1000),
                        Location = factor("OS5", levels = levels(Wahoo$Location)),
                        cmonth = cos(2*pi*3/12),
                        smonth = sin(2*pi*3/12),
                        Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model11, newdata_W, se.fit=TRUE)
newdata_W$prob  <- inv.logit(preds$fit)
newdata_W$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_W$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model12, newdata_W, se.fit=TRUE)
newdata_W$predicted <- exp(preds2$fit)*newdata_W$prob
newdata_W$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_W$lprob
newdata_W$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_W$hprob

#predict CPUE estimates per year
Year <- data.frame(YearS = seq(1,13, len=13),
                   Location = factor("OS5", levels = levels(Wahoo$Location)),
                   cmonth = cos(2*pi*3/12),
                   smonth = sin(2*pi*3/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model11, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model12, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#one estimate for each year, with SE=1.96*se
desc13 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                              SE = Year$predicted - Year$lower))
desc13
desc13 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 4C: CPUE plot

Fig4C<- ggplot() + 
  geom_ribbon(data=newdata_W, 
              aes(x=YearS+2006, ymin=lower, ymax=higher), 
              fill="violetred")+
  geom_line(data=newdata_W,
            aes(x=YearS+2006, y=predicted), size=.8)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 13, 2),
                     limits=c(0, 13))+ 
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#seasonal trends in CPUE of wahoo:
#GAM for wahoo only
#only include crafts targeting pelagic prey

#step 1: model catch / no catch
Mod13 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location +
              offset(log(Fishing_Effort)), 
            family = binomial (link = "logit"), na.action="na.fail",
            data=Wahoo)

#dredge for model selection
DredgeMod13 <- dredge(Mod13, subset = (cmonth|!smonth) && (smonth|!cmonth),
                     fixed = "offset(log(Fishing_Effort))")
DredgeMod13

#the first model is the best model
Model13 <- get.models(DredgeMod13, subset=1)[[1]]
summary.gam(Model13)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model13)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but mostly due to binomial 
#distribution so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Mod14 <- gam(Wahoo.kg ~ s(YearS) + cmonth + smonth  + Location +
               offset(log(Fishing_Effort)), 
             family = Gamma (link = "log"), na.action="na.fail",
             data=Wahoo2)

#dredge for model selection
DredgeMod14 <- dredge(Mod14, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod14

#the first model is the best model
Model14 <- get.models(DredgeMod14, subset=1)[[1]]
summary.gam(Model14)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model14)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking good

#create an empty data frame for predictions based on these models
#selecting parameters values to fix at
t <- Wahoo %>%
  group_by(Location) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#central option = OS5
t <- Wahoo %>%
  group_by(Location, Year) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, Location == "OS5")
as.data.frame(arrange(t, mean))
#central option for OS5 = 2019 (year 13)
newdata_WM <- data.frame(YearS = 13,
                         Location = factor("OS5", levels = levels(Wahoo$Location)),
                         cmonth = cos(2*pi*seq(1,12, len=1000)/12),
                         smonth = sin(2*pi*seq(1,12, len=1000)/12),
                         Fishing_Effort = 8,
                         Month = seq(1,12, len=1000))

#predict probability of catch
preds <- predict(Model13, newdata_WM, se.fit=TRUE)
newdata_WM$prob  <- inv.logit(preds$fit)
newdata_WM$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_WM$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model14, newdata_WM, se.fit=TRUE)
newdata_WM$predicted <- exp(preds2$fit)*newdata_WM$prob
newdata_WM$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_WM$lprob
newdata_WM$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_WM$hprob

#predict CPUE estimates per month
Month <- data.frame(YearS = 13,
                    Location = factor("OS5", levels = levels(Wahoo$Location)),
                    cmonth = cos(2*pi*seq(1,12, len=12)/12),
                    smonth = sin(2*pi*seq(1,12, len=12)/12),
                    Fishing_Effort = 8,
                    Month = seq(1,12, len=12))

#predict probability of catch
preds <- predict(Model13, Month, se.fit=TRUE)
Month$prob <- inv.logit(preds$fit)
Month$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Month$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model14, Month, se.fit=TRUE)
Month$predicted <- exp(preds2$fit)*Month$prob
Month$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$lprob
Month$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$hprob

#one estimate for each year, with SE=1.96*se
desc14 <- as.data.frame(cbind(Month = Month$Month, Pred = Month$predicted, 
                              SE = Month$predicted - Month$lower))
desc14
desc14 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 4D: CPUE plot

Fig4D <- ggplot() + 
  geom_ribbon(data=newdata_WM, 
              aes(x=Month, ymin=lower, ymax=higher), 
              fill="violetred")+
  geom_line(data=newdata_WM,
            aes(x=Month, y=predicted), size=.8)+
  scale_x_continuous(name="Month", breaks=seq(1, 12, 1))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 5, 1),
                     limits=c(0,5))+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#plot Fig4B together (wahoo trends)
Fig4CD <- plot_grid(Fig4C, Fig4D, align = "v", nrow=2, labels=c("C", "D"))

#plot yellowfin and wahoo together
FigABCD <- plot_grid(Fig4AB, Fig4CD, align="v", nrow=1 )
FigABCD

#GAM for groupers only
#only include crafts targeting reef prey
Grouper <- subset(TOTAL, CT != "BW/OM")
#histogram of fishing effort
hist(Grouper$Fishing_Effort)
# right skewed 
hist(log(Grouper$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Grouper$Groupers.kg)
hist(log(Grouper$Groupers.kg))

#step 1: model catch / no catch
Grouper <- mutate(Grouper, Presence = ifelse(Groupers.kg == 0, 0, 1))



Mod15 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location + CT +
               offset(log(Fishing_Effort)), 
             family = binomial (link = "logit"), na.action="na.fail",
             data=Grouper)

#dredge for model selection
DredgeMod15 <- dredge(Mod15, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod15

#the first model is the best model
Model15 <- get.models(DredgeMod15, subset=2)[[1]]
summary.gam(Model15)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model15)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but due to binomial
#distribution so bear in mind when checking

#whether the predicted values are reasonable
#step 2: model positive catch values
Grouper2 <- as.data.frame(subset(Grouper, Groupers.kg >0))
#histogram of fishing effort
hist(Grouper2$Fishing_Effort)
# right skewed 
hist(log(Grouper2$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Grouper2$Groupers.kg)
hist(log(Grouper2$Groupers.kg))


Mod16 <- gam(Groupers.kg ~ s(YearS) + cmonth + smonth  + Location + CT +
               offset(log(Fishing_Effort)), 
             family = Gamma (link = "log"), na.action="na.fail",
             data=Grouper2)

#dredge for model selection
DredgeMod16 <- dredge(Mod16, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod16

#the first model is the best model
Model16 <- get.models(DredgeMod16, subset=1)[[1]]
summary.gam(Model16)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model16)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking great 

#create an empty data frame for predictions based on these models
#selecting parameters values to fix at
t <- Grouper %>%
  group_by(CT) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#LCM best option
t <- Grouper %>%
  group_by(CT, Location) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
as.data.frame(arrange(t, mean))
#central option = B
t <- Grouper %>%
  group_by(CT, Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
t <- subset(t, Location == "B")
as.data.frame(arrange(t, mean))
#central option for OS1 = 12 (Dec)
newdata_G <- data.frame(YearS = seq(1,13, len=1000),
                        CT = factor("MAKO", levels = levels(Grouper$CT)),
                        Location = factor("B", levels = levels(Grouper$Location)),
                        cmonth = cos(2*pi*12/12),
                        smonth = sin(2*pi*12/12),
                        Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model15, newdata_G, se.fit=TRUE)
newdata_G$prob  <- inv.logit(preds$fit)
newdata_G$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_G$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model16, newdata_G, se.fit=TRUE)
newdata_G$predicted <- exp(preds2$fit)*newdata_G$prob
newdata_G$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_G$lprob
newdata_G$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_G$hprob

#predict CPUE estimates per year
Year <- data.frame(YearS = seq(1,13, len=13),
                   CT = factor("MAKO", levels = levels(Grouper$CT)),
                   Location = factor("B", levels = levels(Grouper$Location)),
                   cmonth = cos(2*pi*12/12),
                   smonth = sin(2*pi*12/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model15, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model16, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#one estimate for each year, with SE=1.96*se
desc15 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                              SE = Year$predicted - Year$lower))
desc15
desc15 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))

#Figure 4E: CPUE plot

Fig4E <- ggplot() + 
  geom_ribbon(data=newdata_G, 
              aes(x=YearS+2006, ymin=lower, ymax=higher), 
              fill="lightgreen")+
  geom_line(data=newdata_G,
            aes(x=YearS+2006, y=predicted), size=.8)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 7, 2),
                     limits=c(0, 8))+ 
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#seasonal trends in CPUE of groupers:
#GAM for groupers only
#only include crafts targeting reef prey

#step 1: model catch / no catch
Mod17 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location +
               offset(log(Fishing_Effort)), 
             family = binomial (link = "logit"), na.action="na.fail",
             data=Grouper)

#dredge for model selection
DredgeMod17 <- dredge(Mod17, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod17

#the second model is the best model
Model17 <- get.models(DredgeMod17, subset=2)[[1]]
summary.gam(Model17)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model17)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but due to binomial 
#distribution, so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Mod18 <- gam(Groupers.kg ~ s(YearS) + cmonth + smonth  + Location +
               offset(log(Fishing_Effort)), 
             family = Gamma (link = "log"), na.action="na.fail",
             data=Grouper2)

#dredge for model selection
DredgeMod18 <- dredge(Mod18, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod18

#the first model is the best model
Model18 <- get.models(DredgeMod18, subset=1)[[1]]
summary.gam(Model18)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model18)
#deviance vs quantiles plot close to 1:1 straight line
#other plots look good

#create an empty data frame for predictions based on these models
#selecting parameters values to fix at
#LCM
t <- Grouper %>%
  group_by(CT, Location) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "LCM")
as.data.frame(arrange(t, mean))
#central option = B
t <- Grouper %>%
  group_by(CT, Location, Year) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "LCM")
t <- subset(t, Location == "B")
as.data.frame(arrange(t, mean))
#central option for OS5 = 2010 (year 4)

newdata_GM <- data.frame(YearS = 4,
                         CT = factor("LCM", levels = levels(Grouper$CT)),
                         Location = factor("B", levels = levels(Grouper$Location)),
                         cmonth = cos(2*pi*seq(1,12, len=1000)/12),
                         smonth = sin(2*pi*seq(1,12, len=1000)/12),
                         Fishing_Effort = 8,
                         Month = seq(1,12, len=1000))

#predict probability of catch
preds <- predict(Model17, newdata_GM, se.fit=TRUE)
newdata_GM$prob  <- inv.logit(preds$fit)
newdata_GM$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_GM$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model18, newdata_GM, se.fit=TRUE)
newdata_GM$predicted <- exp(preds2$fit)*newdata_GM$prob
newdata_GM$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_GM$lprob
newdata_GM$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_GM$hprob

#predict CPUE estimates per month
Month <- data.frame(YearS = 4,
                    CT = factor("LCM", levels = levels(Grouper$CT)),
                    Location = factor("B", levels = levels(Grouper$Location)),
                    cmonth = cos(2*pi*seq(1,12, len=12)/12),
                    smonth = sin(2*pi*seq(1,12, len=12)/12),
                    Fishing_Effort = 8,
                    Month = seq(1,12, len=12))

#predict probability of catch
preds <- predict(Model17, Month, se.fit=TRUE)
Month$prob <- inv.logit(preds$fit)
Month$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Month$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model18, Month, se.fit=TRUE)
Month$predicted <- exp(preds2$fit)*Month$prob
Month$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$lprob
Month$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$hprob

#one estimate for each month, with SE=1.96*se
desc16 <- as.data.frame(cbind(Month = Month$Month, Pred = Month$predicted, 
                              SE = Month$predicted - Month$lower))
desc16
desc16 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 4F: CPUE plot

Fig4F <- ggplot() + 
  geom_ribbon(data=newdata_GM, 
              aes(x=Month, ymin=lower, ymax=higher), 
              fill="lightgreen")+
  geom_line(data=newdata_GM,
            aes(x=Month, y=predicted), size=.8)+
  scale_x_continuous(name="Month", breaks=seq(1, 12, 1))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 5, 1),
                     limits=c(0,5))+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#plot Fig4EF together (grouper trends)
Fig4EF <- plot_grid(Fig4E, Fig4F, align = "v", nrow=2, labels=c("E", "F"))


#GAM for emperors only
#only include crafts targeting reef prey
Emperor <- subset(TOTAL, CT != "BW/OM")
#histogram of fishing effort
hist(Emperor$Fishing_Effort)
# right skewed 
hist(log(Emperor$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Emperor$Emporers.kg)
hist(log(Emperor$Emporers.kg))

#step 1: model catch / no catch
Emperor <- mutate(Emperor, Presence = ifelse(Emporers.kg == 0, 0, 1))
Mod19 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location + CT +
               offset(log(Fishing_Effort)), 
             family = binomial (link = "logit"), na.action="na.fail",
             data=Emperor)

#dredge for model selection
DredgeMod19 <- dredge(Mod19, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod19

#the second model is the best model
Model19 <- get.models(DredgeMod19, subset=2)[[1]]
summary.gam(Model19)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model19)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great but due to binomial
#distribution so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Emperor2 <- as.data.frame(subset(Emperor, Emporers.kg >0))

#histogram of fishing effort
hist(Emperor2$Fishing_Effort)
# right skewed 
hist(log(Emperor2$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Emperor2$Emporers.kg)
hist(log(Emperor2$Emporers.kg))

Mod20 <- gam(Emporers.kg ~ s(YearS) + cmonth + smonth  + Location + CT +
               offset(log(Fishing_Effort)), 
             family = Gamma (link = "log"), na.action="na.fail",
             data=Emperor2)

#dredge for model selection
DredgeMod20 <- dredge(Mod20, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod20

#the second model is the best model
Model20 <- get.models(DredgeMod20, subset=2)[[1]]
summary.gam(Model20)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model20)
#deviance vs quantiles plot close to 1:1 straight line
#other plots look good 

#create an empty data frame for predictions based on these models
#selecting parameters values to fix at
t <- Emperor %>%
  group_by(CT) %>%
  summarise(mean = mean(Fishing_Effort))
as.data.frame(arrange(t, mean))
#LCM best option
t <- Emperor %>%
  group_by(CT, Location) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
as.data.frame(arrange(t, mean))
#central option = B
t <- Emperor %>%
  group_by(CT, Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
t <- subset(t, Location == "B")
as.data.frame(arrange(t, mean))
#central option for OS1 = 12 (Nov)
newdata_E <- data.frame(YearS = seq(1,13, len=1000),
                        CT = factor("MAKO", levels = levels(Emperor$CT)),
                        Location = factor("B", levels = levels(Emperor$Location)),
                        cmonth = cos(2*pi*12/12),
                        smonth = sin(2*pi*12/12),
                        Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model19, newdata_E, se.fit=TRUE)
newdata_E$prob  <- inv.logit(preds$fit)
newdata_E$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_E$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model20, newdata_E, se.fit=TRUE)
newdata_E$predicted <- exp(preds2$fit)*newdata_E$prob
newdata_E$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_E$lprob
newdata_E$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_E$hprob

#predict CPUE estimates per year
Year <- data.frame(YearS = seq(1,13, len=13),
                   CT = factor("MAKO", levels = levels(Emperor$CT)),
                   Location = factor("B", levels = levels(Emperor$Location)),
                   cmonth = cos(2*pi*12/12),
                   smonth = sin(2*pi*12/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model19, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model20, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#one estimate for each year, with SE=1.96*se
desc17 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                              SE = Year$predicted - Year$lower))
desc17
desc17 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))
#Figure 4G: CPUE plot

Fig4G <- ggplot() + 
  geom_ribbon(data=newdata_E, 
              aes(x=YearS+2006, ymin=lower, ymax=higher), 
              fill="orange")+
  geom_line(data=newdata_E,
            aes(x=YearS+2006, y=predicted), size=.8)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 7, 2),
                     limits=c(0, 8))+ 
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#seasonal trends in CPUE of emperors:
#GAM for emperors only
#only include crafts targeting reef prey

#step 1: model catch / no catch
Mod21 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location +
               offset(log(Fishing_Effort)), 
             family = binomial (link = "logit"), na.action="na.fail",
             data=Emperor)

#dredge for model selection
DredgeMod21 <- dredge(Mod21, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod21

#the first model is the best model (simplest model including seasonal variation with delta<4)
Model21 <- get.models(DredgeMod21, subset=1)[[1]]
summary.gam(Model21)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model21)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great due to binomial
#distribution so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Mod22 <- gam(Emporers.kg ~ s(YearS) + cmonth + smonth  + Location +
               offset(log(Fishing_Effort)), 
             family = Gamma (link = "log"), na.action="na.fail",
             data=Emperor2)

#dredge for model selection
DredgeMod22 <- dredge(Mod22, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod22

#the first model is the best model
Model22 <- get.models(DredgeMod22, subset=1)[[1]]
summary.gam(Model22)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model22)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking good 

#create an empty data frame for predictions based on these models
#selecting parameters values to fix at
#MAKO
#B
t <- Emperor %>%
  group_by(CT, Location, Year) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
t <- subset(t, Location == "B")
as.data.frame(arrange(t, mean))
#central option for OS5 = 2011 (year 5)

newdata_EM <- data.frame(YearS = 5,
                         CT = factor("MAKO", levels = levels(Grouper$CT)),
                         Location = factor("B", levels = levels(Grouper$Location)),
                         cmonth = cos(2*pi*seq(1,12, len=1000)/12),
                         smonth = sin(2*pi*seq(1,12, len=1000)/12),
                         Fishing_Effort = 8,
                         Month = seq(1,12, len=1000))

#predict probability of catch
preds <- predict(Model21, newdata_EM, se.fit=TRUE)
newdata_EM$prob  <- inv.logit(preds$fit)
newdata_EM$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_EM$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model22, newdata_EM, se.fit=TRUE)
newdata_EM$predicted <- exp(preds2$fit)*newdata_EM$prob
newdata_EM$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_EM$lprob
newdata_EM$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_EM$hprob

#predict CPUE estimates per month
Month <- data.frame(YearS = 5,
                    CT = factor("MAKO", levels = levels(Grouper$CT)),
                    Location = factor("B", levels = levels(Grouper$Location)),
                    cmonth = cos(2*pi*seq(1,12, len=12)/12),
                    smonth = sin(2*pi*seq(1,12, len=12)/12),
                    Fishing_Effort = 8,
                    Month = seq(1,12, len=12))

#predict probability of catch
preds <- predict(Model21, Month, se.fit=TRUE)
Month$prob <- inv.logit(preds$fit)
Month$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Month$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model22, Month, se.fit=TRUE)
Month$predicted <- exp(preds2$fit)*Month$prob
Month$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$lprob
Month$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$hprob

#one estimate for each month, with SE=1.96*se
desc18 <- as.data.frame(cbind(Month = Month$Month, Pred = Month$predicted, 
                              SE = Month$predicted - Month$lower))
desc18
desc18 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))


#Figure 4H: CPUE plot

Fig4H <- ggplot() + 
  geom_ribbon(data=newdata_EM, 
              aes(x=Month, ymin=lower, ymax=higher), 
              fill="orange")+
  geom_line(data=newdata_EM,
            aes(x=Month, y=predicted), size=.8)+
  scale_x_continuous(name="Month", breaks=seq(1, 12, 1))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 5, 1),
                     limits=c(0,5))+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#plot Fig4GH together (emperor trends)
Fig4GH <- plot_grid(Fig4G, Fig4H, align = "v", nrow=2, labels=c("G", "H"))


#GAM for snappers only
#only include crafts targeting reef prey
Snapper <- subset(TOTAL, CT != "BW/OM")
#histogram of fishing effort
hist(Snapper$Fishing_Effort)
# right skewed 
hist(log(Snapper$Fishing_Effort), breaks=10)
#modeled as logged offset

#histogram of response variable
hist(Snapper$Snappers.kg)
hist(log(Snapper$Snappers.kg))

#step 1: model catch / no catch
Snapper <- mutate(Snapper, Presence = ifelse(Snappers.kg == 0, 0, 1))
Mod23 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location + CT +
               offset(log(Fishing_Effort)), 
             family = binomial (link = "logit"), na.action="na.fail",
             data=Snapper)

#dredge for model selection
DredgeMod23 <- dredge(Mod23, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod23

#the second model is the best model
Model23 <- get.models(DredgeMod23, subset=2)[[1]]
summary.gam(Model23)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model23)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great mostly due to binomial
#distribution so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Snapper2 <- as.data.frame(subset(Snapper, Snappers.kg >0))
Mod24 <- gam(Snappers.kg ~ s(YearS) + cmonth + smonth  + Location + CT +
               offset(log(Fishing_Effort)), 
             family = Gamma (link = "log"), na.action="na.fail",
             data=Snapper2)

#dredge for model selection
DredgeMod24 <- dredge(Mod24, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod24

#the second model is the best model
Model24 <- get.models(DredgeMod24, subset=2)[[1]]
summary.gam(Model24)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model24)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking good

#create an empty data frame for predictions based on these models
#selecting parameters values to fix at
#MAKO best option
t <- Snapper %>%
  group_by(CT, Location) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
as.data.frame(arrange(t, mean))
#central option = B
t <- Snapper %>%
  group_by(CT, Location, Month) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
t <- subset(t, Location == "B")
as.data.frame(arrange(t, mean))
#central option for OS1 = 12 (Nov)
newdata_S <- data.frame(YearS = seq(1,13, len=1000),
                        CT = factor("MAKO", levels = levels(Snapper$CT)),
                        Location = factor("B", levels = levels(Snapper$Location)),
                        cmonth = cos(2*pi*12/12),
                        smonth = sin(2*pi*12/12),
                        Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model23, newdata_S, se.fit=TRUE)
newdata_S$prob  <- inv.logit(preds$fit)
newdata_S$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_S$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model24, newdata_S, se.fit=TRUE)
newdata_S$predicted <- exp(preds2$fit)*newdata_S$prob
newdata_S$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_S$lprob
newdata_S$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_S$hprob

#predict CPUE estimates per year
Year <- data.frame(YearS = seq(1,13, len=13),
                   CT = factor("MAKO", levels = levels(Snapper$CT)),
                   Location = factor("B", levels = levels(Snapper$Location)),
                   cmonth = cos(2*pi*12/12),
                   smonth = sin(2*pi*12/12),
                   Fishing_Effort = 8)

#predict probability of catch
preds <- predict(Model23, Year, se.fit=TRUE)
Year$prob <- inv.logit(preds$fit)
Year$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Year$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model24, Year, se.fit=TRUE)
Year$predicted <- exp(preds2$fit)*Year$prob
Year$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$lprob
Year$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Year$hprob

#one estimate for each year, with SE=1.96*se
desc19 <- as.data.frame(cbind(Year$YearS+2006, Pred = Year$predicted, 
                              SE = Year$predicted - Year$lower))
desc19
desc19 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))

#Figure 4I: CPUE plot

Fig4I <- ggplot() + 
  geom_ribbon(data=newdata_S, 
              aes(x=YearS+2006, ymin=lower, ymax=higher), 
              fill="red")+
  geom_line(data=newdata_S,
            aes(x=YearS+2006, y=predicted), size=.8)+
  scale_x_continuous(name="Year", breaks=seq(2007, 2019,3))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 7, 2),
                     limits=c(0, 8))+ 
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#seasonal trends in CPUE of snappers:
#GAM for snappers only
#only include crafts targeting reef prey

#step 1: model catch / no catch
Mod25 <- gam(Presence ~ s(YearS) + cmonth + smonth + Location +
               offset(log(Fishing_Effort)), 
             family = binomial (link = "logit"), na.action="na.fail",
             data=Snapper)

#dredge for model selection
DredgeMod25 <- dredge(Mod25, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod25

#the first model is the best model
Model25 <- get.models(DredgeMod25, subset=1)[[1]]
summary.gam(Model25)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model25)
#deviance vs quantiles plot close to 1:1 straight line
#other plots not looking great because of bonimial 
#distribution so bear in mind when checking
#whether the predicted values are reasonable

#step 2: model positive catch values
Mod26 <- gam(Snappers.kg ~ s(YearS) + cmonth + smonth  + Location +
               offset(log(Fishing_Effort)), 
             family = Gamma (link = "log"), na.action="na.fail",
             data=Snapper2)

#dredge for model selection
DredgeMod26 <- dredge(Mod26, subset = (cmonth|!smonth) && (smonth|!cmonth),
                      fixed = "offset(log(Fishing_Effort))")
DredgeMod26

#the first model is the best model
Model26 <- get.models(DredgeMod26, subset=1)[[1]]
summary.gam(Model26)

#diagnostic plots
par(mfrow=c(2,2))
gam.check(Model26)
#deviance vs quantiles plot close to 1:1 straight line
#other plots looking good

#create an empty data frame for predictions based on these models
#selecting parameters values to fix at
#MAKO
#B
t <- Snapper %>%
  group_by(CT, Location, Year) %>%
  summarise(mean = mean(Fishing_Effort))
t <- subset(t, CT == "MAKO")
t <- subset(t, Location == "B")
as.data.frame(arrange(t, mean))
#central option for OS5 = 2011 (year 5)

newdata_SM <- data.frame(YearS = 5,
                         CT = factor("MAKO", levels = levels(Snapper$CT)),
                         Location = factor("B", levels = levels(Snapper$Location)),
                         cmonth = cos(2*pi*seq(1,12, len=1000)/12),
                         smonth = sin(2*pi*seq(1,12, len=1000)/12),
                         Fishing_Effort = 8,
                         Month = seq(1,12, len=1000))

#predict probability of catch
preds <- predict(Model25, newdata_SM, se.fit=TRUE)
newdata_SM$prob  <- inv.logit(preds$fit)
newdata_SM$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
newdata_SM$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model26, newdata_SM, se.fit=TRUE)
newdata_SM$predicted <- exp(preds2$fit)*newdata_SM$prob
newdata_SM$lower  <- exp(preds2$fit- 1.96*preds2$se.fit)*newdata_SM$lprob
newdata_SM$higher <- exp(preds2$fit+ 1.96*preds2$se.fit)*newdata_SM$hprob

#predict CPUE estimates per month
Month <- data.frame(YearS = 5,
                    CT = factor("MAKO", levels = levels(Snapper$CT)),
                    Location = factor("B", levels = levels(Snapper$Location)),
                    cmonth = cos(2*pi*seq(1,12, len=12)/12),
                    smonth = sin(2*pi*seq(1,12, len=12)/12),
                    Fishing_Effort = 8,
                    Month = seq(1,12, len=12))

#predict probability of catch
preds <- predict(Model25, Month, se.fit=TRUE)
Month$prob <- inv.logit(preds$fit)
Month$lprob <- inv.logit(preds$fit - 1.96*preds$se.fit)
Month$hprob <- inv.logit(preds$fit + 1.96*preds$se.fit)

#predict catch in kg
preds2 <- predict(Model26, Month, se.fit=TRUE)
Month$predicted <- exp(preds2$fit)*Month$prob
Month$lower  <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$lprob
Month$higher <- exp(preds2$fit - 1.96*preds2$se.fit)*Month$hprob

#one estimate for each month, with SE=1.96*se
desc20 <- as.data.frame(cbind(Month = Month$Month, Pred = Month$predicted, 
                              SE = Month$predicted - Month$lower))
desc20
desc20 %>%
  summarise(mean= mean(Pred),
            se = 1.96*std.error(Pred))

#Figure 4J: CPUE plot

Fig4J <- ggplot() + 
  geom_ribbon(data=newdata_SM, 
              aes(x=Month, ymin=lower, ymax=higher), 
              fill="red")+
  geom_line(data=newdata_SM,
            aes(x=Month, y=predicted), size=.8)+
  scale_x_continuous(name="Month", breaks=seq(1, 12, 1))+
  scale_y_continuous(name="CPUE", breaks=seq(0, 5, 1),
                     limits=c(0,5))+
  theme_classic()+
  theme(axis.line=element_line(size=.8))

#plot Fig4IJ together (snapper trends)
Fig4IJ <- plot_grid(Fig4I, Fig4J, align = "v", nrow=2, labels=c("I", "J"))

Fig4EFGHIJ <- plot_grid(Fig4EF, Fig4GH, Fig4IJ, nrow=1)

FigABCD
Fig4EFGHIJ

###

#Stable isotope analysis:
##both species, DC vs all over islands
library(SIBER)

#looking at shark lengths at DC compared to other islands
lengths <- read.csv("SIA.csv")
lengthsDG <- as.data.frame(subset(lengths, Binary_Location == "DiegoGarcia"))
lengthsWA <- as.data.frame(subset(lengths, Binary_Location == "WiderArchipelago"))

par(mfrow=c(2,1))
hist(as.numeric(lengthsDG$TL..cm.))
hist(as.numeric(lengthsWA$TL..cm.))
#similar enough total lengths so no need to account for this in SIA analyses

mydata <- read.csv('isotopes.csv', header=TRUE)
mydata <- rename(mydata, iso1 = ï..iso1)
siber.example <- createSiberObject(mydata)
siber.example$sample.sizes

#Figure 5: plotting biplots of the data in ggplot 
ggplot()+
  geom_point(data=mydata, aes(x=iso1, y=iso2, colour=community, pch=group), 
             size=1.3, alpha=.8)+
  labs(x=expression({delta}^13*C~'\u2030'), 
       y=expression({delta}^15*N~'\u2030'))+
  stat_ellipse(data=mydata, aes(x=iso1, y=iso2, colour=community, lty=group), 
               level=.40, size=.7)+
  scale_colour_manual(name = "Location", 
                      labels = c("Diego Garcia", "Wider Archipelago"),
                      values = c("darkorchid4", "coral"))+
  scale_linetype_manual(name= "Species",
                        labels = c("Grey reef shark",
                                   "Silvertip shark"),
                        values = c(2, 1))+
  scale_shape_discrete(name= "Species",
                       labels = c("Grey reef shark",
                                  "Silvertip shark"))+
  theme_classic()+
  theme(axis.line=element_line(size=.8))
#Export and Save as Image....

#basic summary statistics 
#calculate summary statistics for each group, TA, SEA, and SEAc
#ML = maximum likelihood
group.ML <- groupMetricsML(siber.example)
print(group.ML)
#TA = total area of convex hull of the group, indication of niche width
#SEA= standard ellipse area (40% of points)
#SEAc = SEA corrected for small sample size n<10

#descriptive stats

#Table 3: range, mrean and sd of N and C isotopes


#N=
nrow(mydata)

#two-way MANOVA to compare regional mean C and N values for both species

#boxplots
boxplot(mydata$iso1)
boxplot(mydata$iso2)
#some outliers but nothing too biologically unrealistic

#histograms
hist(mydata$iso1)
hist(mydata$iso2)
#very close to normally distributed

#homogeneity of variances
var(mydata$iso1) /var(mydata$iso2)
#similar variances, only 2x different (>4x)

#colinearity in explanatory variables
chisq.test(mydata$group, mydata$community, correct=FALSE)
#these variables are independent of one another

#two-way MANOVA
mod <- manova(cbind(iso1, iso2) ~ group + community, data = mydata)
summary(mod)
#indicates significant effects of both species (group) and atoll (community)
summary.aov(mod)
#apart from no significant effect of species on N (feeding at the same trophic level)
mod_interaction <- manova(cbind(iso1, iso2) ~ group + community + group*community, data = mydata)
summary(mod_interaction)
#interaction not significant so removed
summary(mod, test="Wilks")

#two-way ANOVA for C values
modC <- lm(iso1 ~ group + community, data=mydata)
#interaction not significant so removed
anova(modC)
total <- 21.268 + 44.505 + 163.054
#effect size of community
21.268/total
#effect size of group
44.505/total

#two way ANOVA for N values
modN <- lm(iso2 ~ Species + Location, data=mydata)
anova(modN)
total <- 1.907 + 9.731 + 90.815
#effect size for community
1.907/total
#effect size for group
9.731/total

#investigation: SEA overlaps
#calculation of standard ellipse overlap using Maximum Likelihood estimation
#set ellipses names to group names of siber.example
ellipse1 <- "DiegoGarcia.Silvertip"
ellipse2 <- "DiegoGarcia.GreyReef"
ellipse3 <- "WiderArchipelago.Silvertip"
ellipse4 <- "WiderArchipelago.GreyReef"

#Diego Garcia silvertip vs Diego Garcia grey reef 
DGSDGG <- maxLikOverlap(ellipse1, ellipse2, siber.example,
                        p.interval=NULL, n=100)
#overlap as a percent of non-overlapping area
DGSDGG[3] / (DGSDGG[1] + DGSDGG[2] - DGSDGG[3])

#Diego Garcia silvertip vs Wider Archipelago silvertip
DGSWAS <- maxLikOverlap(ellipse1, ellipse3, siber.example,
                        p.interval=NULL, n=100)
#overlap as a percent of non-overlapping area
DGSWAS[3] / (DGSWAS[1] + DGSWAS[2] - DGSWAS[3])

#Diego Garcia silvertip vs Wider Archipelago grey reef
DGSWAG <- maxLikOverlap(ellipse1, ellipse4, siber.example,
                        p.interval=NULL, n=100)
#overlap as a percent of non-overlapping area
DGSWAG[3] / (DGSWAG[1] + DGSWAG[2] - DGSWAG[3])

#Diego Garcia grey reef vs Wider Archipelago silvertip
DGGWAS <- maxLikOverlap(ellipse2, ellipse3, siber.example,
                        p.interval=NULL, n=100)
#overlap as a percent of non-overlapping area
DGGWAS[3] / (DGGWAS[1] + DGGWAS[2] - DGGWAS[3])

#Diego Garcia grey reef vs Wider Archipelago grey reef
DGGWAG <- maxLikOverlap(ellipse2, ellipse4, siber.example,
                        p.interval=NULL, n=100)
#overlap as a percent of non-overlapping area
DGGWAG[3] / (DGGWAG[1] + DGGWAG[2]- DGGWAG[3])

#Wider Archipelago grey reef vs Wider Archipelago silvertip
WAGWAS <- maxLikOverlap(ellipse3, ellipse4, siber.example,
                        p.interval=NULL, n=100)
#overlap as a percent of non-overlapping area
WAGWAS[3] / (WAGWAS[1] + WAGWAS[2] - WAGWAS[3])

mydata2 <- read.csv('isotopes1.csv', header=TRUE)
mydata2 <- rename(mydata2, iso1 = ï..iso1)

#Supplementary SIA:
#plotting biplots of the data in ggplot
ggplot()+
  geom_point(data=mydata2, aes(x=iso1, y=iso2, colour=community, pch=group), 
             size=1.3, alpha=.8)+
  labs(x=expression({delta}^13*C~'\u2030'), 
       y=expression({delta}^15*N~'\u2030'))+
  stat_ellipse(data=mydata2, aes(x=iso1, y=iso2, colour=community, lty=group), 
               level=.40, size=.7)+
  theme_classic()+
  theme(axis.line=element_line(size=.8))
