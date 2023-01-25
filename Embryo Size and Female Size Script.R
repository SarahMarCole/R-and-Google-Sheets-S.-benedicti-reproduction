#Take MBS from Embryo Size Script
#Take Total from Num Embryos Script

#Combine so that all broods are matched with embryo size and female size after mean for brood is found
SizeEmb_Female<-merge(MBS, Total, by=c("Date", "BroodName")) %>%
  tidyr::drop_na(FemaleLength) #drops values where female is not included







#subset into populations
library(dplyr)
Bay_EF<-filter(SizeEmb_Female, Origin.x=="Bay")
LB_EF<-filter(SizeEmb_Female, Origin.x=="LB")
F2_EF<-filter(SizeEmb_Female, Origin.x=="F2")

#Female Area x Embryo Area
plot.new()
plot(Bay_EF$FemaleArea, Bay_EF$EmbryoSize, 
     xlab="Female Area mm²",ylab="Embryo Area µm²", bg="chartreuse4", pch=24,
     ylim=c(1500,11810),xlim=c(0.5,2.5) , bty="l", cex.lab=1.5)
points(LB_EF$FemaleArea, LB_EF$EmbryoSize, pch=21, bg=c("chocolate1"))
#points(Baruch$FemaleArea, Baruch$NumEmbryo, pch=22, bg=c("darkorchid3"))
points(F2_EF$FemaleArea, F2_EF$EmbryoSize, pch=23, bg=c("darkorchid3"))

#res_Bay_EF<-cor.test(Bay_EF$FemaleArea, Bay_EF$EmbryoSize, method="kendall")
#res_Bay_EF
lm_Bay_EF<-lm(Bay_EF$EmbryoSize ~ Bay_EF$FemaleArea, data = Bay_EF)
summary(lm_Bay_EF)
abline(lm_Bay_EF, col="chartreuse4")

lm_LB_EF<-lm(LB_EF$EmbryoSize ~ LB_EF$FemaleArea, data = LB_EF)
summary(lm_LB_EF)
abline(lm_LB_EF, col="chocolate1")

lm_F2_EF<-lm(F2_EF$EmbryoSize ~ F2_EF$FemaleArea, data = F2_EF)
summary(lm_F2_EF)
abline(lm_F2_EF, col="darkorchid3")

#Female Lenth x Embryo Size
plot.new()
plot(Bay_EF$FemaleLength, Bay_EF$EmbryoSize, 
     xlab="Female Length mm",ylab="Embryo Area µm²", bg="chartreuse4", pch=24,
     ylim=c(1500,11810),xlim=c(2.75,10) , bty="l", cex.lab=1.5)
points(LB_EF$FemaleLength, LB_EF$EmbryoSize, pch=21, bg=c("chocolate1"))
#points(Baruch$FemaleLength, Baruch$NumEmbryo, pch=22, bg=c("darkorchid3"))
points(F2_EF$FemaleLength, F2_EF$EmbryoSize, pch=23, bg=c("darkorchid3"))

lm_Bay_EF<-lm(Bay_EF$EmbryoSize ~ Bay_EF$FemaleLength, data = Bay_EF)
summary(lm_Bay_EF)
abline(lm_Bay_EF, col="chartreuse4")

lm_LB_EF<-lm(LB_EF$EmbryoSize ~ LB_EF$FemaleLength, data = LB_EF)
summary(lm_LB_EF)
abline(lm_LB_EF, col="chocolate1")

lm_F2_EF<-lm(F2_EF$EmbryoSize ~ F2_EF$FemaleLength, data = F2_EF)
summary(lm_F2_EF)
abline(lm_F2_EF, col="darkorchid3")


#Reproductive Output-------

#Create reproductive output 
SizeEmb_Female$R0<-(SizeEmb_Female$NumEmbryo/ (SizeEmb_Female$FemaleLength)) * SizeEmb_Female$EmbryoSize

# number of embryos/ Female length x embryo size 


group_by(SizeEmb_Female, Origin.x) %>%
  summarise(
    count=n(),
         median=median(R0, na.rm = TRUE),
         IQR=IQR(R0, na.rm = TRUE)
       )


SizeEmb_Female$Origin<-SizeEmb_Female$Origin.x
library("ggpubr")
ggboxplot(SizeEmb_Female, x = "Origin", y = "R0", 
          color = "Origin", palette = c("chartreuse4", "chocolate1",  "darkorchid3"),#, "purple"),
          ylab = "Reproductive Output", xlab = "Origin") +
  
  theme(axis.title =element_text(size=20), legend.text = element_text(size=20), 
        axis.text = element_text(size=16), legend.title = element_text(size= 20))


kruskal.test(R0 ~ Origin.x, data = SizeEmb_Female) #p<0.05 significant differences between populations

R0.aov <- aov(R0 ~ Origin, data = SizeEmb_Female)
summary(R0.aov)
library(multcomp)
summary(glht(R0.aov, linfct = mcp(Origin = "Tukey")))



#if there is a sig diff in kruskal wallis, where these are
pairwise.wilcox.test(SizeEmb_Female$R0, SizeEmb_Female$Origin.x,
                     p.adjust.method = "BH")

# Find mean Number of Embryos for specific females over lifetime amount of broods
library(dplyr)
MNEF<-SizeEmb_Female %>%
  group_by(Origin.x,`BroodName`) %>%      #Group Data by what you want to bin
  mutate(NumEmbryo_mean=mean(NumEmbryo)) %>% #find mean of results
  mutate(NumEmbryo_sd=sd(NumEmbryo)) %>%
  distinct(Origin.x, `BroodName`, .keep_all=TRUE) %>%  #tell what columns are group
  # dplyr::select(everything()) %>% #Put month and year in front
  tidyr::drop_na(NumEmbryo)   


MNEF$NumEmbryo_correctFemaleArea<-(MNEF$NumEmbryo_mean)/ MNEF$FemaleLength
MNEF$NumEmbryo_sd_corrected<-(MNEF$NumEmbryo_sd)/MNEF$FemaleLength

Bay_MNEF<-filter(MNEF, Origin.x=="Bay")
LB_MNEF<-filter(MNEF, Origin.x=="LB")
F2_MNEF<-filter(MNEF, Origin.x=="F2")

x_Bay<-c(1:10)

x_LB<-c(16:26)

x_F2<-c(36:47)


plot.new()
plot(x_Bay, y=Bay_MNEF$NumEmbryo_correctFemaleArea, 
     xlab="Origin",ylab="Embryos per Brood / Female Length (mm²)", bg="chartreuse4", pch=24,
     ylim=c(0,72),xlim=c(0,50) , bty="l", xaxt = "n", cex.lab=1.5, cex.axis=1.5)

#plot the median onto this as well
segments(x0=1, x1=10, y0=37.2, y1=37.2, lwd=3, col="chartreuse4")
segments(x0=16, x1=26, y0=5.09, y1=5.09, lwd=3, col="chocolate1")
segments(x0=36, x1=47, y0=16.4, y1=16.4, lwd=3, col="darkorchid3")
points(x_LB, y=LB_MNEF$NumEmbryo_correctFemaleArea, pch=21, bg=c("chocolate1"))
#points(Baruch$FemaleLength, Baruch$NumEmbryo, pch=22, bg=c("darkorchid3"))
points(x_F2, y=F2_MNEF$NumEmbryo_correctFemaleArea, pch=23, bg=c("darkorchid3"))
axis(1, at=c(5,20.5, 41), labels=c("Bay","LB","F1"), las=0, cex=1.5)

arrows(x_Bay, Bay_MNEF$NumEmbryo_correctFemaleArea-Bay_MNEF$NumEmbryo_sd_corrected, x_Bay, Bay_MNEF$NumEmbryo_correctFemaleArea+Bay_MNEF$NumEmbryo_sd_corrected, length=0.05, angle=90, code=3, col="chartreuse4")
arrows(x_LB, LB_MNEF$NumEmbryo_correctFemaleArea-LB_MNEF$NumEmbryo_sd_corrected, x_LB, LB_MNEF$NumEmbryo_correctFemaleArea+LB_MNEF$NumEmbryo_sd_corrected, length=0.05, angle=90, code=3, col="chocolate1")
arrows(x_F2, F2_MNEF$NumEmbryo_correctFemaleArea-F2_MNEF$NumEmbryo_sd_corrected, x_F2, F2_MNEF$NumEmbryo_correctFemaleArea+F2_MNEF$NumEmbryo_sd_corrected, length=0.05, angle=90, code=3, col="darkorchid3")



#legend ("topright",
##        cex=1,
#        c("Bayonne", "Long Beach", "F2"), 
#        pt.cex=1, 
#        pt.bg=c("chartreuse4", "chocolate1","darkorchid3"), pch=c(24,21,22, 23))
