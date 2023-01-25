library(readxl)
Bay <-read_excel("Documents/Worm Brood Information.xlsx", 
                                            sheet = "Bay")

#View(Worm_Brood_Information)

LB<- read_excel("Documents/Worm Brood Information.xlsx",  
                sheet = "LB")

Baruch<-read_excel("Documents/Worm Brood Information.xlsx",  
                   sheet = "Baruch")

F2<-read_excel("Documents/Worm Brood Information.xlsx",  
               sheet = "F2")

#reduce to desired columns only

cols<-c("Origin","BroodName","FemaleLength", "FemaleArea","NumEmbryo" 
        , "BroodNum","Date" )

Bay<-Bay[cols]%>%
  tidyr::drop_na(NumEmbryo)  %>%
  tidyr::drop_na(FemaleArea)
  


hist(Bay$NumEmbryo)

LB<-LB[cols] %>%
  tidyr::drop_na(NumEmbryo)
LB$NumEmbryo<-as.numeric(LB$NumEmbryo)
hist(LB$NumEmbryo)

LB<- tidyr::drop_na(LB, NumEmbryo)

#Baruch<-Baruch[cols]
#hist(Baruch$NumEmbryo)

F2<-F2[cols] %>%
  tidyr::drop_na(NumEmbryo)
#combine into 1 dataframe
Total<-rbind(Bay, LB, F2) %>%
  tidyr::drop_na(NumEmbryo)   


#Change embryo number, length and area to numeric
Total$NumEmbryo<-as.numeric(Total$NumEmbryo)
Total$FemaleLength<-as.numeric(Total$FemaleLength)
Total$FemaleArea<-as.numeric(Total$FemaleArea)

#test normality
shapiro.test(Total$NumEmbryo) #p<.05 not normal
#Correct for Female Length
Total$NumEmbryo_corrected<-(Total$NumEmbryo / Total$FemaleLength)

library(dplyr)
#Create boxplot for Number of Embryos and find Median---------
group_by(Total, Origin) %>%
  summarise(
    count=n(),
    median=median(NumEmbryo_corrected, na.rm = TRUE),
    IQR=IQR(NumEmbryo_corrected, na.rm = TRUE)
  )




#library("ggpubr")                                 #Part of combined plot with individual females now
#ggboxplot(Total, x = "Origin", y = "NumEmbryo", 
 #         color = "Origin", palette = c("chartreuse4", "chocolate1", "darkorchid3", "blue","purple"),
  #        ylab = "Number of Embryo per Brood", xlab = "Origin")


# Only for 2 populations
#res <- wilcox.test(NumEmbryo ~ Origin, data = Total,
                 #  exact = FALSE)
#res

#3+ populations
kruskal.test(NumEmbryo_corrected ~ Origin, data = Total) #p<0.05 significant differences between populations

#if there is a sig diff in kruskal wallis, where these are
pairwise.wilcox.test(Total$NumEmbryo_corrected, Total$Origin,
                     p.adjust.method = "BH")
#Create log for each
Bay$logFemaleArea<- log(Bay$FemaleArea)

#Female Length x Female Area----
plot(Bay$FemaleArea, Bay$FemaleLength, ylab = "Female Length mm ", xlab = "Female Area mm²", cex.lab=1.5,
     bg="chartreuse4", pch=24, xlim=c(0, 8.5), ylim = c(2, 15.5))
Baylm_Size<-lm(Bay$FemaleLength ~ Bay$FemaleArea, data = Bay)
summary(Baylm_Size)
abline(Baylm_Size, col="chartreuse4")


points(LB$FemaleArea, LB$FemaleLength, pch=21, bg=c("chocolate1"))
LBlm_Size<-lm(LB$FemaleLength ~ LB$FemaleArea, data = LB)
summary(LBlm_Size)
abline(LBlm_Size, col="chocolate1")

points(F2$FemaleArea, F2$FemaleLength, pch=23, bg=c("darkorchid3"))
F2lm_Size<-lm(F2$FemaleLength ~ F2$FemaleArea, data=F2)
summary(F2lm_Size)
abline(F2lm_Size, col="darkorchid3")


# Female Area x Brood Size-------
par(mfrow=c(2,2))

plot(Bay$logFemaleArea, Bay$NumEmbryo, 
     xlab="log(Female Area mm²)",ylab="Embryos per Brood", bg="chartreuse4", pch=24,
     ylim=c(0,500),xlim=c(-1,3) , bty="l", cex.lab=1.5)

points(log(LB$FemaleArea), LB$NumEmbryo, pch=21, bg=c("chocolate1"))
#points(Baruch$FemaleArea, Baruch$NumEmbryo, pch=22, bg=c("darkorchid3"))
points(log(F2$FemaleArea), F2$NumEmbryo, pch=23, bg=c("darkorchid3"))
 
F1<-c(("F"_1))

plot.new()
legend ("topright",
     cex=1.5,
      c("Planktotroph", "Lecithotroph", "F"), 
       pt.cex=1, 
     pt.bg=c("chartreuse4", "chocolate1","darkorchid3"), pch=c(24,21, 23))

#res_Bay<-cor.test(Bay$FemaleArea, Bay$NumEmbryo, method="kendall")
#res_Bay
lm_Bay<-lm(Bay$NumEmbryo ~ Bay$logFemaleArea, data = Bay)
summary(lm_Bay)
abline(lm_Bay, col="chartreuse4")

LB$NumEmbryo<-as.numeric(LB$NumEmbryo)
#res_LB<-cor.test(LB$FemaleArea, LB$NumEmbryo, method="kendall")
#res_LB

lm_LB<-lm(LB$NumEmbryo ~(log(LB$FemaleArea)), data = LB)
summary(lm_LB)
abline(lm_LB, col="chocolate1")



#Baruch$NumEmbryo<-as.numeric(Baruch$NumEmbryo)
#res_Baruch<-cor.test(Baruch$FemaleArea, Baruch$NumEmbryo, method="kendall")
#res_Baruch

F2$NumEmbryo<-as.numeric(F2$NumEmbryo)
#res_F2<-cor.test(F2$FemaleArea, F2$NumEmbryo, method="kendall")
#res_F2
lm_F2<-lm(F2$NumEmbryo ~(log(F2$FemaleArea)), data = F2)
summary(lm_F2)
abline(lm_F2, col="darkorchid3")


# Female Length x Brood Size
#plot.new()
plot((log(Bay$FemaleLength)), Bay$NumEmbryo, 
     xlab="log(Female Length mm)",ylab="Embryos per Brood", bg="chartreuse4", pch=24,
     ylim=c(0,500),xlim=c(1,3) , bty="l", cex.lab=1.5)
points(log(LB$FemaleLength), LB$NumEmbryo, pch=21, bg=c("chocolate1"))
#points(Baruch$FemaleLength, Baruch$NumEmbryo, pch=22, bg=c("darkorchid3"))
points(log(F2$FemaleLength), F2$NumEmbryo, pch=23, bg=c("darkorchid3"))



#legend ("topright",
#        cex=1,
#        c("Bayonne", "Long Beach", "F1"), 
 #       pt.cex=1, 
  #    pt.bg=c("chartreuse4", "chocolate1","darkorchid3"), pch=c(24,21, 23))

#res_Bay<-cor.test(Bay$FemaleLength, Bay$NumEmbryo, method="kendall")
#res_Bay
lm_Bay<-lm(Bay$NumEmbryo ~ (log(Bay$FemaleLength)), data = Bay)
summary(lm_Bay)
abline(lm_Bay, col="chartreuse4")

LB$NumEmbryo<-as.numeric(LB$NumEmbryo)
#res_LB<-cor.test(LB$FemaleLength, LB$NumEmbryo, method="kendall")
#res_LB

lm_LB<-lm(LB$NumEmbryo ~(log(LB$FemaleLength)), data = LB)
summary(lm_LB)
abline(lm_LB, col="chocolate1")



#Baruch$NumEmbryo<-as.numeric(Baruch$NumEmbryo)
#res_Baruch<-cor.test(Baruch$FemaleLength, Baruch$NumEmbryo, method="kendall")
#res_Baruch

F2$NumEmbryo<-as.numeric(F2$NumEmbryo)
#res_F2<-cor.test(F2$FemaleLength, F2$NumEmbryo, method="kendall")
#res_F2
lm_F2<-lm(F2$NumEmbryo ~(log(F2$FemaleLength)), data = F2)
summary(lm_F2)
abline(lm_F2, col="darkorchid3")


#Repeat broods----------

library(dplyr)

BroodNum<-tidyr::drop_na(Total, NumEmbryo)

group_by(BroodNum, BroodNum, Origin) %>%
  summarise(
    count = n(),
    mean = mean(NumEmbryo, na.rm = TRUE),
    sd = sd(NumEmbryo, na.rm = TRUE),
    median = median(NumEmbryo, na.rm = TRUE),
    IQR = IQR(NumEmbryo, na.rm = TRUE)
  )

library(ggpubr)

ggboxplot(Bay, x = "BroodNum", y = "NumEmbryo", 
          color = "BroodNum", #palette = c("darkorchid3", "blue", "gold", "purple","black"),
        #order = c("1", "2", "3","4","5"),
          ylab = "Number of Embryos", xlab = "Brood Number")



ggline(Bay, x = "BroodNum", y = "NumEmbryo", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3","4","5"),
       ylab = "Number of Embryos", xlab = "Brood Number")

Bay %>%
  #group_by(BroodNum) %>%
  shapiro_test(NumEmbryo)
library(rstatix)
res.aov<-anova_test(data=Bay, dv=NumEmbryo, wid=BroodName, within=BroodNum)
get_anova_table(res.aov)
