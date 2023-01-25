library(readxl)
Bay_Size <-read_excel("Documents/Worm Brood Information.xlsx", 
                 sheet = "Bay Embryo Sizes")
LB_Size <-read_excel("Documents/Worm Brood Information.xlsx", 
                      sheet = "LB New Embryo Sizes")
#Baruch_Size <-read_excel("Documents/Worm Brood Information.xlsx", 
                    # sheet = "Baruch Embryo Sizes")

F2_Size<-read_excel("Documents/Worm Brood Information.xlsx", 
               sheet = "F2 Embryo Sizes")

#only take the wanted columns
cols<-c(1, 2, 4:15)
cols_f2<-c(1,2, 4,6:16)
Bay_Size<-Bay_Size[cols]
LB_Size<-LB_Size[cols]
#Baruch_Size<-Baruch_Size[cols]
F2_Size<-F2_Size[cols_f2]



Total_Size<-rbind(Bay_Size, LB_Size, F2_Size) #, Baruch_Size
#reorganize to long table for data
library(tidyr)

Total_Size <- pivot_longer(Total_Size, cols=5:14, names_to = "EmbryoNumber", values_to = "EmbryoSize") 

Total_Size<-na.omit(Total_Size)


#Find mean of the 10 sampled embryos for each brood-------
library(dplyr)
MBS<-Total_Size %>%
  group_by(Origin, Total_Size$`BroodName`,Total_Size$`Stage Found`, Total_Size$`Date`) %>%      #Group Data by what you want to bin
  mutate(EmbryoSize=mean(EmbryoSize)) %>%                         #find mean of results
  distinct(Origin, Total_Size$`BroodName`,Total_Size$`Stage Found`, Total_Size$`Date`, .keep_all=TRUE) %>%  #tell what columns are group
 # dplyr::select(everything()) %>% #Put month and year in front
  tidyr::drop_na(EmbryoSize)   



#put origin in order
#x<-c("Bay", "LB", "Baruch")

#MBS<-
 # MBS %>%
  #arrange(factor(Origin, levels = x))


# plot for just origin---------

MBS<-MBS %>%
  filter(`Stage Found`!="prototroch")

library("ggpubr")
ggboxplot(MBS, x = "Origin", y = "EmbryoSize", 
          color = "Origin", palette = c("chartreuse4", "chocolate1", "darkorchid3"),#, "purple"),
          ylab = "Embryo Area µm²", xlab = "Origin") 



group_by(MBS, Origin) %>%
  summarise(
    count=n(),
    median=median(EmbryoSize, na.rm = TRUE),
    IQR=IQR(EmbryoSize, na.rm = TRUE))


#1 cell--

#single_cell<-MBS %>%
 # filter(`Stage Found`=="1 cell")

#library("ggpubr")
#ggboxplot(single_cell, x = "Origin", y = "EmbryoSize", 
#          color = "Origin", palette = c("chartreuse4", "chocolate1",  "darkorchid3"),#, "purple"),
#          ylab = "Embryo Area µm²", xlab = "Origin")


#group_by(single_cell, Origin, `Stage Found`) %>%
 # summarise(
  #  count=n(),
  #  median=median(EmbryoSize, na.rm = TRUE),
  #  IQR=IQR(EmbryoSize, na.rm = TRUE))
    
# plot for origin and stage found
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

#order Embryo Stage and change column name

MBS$`Embryo Stage`<-MBS$`Stage Found`

MBS$`Embryo Stage`<-factor(MBS$`Embryo Stage`, levels=c("1 cell","trefoil", "2 cell", "4 cell", "8 cell", "16 cell", "blastula", "gastrula"), ordered=TRUE)

MBS$Origin<-factor(MBS$Origin, levels = c("Bay","LB","F1"))


MBS[is.na(MBS)] <- "F1"
#Embryo_order<-c("1 cell","trefoil", "2 cell", "4 cell", "8 cell", "16 cell", "blastula", "gastrula")



library("ggpubr")
ggboxplot(MBS, x = "Origin", y = "EmbryoSize", color = "Embryo Stage", ylab = "Embryo Area µm²") +
    

  geom_segment(aes(x = 0.5,xend = 1.5,y = 2236,yend = 2236), color="chartreuse4") +
  geom_segment(aes(x = 1.5,xend = 2.5,y = 7812,yend = 7812), color="chocolate1") +
  geom_segment(aes(x = 2.5,xend = 3.5,y = 3895,yend = 3895), color="darkorchid3") +
  
theme(axis.title =element_text(size=20), legend.text = element_text(size=20), 
      axis.text = element_text(size=16), legend.title = element_text(size= 20))

#anova for embryo size 
MBS$Origin <- factor(MBS$Origin)
res.aov3 <- aov(EmbryoSize ~ Origin* `Stage Found`, data = MBS)
summary(res.aov3)

#tukey tests to find differences
library(multcomp)
summary(glht(res.aov3, linfct = mcp(Origin = "Tukey")))




