cricket<-read.csv("Herbexclusion.csv")
cricket
names(cricket)
class(cricket$year)
class(cricket$month)
class(cricket$sampling)
class(cricket$location)
class(cricket$block)
class(cricket$treatment)
class(cricket$leaves_num)
class(cricket$disease_leaves)
class(cricket$chewed_leaves)

#Change categories to as factors 
cricket2<-cricket
cricket2<-cricket2[c(1,2,4,5,6,7,8,9)]
cricket2$block<-as.factor(cricket2$block)
cricket2$year<-as.factor(cricket2$year)
cricket2$month<-as.integer(cricket2$month)
cricket2$treatment<-as.factor(cricket2$treatment)
cricket2$location<-as.factor(cricket2$location)
cricket2$leaves_num<-as.integer(cricket2$leaves_num)
cricket2$disease_leaves<-as.integer(cricket2$disease_leaves)
cricket2$chewed_leaves<-as.integer(cricket2$chewed_leaves)


library(vegan)
library(ggplot2)
library(dplyr)
install.packages("ggdist")
library(ggdist)
library(tidyverse)
library(ISwR)
install.packages("Matrix")
library(lme4)
install.packages("AICmodavg")

#DATA EXPLORING SECTION 



#WORKING WITH THE DATA AND DETERMINING OUTLIERS 

ggplot(cricket2,aes(x=location,y=disease_leaves))+
  geom_boxplot()+
  ggtitle("Number of Disease Leaves at Each Site")+
  theme_classic(base_family="serif")+
  xlab("Location")+
  ylab("Disease Leaves")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(cricket2,aes(x=treatment,y=disease_leaves))+
  geom_boxplot()+
  ggtitle("Number of Disease Leaves for each Treatment Type")+
  theme_classic(base_family="serif")+
  xlab("Treatment")+
  ylab("Disease Leaves")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(cricket2,aes(x=location,y=chewed_leaves))+
  geom_boxplot()+
  ggtitle("Number of Chewed Leaves for at Each Site")+
  theme_classic(base_family="serif")+
  xlab("Location")+
  ylab("Chewed Leaves")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(cricket2,aes(x=treatment,y=chewed_leaves))+
  geom_boxplot()+
  ggtitle("Number of Chewed Leaves for each Treatment")+
  theme_classic(base_family="serif")+
  xlab("Treatment")+
  ylab("Chewed Leaves")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(cricket2,aes(x=location,y=leaves_num))+
  geom_boxplot()+
  ggtitle("Total Leaf Count at Each Location")+
  theme_classic(base_family="serif")+
  xlab("Location")+
  ylab("Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(cricket2,aes(x=treatment,y=leaves_num))+
  geom_boxplot()+
  ggtitle("Total Leaf Count for each Treatment ")+
  theme_classic(base_family="serif")+
  xlab("Treatment")+
  ylab("Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))


#ggbetweenstats(data=mydata,x =factor_2, y=factor_1,outlier.tagging=TRUE,outlier.label = Sample_Name)
library(ggstatsplot)

ggbetweenstats(data=cricket2,x=location,y=disease_leaves,outlier.tagging = TRUE)+
  ggtitle("Determining Outliers in Disease Leaves at Testing Sites")+
  xlab("Location")+
  ylab("Diseased Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x =location, y=disease_leaves,outlier.tagging=TRUE,outlier.label = treatment,)+
  ggtitle("Determining Outliers in Disease Leaves at Testing Sites")+
  xlab("Location")+
  ylab("Diseased Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x =treatment, y=disease_leaves,outlier.tagging=TRUE,outlier.label = location)+
  ggtitle("Determining Outliers in Disease Leaves in each Treatment")+
  xlab("Treatment")+
  ylab("Diseased Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x =treatment, y=disease_leaves,outlier.tagging=TRUE)+
  ggtitle("Determining Outliers in Disease Leaves in each Treatment")+
  xlab("Treatment")+
  ylab("Diseased Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x=location,y=chewed_leaves,outlier.tagging = TRUE)+
  ggtitle("Determining Outliers in Chewed Leaves at Each Site")+
  xlab("Location")+
  ylab("Chewed Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x =location, y=chewed_leaves,outlier.tagging=TRUE,outlier.label = treatment)+
  ggtitle("Determining Outliers in Chewed Leaves at Each Sites")+
  xlab("Location")+
  ylab("Chewed Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))
  
ggbetweenstats(data=cricket2,x=treatment,y=chewed_leaves,outlier.tagging = TRUE)+
  ggtitle("Determining Outliers in Chewed Leaves in Each Treatment")+
  xlab("Treatment")+
  ylab("Chewed Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x =treatment, y=chewed_leaves,outlier.tagging=TRUE,outlier.label = location)+
  ggtitle("Determining Outliers in Chewed Leaves in Each Treatment")+
  xlab("Treatment")+
  ylab("Chewed Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))


ggbetweenstats(data=cricket2,x=location,y=leaves_num,outlier.tagging = TRUE)+
  ggtitle("Determining Outliers in Leaf Count at Each Site")+
  xlab("Location")+
  ylab("Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x =location, y=leaves_num,outlier.tagging=TRUE,outlier.label = treatment)+
  ggtitle("Determining Outliers in Leaf Count at Each Site")+
  xlab("Location")+
  ylab("Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x=treatment,y=leaves_num,outlier.tagging = TRUE)+
  ggtitle("Determining Outliers in Leaf Count for each Treatment")+
  xlab("Treatment")+
  ylab("Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

ggbetweenstats(data=cricket2,x =treatment, y=leaves_num,outlier.tagging=TRUE,outlier.label = location)+
  ggtitle("Determining Outliers in Leaf Count for each Treatment")+
  xlab("Treatment")+
  ylab("Leaf Count")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))


#Outliers are present. should a levene test be conducted?
cricket3<-cricket2[-74,]


#levene's test for homoogenity of variance
#levene's test is similary to bartlett's: maybe non normal, some outliers are present 

library(car)
levene_dl_result<-leveneTest(disease_leaves~location,data=cricket2)
levene_dt_result<-leveneTest(disease_leaves~treatment,data=cricket2)
levene_cl_results<-leveneTest(chewed_leaves~location,data=cricket2)
levene_ct_results<-leveneTest(chewed_leaves~treatment,data=cricket2)
levene_ll_results<-leveneTest(leaves_num~location,data=cricket2)
levene_lt_results<-leveneTest(leaves_num~treatment,data=cricket2)
levene_dl_result
levene_dt_result
levene_cl_results
levene_ct_results
  levene_ll_results
levene_lt_results


shapiro.test(cricket2$disease_leaves)
ggqqplot(cricket2$disease_leaves)+
  ggtitle("Diseased Leaves Distribution Test for Normality")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))
  
ggqqplot(cricket2$chewed_leaves)+
  ggtitle("Chewed Leaves Distribution Test for Normality")+
  theme_classic(base_family="serif")+
  theme(plot.title = element_text(hjust = 0.5))

shapiro.test(cricket2$chewed_leaves)
?shapiro.test
library(ggpubr)

#Run NMDS on the data points 
names(cricket2)
qual<-cricket2[c(1,2,3,4,5)]
meas<-cricket2[c(6,7,8)]
meas.mds<-metaMDS(comm=meas,distance = "euclidean", trace = FALSE,autotransform = FALSE,k=2)
plot(meas.mds$points)
meas.mds$stress
nmds<-data.frame(meas.mds$points)
nmds<-cbind(nmds, qual)

ggplot(nmds, aes(MDS1, MDS2)) +
  geom_point(size=3, stroke=1, color="black", 
             aes(shape = location, fill = treatment)) + 
  theme_classic(base_size=17,base_family="serif") + scale_shape_manual(values=c(23,24,25)) +
  scale_fill_manual(values =c("#8FC3F0","#A50F3F","lightgreen"))+
  stat_ellipse(aes(linetype=location, color=treatment),lwd=1.1)+scale_color_manual(values =c("#8FC3F0","#A50F3F","lightgreen"))+
  guides(fill = guide_legend(override.aes = list(color=c("#8FC3F0","#A50F3F","lightgreen"))),
         color = guide_legend(override.aes = list(shape = 21)))+
  ggtitle("Non-Metric Mulidimensional Scaling of Data")+
  theme(plot.title = element_text(hjust = 0.5))

#Making mean data (summary statistics)

library(Rmisc)
dev.off()
library(ggplot2)
summary.disease<-summarySE(cricket2, measurevar = "disease_leaves",groupvars = cbind("treatment","location"),na.rm= TRUE)
summary.disease
ggplot(summary.disease,aes(fill=location,y=disease_leaves, x=treatment))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_errorbar(aes(ymin=disease_leaves-se, ymax=disease_leaves+se),width = 0.3, position = position_dodge(0.9))+
  scale_fill_manual(values =c("#8FC3F0","#A50F3F","lightgreen"))+
  theme_classic(base_family="serif")+
  xlab("Treatment")+
  ylab("Disease Leaves")+
  ggtitle("Average Disease Leaves for Each Treatment")+
  theme(plot.title = element_text(hjust = 0.5))

summary.chewed<-summarySE(cricket2, measurevar = "chewed_leaves",groupvars = cbind("treatment","location"),na.rm= TRUE)
summary.chewed
ggplot(summary.chewed,aes(fill=location,y=chewed_leaves, x=treatment))+
  geom_bar(position = position_dodge(), stat = "identity")+
  geom_errorbar(aes(ymin=chewed_leaves-se, ymax=chewed_leaves+se),width = 0.3, position = position_dodge(0.9))+
  scale_fill_manual(values =c("#8FC3F0","#A50F3F","lightgreen"))+
  theme_classic(base_family="serif")+
  xlab("Treatment")+
  ylab("Chewed Leaves")+
  ggtitle("Average Chewed Leaves for Each Treatment")+
  theme(plot.title = element_text(hjust = 0.5))

#Linear regression model of chewed & disease leaves 

glmcricket<-glm(disease_leaves~chewed_leaves,data=cricket2,family = poisson)
summary.glm(glmcricket) 
ggplot(glmcricket,aes(x=chewed_leaves, y=disease_leaves))+
  geom_point()+geom_smooth(method=lm)
library(ggplot2)


# should maybe isolate each site to determine if differences exist around locations?
cricketboundary<-cricket2%>%subset(location=="Boundary")
cricketboundary
glmcricketboundary<-glm(disease_leaves~chewed_leaves,data=cricketboundary,family=poisson)
summary.glm(glmcricketboundary)


ggplot(glmcricketboundary,aes(x=chewed_leaves,y=disease_leaves))+
  geom_point()+stat_smooth(method=glm)+
  ggtitle("Disease Leaves given Chewed Leaves at Boundary Site")+
  scale_fill_manual(values =c("#8FC3F0","#A50F3F","lightgreen"))+
  theme_classic(base_family="serif")+
  xlab("Chewed Leaf Count")+
  ylab("Disease Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))

cricketfarlive<-cricket2%>%subset(location=="Far_Live")
cricketfarlive

glmcricketfarlive<-glm(disease_leaves~chewed_leaves,data=cricketfarlive, family=poisson)
summary.glm(glmcricketfarlive)

ggplot(glmcricketfarlive,aes(x=chewed_leaves,y=disease_leaves))+
  geom_point()+stat_smooth(method=glm)+
  ggtitle("Disease Leaves given Chewed Leaves at Far Live Site")+
  theme_classic(base_family="serif")+
  xlab("Chewed Leaf Count")+
  ylab("Disease Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))

cricketlive<-cricket2%>%subset(location=="Live")
cricketlive
glmcricketlive<-glm(disease_leaves~chewed_leaves,data=cricketlive,family=poisson)
summary.glm(glmcricketlive)
ggplot(glmcricketlive,aes(x=chewed_leaves,y=disease_leaves))+
  geom_point()+stat_smooth(method=glm)+
  ggtitle("Disease Leaves given Chewed Leaves at Live Site")+
  theme_classic(base_family="serif")+
  xlab("Chewed Leaf Count")+
  ylab("Disease Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))

cricketsubt1<-cricket2%>%subset(treatment=="1")
cricketsubt1
glmcricketsubt1<-glm(disease_leaves~chewed_leaves,data=cricketsubt1, poisson(link = "log"))
summary.glm(glmcricketsubt1)

ggplot(glmcricketsubt1,aes(x=chewed_leaves,y=disease_leaves))+
  geom_point()+stat_smooth(method=glm)+
  ggtitle("Disease Leaves given Chewed Leaves(Treatment Group 1)")+
  theme_classic(base_family="serif")+
  xlab("Chewed Leaf Count")+
  ylab("Disease Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))

cricketsubt2<-cricket2%>%subset(treatment=="2")
cricketsubt2
glmcricketsubt2<-glm(disease_leaves~chewed_leaves,data=cricketsubt2,family = "poisson")
summary.glm(glmcricketsubt2)

ggplot(glmcricketsubt2,aes(x=chewed_leaves,y=disease_leaves))+
  geom_point()+stat_smooth(method=glm)+
  ggtitle("Disease Leaves given Chewed Leaves (Treatment Group 2)")+
  theme_classic(base_family="serif")+
  xlab("Chewed Leaf Count")+
  ylab("Disease Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))
 
cricketsubt3<-cricket2%>%subset(treatment=="3")
cricketsubt3
glmcricketsubt3<-glm(disease_leaves~chewed_leaves,data=cricketsubt3, family="poisson")
summary.glm(glmcricketsubt3)

ggplot(glmcricketsubt3,aes(x=chewed_leaves,y=disease_leaves))+
  geom_point()+stat_smooth(method=glm)+
  ggtitle("Disease Leaves given Chewed Leaves (Treatment Group 3)")+
  theme_classic(base_family="serif")+
  xlab("Chewed Leaf Count")+
  ylab("Disease Leaf Count")+
  theme(plot.title = element_text(hjust = 0.5))

#running glmers and comparing AIC 
library(visreg)
library(lme4)
library(predictmeans)
library(mosaic)
library(car)f
library(influence.ME)
library(DHARMa)
library(tidyverse)

modeldiseasea<-glmer(disease_leaves~location*treatment + (1|chewed_leaves)+
                       (1|leaves_num), data=cricket2, family = "poisson")
summary(modeldiseasea)

modeldiseaseb<-glmer(disease_leaves~location + (1|chewed_leaves)+
                       (1|leaves_num), data=cricket2, family = "poisson")
summary(modeldiseaseb)

anova(modeldiseasea,modeldiseaseb)

#model a is the better fit 
modeldiseasec<-glmer(disease_leaves~treatment + (1|chewed_leaves)+
                       (1|leaves_num), data=cricket2, family = "poisson")
summary(modeldiseasec)

anova(modeldiseasea,modeldiseasec)

#model a is a better fit 
modeldiseased<-glmer(disease_leaves~location+treatment + (1|chewed_leaves), data=cricket2, family = "poisson")
anova(modeldiseasea,modeldiseased)

#Model a is the best fit 

#Comparing means of data-Kruskal Wallis

library(FSA)
install.packages("FSA")
install.packages("dunn.test")
library(dunn.test)

kruskal.test(disease_leaves~treatment,data=cricket2)#total data sig 
kruskal.test(chewed_leaves~treatment,data=cricket2)#total data sig 
kruskal.test(disease_leaves~location,data=cricket2)#total data sig 
kruskal.test(chewed_leaves~location,data=cricket2)#not data sig 
dunnTest(disease_leaves~treatment,data=cricket2, method = "bonferroni")
dunnTest(chewed_leaves~treatment,data=cricket2,method="bonferroni")
dunnTest(disease_leaves~location,data=cricket2,method="bonferroni")
dunnTest(chewed_leaves~location,data=cricket2,method="bonferroni")




#Comparing means of subsetted data 
kruskal.test(disease_leaves~treatment,data=cricketboundary)#stat sig
kruskal.test(chewed_leaves~treatment,data=cricketboundary)#statsig
dunnTest(disease_leaves~treatment,data=cricketboundary, method = "bonferroni")
dunnTest(chewed_leaves~treatment,data=cricketboundary, method = "bonferroni")


kruskal.test(disease_leaves~treatment,data=cricketfarlive)#stat sig
kruskal.test(chewed_leaves~treatment,data=cricketfarlive)#notstatsig
dunnTest(disease_leaves~treatment,data=cricketfarlive, method = "bonferroni")
dunnTest(chewed_leaves~treatment,data=cricketfarlive, method = "bonferroni")

kruskal.test(disease_leaves~treatment,data=cricketlive)#stat sig
kruskal.test(chewed_leaves~treatment,data=cricketlive)#notstatsig
dunnTest(disease_leaves~treatment,data=cricketlive, method = "bonferroni")
dunnTest(chewed_leaves~treatment,data=cricketlive, method = "bonferroni")


kruskal.test(disease_leaves~location,data=cricketsubt3)#statsig
kruskal.test(chewed_leaves~location,data=cricketsubt3)#notstat sig
dunnTest(disease_leaves~location,data=cricketsubt3, method = "bonferroni")
dunnTest(chewed_leaves~location,data=cricketsubt3, method = "bonferroni")


kruskal.test(disease_leaves~location,data=cricketsubt2)#notstatsig
kruskal.test(chewed_leaves~location,data=cricketsubt2)#not stat sig
dunnTest(disease_leaves~location,data=cricketsubt2, method = "bonferroni")
dunnTest(chewed_leaves~location,data=cricketsubt2, method = "bonferroni")

kruskal.test(disease_leaves~location,data=cricketsubt1)#notstatsig
kruskal.test(chewed_leaves~location,data=cricketsubt1)#stat sig
dunnTest(disease_leaves~location,data=cricketsubt1, method = "bonferroni")
dunnTest(chewed_leaves~location,data=cricketsubt1, method = "bonferroni")
