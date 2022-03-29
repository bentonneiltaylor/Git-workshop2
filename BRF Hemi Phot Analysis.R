#### BRF Hemispherical Photo Analysis - 2021 ######
#### Ben Taylor - 10/14/2021

#library(lme4);library(jtools)
library(multcomp);library(ggplot2)
#setwd("~/Desktop/Work Files/Research/BRF Deer Girdling/Data & Analysis Files")
gla.dat<-read.csv("BRF_Hemi Phot GLA data.csv")
img.match<-read.csv("BRF Photo File Matching.csv")

img.match<-img.match[img.match$analyze==1,]

dat<-merge(gla.dat,img.match,by="image.file", all.x=T,all.y=T)
dat$gird.trt<-as.factor(dat$gird.trt)
dat$exc.trt<-as.factor(dat$exc.trt)
dat$row<-as.factor(dat$row)
write.csv(dat[,c(25:29,5,20:22)], file="BRF Hemiphoto Data_Post-process.csv", row.names=F)

#Pulling out Plot/Treatment Info
plt.info<-dat[,c(25:29)]
plt.info<-unique(plt.info)
write.csv(plt.info, file="BRF 2021 Plot Info Matching File.csv", row.names=F)

#Running Initial ANOVA's on Transmittance and Canopy Openness
with(dat, summary(aov(X..Trans.Dir~gird.trt*exc.trt+row)))
with(dat, summary(aov(X..Trans.Dif~gird.trt*exc.trt+row)))
with(dat, summary(aov(X..Cnpy.Open~gird.trt*exc.trt+row)))

with(dat, tapply(X..Trans.Dir,gird.trt,median, na.rm=T))
with(dat, tapply(X..Trans.Dif,gird.trt,median, na.rm=T))
with(dat, tapply(X..Cnpy.Open,gird.trt,median, na.rm=T))

co_aov<-with(dat, aov(X..Cnpy.Open~gird.trt+row))
summary(co_aov)
tuk.co<-cld(glht(co_aov, linfct=mcp(gird.trt="Tukey")))
tuk.co

#### Boxplot of Girdling Treatment #####
ggplot(dat, aes(x=factor(gird.trt,level=c('C','N','O50','O')), y=X..Cnpy.Open))+ #fill for dif colors
  geom_boxplot(notch=F)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour="Black", size=15, angle=90,vjust=.75,hjust=.75),
        axis.title.y=element_text(size=20), axis.text.y=element_text(colour="Black", size=20),
        axis.line.y=element_line(color="black", size=1),axis.line.x=element_line(color="black", size=1),
        plot.title=element_text(size=20), legend.position="right")

#### Paired Boxplot of Girdling and Exclosure Treatment ####
ggplot(dat, aes(x=factor(gird.trt,level=c('C','N','O50','O')), y=X..Cnpy.Open, fill=exc.trt))+ #fill for dif colors
  geom_boxplot(notch=F)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour="Black", size=15, angle=90,vjust=.75,hjust=.75),
        axis.title.y=element_text(size=20), axis.text.y=element_text(colour="Black", size=20),
        axis.line.y=element_line(color="black", size=1),axis.line.x=element_line(color="black", size=1),
        plot.title=element_text(size=20), legend.position="right")

