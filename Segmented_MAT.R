library(ggplot2)
library(mgcv)
library(plotly)
library(lme4)
library(segmented)

irish<-read.csv("IrishSea-MedSea-RedSea.csv",header=T)
tara<-read.csv("TaraOceanExpedition.csv",header=T)
names(irish)


model2<-lm(Td~poly(MAT_BioOracle, 2, raw=TRUE),data=irish)
summary(model2)
plot(model2,2)

iri_Td<-ggplot(irish,aes(MAT_BioOracle,Td))+
  geom_point()+
  theme_classic()+ 
  # geom_smooth(method="lm",se=FALSE,color="black")+
  geom_smooth(method="lm", se=TRUE, fill=NA,
  formula=y ~ poly(x, 2, raw=TRUE),colour="black")+
  xlab("Mean Average Temperature °C")+
  ylab("Denaturating Temperature °C ")+
  scale_x_continuous(breaks = round(seq(5, 30, by = 1),0))+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))
iri_Td

model2<-lm(Topt~poly(MAT_BioOracle, 2, raw=TRUE),data=irish)
summary(model2)
plot(model2,2)


iri_Topt<-ggplot(irish,aes(MAT_BioOracle,Topt))+
  geom_point()+
  theme_classic()+ 
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="black")+
  xlab("Mean Average Temperature °C")+
  ylab("Optimal Temperature °C ")+
  scale_x_continuous(breaks = round(seq(5, 30, by = 1),0))+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))

iri_Topt

model2<-lm(Tp~poly(MAT_BioOracle, 2, raw=TRUE),data=irish[irish$Tp!='NO',])
summary(model2)
plot(model2,2)

iri_Tp<-ggplot(irish[irish$Tp!='NO',],aes(MAT_BioOracle,as.numeric(Tp)))+
  geom_point()+
  theme_classic()+ 
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="black")+
  xlab("Mean Average Temperature °C")+
  ylab("Phase Trasnition Temperature (Tp) ")+
  scale_x_continuous(breaks = round(seq(5, 30, by = 1),0))+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))


iri_Tp





names(tara)
str(tara)
tara_td<-ggplot(tara,aes(MAT,Td))+
  geom_point()+
  theme_classic()+
  stat_smooth(method="loess",se=FALSE, color='black')+
  xlab("Mean Average Temperature °C")+
  ylab("Denaturating Temperature °C ")+
  theme(legend.position = "bottom",
         legend.box = "vertical",
         axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
         axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
         axis.title.y = element_text(size = 12),
         legend.text=element_text(size=12))
tara_td

# model<-lm(Td~MAT,data=tara)
# summary(model)
# anova(model, test = "F")
# plot(model,2)


# segmented.mod <- segmented(model, seg.Z = ~MAT)
# summary(segmented.mod)

# my.fitted <- fitted(segmented.mod)
# my.model <- data.frame(Temperature = tara$MAT, td = my.fitted)



tara_td_seg<-ggplot(tara,aes(MAT,Td))+
  geom_point()+
  theme_classic()+
  xlab("Mean Average Temperature °C")+
  ylab("Denaturating Temperature °C ")+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
        geom_smooth(method="lm", se=TRUE, fill=NA,
        formula=y ~ poly(x, 2, raw=TRUE),colour="black")



  # tara_td_seg=tara_td_seg+geom_line(data = my.model, aes(x = Temperature, y = td), colour = "black",lwd=1)+
  # geom_vline(xintercept = 25, colour="red", linetype = "longdash")

tara_td_seg

model2<-lm(Td~poly(MAT, 2, raw=TRUE),data=tara)
summary(model2)
plot(model2,2)


names(tara)
#model<-lm(Topt~MAT,data=tara)
#summary(model)
#anova(model, test = "F")
#plot(model,2)


#segmented.mod <- segmented(model, seg.Z = ~MAT)
#summary(segmented.mod)
#my.fitted <- fitted(segmented.mod)
#my.model <- data.frame(Temperature = tara$MAT, topt = my.fitted)



tara_topt_seg<-ggplot(tara,aes(MAT,Topt))+
  geom_point()+
  theme_classic()+
  xlab("Mean Average Temperature °C")+
  ylab("Optimal Temperature °C ")+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 3, raw=TRUE),colour="black")

#tara_topt_seg=tara_topt_seg+geom_line(data = my.model, aes(x = Temperature, y = topt), lwd=1,colour = "black")+
#  geom_vline(xintercept = 27.7, colour="red", linetype = "longdash")

model2<-lm(Topt~poly(MAT, 3, raw=TRUE),data=tara)
summary(model2)
plot(model2,2)


names(tara)
tara_ph<-tara[tara$Tp!='NO',]
str(tara_ph)
tara_ph$Tp<-as.numeric(tara_ph$Tp)

model2<-lm(Tp~poly(MAT, 2, raw=TRUE),data=tara_ph)
summary(model2)
anova(model2)



#segmented.mod <- segmented(model, seg.Z = ~MAT)
#summary(segmented.mod)
#my.fitted <- fitted(segmented.mod)
#my.model <- data.frame(Temperature = tara_ph$MAT, tp = my.fitted)



tara_tp_seg<-ggplot(tara_ph,aes(MAT,Tp))+
  geom_point()+
  theme_classic()+
  xlab("Mean Average Temperature °C")+
  ylab("Phase Trasnition Temperature (Tp) ")+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="black")

#tara_tp_seg=tara_tp_seg+geom_line(data = my.model, aes(x = Temperature, y = tp), lwd=1,colour = "black")+
#geom_vline(xintercept = 21.63, colour="red", linetype = "longdash")



#nel caso serve a fare le regressioni
gridExtra::grid.arrange(iri_Td,iri_Topt,iri_Tp,tara_td_seg,tara_topt_seg,tara_tp_seg,nrow=2,ncol=3)


names(tara)
str(tara)
tara_td<-ggplot(tara,aes(Range,Td))+
  geom_point()+
  theme_classic()+
  stat_smooth(method="loess",se=FALSE, color='black')+
  xlab("Mean Average Temperature °C")+
  ylab("Denaturating Temperature °C ")+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))
tara_td

model<-lm(Td~Range,data=tara)
summary(model)
anova(model, test = "F")
plot(model,2)


segmented.mod <- segmented(model, seg.Z = ~Range)
summary(segmented.mod)

my.fitted <- fitted(segmented.mod)
my.model <- data.frame(Temperature = tara$Range, td = my.fitted)



tara_td_range<-ggplot(tara,aes(Range,Td))+
  geom_point()+
  theme_classic()+
  xlab("Thermal Variation °C")+
  ylab("Denaturating Temperature °C ")+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
        geom_line(data = my.model, aes(x = Temperature, y = td), colour = "black",lwd=1)+
        geom_vline(xintercept = 4.9, colour="red", linetype = "longdash")

tara_td_range

names(tara)
model<-lm(Topt~Range,data=tara)
summary(model)
anova(model, test = "F")
plot(model,2)


segmented.mod <- segmented(model, seg.Z = ~Range)
summary(segmented.mod)
my.fitted <- fitted(segmented.mod)
my.model <- data.frame(Temperature = tara$Range, topt = my.fitted)



tara_topt_range<-ggplot(tara,aes(Range,Topt))+
  geom_point()+
  theme_classic()+
  xlab("Thermal Variation °C")+
  ylab("Optimal Temperature °C ")+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
        geom_line(data = my.model, aes(x = Temperature, y = topt), lwd=1,colour = "black")+
        geom_vline(xintercept = 4.9, colour="red", linetype = "longdash")


tara_topt_range


names(tara)
tara_ph<-tara[tara$Tp!='NO',]
str(tara_ph)
tara_ph$Tp<-as.numeric(tara_ph$Tp)
model<-lm(Tp~Range,data=tara_ph)
summary(model)
anova(model, test = "F")
plot(model,2)


segmented.mod <- segmented(model, seg.Z = ~Range)
summary(segmented.mod)
my.fitted <- fitted(segmented.mod)
my.model <- data.frame(Temperature = tara_ph$MAT, tp = my.fitted)



tara_tp_range<-ggplot(tara_ph,aes(Range,Tp))+
  geom_point()+
  theme_classic()+
  xlab("Thermal Variation °C")+
  ylab("Phase Trasnition Temperature (Tp) ")+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
  geom_smooth(method="lm",se=FALSE,color="black")
  tara_tp_range


#nel caso serve a fare le regressioni
gridExtra::grid.arrange(iri_Td,iri_Topt,iri_Tp,tara_td_range,tara_topt_range,tara_tp_range,nrow=2,ncol=3)


