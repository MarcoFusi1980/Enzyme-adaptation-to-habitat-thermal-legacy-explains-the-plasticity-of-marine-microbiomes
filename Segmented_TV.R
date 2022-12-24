library(ggplot2)
library(mgcv)
library(plotly)
library(lme4)
library(segmented)

irish<-read.csv("IrishSea-MedSea-RedSea1.csv",header=T)
tara<-read.csv("TaraOceanExpedition.csv",header=T)
names(irish)

model<-lm(Td~I(1/Range),data=irish)
summary(model)
anova(model, test = "F")
plot(model,2)


iri_Td<-ggplot(irish,aes(Range,Td,color=MAT))+
  geom_point()+
  theme_classic()+ 
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ I(1/x),colour="black")+
  xlab("Thermal Variation °C")+
  ylab("Denaturating Temperature °C ")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")

iri_Td


iri_Topt<-ggplot(irish,aes(Range,Topt,color=MAT))+
  geom_point()+
  theme_classic()+ 
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ I(1/x),colour="black")+
  xlab("Thermal Variation °C")+
  ylab("Optimal Temperature °C ")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")

iri_Topt
model<-lm(Topt~I(1/Range),data=irish)
summary(model)
anova(model, test = "F")
plot(model,2)


iri_Tp<-ggplot(irish,aes(Range,as.numeric(Tp),color=MAT))+
  geom_point()+
  theme_classic()+ 
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ I(1/x),colour="black")+
  xlab("Thermal Variation °C")+
  ylab("Phase Trasnition Temperature (Tp) ")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")

iri_Tp

model<-lm(Tp~I(1/Range),data=irish[irish$Tp!='NO',])
summary(model)
anova(model, test = "F")
plot(model,2)



names(tara)
str(tara)

model<-lm(Td~Range,data=tara)
summary(model)
anova(model, test = "F")
plot(model,2)


segmented.mod <- segmented(model, seg.Z = ~Range)
summary(segmented.mod)

my.fitted <- fitted(segmented.mod)
my.model <- data.frame(Temperature = tara$Range, td = my.fitted)



tara_td_seg<-ggplot(tara,aes(Range,Td,color=MAT))+
  geom_point()+
  theme_classic()+
  xlab("Thermal Variation °C")+
  ylab("Denaturating Temperature °C ")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12), 
        legend.position="none")+
  #geom_line(data = my.model, aes(x = Temperature, y = td), colour = "black",lwd=1)+
  #geom_vline(xintercept = 5, colour="red", linetype = "longdash")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")

tara_td_seg


names(tara)
model<-lm(Topt~Range,data=tara)
summary(model)
anova(model, test = "F")
plot(model,2)


segmented.mod <- segmented(model, seg.Z = ~Range)
summary(segmented.mod)
my.fitted <- fitted(segmented.mod)
my.model <- data.frame(Temperature = tara$Range, topt = my.fitted)



tara_topt_seg<-ggplot(tara,aes(Range,Topt,color=MAT))+
  geom_point()+
  theme_classic()+
  xlab("Thermal Variation °C")+
  ylab("Optimal Temperature °C ")+
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
  #geom_line(data = my.model, aes(x = Temperature, y = topt), lwd=1,colour = "black")+
  #geom_vline(xintercept = 4.9, colour="red", linetype = "longdash")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")

tara_topt_seg



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
my.model <- data.frame(Temperature = tara_ph$Range, tp = my.fitted)



tara_tp_seg<-ggplot(tara_ph,aes(Range,Tp,color=MAT))+
  geom_point()+
  theme_classic()+
  xlab("Thermal Variation °C")+
  ylab("Phase Trasnition Temperature (Tp) ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12))+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")
tara_tp_seg




#nel caso serve a fare le regressioni
gridExtra::grid.arrange(iri_Td,iri_Topt,iri_Tp,tara_td_seg,tara_topt_seg,tara_tp_seg,nrow=2,ncol=3)




iri<-ggplot(irish,aes(Td,Topt,color=MAT))+
  geom_point()+
  theme_classic()+ 
  ylab("Optimal Temperature °C")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")

iri
model<-lm(Topt~Td,data=irish)
summary(model)
anova(model, test = "F")
plot(model,2)



taras<-ggplot(tara,aes(Td,Topt,color=MAT))+
  geom_point()+
  theme_classic()+
  ylab("Optimal Temperature °C")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 3, raw=TRUE),colour="black")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")
taras

model1<-lm(Topt~poly(Td,3), data=tara)
summary(model1)
plot(model1,2)
AIC(model1,model)



gridExtra::grid.arrange(iri,taras,ncol=1)

gridExtra::grid.arrange(iri_Td,iri_Topt,iri_Tp,iri,iritpmat,tara_td_seg,tara_topt_seg,tara_tp_seg,taras,tarastpmat,nrow=2,ncol=5)




iri_range<-ggplot(irish,aes(Td,Topt,color=Range))+
  geom_point()+
  theme_classic()+ 
  ylab("Optimal Temperature °C")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    high = "red")


iri_range

taras_range<-ggplot(tara,aes(Td,Topt,color=Range))+
  geom_point()+
  theme_classic()+
  ylab("Optimal Temperature °C")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 3, raw=TRUE),colour="black")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    high = "red")
taras_range

gridExtra::grid.arrange(iri_range,taras_range,ncol=2)


#Con thermal stability


iritpmat<-ggplot(irish,aes(Td,Tp,color=MAT))+
  geom_point()+
  theme_classic()+ 
  ylab("Phase Trasnition Temperature (Tp)")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")


iritpmat
model<-lm(Tp~Td,data=irish)
summary(model)
anova(model, test = "F")
plot(model,2)

names(tara)

model<-lm(as.numeric(Tp)~poly(Td, 2), data=tara[tara$Tp!='NO',])
model1<-lm(as.numeric(Tp)~Td, data=tara[tara$Tp!='NO',])
plot(model1,2)
AIC(model1,model)

tarastpmat<-ggplot(tara[tara$Tp!='NO',],aes(Td,as.numeric(Tp),color=MAT))+
  geom_point()+
  theme_classic()+
  ylab("Phase Trasnition Temperature (Tp)")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="black")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")
tarastpmat

iri
model<-lm(as.numeric(Tp)~poly(Td, 2, raw=TRUE),data=tara[tara$Tp!='NO',])
summary(model)
anova(model, test = "F")
plot(model,2)


gridExtra::grid.arrange(iritpmat,tarastpmat,ncol=2)

iritprange<-ggplot(irish,aes(Td,Tp,color=Range))+
  geom_point()+
  theme_classic()+ 
  ylab("Phase Trasnition Temperature (Tp)")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    high = "red")


iritprange

model<-lm(Topt~Td,data=irish)
summary(model)
anova(model, test = "F")
plot(model,2)

names(tara)

model<-lm(as.numeric(Tp)~poly(Td, 2), data=tara[tara$Tp!='NO',])
model1<-lm(as.numeric(Tp)~Td, data=tara[tara$Tp!='NO',])
plot(model1,2)
AIC(model1,model)

tarastprange<-ggplot(tara[tara$Tp!='NO',],aes(Td,as.numeric(Tp),color=Range))+
  geom_point()+
  theme_classic()+
  ylab("Phase Trasnition Temperature (Tp)")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE),colour="black")+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    high = "red")
tarastprange

gridExtra::grid.arrange(iritprange,tarastprange,ncol=2)


#Tutto assieme
names(tara)
tara_reduced<-tara%>%select(MAT,Td,Topt,Tp,Range)
names(irish)
irish_reduced<-irish%>%select(MAT,Td,Topt,Tp,Range)
tara_reduced$Dataset<-'tara'
irish_reduced$Dataset<-'irish'

total_reduced<-rbind(irish_reduced,tara_reduced)


total_reduced_Tp<-total_reduced[total_reduced$Tp!='NO'|is.na(total_reduced$Tp),]
totaltpmat<-ggplot(total_reduced_Tp,aes(Td,as.numeric(Tp),color=MAT))+
  geom_point()+
  theme_classic()+ 
  ylab("Phase Trasnition Temperature (Tp)")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")


totaltpmat

totaltoptmat<-ggplot(total_reduced,aes(Td,Topt,color=MAT))+
  geom_point()+
  theme_classic()+ 
  ylab("Optimal Temperature °C")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    midpoint = 15,
    high = "red")


totaltoptmat

totaltprange<-ggplot(total_reduced_Tp,aes(Td,as.numeric(Tp),color=Range))+
  geom_point()+
  theme_classic()+ 
  ylab("Phase Trasnition Temperature (Tp)")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    high = "red")


totaltprange

totaltoptrange<-ggplot(total_reduced,aes(Td,Topt,color=Range))+
  geom_point()+
  theme_classic()+ 
  ylab("Optimal Temperature °C")+
  xlab("Denaturating Temperature °C ")+
  geom_smooth(method = "lm", color="black",se=F)+
  theme(axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 11), 
        axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),
        legend.position = "none")+
  scale_colour_gradient2(
    low = "deepskyblue",
    mid = "green",
    high = "red")

totaltoptrange

gridExtra::grid.arrange(totaltpmat,totaltprange,totaltoptmat,totaltoptrange,nrow=2)
