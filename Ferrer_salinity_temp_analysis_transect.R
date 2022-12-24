install.packages("ggpmisc")
library(ggplot2)
install.packages("ggpmisc", repos = "https://cran.rstudio.com")
a<-read.table("Transect.csv",header=T,sep=',')
summary(a)

unique(a$Enzyme)
# # # # # # # Esterasi
a<-a[a$Enzyme!="EXDO",]

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
names(a)
df<-as.data.frame(cbind(a$Mean_Salinity,a$Td))
colnames(df) <- c("x", "y")
sal_td<-ggplot(a,aes(x=Mean_Salinity,y=Td))+
  geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denturation Temperature (°C)")+
  ylim(0,100)+
  xlab("Salinity mg/L")+
  geom_text(x = 35, y = 90, label = lm_eqn(df), parse = TRUE)
sal_td

df<-as.data.frame(cbind(a$Mean_pH,a$Td))
colnames(df) <- c("x", "y")
ph_td<-ggplot(a,aes(x=Mean_pH,y=Td))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denaturation Temperature (°C)")+
  xlab("pH") +
  ylim(0,100)+
  geom_text(x = 7.1, y = 90, label = lm_eqn(df), parse = TRUE)

ph_td


df<-as.data.frame(cbind(a$Mean_Salinity,a$Topt))
colnames(df) <- c("x", "y")
sal_topt<-ggplot(a,aes(x=Mean_Salinity,y=Topt))+
  geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Optimal Activity Temperature (°C)")+
  ylim(0,100)+
  xlab("Salinity mg/L")+
  geom_text(x = 35, y = 90, label = lm_eqn(df), parse = TRUE)
sal_topt

df<-as.data.frame(cbind(a$Mean_pH,a$Topt))
colnames(df) <- c("x", "y")
ph_topt<-ggplot(a,aes(x=Mean_pH,y=Topt))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Optimal Activity Temperature (°C)")+
  xlab("pH") +
  ylim(0,100)+
  geom_text(x = 7.1, y = 90, label = lm_eqn(df), parse = TRUE)

ph_topt

df<-as.data.frame(cbind(a$Mean_Salinity,a$Tp))
colnames(df) <- c("x", "y")
sal_tp<-ggplot(a,aes(x=Mean_Salinity,y=Tp))+
  geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Phase Transition Temperature (°C)")+
  xlab("Salinity mg/L")+
  geom_text(x = 35, y = 370, label = lm_eqn(df), parse = TRUE)
sal_tp

df<-as.data.frame(cbind(a$Mean_pH,a$Tp))
colnames(df) <- c("x", "y")
ph_tp<-ggplot(a,aes(x=Mean_pH,y=Tp))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Phase Transition Temperature (°C)")+
  xlab("pH") +
  geom_text(x = 7.1, y = 370, label = lm_eqn(df), parse = TRUE)

ph_tp





model0<-lm(Tp ~ 1,data=a)
model1<-lm(Tp~MAT,data=a)
model1.1<-lm(Tp~MAT+Mean_Salinity,data=a)
model1.2<-lm(Tp~MAT+Mean_Salinity+Mean_pH,data=a)
model1.3<-lm(Tp~MAT+Mean_pH,data=a)
model1.4<-lm(Tp~Mean_Salinity+Mean_pH,data=a)
model2<-lm(Tp~Mean_pH,data=a)
model3<-lm(Tp~Mean_Salinity,data=a)
plot(model2,2)


AIC(model0,model1,model1.1,model1.2,model1.3,model1.4,model2,model3)

anova(model0)
anova(model1)
anova(model1.1)
anova(model1.2)
anova(model1.3)
anova(model1.4)
anova(model2)
anova(model3)
summary(model0)
summary(model1)
summary(model2)
summary(model3)
summary(model1.1)
summary(model1.2)
summary(model1.3)
summary(model1.4)


gridExtra::grid.arrange(sal_td,ph_td,sal_topt,ph_topt,sal_tp,ph_tp)





# # # # # # # Esterasi
a<-a[a$Enzyme!="EXDO",]

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
names(a)
df<-as.data.frame(cbind(a$Mean_Salinity,a$Td))
colnames(df) <- c("x", "y")
sal_td<-ggplot(a,aes(x=Mean_Salinity,y=Td))+
  geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denturation Temperature (°C)")+
  ylim(0,100)+
  xlab("Salinity mg/L")+
  geom_text(x = 35, y = 90, label = lm_eqn(df), parse = TRUE)
sal_td

df<-as.data.frame(cbind(a$Mean_pH,a$Td))
colnames(df) <- c("x", "y")
ph_td<-ggplot(a,aes(x=Mean_pH,y=Td))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denaturation Temperature (°C)")+
  xlab("pH") +
  ylim(0,100)+
  geom_text(x = 7.1, y = 90, label = lm_eqn(df), parse = TRUE)

ph_td


df<-as.data.frame(cbind(a$Mean_Salinity,a$Topt))
colnames(df) <- c("x", "y")
sal_topt<-ggplot(a,aes(x=Mean_Salinity,y=Topt))+
  geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Optimal Activity Temperature (°C)")+
  ylim(0,100)+
  xlab("Salinity mg/L")+
  geom_text(x = 35, y = 90, label = lm_eqn(df), parse = TRUE)
sal_topt

df<-as.data.frame(cbind(a$Mean_pH,a$Topt))
colnames(df) <- c("x", "y")
ph_topt<-ggplot(a,aes(x=Mean_pH,y=Topt))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Optimal Activity Temperature (°C)")+
  xlab("pH") +
  ylim(0,100)+
  geom_text(x = 7.1, y = 90, label = lm_eqn(df), parse = TRUE)

ph_topt

df<-as.data.frame(cbind(a$Mean_Salinity,a$Tp))
colnames(df) <- c("x", "y")
sal_tp<-ggplot(a,aes(x=Mean_Salinity,y=Tp))+
  geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Phase Transition Temperature (°C)")+
  xlab("Salinity mg/L")+
  geom_text(x = 35, y = 370, label = lm_eqn(df), parse = TRUE)
sal_tp

df<-as.data.frame(cbind(a$Mean_pH,a$Tp))
colnames(df) <- c("x", "y")
ph_tp<-ggplot(a,aes(x=Mean_pH,y=Tp))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Phase Transition Temperature (°C)")+
  xlab("pH") +
  geom_text(x = 7.1, y = 370, label = lm_eqn(df), parse = TRUE)

ph_tp





model0<-lm(Tp ~ 1,data=a)
model1<-lm(Tp~MAT,data=a)
model1.1<-lm(Tp~MAT+Mean_Salinity,data=a)
model1.2<-lm(Tp~MAT+Mean_Salinity+Mean_pH,data=a)
model1.3<-lm(Tp~MAT+Mean_pH,data=a)
model1.4<-lm(Tp~Mean_Salinity+Mean_pH,data=a)
model2<-lm(Tp~Mean_pH,data=a)
model3<-lm(Tp~Mean_Salinity,data=a)
plot(model2,2)


AIC(model0,model1,model1.1,model1.2,model1.3,model1.4,model2,model3)

anova(model0)
anova(model1)
anova(model1.1)
anova(model1.2)
anova(model1.3)
anova(model1.4)
anova(model2)
anova(model3)
summary(model0)
summary(model1)
summary(model2)
summary(model3)
summary(model1.1)
summary(model1.2)
summary(model1.3)
summary(model1.4)


gridExtra::grid.arrange(sal_td,ph_td,sal_topt,ph_topt,sal_tp,ph_tp)






ggplot(a,aes(x=MAT,y=Mean_Salinity..g.L.1.))+geom_point()+stat_smooth(method="lm")









b<-read.table("~/Dropbox/Articoli/2020_Marasco et al_Termal regime marine sediment/000_Manoscritto/202009_New_Version/Ferrer_data/Heatmap_Salinity.txt",header=T,sep='\t')
summary(b)



df<-as.data.frame(cbind(b$MAT,b$Optima))
colnames(df) <- c("x", "y")

temp<-ggplot(b,aes(x=MAT,y=Optima))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Optimal Activity Salinity (M of NaCl)")+
  xlab("Temperature °C") +
  ylim(-0.5,3)+
  geom_text(x = 15, y = 2.9, label = lm_eqn(df), parse = TRUE)
temp

df<-as.data.frame(cbind(b$Mean_pH,b$Optima))
colnames(df) <- c("x", "y")
ph<-ggplot(b,aes(x=Mean_pH,y=Optima))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylim(-0.5,3)+
  ylab("Optimal Activity Salinity (M of NaCl)")+
  xlab("pH")+
  geom_text(x = 7.2, y = 2.9, label = lm_eqn(df), parse = TRUE)
ph
df<-as.data.frame(cbind(b$Mean_Salinity..g.L.1.,b$Optima))
colnames(df) <- c("x", "y")
sal<-ggplot(b,aes(x=Mean_Salinity..g.L.1.,y=Optima))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylim(-0.5,3)+
  ylab("Optimal Activity Salinity (M of NaCl)")+
  xlab("Salinity (ppt)")+
  geom_text(x = 34, y = 2.9, label = lm_eqn(df), parse = TRUE)
sal


model0<-lm(Optima ~ 1,data=b)
model1<-lm(Optima~MAT,data=b)
model1.1<-lm(Optima~MAT+Mean_Salinity..g.L.1.,data=b)
model1.4<-lm(Optima~MAT+Mean_Salinity..g.L.1.+Mean_pH,data=b)
model1.2<-lm(Optima~MAT+Mean_pH,data=b)
model1.3<-lm(Optima~Mean_Salinity..g.L.1.+Mean_pH,data=b)


AIC(model0,model1,model1.1,model1.2,model1.3,model1.4,model2,model3)

model2<-lm(Optima~Mean_pH,data=b)
model3<-lm(Optima~Mean_Salinity..g.L.1.,data=b)
plot(model2,2)


anova(model0)
anova(model1)
anova(model1.1)
anova(model1.2)
anova(model1.3)
anova(model1.4)
anova(model2)
anova(model3)
summary(model0)
summary(model1)
summary(model2)
summary(model3)
summary(model1.1)
summary(model1.2)
summary(model1.3)
summary(model1.4)

c<-read.table("~/Dropbox/Articoli/2020_Marasco et al_Termal regime marine sediment/000_Manoscritto/202009_New_Version/Ferrer_data/Protein_denaturating.txt",header=T,sep='\t')
summary(c)

df<-as.data.frame(cbind(c$MAT,c$Topt))
colnames(df) <- c("x", "y")
temp<-ggplot(c,aes(x=MAT,y=Denaturing_Temperature..deg.C..))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denaturating Temperature (°C)")+
  xlab("Temperature °C")+
  geom_text(x = 20, y = 80, label = lm_eqn(df), parse = TRUE)
temp





df<-as.data.frame(cbind(c$Mean_pH,c$Denaturing_Temperature..deg.C..))
colnames(df) <- c("x", "y")
ph<-ggplot(c,aes(x=Mean_pH,y=Denaturing_Temperature..deg.C..))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denaturating Temperature (°C)")+
  xlab("pH")+
  geom_text(x = 7.2, y = 80, label = lm_eqn(df), parse = TRUE)
ph



df<-as.data.frame(cbind(c$Mean_Salinity..g.L.1.,c$Denaturing_Temperature..deg.C..))
colnames(df) <- c("x", "y")
sal<-ggplot(c,aes(x=Mean_Salinity..g.L.1.,y=Denaturing_Temperature..deg.C..))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denaturating Temperature (°C)")+
  xlab("Salinity (ppt)")+
  geom_text(x = 34, y = 80, label = lm_eqn(df), parse = TRUE)
sal


model0<-lm(Denaturing_Temperature..deg.C.. ~ 1,data=c)
model1<-lm(Denaturing_Temperature..deg.C..~MAT,data=c)
model1.1<-lm(Denaturing_Temperature..deg.C..~MAT+Mean_Salinity..g.L.1.,data=c)
model1.4<-lm(Denaturing_Temperature..deg.C..~MAT+Mean_Salinity..g.L.1.+Mean_pH,data=c)
model1.2<-lm(Denaturing_Temperature..deg.C..~MAT+Mean_pH,data=c)
model1.3<-lm(Denaturing_Temperature..deg.C..~Mean_Salinity..g.L.1.+Mean_pH,data=c)
model2<-lm(Denaturing_Temperature..deg.C..~Mean_pH,data=c)
model3<-lm(Denaturing_Temperature..deg.C..~Mean_Salinity..g.L.1.,data=c)

AIC(model0,model1,model1.1,model1.2,model1.3,model1.4,model2,model3)

anova(model0)
anova(model1)
anova(model1.1)
anova(model1.2)
anova(model1.3)
anova(model1.4)
anova(model2)
anova(model3)
summary(model0)
summary(model1)
summary(model2)
summary(model3)
summary(model1.1)
summary(model1.2)
summary(model1.3)
summary(model1.4)

d<-read.table("~/Dropbox/Articoli/2020_Marasco et al_Termal regime marine sediment/000_Manoscritto/202004_version/Ferrer_data/Tara_analysis.txt",header=T,sep='\t')
summary(d)

df<-as.data.frame(cbind(d$Mean_Temperature,d$Denaturing_Temperature))
colnames(df) <- c("x", "y")
temp<-ggplot(d,aes(x=Mean_Temperature,y=Denaturing_Temperature))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ylab("Denaturating Temperature (°C)")+
  xlab("Temperature (°C)")+
  geom_text(x = 5, y = 60, label = lm_eqn(df), parse = TRUE)
temp
df<-as.data.frame(cbind(d$pH,d$Denaturing_Temperature))
colnames(df) <- c("x", "y")
ph<-ggplot(d,aes(x=pH,y=Denaturing_Temperature))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ggtitle("Tara ocean cloned enzymes")+
  ylab("Denaturating Temperature (°C)")+
  xlab("pH")+
  geom_text(x = 7.6, y = 60, label = lm_eqn(df), parse = TRUE)
ph
df<-as.data.frame(cbind(d$Salinity,d$Denaturing_Temperature))
colnames(df) <- c("x", "y")
sal<-ggplot(d,aes(x=Salinity,y=Denaturing_Temperature))+geom_point()+stat_smooth(method="lm",color="black")+
  theme_classic()+
  ggtitle("Tara ocean cloned enzymes")+
  ylab("Denaturating Temperature (°C)")+
  xlab("Salinity (ppt)")+
  geom_text(x = 35, y = 60, label = lm_eqn(df), parse = TRUE)
sal

model0<-lm(Denaturing_Temperature ~ 1,data=d)
model1<-lm(Denaturing_Temperature~Mean_Temperature,data=d)
model1.1<-lm(Denaturing_Temperature~Mean_Temperature+Salinity,data=d)
model1.4<-lm(Denaturing_Temperature~Mean_Temperature+Salinity+pH,data=d)
model1.2<-lm(Denaturing_Temperature~Mean_Temperature+pH,data=d)
model1.3<-lm(Denaturing_Temperature~Salinity+pH,data=d)
model2<-lm(Denaturing_Temperature~pH,data=d)
model3<-lm(Denaturing_Temperature~Salinity,data=d)

AIC(model0,model1,model1.1,model1.2,model1.3,model1.4,model2,model3)

anova(model0)
anova(model1)
anova(model1.1)
anova(model1.2)
anova(model1.3)
anova(model1.4)
anova(model2)
anova(model3)
summary(model0)
summary(model1)
summary(model2)
summary(model3)
summary(model1.1)
summary(model1.2)
summary(model1.3)
summary(model1.4)

