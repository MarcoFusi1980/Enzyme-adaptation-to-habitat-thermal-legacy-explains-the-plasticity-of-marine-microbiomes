install.packages("ggpmisc")

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
library(ggplot2)
install.packages("ggpmisc", repos = "https://cran.rstudio.com")
b<-read.table("stat_Fig1.csv",header=T,sep=',')
summary(b)

unique(b$Enzyme)

a<-b[b$Enzyme=='EXDO',]


names(a)


df<-as.data.frame(cbind(a$Mean_salinity,a$Topt))
colnames(df) <- c("x", "y")
sal_topt<-ggplot(a,aes(x=Mean_salinity,y=Topt))+
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


model0<-lm(Topt ~ 1,data=a)
model1<-lm(Topt~MAT,data=a)
model1.1<-lm(Topt~MAT+Mean_salinity,data=a)
model1.2<-lm(Topt~MAT+Mean_salinity+Mean_pH,data=a)
model1.3<-lm(Topt~MAT+Mean_pH,data=a)
model1.4<-lm(Topt~Mean_salinity+Mean_pH,data=a)
model2<-lm(Topt~Mean_pH,data=a)
model3<-lm(Topt~Mean_salinity,data=a)
plot(model2,2)


AIC(model0,model1,model1.1,model1.2,model1.3,model1.4,model2,model3)

q<-summary(model0)
q1<-summary(model1)
q2<-summary(model2)
q3<-summary(model3)
q4<-summary(model1.1)
q5<-summary(model1.2)
q6<-summary(model1.3)
q7<-summary(model1.4)

as.data.frame(c(q1$r.squared,q2$r.squared,q3$r.squared,q4$r.squared,
  q5$r.squared,q6$r.squared,q7$r.squared))

gridExtra::grid.arrange(sal_topt,ph_topt)



