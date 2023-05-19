##Final surface area analysis
# Set your working directory to where your excel file is saved:
rm(list=ls())
setwd("C:/Users/annes/Documents/R_Statistique/Statistique_Rasha")

# load required packages
library(openxlsx)
library(lme4)
library(emmeans)
library(multcomp)
library(car)
library(MASS)
library(nlme)

# Import the data
df <- within(read.xlsx(xlsxFile="surface_area_original.xlsx"),{
  Infection_level = as.factor(Infection_Level)
  AnalysedRegionArea = as.numeric(AnalysedRegionArea)
  Host=as.factor(Host)
  Treatment=as.factor(Treatment)
})
str(df)
df$HostTreatment<-paste0(df$Host,df$Treatment)

# Fit a linear model
model<-lme(AnalysedRegionArea ~ Infection_level,random=~1|HostTreatment,weights=varIdent(form=~1|Infection_level),data=df)

summary(model)

joint_tests(model)

# Infection_level is significant difference

# Normality
r <- residuals(model)

require(mgcv)

ScaledResid <-function(object){ V = extract.lme.cov(object, object$data);
C <- chol(as.matrix(V));
rm <- residuals(object, level = 0);
ScaledRes <- solve(t(C)) %*% rm;
return(ScaledRes);}


r <- ScaledResid(model)
hist(r,freq=F)
xfit<-seq(min(r),max(r),length=40)
yfit<-dnorm(xfit, mean=mean(r), sd=sd(r))
lines(xfit, yfit,col="red",lwd=2) 
shapiro.test(r) 
e1071::kurtosis(r)  

# Equality of variances
plot(fitted(model, level=0), r, pch=16, ylab="Normalized residuals", xlab="Predicted values")
abline(h=0, lty=2)
cor.test(fitted(model3, level=0),r,method="spearman",exact=FALSE)
leveneTest(as.vector(r),group=df$Infection_level)

# Tukey's multiple comparisons
a1 = emmeans(model, ~ Infection_level, type="response"); a1
cld(a1, Letters=letters)

