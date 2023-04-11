#initialize and loda the libraries
library(gplots)
library(RColorBrewer)
library(pheatmap)

#reading the csv file with the data
df<- read.csv("/Users/quincy/Documents/Research/Raahat - MESA/MESA_Raahat_V5_Model3.csv" ,header=TRUE)
which(is.na(df))

# remove certain unwanted columns
drop1 <- c(colnames(df)[0:1])
df = df[,!(names(df) %in% drop1)]
dff1 <- df[colMeans(is.na(df)) <= 0.1 & colMeans((df == 0), na.rm = T) <= 0.1]
dff1<-df

#replacing NA values with '0'
dff1[is.na(dff1)] <- 0

model_pfvol1 =lm(pfvol1 ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol1_quartile =lm(pfvol1_quartile ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_next =lm(pfvol_next ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_next_quartile =lm(pfvol_next_quartile ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_change_absolute =lm(pfvol_change_absolute ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_change_percent =lm(pfvol_change_percent ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_quartile_change =lm(pfvol_quartile_change ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)

model_pfvol1_quartile =lm(Pericardial.Fat.Q4..Exam.1. ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
summary(model_pfvol1_quartile)
confint(model_pfvol1_quartile, level=0.95)

model_pfvol1 =lm(Pericardial.Fat..Exam.1. ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
summary(model_pfvol1)
confint(model_pfvol1, level=0.95)

model_pfvol1_next =lm(Pericardial.Fat..Proximal.Exam. ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
summary(model_pfvol1_next)
confint(model_pfvol1_next, level=0.95)



model_pfvol1 =lm(Pericardial.Fat..Proximal.Exam. ~laaef5, data=dff1)
summary(model_pfvol1)
confint(model_pfvol1, level=0.95)



summary(model_pfvol1)
summary(model_pfvol1_quartile)
summary(model_pfvol_next)
summary(model_pfvol_next_quartile)
summary(model_pfvol_change_absolute)
summary(model_pfvol_change_percent)
summary(model_pfvol_quartile_change)

#creating indices
library(caret)

set.seed(100)

trainIndex <- createDataPartition(dff1$laaef5,p=0.8,list=FALSE)
train <- dff1[trainIndex,]
test <- dff1[-trainIndex,]

modelALL =lm(laaef5 ~frci081c+ascvd1c+chdriskcac1c+age1c+race1c_category+gender1_category+
            site1c_category+bmi1c+bmic_next+bmic_change_absolute+bmic_change_percent+
            cig1c_category+sbp1c+dbp1c+glucos1c+htnmed1c_category+ldl1+ldl_next+
            ldl_change_absolute+ldl_change_percent+hdl1+hdl_next+hdl_change_absolute+
            hdl_change_percent+chol1+chol_next+chol_change_absolute+chol_change_percent+
            trig1+trig_next+trig_change_absolute+trig_change_percent+ohga1c_category+
            lipid1c_category+mtsy031c_category+highbp1_category+diabet1_category+
            aspnow1_category+educ1_category+income1_category+curalc1_category+ace1c_category+
            aced1c_category+beta1c_category+betad1c_category+insln1c_category+sttn1c_category+
            waistcm1+waistcm_next+waistcm_change_absolute+waistcm_change_percent+ualbcre1+
            fhha1c_category+hcytot1+il61+crp1+cepgfr1c+agatpm1c+agatpm1c_category+pfvol1+
            pfvol1_quartile+pfvol_next+pfvol_next_quartile+pfvol_change_absolute+
            pfvol_change_percent+pfvol_quartile_change, data=train)

modelUn = lm(laaef5 ~Pericardial.Fat..Exam.1., data=dff1)
model1 =lm(laaef5 ~Age+Race+Gender+MESA.Site+Pericardial.Fat..Exam.1., data=dff1)
model2 =lm(laaef5 ~Age+Race+Gender+MESA.Site+BMI..Proximal.Exam.+Smoking.Status+High.Blood.Pressure+Diabetes+Pericardial.Fat..Exam.1., data=dff1)
model3 =lm(laaef5 ~Age+Race+Gender+MESA.Site+BMI..Proximal.Exam.+Smoking.Status+High.Blood.Pressure+Diabetes+Fasting.Glucose..Exam.1.+LDL..Exam.1.+HDL..Exam.1.+Cholesterol..Proximal.Exam.+Triglycerides..Exam.1.+CKD.EPI.EGFR..Exam.1.+CAC.Score..Exam.1.+Pericardial.Fat..Exam.1., data=dff1)
model3_stand <- lm(scale(laaef5) ~scale(Age)+
                           scale(Race.Black.African.American)+scale(Race.Chinese.American)+
                           scale(Race.Hispanic)+scale(Gender)+scale(MESA.Site.JHU)+
                           scale(MESA.Site.NWU)+scale(MESA.Site.UCLA)+scale(MESA.Site.UMN)+
                           scale(MESA.Site.COL)+scale(BMI..Proximal.Exam.)+scale(Never.Smoker)+
                           scale(Former.Smoker)+scale(Current.Smoker)+scale(High.Blood.Pressure)+
                           scale(Diabetes)+scale(Fasting.Glucose..Exam.1.)+scale(LDL..Exam.1.)+
                           scale(HDL..Exam.1.)+scale(Cholesterol..Proximal.Exam.)+
                           scale(Triglycerides..Exam.1.)+scale(CKD.EPI.EGFR..Exam.1.)+
                           scale(CAC.Score..Exam.1.)+scale(Pericardial.Fat.Q1..Exam.1.)+
                           scale(Pericardial.Fat.Q2..Exam.1.)+scale(Pericardial.Fat.Q3..Exam.1.)+
                           scale(Pericardial.Fat.Q4..Exam.1.), data = dff1)


summary(modelUn)
summary(model1)
summary(model2)
summary(model3)
summary(model3_stand)
options(scipen=999)

confint(modelUn, level=0.95)
confint(model1, level=0.95)
confint(model2, level=0.95)
confint(model3, level=0.95)

library(lm.beta) # Load lm.beta package
model3B <- lm.beta(model3)

library(tidyverse)
library(stargazer)
library(patchwork)

#process <- preProcess(as.data.frame(dff1), method=c("range"))
#norm_scale <- predict(process, as.data.frame(dff1))
#write.csv(norm_scale, file="/Users/quincy/Documents/Research/Raahat - MESA/MESA_Raahat_V5_Model3_Norm.csv")

brewer.pal(n = 11, name = "PuOr")

p1 <- dff1 %>% 
        ggplot(aes(x = Pericardial.Fat..Exam.1., y = lavmax5, colour = "#2D004B")) +
        ylim(0, 1) +
        geom_smooth(method = "lm", se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lavprea5, colour = "#542788"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lavmin5, colour = "#7F3B08"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lasmax5, colour = "#8073AC"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lasrmax5, colour = "#B2ABD2"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lasre5, colour = "#B35806"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lasra5, colour = "#D8DAEB"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lavolcircle5, colour = "#E08214"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = latef5, colour = "#F7F7F7"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = lapef5, colour = "#FDB863"), se = TRUE) +
        geom_smooth(method = "lm", aes(x = Pericardial.Fat..Exam.1., y = laaef5, colour = "#FEE0B6"), se = TRUE) +
        labs(title="Pericardial Fat and Left Atrial Strain", x ="Pericardial Fat Volume (cm3) - Exam 1", y = "Left Atrial Strain - Normalized", colour='Left Atrial Parameters') +
        theme_bw() +
        theme(plot.title = element_text(face = "bold",hjust = 0.5)) +
        theme(axis.title.x = element_text(face = "bold",hjust = 0.5)) +
        theme(axis.title.y = element_text(face = "bold",hjust = 0.5)) +
        theme(legend.position="right")

p1

library(Metrics)
prediction <- predict(model1_pfvol1, newdata = test)
mae(test$laaef5, prediction)
mse(test$laaef5, prediction)
rmse(test$laaef5, prediction)

layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(model_pfvol1$fitted.values, rstudent(model_pfvol1),
     main="Multi Fit Studentized Residuals",
     xlab="Predictions",ylab="Studentized Resid",
     ylim=c(-2.5,2.5))
abline(h=0, lty=2)
plot(dff1$site1c_category, model1_pfvol1$residuals,
     main="Residuals by PVOL",
     xlab="PVOL",ylab="Residuals")
abline(h=0,lty=2)
hist(model1_pfvol1$residuals,main="Histogram of Residuals")
qqnorm(model1_pfvol1$residuals)
qqline(model1_pfvol1$residuals)

library(fBasics)
#Null Hypothesis: Skewness and Kurtosis are equal to zero
jarqueberaTest(model_pfvol1_quartile$residuals)

library(lmtest)
#Null Hypothesis: Errors are serially UNcorrelated
dwtest(model_pfvol1)
