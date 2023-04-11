# Initialize and load the libraries
library(gplots)
library(RColorBrewer)
library(pheatmap)
library(matrixStats)

# Reading the CSV file with the data
df<- read.csv("path/to/file.csv" ,header=TRUE)
#df <-  df[!(is.na(df$lavmax5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lavprea5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lavmin5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lasmax5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lasrmax5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lasre5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lasra5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lavolcircle5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$latef5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$lapef5) | df$lavmax5==""), ]
#df <-  df[!(is.na(df$laaef5) | df$lavmax5==""), ]
which(is.na(df))

# Remove unwanted columns and fill with Median values
drop1 <- c(colnames(df)[0:1])
df = df[,!(names(df) %in% drop1)]
dff1 <- df[colMedians(is.na(df)) <= 0.1 & colMedians((df == 0), na.rm = T) <= 0.1]
dff1<-df

# Replacing NA values with '0'
dff1[is.na(dff1)] <- 0

# Multiple Linear Regression for Variable
model_pfvol1 =lm(pfvol1 ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol1_quartile =lm(pfvol1_quartile ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_next =lm(pfvol_next ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_next_quartile =lm(pfvol_next_quartile ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_change_absolute =lm(pfvol_change_absolute ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_change_percent =lm(pfvol_change_percent ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)
model_pfvol_quartile_change =lm(pfvol_quartile_change ~lavmax5+lavprea5+lavmin5+lasmax5+lasrmax5+lasre5+lasra5+lavolcircle5+latef5+lapef5+laaef5, data=dff1)

# Adjusted R2 and other metrics
summary(model_pfvol1)
summary(model_pfvol1_quartile)
summary(model_pfvol_next)
summary(model_pfvol_next_quartile)
summary(model_pfvol_change_absolute)
summary(model_pfvol_change_percent)
summary(model_pfvol_quartile_change)

# Linear Regression
modelUn = lm(laaef5 ~pfvol1, data=dff1)
model1 =lm(laaef5 ~Age+Race+Gender+MESA.Site+pfvol1, data=dff1)
model2 =lm(laaef5 ~Age+Race+Gender+MESA.Site+BMI..Proximal.Exam.+Smoking.Status+High.Blood.Pressure+Diabetes+pfvol1, data=dff1)
model3 =lm(laaef5 ~Age+Race+Gender+MESA.Site+BMI..Proximal.Exam.+Smoking.Status+High.Blood.Pressure+Diabetes+Fasting.Glucose..Exam.1.+LDL..Exam.1.+HDL..Exam.1.+Cholesterol..Proximal.Exam.+Triglycerides..Exam.1.+CKD.EPI.EGFR..Exam.1.+CAC.Score..Exam.1.+pfvol1, data=dff1)

# Adjusted R2 and other metrics
summary(modelUn)
summary(model1)
summary(model2)
summary(model3)

# 95% CI of Regression Coefficient
confint(modelUn, level=0.95)
confint(model1, level=0.95)
confint(model2, level=0.95)
confint(model3, level=0.95)

# Graph Linear Regressions
library(patchwork)

# Normalize values of left atrial strain
process <- preProcess(as.data.frame(dff1), method=c("range"))
norm_scale <- predict(process, as.data.frame(dff1))
write.csv(norm_scale, file="path/to/file.csv")

# Graph
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

# Calculate MAE, MSE, and, RSME
library(Metrics)
prediction <- predict(model_pfvol1, newdata = test)
mae(test$laaef5, prediction)
mse(test$laaef5, prediction)
rmse(test$laaef5, prediction)

library(fBasics)
#Null Hypothesis: Skewness and Kurtosis are equal to zero
jarqueberaTest(model_pfvol1$residuals)

library(lmtest)
#Null Hypothesis: Errors are serially UNcorrelated
dwtest(model_pfvol1)
