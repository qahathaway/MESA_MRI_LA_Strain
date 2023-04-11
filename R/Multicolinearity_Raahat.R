#initialize and loda the libraries
library(gplots)
library(RColorBrewer)
library(pheatmap)


#reading the csv file with the data
df<- read.csv("/Users/quincy/Documents/Research/Raahat - MESA/MESA_Raahat_V5_Model1.csv" ,header=TRUE)
which(is.na(df))


# remove certain unwanted columns
drop1 <- c(colnames(df)[0:1])
df = df[,!(names(df) %in% drop1)]
df$bmic_change_percent <- as.numeric(df$bmic_change_percent)
df$ldl_change_percent <- as.numeric(df$ldl_change_percent)
df$hdl_change_percent <- as.numeric(df$hdl_change_percent)
df$chol_change_percent <- as.numeric(df$chol_change_percent)
df$trig_change_percent <- as.numeric(df$trig_change_percent)
df$waistcm_change_percent <- as.numeric(df$waistcm_change_percent)

dff1 <- df[colMeans(is.na(df)) <= 0.1 & colMeans((df == 0), na.rm = T) <= 0.1]
dff1<-df

#replacing NA values with '0'
dff1[is.na(dff1)] <- 0

# correlation analysis
library(ggplot2)
library(reshape2)
library(corrplot)
library(Hmisc)

matriz_cor <-cor(data.matrix(dff1[,1:ncol(dff1)]))
write.csv(matriz_cor, file="/Users/quincy/Documents/Research/Raahat - MESA/MESA_Raahat_Correlation2.csv")

for (i in 1:nrow(matriz_cor)){
  correlations <-  which((abs(matriz_cor[i,]) > 0.5) & (matriz_cor[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(dff1)[i+1])
    print(correlations)
  }
}

corrplot(matriz_cor, method="circle", type = "lower", tl.cex = .8, tl.col = "black", order="hclust")
cor(dff1$lavmax5, dff1$pfvol1)
cor.test(dff1$lavmax5, dff1$pfvol1)

library(psych)
LAcorr <- corr.test(dff1[77], dff1[1:66])
write.csv(LAcorr$ci2, file="/Users/quincy/Documents/Research/Raahat - MESA/MESA_Raahat_Correlation_laaef5.csv")

library(performance)
#reading the csv file with the data
dff1<- read.csv("/Users/quincy/Documents/Research/HVI/MESA/FINAL/MESA_V18-Multicolinearity-M.csv" ,header=TRUE)
dff1[is.na(dff1)] <- 0

# fit model
model <- lm(event ~Age+Race+Gender+Diabetes+Hypertension+Hyperlipidemia+
              Statin+Smoking+Metabolic_Syndrome+Mean_SBP+Mean_DBP+LDL+HDL+Chol+
              Alb_Creat_Ratio+Pack_Years+BMI+Walking_Min_Wk+Education+Income+FH_Heart+
              Homocysteine+IL6+Plasmin_Antiplasmin+Fibrinogen_Antigen+
              C_Reactive_Protein+D_Dimer+Factor_VIII+Left_Ventricular_Area+
              LV_EF+Pericardial_Fat+Coronary_Calcium, data = dff1)


# now check for multicollinearity
check_collinearity(model)

x <- check_collinearity(model)
plot(x)

