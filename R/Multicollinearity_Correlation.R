# Initialize and load the libraries
library(gplots)
library(RColorBrewer)
library(pheatmap)

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

# correlation analysis
library(ggplot2)
library(reshape2)
library(corrplot)
library(Hmisc)

matriz_cor <-cor(data.matrix(dff1[,1:ncol(dff1)]))
write.csv(matriz_cor, file="path/to/file.csv")

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
write.csv(LAcorr$ci2, file="path/to/file.csv")
