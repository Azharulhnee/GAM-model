install.packages("here")
install.packages( "readr")
install.packages("janitor")
install.packages("mgcv")
install.packages("gratia")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("countreg", dependencies = TRUE, repos="http://R-Forge.R-project.org")

library(here)
library(readr)
library(janitor)
library(mgcv)
library(gratia)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(countreg)

GAMThdata <- read.csv(choose.files("D:/Thesis data/One model for all data/Three_stations_of_all_city"))
GAMThdata
plot(GAMThdata)

#data distribution of response varible
PMten<-(GAMThdata$PMten_Deposition)
hist(GAMThdata$PMten)
hist(GAMThdata$PMten.concentration)
hist(GAMThdata$Deposition.rate)


#finding the outlayer
boxplot.stats(GAMThdata$Deposition.rate)$out



# Generating a normal box plot
boxplot(GAMThdata$Deposition.rate,
        frame.plot = F,
        ylab = "PM10 deposition rate / flux  (??g/m3)",
        col = "Grey",
        #notch = TRUE,
        boxwex =0.5,
        main = "Boxplot of PM10 deposition flux distribution")



# writting any kind of text
text(1, "outlayer", col = "black", adj = 3)

# writting P value
text(50, "Outlayer", col = "black", adj = -.4)


mtext(paste("Outliers: ", paste(out, collapse = ", ")))
iqr <- IQR(GAMThdata$Deposition.rate)
up <-  quantile(GAMThdata$Deposition.rate, 0.75) + 1.5*iqr # Upper Range  
low<- quantile(GAMThdata$Deposition.rate, 0.25) - 1.5*iqr # Lower Range???


eliminated <- subset(GAMThdata, GAMThdata$Deposition.rate > low & GAMThdata$Deposition.rate < up)
hist(eliminated$Deposition.rate)

boxplot(eliminated$Deposition.rate,
        frame.plot = F,
        ylab = "PM10 deposition rate / flux  (??g/m3)",
        col = "Grey",
        #notch = TRUE,
        boxwex =0.5,
        main = "Boxplot of PM10 deposition rate distribution")
# writting any kind of text
text(1, "outlayer", col = "black", adj = 3)

iqr <- IQR(eliminated$Greenspace)
up <-  quantile(eliminated$Greenspace, 0.75) + 1.5*iqr # Upper Range  
low<- quantile(eliminated$Greenspace, 0.25) - 1.5*iqr # Lower Range???


eliminatedG <- subset(eliminated, eliminated$Greenspace > low & eliminated$Greenspace < up)
hist(eliminatedG$Greenspace)

iqr <- IQR(eliminatedG$Greenspace)
up <-  quantile(eliminated$Greenspace, 0.75) + 1.5*iqr # Upper Range  
low<- quantile(eliminated$Greenspace, 0.25) - 1.5*iqr # Lower Range???





#GAM model with multiple variables
GAMMlt <- gam(PMten.concentration ~ s(Greenspace, k = 25) + s(Surface_Temperature) +
                s(Precipitation) +s( Wind_Pressure) + year, 
              data = eliminatedG,family = scat(),  method = "REML")

#sink()
summary(GAMMlt)

#sink("AllthreeS_GAMoutput.doc")
#summary(GAMMlt)



#showing GAM podelin the plot
plot(GAMMlt, all.terms = TRUE, pages = 1, se = TRUE, scheme = 1, shade = TRUE,shade.col = "hot pink",)

#showing GAM model in the plot changing xais and ylab
par(mfrow=c(2, 3))

plot(GAMMlt, all.terms = TRUE, se = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(Greenspace,6.64)", ylim = c(-15, 25), cex.lab=1.2, select = 1, xlab="Greenspace (ha)")

#plot(GAMMlt, all.terms = TRUE, se = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     #ylab = "s(Built-up area,7.31)",ylim = c(-0.3, 0.4), cex.lab=1.4, select = 2, xlab="Built-up area (ha)")

plot(GAMMlt, all.terms = TRUE, se = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(LST,6.44)", ylim = c(-15, 30), cex.lab=1.2, select = 2, xlab="Land Surface temperature (°C)")

plot(GAMMlt, all.terms = TRUE, se = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(Precipitation,5.02)", ylim = c(-10, 20), cex.lab=1.2, select = 3, xlab="Precipitation (mm/day)")

plot(GAMMlt, all.terms = TRUE, se = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(Surface pressure,7.02)",ylim = c(-15, 20), cex.lab=1.2, select = 4, xlab="Surface pressure (kPa)")

plot(GAMMlt, all.terms = TRUE, se = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "", cex.lab=1.4, select = 5, xlab="")




#showing the y axis value with data points renaming the ylab
par(mfrow=c(2, 3))

plot(GAMMlt, all.terms = TRUE, se = TRUE,residuals = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(Greenspace,6.67)", ylim = c(-0.1, 0.15), cex.lab=1.4, select = 1, xlab="Greenspace (ha)")

#plot(GAMMlt, all.terms = TRUE, se = TRUE, residuals = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
 #    ylab = "s(Built-up area,7.31)",ylim = c(-0.3, 0.4), cex.lab=1.4, select = 2, xlab="Built-up area (ha)")

plot(GAMMlt, all.terms = TRUE, se = TRUE,residuals = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(Land Surface temperature,6.44)", ylim = c(-0.1, 0.2), cex.lab=1.4, select = 2, xlab="Land Surface temperature (°C)")

plot(GAMMlt, all.terms = TRUE, se = TRUE,residuals = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(Precipitation,5.02)", ylim = c(-0.1, 0.15), cex.lab=1.4, select = 3, xlab="Precipitation (mm/day)")

plot(GAMMlt, all.terms = TRUE, se = TRUE,residuals = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "s(Surface pressure,1.7)",ylim = c(-0.1, 0.15), cex.lab=1.4, select = 4, xlab="Surface pressure (kPa)")

plot(GAMMlt, all.terms = TRUE, se = TRUE,residuals = TRUE, scheme = 1, shade = TRUE, shade.col = "hot pink",
     ylab = "", cex.lab=1.4, select = 5, xlab="")





#showing the above plot with residual
plot(GAMMlt, all.terms = TRUE, residuals = TRUE, pages = 1, se = TRUE, scheme = 1, pch = 19, shade = TRUE,
     shade.col = "hot pink", seWithMean = TRUE, shift = coef(GAMMlt) [1])




#checking the GAM model if the choosing of basis function is correct or not
gam.check(GAMMlt, all.terms = TRUE, pages = 1)



#showing different residuals plots together
appraise(GAMMlt, method = "simulate") # model diagnostics


#checking overall model concuvarity 
concurvity(GAMMlt, full = TRUE)

#change it to False  when it gives larger value (than 0.8)
concurvity(GAMMlt, full = FALSE)


