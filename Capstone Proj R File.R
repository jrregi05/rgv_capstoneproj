library(raster)
library(maptools)
library(sp)
library(spdep)
library(rgdal)
library(mapcan)
library(pgirmess)
library(GISTools)
library(GWmodel)
library(cartography)
library(RColorBrewer)
library(ggplot2)
library(cowplot)

rgvtracts <- shapefile("D:\\OU\\Spring 2023\\Thinking about Geography and Environmental Sustainability\\Capstone Proj\\Capstone Project Maps\\Texas 2020 Block Groups\\Study Region and Data Project.shp")

# ANOVA
emply <- aov(formula = rgvtracts$Total_Empl ~ rgvtracts$CountyName)
summary(emply)
TukeyHSD(emply)
boxplot(rgvtracts$Total_Empl ~ rgvtracts$CountyName, xlab = "County", ylab = "Employer Health Insurance", col = "red")

direct <- aov(formula = rgvtracts$Total_Dire ~ rgvtracts$CountyName)
summary(direct)
TukeyHSD(direct)
boxplot(rgvtracts$Total_Dire ~ rgvtracts$CountyName, xlab = "County", ylab = "Individual Health Insurance", col = "orange")

medicare <- aov(formula = rgvtracts$Total_Medi ~ rgvtracts$CountyName)
summary(medicare)
TukeyHSD(medicare)
boxplot(rgvtracts$Total_Medi ~ rgvtracts$CountyName, xlab = "County", ylab = "Medicare Health Insurance", col = "yellow")

medicaid <- aov(formula = rgvtracts$Total_Me_1 ~ rgvtracts$CountyName)
summary(medicaid)
TukeyHSD(medicaid)
boxplot(rgvtracts$Total_Me_1 ~ rgvtracts$CountyName, xlab = "County", ylab = "Medicaid Health Insurance", col = "purple")

tricare <- aov(formula = rgvtracts$Total_Tric ~ rgvtracts$CountyName)
summary(tricare)
TukeyHSD(tricare)
boxplot(rgvtracts$Total_Tric ~ rgvtracts$CountyName, xlab = "County", ylab = "Tricare Health Insurance", col = "green")

vahealth <- aov(formula = rgvtracts$Total_VA_h ~ rgvtracts$CountyName)
summary(vahealth)
TukeyHSD(vahealth)
boxplot(rgvtracts$Total_VA_h ~ rgvtracts$CountyName, xlab = "County", ylab = "VA Health Insurance", col = "brown")

nohealth <- aov(formula = rgvtracts$Total_No_I ~ rgvtracts$CountyName)
summary(nohealth)
TukeyHSD(nohealth)
boxplot(rgvtracts$Total_No_I ~ rgvtracts$CountyName, xlab = "County", ylab = "No Health Insurance", col = "blue")



# lagged means
coords <- coordinates(rgvtracts)
nb.queen <- poly2nb(rgvtracts)
plot(rgvtracts)
plot(nb.queen, coords, add=T, pch=16, col="red")
lw.queen <- nb2listw(nb.queen)

T_WHITE.lagged.means <- lag.listw(lw.queen, rgvtracts$White)
plot(rgvtracts$White, T_WHITE.lagged.means, col = "blue", ylab = "Spatially Lagged rgvtracts$White")
  bestfit <- lm(T_WHITE.lagged.means~rgvtracts$White)
  abline(bestfit)
summary(bestfit)
# p-val: 1.493e-09

T_BLACK.lagged.means <- lag.listw(lw.queen, rgvtracts$Black_or_A)
plot(rgvtracts$Black_or_A, T_BLACK.lagged.means, col = "red", ylab = "Spatially Lagged rgvtracts$Black")
  bestfit <- lm(T_BLACK.lagged.means~rgvtracts$Black_or_A)
  abline(bestfit)
summary(bestfit)
# p-val 0.03021

T_AMIND.lagged.means <- lag.listw(lw.queen, rgvtracts$American_I)
plot(rgvtracts$American_I, T_AMIND.lagged.means, col = "purple", xlab = "rgvtracts$AmericanIndian", ylab = "Spatially Lagged rgvtracts$AmericanIndian")
  bestfit <- lm(T_AMIND.lagged.means~rgvtracts$American_I)
  abline(bestfit)
summary(bestfit)
# p-val 0.3585
# reject null and accept CSR

T_ASIAN.lagged.means <- lag.listw(lw.queen, rgvtracts$Asian)
plot(rgvtracts$Asian, T_ASIAN.lagged.means, col = "green", ylab = "Spatially Lagged rgvtracts$Asian")
  bestfit <- lm(T_ASIAN.lagged.means~rgvtracts$Asian)
  abline(bestfit)
summary(bestfit)
# p-val < 2.2e-16

T_NATIVEHAW.lagged.means <- lag.listw(lw.queen, rgvtracts$Native_Haw)
plot(rgvtracts$Native_Haw, T_NATIVEHAW.lagged.means, col = "yellow", xlab = "rgvtracts$NativeHawaiian", ylab = "Spatially Lagged rgvtracts$NativeHawaiian")
  bestfit <- lm(T_NATIVEHAW.lagged.means~rgvtracts$Native_Haw)
  abline(bestfit)
summary(bestfit)
# p-val 0.5689
# reject null and accept CSR

T_OTHER.lagged.means <- lag.listw(lw.queen, rgvtracts$Some_other)
plot(rgvtracts$Some_other, T_OTHER.lagged.means, col = "orange", xlab = "rgvtracts$SomeOtherRace", ylab = "Spatially Lagged rgvtracts$SomeOtherRace")
  bestfit <- lm(T_OTHER.lagged.means~rgvtracts$Some_other)
  abline(bestfit)
summary(bestfit)
# p-val < 2.2e-16

T_MEDAGE.lagged.means <- lag.listw(lw.queen, rgvtracts$Median_Age)
plot(rgvtracts$Median_Age, T_MEDAGE.lagged.means, col = "blue", xlab = "rgvtracts$MedianAge", ylab = "Spatially Lagged rgvtracts$MedianAge")
  bestfit <- lm(T_MEDAGE.lagged.means~rgvtracts$Median_Age)
  abline(bestfit)
summary(bestfit)
# p-val 2.595e-15

T_MEDHOU.lagged.means <- lag.listw(lw.queen, rgvtracts$Median_hou)
plot(rgvtracts$Median_hou, T_MEDHOU.lagged.means, col = "red", xlab = "rgvtracts$MedianIncome", ylab = "Spatially Lagged rgvtracts$MedianIncome")
  bestfit <- lm(T_MEDHOU.lagged.means~rgvtracts$Median_hou)
  abline(bestfit)
summary(bestfit)
# p-val < 2.2e-16

T_ESEDU.lagged.means <- lag.listw(lw.queen, rgvtracts$ES_Total)
plot(rgvtracts$ES_Total, T_ESEDU.lagged.means, col = "blue", xlab = "rgvtracts$ESEducation", ylab = "Spatially Lagged rgvtracts$ESEducation")
  bestfit <- lm(T_ESEDU.lagged.means~rgvtracts$ES_Total)
  abline(bestfit)
summary(bestfit)
# p-val 3.527e-12

T_MSEDU.lagged.means <- lag.listw(lw.queen, rgvtracts$MS_Total)
plot(rgvtracts$MS_Total, T_MSEDU.lagged.means, col = "red", xlab = "rgvtracts$MSEducation", ylab = "Spatially Lagged rgvtracts$MSEducation")
  bestfit <- lm(T_MSEDU.lagged.means~rgvtracts$MS_Total)
  abline(bestfit)
summary(bestfit)
# p-val < 2.2e-16

T_HSEDU.lagged.means <- lag.listw(lw.queen, rgvtracts$HS_Total)
plot(rgvtracts$HS_Total, T_HSEDU.lagged.means, col = "purple", xlab = "rgvtracts$HSEducation", ylab = "Spatially Lagged rgvtracts$HSEducation")
  bestfit <- lm(T_HSEDU.lagged.means~rgvtracts$HS_Total)
  abline(bestfit)
summary(bestfit)
# p-val 8.901e-06

T_OTHEREDU.lagged.means <- lag.listw(lw.queen, rgvtracts$Other)
plot(rgvtracts$Other, T_OTHEREDU.lagged.means, col = "green", xlab = "rgvtracts$OtherEducation", ylab = "Spatially Lagged rgvtracts$OtherEducation")
  bestfit <- lm(T_OTHEREDU.lagged.means~rgvtracts$Other)
  abline(bestfit)
summary(bestfit)
# p-val 3.912e-13

T_ASSOCIATE.lagged.means <- lag.listw(lw.queen, rgvtracts$Associate_)
plot(rgvtracts$Associate_, T_ASSOCIATE.lagged.means, col = "yellow", xlab = "rgvtracts$AssociatesDegree", ylab = "Spatially Lagged rgvtracts$AssociatesDegree")
  bestfit <- lm(T_ASSOCIATE.lagged.means~rgvtracts$Associate_)
  abline(bestfit)
summary(bestfit)
# p-val 4.259e-12

T_BACHELOR.lagged.means <- lag.listw(lw.queen, rgvtracts$Bachelor_s)
plot(rgvtracts$Bachelor_s, T_BACHELOR.lagged.means, col = "orange", xlab = "rgvtracts$BachelorsDegree", ylab = "Spatially Lagged rgvtracts$BachelorsDegree")
  bestfit <- lm(T_BACHELOR.lagged.means~rgvtracts$Bachelor_s)
  abline(bestfit)
summary(bestfit)
# p-val < 2.2e-16

T_MASTER.lagged.means <- lag.listw(lw.queen, rgvtracts$Master_s_d)
plot(rgvtracts$Master_s_d, T_MASTER.lagged.means, col = "pink", xlab = "rgvtracts$MastersDegree", ylab = "Spatially Lagged rgvtracts$MastersDegree")
  bestfit <- lm(T_MASTER.lagged.means~rgvtracts$Master_s_d)
  abline(bestfit)
summary(bestfit)
# p-val < 2.2e-16

T_DOCTOR.lagged.means <- lag.listw(lw.queen, rgvtracts$Doctorate)
plot(rgvtracts$Doctorate, T_DOCTOR.lagged.means, col = "brown", xlab = "rgvtracts$DoctorateDegree", ylab = "Spatially Lagged rgvtracts$DoctorateDegree")
  bestfit <- lm(T_DOCTOR.lagged.means~rgvtracts$Doctorate)
  abline(bestfit)
summary(bestfit)
# p-val 1.177e-09

T_NOEDU.lagged.means <- lag.listw(lw.queen, rgvtracts$No_Edu)
plot(rgvtracts$No_Edu, T_NOEDU.lagged.means, col = "black", xlab = "rgvtracts$NoEducation", ylab = "Spatially Lagged rgvtracts$NoEducation")
  bestfit <- lm(T_NOEDU.lagged.means~rgvtracts$No_Edu)
  abline(bestfit)
summary(bestfit)
# p-val 0.006162



# getis ord
bbox(rgvtracts)
rgv.nb <- poly2nb(rgvtracts)
rgv.listw <- nb2listw(rgv.nb)
pval.shade <- shading(c(0.01,0.05,0.1), cols=rev(brewer.pal(4,'PuRd')))

# white
white.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
white.lG <- localG(rgvtracts$White, white.lw.g)
# pvals
whitepvals <- dnorm(white.lG)

# black
black.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
black.lG <- localG(rgvtracts$Black_or_A, black.lw.g)
# pvals
blackpvals <- dnorm(black.lG)

# asian
asian.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
asian.lG <- localG(rgvtracts$Asian, asian.lw.g)
# pvals
asianpvals <- dnorm(asian.lG)

# some other
someother.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
someother.lG <- localG(rgvtracts$Some_other, someother.lw.g)
# pvals
someotherpvals <- dnorm(someother.lG)

# es total
estot.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
estot.lG <- localG(rgvtracts$ES_Total, estot.lw.g)
# pvals
estotpvals <- dnorm(estot.lG)

# ms total
mstot.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
mstot.lG <- localG(rgvtracts$MS_Total, mstot.lw.g)
# pvals
mstotpvals <- dnorm(mstot.lG)

# hs total
hstot.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
hstot.lG <- localG(rgvtracts$HS_Total, hstot.lw.g)
# pvals
hstotpvals <- dnorm(hstot.lG)

# other edu total
other.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
other.lG <- localG(rgvtracts$Other, other.lw.g)
# pvals
otherpvals <- dnorm(other.lG)

# assoc total
assoc.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
assoc.lG <- localG(rgvtracts$Associate_, assoc.lw.g)
# pvals
assocpvals <- dnorm(assoc.lG)

# bach total
bach.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
bach.lG <- localG(rgvtracts$Bachelor_s, bach.lw.g)
# pvals
bachpvals <- dnorm(bach.lG)

# master edu total
master.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
master.lG <- localG(rgvtracts$Master_s_d, master.lw.g)
# pvals
masterpvals <- dnorm(master.lG)

# doc edu total
doc.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
doc.lG <- localG(rgvtracts$Doctorate, doc.lw.g)
# pvals
docpvals <- dnorm(doc.lG)

# no edu total
noedu.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
noedu.lG <- localG(rgvtracts$No_Edu, noedu.lw.g)
# pvals
noedupvals <- dnorm(noedu.lG)

# median age total
medage.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
medage.lG <- localG(rgvtracts$Median_Age, medage.lw.g)
# pvals
medagepvals <- dnorm(medage.lG)

# med inc total
medinc.lw.g <- nb2listw(rgv.nb, style="B")
plot(rgv.nb, coordinates(rgvtracts), add=T, col="red")
medinc.lG <- localG(rgvtracts$Median_hou, medinc.lw.g)
# pvals
medincpvals <- dnorm(medinc.lG)



# geographically weighted regression
# global model
empl.ols <- lm(Total_Empl~White+Black_or_A+Asian+Some_other+
            ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
            Median_Age+Median_hou, data=rgvtracts)
summary(empl.ols)
rgvtracts$emplresid <- resid(empl.ols)
moran.test(rgvtracts$emplresid, rgv.listw)

dire.ols <- lm(Total_Dire~White+Black_or_A+Asian+Some_other+
                 ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                 Median_Age+Median_hou, data=rgvtracts)
summary(dire.ols)
rgvtracts$direresid <- resid(dire.ols)
moran.test(rgvtracts$direresid, rgv.listw)

medicare.ols <- lm(Total_Medi~White+Black_or_A+Asian+Some_other+
                     ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                     Median_Age+Median_hou, data=rgvtracts)
summary(medicare.ols)
rgvtracts$medicareresid <- resid(medicare.ols)
moran.test(rgvtracts$medicareresid, rgv.listw)

medicaid.ols <- lm(Total_Me_1~White+Black_or_A+Asian+Some_other+
                     ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                     Median_Age+Median_hou, data=rgvtracts)
summary(medicaid.ols)
rgvtracts$medicaidresid <- resid(medicaid.ols)
moran.test(rgvtracts$medicaidresid, rgv.listw)

tricare.ols <- lm(Total_Tric~White+Black_or_A+Asian+Some_other+
                    ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                    Median_Age+Median_hou, data=rgvtracts)
summary(tricare.ols)
rgvtracts$tricareresid <- resid(tricare.ols)
moran.test(rgvtracts$tricareresid, rgv.listw)
# not significant for a gwr model

vahealth.ols <- lm(Total_VA_h~White+Black_or_A+Asian+Some_other+
                     ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                     Median_Age+Median_hou, data=rgvtracts)
summary(vahealth.ols)
rgvtracts$vahealthresid <- resid(vahealth.ols)
moran.test(rgvtracts$vahealthresid, rgv.listw)
# not significant for a gwr model

noins.ols <- lm(Total_No_I~White+Black_or_A+Asian+Some_other+
                  ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                  Median_Age+Median_hou, data=rgvtracts)
summary(noins.ols)
rgvtracts$noinsresid <- resid(noins.ols)
moran.test(rgvtracts$noinsresid, rgv.listw)

# gwr model
bw.gwr(Total_Empl~White+Black_or_A+Asian+Some_other+
         ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
         Median_Age+Median_hou, data=rgvtracts, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
gwr.empl <- gwr.basic(Total_Empl~White+Black_or_A+Asian+Some_other+
                        ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                        Median_Age+Median_hou, data=rgvtracts, bw=12378.12, kernel="gaussian")
gwr.empl
rgvtracts$emplstresid <- gwr.empl$SDF$Stud_residual

bw.gwr(Total_Dire~White+Black_or_A+Asian+Some_other+
         ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
         Median_Age+Median_hou, data=rgvtracts, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
gwr.dire <- gwr.basic(Total_Dire~White+Black_or_A+Asian+Some_other+
                        ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                        Median_Age+Median_hou, data=rgvtracts, bw=11199.28, kernel="gaussian")
gwr.dire
rgvtracts$direstresid <- gwr.dire$SDF$Stud_residual

bw.gwr(Total_Medi~White+Black_or_A+Asian+Some_other+
         ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
         Median_Age+Median_hou, data=rgvtracts, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
gwr.medicare <- gwr.basic(Total_Medi~White+Black_or_A+Asian+Some_other+
                            ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                            Median_Age+Median_hou, data=rgvtracts, bw=10233.3, kernel="gaussian")
gwr.medicare
rgvtracts$medicarestres <- gwr.medicare$SDF$Stud_residual

bw.gwr(Total_Me_1~White+Black_or_A+Asian+Some_other+
         ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
         Median_Age+Median_hou, data=rgvtracts, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
gwr.medicaid <- gwr.basic(Total_Me_1~White+Black_or_A+Asian+Some_other+
                            ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                            Median_Age+Median_hou, data=rgvtracts, bw=12448.53, kernel="gaussian")
gwr.medicaid
rgvtracts$medicaidstres <- gwr.medicaid$SDF$Stud_residual

bw.gwr(Total_No_I~White+Black_or_A+Asian+Some_other+
         ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
         Median_Age+Median_hou, data=rgvtracts, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
gwr.noins <- gwr.basic(Total_No_I~White+Black_or_A+Asian+Some_other+
                         ES_Total+MS_Total+HS_Total+Other+Associate_+Bachelor_s+Master_s_d+Doctorate+No_Edu+
                         Median_Age+Median_hou, data=rgvtracts, bw=12450.64, kernel="gaussian")
gwr.noins
rgvtracts$noinsstres <- gwr.noins$SDF$Stud_residual

# export data to CSV for use in GIS
as.data.frame(rgvtracts@data[["emplstresid"]], rgvtracts@data[["GEOID"]])
as.data.frame(rgvtracts@data[["direstresid"]], rgvtracts@data[["GEOID"]])
as.data.frame(rgvtracts@data[["medicarestres"]], rgvtracts@data[["GEOID"]])
as.data.frame(rgvtracts@data[["medicaidstres"]], rgvtracts@data[["GEOID"]])
as.data.frame(rgvtracts@data[["noinsstres"]], rgvtracts@data[["GEOID"]])