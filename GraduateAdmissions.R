#setwd("/Users/yolanda7zhang/Documents/GitHub/GraduateAdmissions")
grad<-read.csv("Admission_Predict_Ver1.1.csv",header = T)
df1<-data.frame(grad)

#library("jtools")

#drop the column Serial.No.
var.out.bool <- !names(df1) %in% c("Serial.No.")
df1 <- df1[,var.out.bool] # or...
df1 <- df1[,var.out.bool, drop = FALSE] 
#df


############### scatterplot matrix ################
#source("pairs.panels.r")
#pairs.panels(df1)

df1["ToeflLOR"]<-df1$TOEFL.Score*df1$LOR
df1["RatingSOP"]<-df1$University.Rating*df1$SOP
attach(df1)
###############least square models######################
#reg.out<-lm(Chance.of.Admit~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR+CGPA+Research)
#summary(reg.out)

#reg.out2<-lm((Chance.of.Admit)^3~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR+CGPA+Research+
#               University.Rating:SOP)
#summary(reg.out2)

reg.out3<-lm((Chance.of.Admit)~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR+CGPA+Research+
               ToeflLOR+RatingSOP)
summary(reg.out3)

#confint(reg.out2, level=.95)
#confint(reg.out3, level=.95)
#summ(reg.out3)
#confint(reg.out3)

                  
##################### test on High + high score MULTIPLE VALUES#############################################

xval3 <- data.frame(GRE.Score=rep(340,10),TOEFL.Score=rep(120,10),University.Rating=rep(5,10)
                    ,SOP=rep(5,10),LOR=seq(from = 0.5, to = 5, by = 0.5),
                    CGPA=rep(10,10),Research=rep(1,10))

xval3["ToeflLOR"]= xval3$TOEFL.Score*xval3$LOR
xval3["RatingSOP"]= xval3$University.Rating*xval3$SOP
conf3 <- predict(reg.out3,xval3,interval=         # Computes confidence bands
                   "confidence")
conf3 # CI for y^3
conf3^(1/3) # CI for y

pred3 <- predict(reg.out3,xval3,interval=         # Computes prediction bands
         "prediction")
pred3 # PI for y^3
pred3^(1/3) # PI of y 

plot(xval3$LOR,conf3[,"fit"]^(1/3),xlab="LOR",   # Plots biomass vs. solar radiation
     ylab="Fitted values (Y-hat ^3)",cex.axis=1.0,       #   with axis labels and a title
     cex.lab=1.0,cex=1.5,pch=16,cex.main=1.0,
     main="Fitted values, CI & PI bands for high profile LOR",
     mgp=c(2.7,1,0),ylim = c(0.60,1.10))

matlines(xval3["LOR"]^(1/3),conf3[,c("lwr","upr")],           # Plots confidence bands in red and
         col="red")                       #   line type 2
matlines(xval3["LOR"]^(1/3),pred3[,c("lwr","upr")],        # Plots prediction bands in red and
         col="blue")  

##################### test on High Profile + low score #############################################
xval4 <- data.frame(GRE.Score=340,TOEFL.Score=120,University.Rating=5,SOP=4.5,LOR=5,CGPA=10,Research=1)          # Sequence of x-values from 0 to 650 by 25
xval4["ToeflLOR"]= xval4$TOEFL.Score*xval4$LOR
xval4["RatingSOP"]= xval4$University.Rating*xval4$SOP

conf4 <- predict(reg.out3,xval4,interval=         # Computes confidence bands
                  "confidence")
conf4 # CI of y^3 
conf4^(1/3)  # CI of y

pred4 <- predict(reg.out3,xval4,interval=         # Computes prediction bands
                  "prediction")
pred4 # PI of y^3
pred4^(1/3) # PI of y 

##################### test on Low profile + high score #############################################

xval5 <- data.frame(GRE.Score=300,TOEFL.Score=113,University.Rating=3,SOP=3,LOR=3,CGPA=8,Research=0)      # Sequence of x-values from 0 to 650 by 25
xval5["ToeflLOR"]= xval5$TOEFL.Score*xval5$LOR
xval5["RatingSOP"]= xval5$University.Rating*xval5$SOP
conf5 <- predict(reg.out3,xval5,interval=         # Computes confidence bands
                   "confidence")
conf5 # CI for y^3
conf5^(1/3) # CI for y

pred5 <- predict(reg.out3,xval5,interval=         # Computes prediction bands
                   "prediction")
pred5 # PI for y^3
pred5^(1/3) # PI of y 


##################### test on Low Profile + low score MULTIPLE VALUES #############################################
xval6 <- data.frame(GRE.Score=300,TOEFL.Score=100,University.Rating=3
                    ,SOP=3,LOR=3,
                    CGPA=8,Research=0)

# Sequence of x-values from 0 to 65 by 25
xval6["ToeflLOR"]= xval6$TOEFL.Score*xval6$LOR
xval6["RatingSOP"]= xval6$University.Rating*xval6$SOP

conf6 <- predict(reg.out3,xval6,interval=         # Computes confidence bands
                   "confidence")
conf6 # CI of y^3 
conf6^(1/3)  # CI of y

pred6 <- predict(reg.out3,xval6,interval=         # Computes prediction bands
                   "prediction")
pred6 # PI of cy^3
pred6^(1/3) # PI of y 



plot(xval6$LOR,conf6[,"fit"],xlab="LOR",   # Plots biomass vs. solar radiation
     ylab="Fitted values (Y-hat ^3)",cex.axis=1.0,       #   with axis labels and a title
     cex.lab=1.0,cex=1.5,pch=16,cex.main=1.0,
     main="Fitted values vs LOR",
     mgp=c(2.7,1,0),ylim = c(0.50,0.90))
#,ylim = c(0.03,0.44)
#, CI & PI bands for LOR

matlines(xval6["LOR"],conf6[,c("lwr","upr")],           # Plots confidence bands in red and
         col="red")                       #   line type 2
matlines(xval6["LOR"],pred6[,c("lwr","upr")],        # Plots prediction bands in red and
         col="blue")  
# LOR vs y
plot(xval6$LOR,conf6[,"fit"]^(1/3),xlab="LOR",   # Plots biomass vs. solar radiation
     ylab="Est. Chance of Admission",cex.axis=1.0,       #   with axis labels and a title
     cex.lab=1.0,cex=1.5,pch=16,cex.main=1.0,
     main="LOR vs Chance of Admission (TOEFL = 120)",
     mgp=c(2.7,1,0),ylim = c(0.7,1.1))


matlines(xval6["LOR"],conf6[,c("lwr","upr")]^(1/3),           # Plots confidence bands in red and
         col="red")                       #   line type 2
matlines(xval6["LOR"],pred6[,c("lwr","upr")]^(1/3),        # Plots prediction bands in red and
         col="blue")

##################### test on Low Profile +RATING + MULTIPLE VALUES #############################################
xval7 <- data.frame(GRE.Score=rep(340,5),TOEFL.Score=rep(120,5),University.Rating=c(1,2,3,4,5)
                    ,SOP=rep(5,5),LOR=rep(5,5),
                    CGPA=rep(10,5),Research=rep(1,5))         # Sequence of x-values from 0 to 650 by 25
xval7["ToeflLOR"]= xval7$TOEFL.Score*xval7$LOR
xval7["RatingSOP"]= xval7$University.Rating*xval7$SOP

conf7 <- predict(reg.out3,xval7,interval=         # Computes confidence bands
                   "confidence")
conf7 # CI of y^3 
conf7^(1/3)  # CI of y

pred7 <- predict(reg.out3,xval7,interval=         # Computes prediction bands
                   "prediction")
pred7 # PI of cy^3
pred7^(1/3) # PI of y 

#plot y vs rating
plot(xval7$University.Rating,conf7[,"fit"]^(1/3),xlab="LOR",   # Plots biomass vs. solar radiation
     ylab="chance of admission (y-hat)",cex.axis=1.0,       #   with axis labels and a title
     cex.lab=1.0,cex=1.5,pch=16,cex.main=1.0,
     main="Fitted values, CI & PI bands for high profile LOR",
     mgp=c(2.7,1,0),ylim = c(0.6,1.1))
#,ylim = c(0.59,0.82)

matlines(xval7["University.Rating"],conf7[,c("lwr","upr")]^(1/3),           # Plots confidence bands in red and
         col="red")                       #   line type 2
matlines(xval7["University.Rating"],pred7[,c("lwr","upr")]^(1/3),        # Plots prediction bands in red and
         col="blue")   

##################### test on Low Profile + low score MULTIPLE VALUES #############################################
xval6 <- data.frame(GRE.Score=rep(300,2),TOEFL.Score=rep(100,2),University.Rating=rep(3,2)
                    ,SOP=rep(3,2),LOR=rep(3,2),
                    CGPA=rep(8,2),Research=seq(from = 0, to = 1, by = 1))         # Sequence of x-values from 0 to 650 by 25

#GRE.Score=300,TOEFL.Score=100,University.Rating=3,SOP=3,LOR=3,CGPA=8,Research=0

xval6["ToeflLOR"]= xval6$TOEFL.Score*xval6$LOR
xval6["RatingSOP"]= xval6$University.Rating*xval6$SOP

conf6 <- predict(reg.out3,xval6,interval=         # Computes confidence bands
                   "confidence")
conf6 # CI of y^3 
conf6^(1/3)  # CI of y

pred6 <- predict(reg.out3,xval6,interval=         # Computes prediction bands
                   "prediction")
pred6 # PI of y^3
pred6^(1/3) # PI of y 



plot(xval6$Research,conf6[,"fit"]^(1/3),xlab="Research",   # Plots biomass vs. solar radiation
     ylab="Chance of Admission",pch=15,
     main="Research vs Chance of Admission"
     )
#,ylim = c(0.03,0.44)
#, CI & PI bands for LOR

matlines(xval6["Research"],conf6[,c("lwr","upr")]^(1/3),           # Plots confidence bands in red and
         col="red")                       #   line type 2
matlines(xval6["Research"],pred6[,c("lwr","upr")]^(1/3),        # Plots prediction bands in red and
         col="blue")  

