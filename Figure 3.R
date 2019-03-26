#Figures
library(dplyr)
library(lmtest)
library(car)

##Figure 1:
pageWidthLarge<- 7.08661
pageHeightLarge <- pageWidthLarge * 1.50
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_1.pdf", width=pageWidthLarge, 
    height= pageHeightLarge, family=fontFamily, paper=pagePaper)

#png("output/Figure_2.png", width=pageWidthLarge, height= pageHeightLarge,
#units= "in",type= "cairo", res = 600)

layout(matrix(c(1,2,3), nrow=3,ncol=1,byrow =  TRUE))
par(mgp=c(2.5,1,0)) 

#a)

year <- count(SuisBdUni, SuicideYear)

head(year)

mod1 <- lm(year$n ~ year$SuicideYear)
summary(mod1)
lrtest(mod1)
confint(mod1, 'year$SuicideYear', level = 0.95)
plot(year$n ~ year$SuicideYear, pch = 16, xlab = "Year", 
     ylab= "Number of suicide", cex.lab = 2.0, cex.axis = 1.5)

abline(-2564.9516, 1.2791, lwd = 2.0)

newx <- seq(min(year$SuicideYear), max(year$SuicideYear))
preds <- predict(x, newdata = data.frame(SuicideYear=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)

mtext("(a)", side = 3, line = 1, adj= 0.009, cex=1.5*par()$cex)
##
#b)

suirate <- read.csv("data/suiciderate.csv")
head(suirate)

mod2 <-  lm(suirate$suiciderate ~ suirate$Year)
summary(mod2)
confint(mod2, 'suirate$Year', level = 0.95)

lrtest(mod2)
plot(suirate$suiciderate ~ suirate$Year, pch = 16,xlab = "Year", 
     ylab= "Suicide rate", cex.lab = 2.0, cex.axis = 1.5)

abline(27.98880, -0.01327, lwd = 2.0)

newx <- seq(min(suirate$Year), max(suirate$Year))
preds <- predict(mod2, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)

mtext("(b)", side = 3, line = 1, adj= 0.009, cex=1.5*par()$cex)
#c)

after2012 <- filter(suirate, suirate$Year>2012)
head(after2012)

mod5 <-  lm(after2012$suiciderate ~ after2012$Year)
summary(mod5)
confint(mod5, 'after2012$Year', level = 0.95)
lrtest(mod5)
plot(after2012$suiciderate ~ after2012$Year, pch = 16,xlab = "Year", 
     ylab= "Suicide rate", cex.lab = 2.0, cex.axis = 1.5)

abline(-773.49735, 0.38439, lwd = 2.0)

##to add the confidence line

newx <- seq(min(after2012$Year), max(after2012$Year))
preds <- predict(mod5, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)
mtext("(c)", side = 3, line = 1, adj= 0.009, cex=1.5*par()$cex)

dev.off()

#Figure 2: 

#a)
pageWidthLarge<- 3.5
pageHeightLarge <- pageWidthLarge * 2.0
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_2.pdf", width=pageWidthLarge, 
    height= pageHeightLarge, family=fontFamily, paper=pagePaper)

#png("output/Figure_2.png", width=pageWidthLarge, height= pageHeightLarge,
#units= "in",type= "cairo", res = 600)

layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE))
par(mgp=c(2.5,1,0)) 

#a)
boxplot(maleage,femaleage, axisnames = FALSE,
        ylab= "Age", cex.lab =1.5,cex.axis = 1.2, 
        col = c("gray65", "gray6"))
mtext(c("Male"),at= 1.0, side =1, line = 1, cex=1.5*par()$cex)
mtext(c("Female"),at= 2.0, side =1, line = 1, cex=1.5*par()$cex)
text(1.5, 28, '*', cex= 2.0)

mtext("(a)", side = 3, line = 1, adj= 0.009, cex=1.5*par()$cex)

#b)

boxplot(log(suiciderate$malesuiciderate), log(suiciderate$femalesuiciderate), 
        ylim= c(-0.8,1.3), ylab= "Suicide rate", cex.lab =1.5,cex.axis = 1.2, 
        col = c("gray65", "gray6"))

mtext(c("Male"),at= 1.0, side =1, line = 1,cex=1.5*par()$cex)
mtext(c("Female"),at= 2.0, side =1, line = 1,cex=1.5*par()$cex)
text(1.5, 1.2, '*', cex= 2.0)

mtext("(b)", side = 3, line = 1, adj= 0.009, cex=1.5*par()$cex)

dev.off()


##Figure 3
pageWidthLarge<- 7.08661
pageHeightLarge <- pageWidthLarge * 1.50
pagePaper <- 'special'
fontFamily <- 'Times'
#pdf("output/Figure_3.pdf", width=pageWidthLarge, 
   # height= pageHeightLarge, family=fontFamily, paper=pagePaper)

png("output/Figure_2.png", width=pageWidthLarge, height= pageHeightLarge,
units= "in",type= "cairo", res = 600)

layout(matrix(c(1,2,3,4,5,6,7,8), nrow=4,ncol=2,byrow =  TRUE))
par(mar=c(1,1,2,2))

##
sex <- count(SuisBdUni, Sex)
sex
slices <- sex$n
labs <- sex$Sex
pie(slices, labels = labs, main ="Sex")
mtext("(a)", side = 3, line = -3, adj= 0.09, cex=1.0)


##
faculty <- count(SuisBdUni, Division)
faculty <- na.omit(faculty)
slices <- faculty$n
labs <- faculty$Division
slices
pie(slices, labels = labs, main = "Faculty", clockwise = TRUE)
mtext("(b)", side = 3, line = -3, adj= 0.15, cex=1.0)

##
AcademicYear <- count(SuisBdUni, AcademicYear)
AcademicYear
AcademicYear <- na.omit(AcademicYear)
slices <- AcademicYear$n
labs <- c("1st", "2nd", "3rd","4th","5th")
pie(slices, labels = labs, main= "Academic year")
mtext("(c)", side = 3, line = -3, adj= 0.09, cex=1.0)

##
uni <- count(SuisBdUni, University.type)
uni
uni <- na.omit(uni)
slices <- uni$n
labs <- c("National university", "Private university", "Public university")
labs
pie(slices, labels = labs, main = "University type")
mtext("(d)", side = 3, line = -3, adj= 0.15, cex=1.0)


###
residence <- count(SuisBdUni, Residence)
residence
slices <- c(32, 50)
labs <- c("Staying with family" ,"Staying outside family")
pie(slices, labels = labs, main = "Residence")
mtext("(g)", side = 3, line = -3, adj= 0.09, cex=1.0)
#
note <- count(SuisBdUni, SuicidalNotes)
note
slices <- c(15,30)
labs <- c("Presnt", "Absent")
pie(slices, labels = labs, main = "Suicide note")
mtext("(h)", side = 3, line = -3, adj= 0.15, cex=1.0)

####
method <- count(SuisBdUni, SuicideMethod)
method
method <- na.omit(method)
slices <- method$n
labs <- method$SuicideMethod
labs
pie(slices, labels = labs, main = "Method")
mtext("(e)", side = 3, line = -3, adj= 0.09, cex=1.0)

###
time <- count(SuisBdUni, Timeofsuicide)
time
time <- na.omit(time)
slices <- time$n
labs <- time$Timeofsuicide
pie(slices, labels = labs, main = "Time of suicide")
mtext("(f)", side = 3, line = -3, adj= 0.15, cex=1.0)
dev.off ()

#####
pageWidthLarge<- 7.08661
pageHeightLarge <- pageWidthLarge * 1.50
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_4.pdf", width=pageWidthLarge, 
    height= pageHeightLarge, family=fontFamily, paper=pagePaper)

#png("output/Figure_2.png", width=pageWidthLarge, height= pageHeightLarge,
#units= "in",type= "cairo", res = 600)

layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE))
par(mgp=c(2.5,1,0))


#a)
par(mar=c(11,4,2,2))          
cause <- count(SuisBdUni, IdentifiableCause)

cause

cause2 <- na.omit(cause)
cause2
barplot(cause2$n, ylab = "Number of suicide", cex.lab = 1.5, cex.axis = 1.5)
mtext(c("Academic \n dissatisfaction"),at= 0.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Depression"),at= 1.85, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Fail in job exam"),at= 3.0, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Family disharmony"),at= 4.25, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Financial constrains"),at= 5.5, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Unsuccessful \n Romantic relationship"),at= 6.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Marital discord"),at= 7.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Psychiatric illness"),at= 9.0, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("Physical illness"),at= 10.25, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)

mtext("(a)", side = 3, line = 0, adj= 0.009, cex=1.5*par()$cex)

#b)

month <- count(SuisBdUni, Month)
month
month <- na.omit (month) 
month
barplot(month$n, ylab = "Number of suicide", cex.axix = 1.5, cex.lab= 1.5)

mtext(c("January"),at= 0.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("February"),at= 1.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("March"),at= 3.0, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("April"),at= 4.25, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("May"),at= 5.5, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("June"),at= 6.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("July"),at= 7.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("August"),at= 9.0, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("September"),at= 10.25, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("October"),at= 11.5, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("November"),at= 12.75, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)
mtext(c("December"),at= 14.0, las= 2, side =1, line = 0.35, cex=1.5*par()$cex)

mtext("(b)", side = 3, line = 0, adj= 0.009, cex=1.5*par()$cex)


dev.off()

sessionInfo()
