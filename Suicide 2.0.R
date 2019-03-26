##Suicide BD universities

SuisBdUni <- read.csv("data/Suicide_BD_unistd.csv")

head(SuisBdUni)
library(dplyr)

sex <- count(SuisBdUni, Sex)
residence <- count(SuisBdUni, Residence)
count(SuisBdUni, SuicidalNotes)
count(SuisBdUni, University.type)
AcademicYear <- count(SuisBdUni, AcademicYear)
class <- count(SuisBdUni, Division)
count(SuisBdUni, SuicideMethod)
year <- count(SuisBdUni, SuicideYear)
count(SuisBdUni, Timeofsuicide)
count(SuisBdUni, University)
cause <- count(SuisBdUni, IdentifiableCause) 
cause

###



##Figure 3
pageWidthLarge<- 7.08661
pageHeightLarge <- pageWidthLarge * 1.50
pagePaper <- 'special'
fontFamily <- 'Times'
pdf("output/Figure_3.pdf", width=pageWidthLarge, 
    height= pageHeightLarge, family=fontFamily, paper=pagePaper)

#png("output/Figure_2.png", width=pageWidthLarge, height= pageHeightLarge,
#units= "in",type= "cairo", res = 600)

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
dev.off ()




##
##Figure 2 ###
#a)
year <- count(SuisBdUni, SuicideYear)
head(year)
plot(year, type ='o', col = 'red', pch= 16, xlab = "Year", 
     ylab= "Number of suicide", cex.lab = 1.2)


x <- lm(year$n ~ year$SuicideYear)
summary(x)
plot(year$n ~ year$SuicideYear, pch = 16, xlab = "Year", 
     ylab= "Number of suicide")

abline(-2564.9516, 1.2791, lwd = 2.0)

newx <- seq(min(year$SuicideYear), max(year$SuicideYear))
preds <- predict(x, newdata = data.frame(SuicideYear=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)

##

#b)
month <- count(SuisBdUni, Month)
month
month <- na.omit (month) 
month
barplot(month$n, ylab = "Number of suicide")
mtext(c("January"),at= 0.75, las= 2, side =1, line = 0.35)
mtext(c("February"),at= 1.75, las= 2, side =1, line = 0.35)
mtext(c("March"),at= 3.0, las= 2, side =1, line = 0.35)
mtext(c("April"),at= 4.25, las= 2, side =1, line = 0.35)
mtext(c("May"),at= 5.5, las= 2, side =1, line = 0.35)
mtext(c("June"),at= 6.75, las= 2, side =1, line = 0.35)
mtext(c("July"),at= 7.75, las= 2, side =1, line = 0.35)
mtext(c("August"),at= 9.0, las= 2, side =1, line = 0.35)
mtext(c("September"),at= 10.25, las= 2, side =1, line = 0.35)
mtext(c("October"),at= 11.5, las= 2, side =1, line = 0.35)
mtext(c("November"),at= 12.75, las= 2, side =1, line = 0.35)
mtext(c("December"),at= 14.0, las= 2, side =1, line = 0.35)

plot(month, type ='o', col = 'red', pch =16, xlab = "Month", 
     ylab = "Number of Suicide", cex.lab =1.2)
a<- month$n
barplot(a, ylab = "Number of suicide")

##
#c)
par(mar=c(8.5,4,4,2))          
cause <- count(SuisBdUni, IdentifiableCause)

cause

cause2 <- na.omit(cause)
cause2
barplot(cause2$n, ylab = "Number of suicide")
mtext(c("Academic frustration"),at= 0.75, las= 2, side =1, line = 0.35)
mtext(c("Depression"),at= 1.85, las= 2, side =1, line = 0.35)
mtext(c("Fail in job exam"),at= 3.0, las= 2, side =1, line = 0.35)
mtext(c("Family disharmony"),at= 4.25, las= 2, side =1, line = 0.35)
mtext(c("Financial constrains"),at= 5.5, las= 2, side =1, line = 0.35)
mtext(c("Love affairs"),at= 6.75, las= 2, side =1, line = 0.35)
mtext(c("Marital discord"),at= 7.75, las= 2, side =1, line = 0.35)
mtext(c("Mental unstability"),at= 9.0, las= 2, side =1, line = 0.35)
mtext(c("Physical illness"),at= 10.25, las= 2, side =1, line = 0.35)
##




slices <- c(9,10,1,6,1,32,3,2,1)
labs <- c("Academic frustration", "Depression", "Fail in job exam", 
          "Family disharmony", "Financial constrains", "Love affairs", "Marital discord",
          "Mental unstability", "Physical illness")

pie(slices, labels = labs,  main = "Identifiable Causes of Suicide")
cause2 <- na.omit(cause)
cause2
barplot(cause2$n, ylab = "Number of suicide")



### Sex speific differences 


age <- mean(SuisBdUni$Age, na.rm = TRUE)
age
sd(SuisBdUni$Age, na.rm = TRUE)

min(SuisBdUni$Age, na.rm = TRUE)
max(SuisBdUni$Age, na.rm = TRUE)

male <- filter(SuisBdUni, Sex =='Male')
maleage <- male$Age
maleage
meanmaleage <- mean(maleage, na.rm = TRUE)
sdmaleage <- sd(maleage, na.rm = TRUE)
meanmaleage
sdmaleage


#
female <- filter(SuisBdUni, Sex == 'Female')
femaleage <- female$Age
femaleage
meanfemaleage <- mean(femaleage, na.rm = 'TRUE')
sdfemaleage <- sd(femaleage, na.rm = TRUE)
meanfemaleage
sdfemaleage

boxplot(maleage,femaleage)


########

maleclass <- count(male, Division)
maleclass
femaleclass <- count(female, Division)
femaleclass

maleyear <- count(male, AcademicYear)
maleyear 
femaleyear <- count(female, AcademicYear)
femaleyear

maleuni <- count(male, University.type)
maleuni
femaleuni <- count(female, University.type)
femaleuni

malemethod <- count(male, SuicideMethod)
malemethod
femalemethod <- count(female, SuicideMethod)
femalemethod

maletime <- count(male, Timeofsuicide)
maletime
femaletime <- count(female, Timeofsuicide)
femaletime

maleresidence <- count(male, Residence)
maleresidence
femaleresidence <- count(female, Residence)
femaleresidence

malenote <- count(male, SuicidalNotes)
malenote
femalenote <- count(female, SuicidalNotes)
femalenote

malecause <- count(male, IdentifiableCause) 
malecause
femalecause <- count(female, IdentifiableCause)
femalecause


maleyear <- count(male, SuicideYear)
maleyear
femaleyear <- count(female, SuicideYear)
femaleyear

maleuniname <- count(male, University)
maleuniname
femaleuniname <- count(female, University)
femaleuniname


###
residence <- count(SuisBdUni, Residence)
residence
slices <- c(50, 32)
labs <- c("Staying with family" ,"Staying outside family")
pie(slices, labels = labs)

#
note <- count(SuisBdUni, SuicidalNotes)
note
slices <- c(15,31)
labs <- c("presnt", "absent")
pie(slices, labels = labs)
#
department <- count(SuisBdUni, Department)
department 
write.csv(department, "department.csv")
#
university <- count(SuisBdUni, University)
write.csv(university, "university.csv")
#

cause<- count(SuisBdUni, IdentifiableCause)
write.csv(cause, "cause.csv")


dev.off()

###########Stats######
#is there a difference between male and female age 


male <- filter(SuisBdUni, Sex =='Male')
maleage <- male$Age
hist(log(maleage))
hist(log(femaleage))
female <- filter(SuisBdUni, Sex == 'Female')
femaleage <- female$Age
hist(maleage)
hist(femaleage)
qqnorm(maleage)
qqline(maleage)
qqnorm(femaleage)
qqline(femaleage)
qqnorm(log(maleage))
qqline(log(maleage))
qqnorm(log(femaleage))
qqline(log(femaleage))

shapiro.test(log(maleage))
shapiro.test(log(femaleage))

shapiro.test(maleage)
shapiro.test(femaleage)

#both maleage and female age are non-normally distributed 

#we will apply Mann whitney U test for that
wilcox.test(maleage, femaleage)


boxplot(maleage,femaleage, axisnames = FALSE)
mtext(c("Male"),at= 1.0, side =1, line = 1)
mtext(c("Female"),at= 2.0, side =1, line = 1)
text(1.5, 28, '*', cex= 1.5)

########
suirate <- read.csv("data/suiciderate.csv")
head(suirate)
mean(suirate$suiciderate)
sd(suirate$suiciderate)

qqnorm(suirate$malesuiciderate)
qqline(suirate$malesuiciderate)
qqnorm(suirate$femalesuiciderate)
qqline(suirate$femalesuiciderate)
qqnorm(log(suirate$femalesuiciderate))
qqline(log(suirate$femalesuiciderate))
qqnorm(sqrt(suirate$femalesuiciderate))
qqline(sqrt(suirate$femalesuiciderate))

shapiro.test(log(suirate$malesuiciderate))
shapiro.test(log(suirate$femalesuiciderate))
t.test(log(suirate$femalesuiciderate), log(suirate$malesuiciderate))
t.test(suirate$malesuiciderate, suirate$femalesuiciderate)

mean(suiciderate$malesuiciderate)
sd(suiciderate$malesuiciderate)
mean(suiciderate$femalesuiciderate)
sd(suiciderate$femalesuiciderate)

###

shapiro.test(suirate$publicunisuiciderate)
shapiro.test(suirate$privateunisuiciderate)

t.test(suirate$publicunisuiciderate, suirate$privateunisuiciderate)

rate<- SuisBdUni %>%
  select(University.type, Sex, SuicideYear)
write.csv(rate, "unisexyear.csv") 


####modeling suicide rate last 9 years and last five years

library(dplyr)
library(lmtest)
library(car)

suirate <- read.csv("data/suiciderate.csv")
suirate
head(suirate)
suiciderate$suiciderate
mean(suiciderate$suiciderate)

####public uni suicide rate
mod.pb <- lm(suirate$publicunisuiciderate ~suirate$Year)
summary(mod.pb)
lrtest(mod.pb)
confint(mod.pb, 'suirate$Year', level = 0.95)
## 

mod.pt <- lm(suirate$privateunisuiciderate ~suirate$Year)
summary(mod.pt)
lrtest(mod.pt)
confint(mod.pt, 'suirate$Year', level = 0.95)
##

mod2 <-  lm(suirate$suiciderate ~ suirate$Year)
qqnorm(residuals(mod2))
summary(mod2)
lrtest(mod2)
plot(suirate$suiciderate ~ suirate$Year, pch = 16,xlab = "Year", 
     ylab= "Suicide rate")

abline(45.24024, -0.02183, lwd = 2.0)

newx <- seq(min(suirate$Year), max(suirate$Year))
newx
preds <- predict(mod2, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)


##male rate and female rate along the years 

mod3 <- lm(suirate$malesuiciderate ~ suirate$Year)
qqnorm(residuals(mod3))
summary(mod3)
confint(mod3, 'suirate$Year', level = 0.95)
lrtest(mod3)
plot(suirate$suiciderate ~ suirate$Year, pch = 16,xlab = "Year", 
     ylab= "Suicide rate")
abline(108.31052, -0.05332, lwd = 2.0)


newx <- seq(min(suirate$Year), max(suirate$Year))
preds <- predict(mod3, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)

##female suicide rate
mod4 <- lm(suirate$femalesuiciderate ~ suirate$Year)
qqnorm(residuals(mod4))
summary(mod4)
confint(mod4, 'suirate$Year', level = 0.95)

lrtest(mod4)
plot(suirate$femalesuiciderate ~ suirate$Year, pch = 16,xlab = "Year", 
     ylab= "Suicide rate")
abline(108.31052, -0.05332, lwd = 2.0)

newx <- seq(min(suirate$Year), max(suirate$Year))
preds <- predict(mod4, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)


### last five year rate

after2012 <- filter(suirate, suirate$Year>2012)
head(after2012)
###
mod.pb5 <- lm(after2012$publicunisuiciderate ~ after2012$Year)
summary(mod.pb5)
lrtest(mod.pb5)
confint(mod.pb5, 'after2012$Year', level = 0.95)
## 

mod.pt5 <- lm(after2012$privateunisuiciderate ~ after2012$Year)
summary(mod.pt5)
lrtest(mod.pt5)
confint(mod.pt5, 'after2012$Year', level = 0.95)
plot(after2012$privateunisuiciderate ~ after2012$Year)

##

mod5 <-  lm(after2012$suiciderate ~ after2012$Year)
summary(mod5)

lrtest(mod5)
plot(after2012$suiciderate ~ after2012$Year, pch = 16,xlab = "Year", 
     ylab= "Suicide rate")

abline(-773.49735, 0.38439, lwd = 2.0)

##to add the confidence line

newx <- seq(min(after2012$Year), max(after2012$Year))
preds <- predict(mod5, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)

##male and female rate last five years

mod6 <-  lm(after2012$malesuiciderate ~ after2012$Year)
summary(mod6)
confint(mod6, 'after2012$Year', level = 0.95)
lrtest(mod6)
plot(after2012$malesuiciderate ~ after2012$Year, pch = 16,xlab = "Year", 
     ylab= "Male suicide rate")

abline(-410.43614, 0.20404, lwd = 2.0)

##to add the confidence line

newx <- seq(min(after2012$Year), max(after2012$Year))
preds <- predict(mod6, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)

#female rate last five years


mod7 <-  lm(after2012$femalesuiciderate ~ after2012$Year)
summary(mod7)
confint(mod7, 'after2012$Year', level = 0.95)
lrtest(mod7)

plot(after2012$femalesuiciderate ~ after2012$Year, pch = 16,xlab = "Year", 
     ylab= "Feale suicide rate")

abline(-1536.7926, 0.7636, lwd = 2.0)
newx <- seq(min(after2012$Year), max(after2012$Year))
preds <- predict(mod7, newdata = data.frame(Year=newx), 
                 interval = 'confidence', level = 0.95)

polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)



#####


maleratemean <- mean(suiciderate$malesuiciderate)
maleratesd <- sd(suiciderate$malesuiciderate)
feamelratemean <- mean(suiciderate$femalesuiciderate)
femaleratesd <- sd(suiciderate$femalesuiciderate)

boxplot(suiciderate$malesuiciderate, suiciderate$femalesuiciderate)

boxplot(log(suiciderate$malesuiciderate), log(suiciderate$femalesuiciderate), ylim= c(-0.8,1.3))

boxplot(suiciderate$publicunisuiciderate, suiciderate$privateunisuiciderate)

boxplot(log(suiciderate$publicunisuiciderate), log(suiciderate$privateunisuiciderate))



####practice 

after2012 <- filter(suirate, suirate$Year>2012)
head(after2011)
b <-  lm(after2012$suiciderate ~ after2012$Year)
plot(after2012$suiciderate ~ after2012$Year, pch = 16,xlab = "Year", 
     ylab= "Suicide rate")
summary(b)
abline(-773.49735, 0.38439, lwd = 2.0)



newx <- seq(min(after2012$Year), max(after2012$Year))
preds <- predict(b, newdata = data.frame(Year=newx), 
                 interval = 'confidence' )


polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), 
        col = rgb(0.1, 0.1, 0.1, 0.2), 
        border = NA)


