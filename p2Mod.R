library(readxl)
library(zoo)

dat <- read_csv("./data/datAll.csv")
dat$X1 <- NULL
dat$YM <- paste0('15-', gsub(" ", "-", dat$YM))
dat$YM <- as.Date(dat$YM, "%d-%b-%Y")

dat <- dat[, c(1,2,4,6,11:16)]

datTrain <- dat[1:120,]
datTest <- dat[121:151,]

##

library(mgcv)

mod <- gam(denSum ~ 
             s(tMeanL3,k=4) + s(cRainL2,k=4) + s(denSumL2,k=4),
           data=datTrain, family=quasipoisson, na.action=na.exclude)

summary(mod)

p <- fitted.values(mod)
#plot(c(1:nrow(dat)), dat$denSum, type="l", ylab="Number Cases", axes=F, xlab="Year")
plot(dat$YM, dat$denSum, type="l", ylab="Number Cases", xlab="Year")
#points(predict(mod, type="response"), type="l", col="red")
points(dat$YM[1:120], p, type="l", col="blue")
points(datTest$YM, predict(mod, type="response", newdata=datTest), type="l", col="red")
#title(main="")

## Train

tr <- round(mean(datTest$denSum, na.rm=T), 0)
p <- fitted.values(mod)

p1 <- p > tr
dengue1 <- datTrain$denSum > tr
xtabs(p1~dengue1)
table(p1,dengue1)
prop.table(table(p1,dengue1))

# Sensitivity: TP/(TP+FN) × 100
senst = (28/(28+5))*100
# Specificity: TN/(TN+FP) × 100
specf = (56/(56+9))*100
# PPV: TP/(TP+FP) × 100
PPV = (28/(28+9))*100
# NPV: TN/(TN+FN) × 100
NPV = (56/(56+5))*100

senst; specf; PPV; NPV
#title(main="Specificity=81.0, Sensitivity=88.9, PPV=74.4, NPV=92.2")
title(main=paste0("Spec=",round(specf,1),
                  ", Sens=",round(senst,1),
                  ", PPV=",round(PPV,1),
                  ", NPV=",round(NPV,1)))

##

## Test

tr <- round(mean(datTest$denSum, na.rm=T), 0)
p <- predict(mod, type="response", newdata=datTest)
p

p1 <- p > tr
dengue1 <- datTest$denSum > tr
xtabs(p1~dengue1)
table(p1,dengue1)
prop.table(table(p1,dengue1))

# Sensitivity: TP/(TP+FN) × 100
senst = (11/(11+2))*100
# Specificity: TN/(TN+FP) × 100
specf = (12/(12+5))*100
# PPV: TP/(TP+FP) × 100
PPV = (11/(11+5))*100
# NPV: TN/(TN+FN) × 100
NPV = (12/(12+2))*100

senst; specf; PPV; NPV
title(sub=paste0("Spec=",round(specf,1),
                  ", Sens=",round(senst,1),
                  ", PPV=",round(PPV,1),
                  ", NPV=",round(NPV,1)))
