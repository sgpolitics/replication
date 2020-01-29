#Read csv file into R: Provide directory information for data file
frame <- read.csv(".../broockman_pa_2009.csv")
attach(frame)

#Drop anomalous observations as outlined
frame$dropflag <- 0
frame$dropflag[t2_3rdpartyinc == 1 | t2_incumbent_has_switched_prty == 1 | t2_specialelectiontoeelect == 1 | t1_atlargeormulticandidate == 1 | t2_redist == 1] <- 1
detach(frame)

data <- subset(frame, dropflag==0, select= -dropflag)

#create these variables so that they can be used in running a 4th order polynomial
data$dwin <- 0
data$dwin[data$dv_c_t1>=0] <- 1
data$dv_c_t1_2 <- data$dv_c_t1^2
data$dv_c_t1_3 <- data$dv_c_t1^3
data$dv_c_t1_4 <- data$dv_c_t1^4

data$i_dv_c_t1 <- data$dv_c_t1 * data$dwin
data$i_dv_c_t1_2 <- data$dv_c_t1_2 * data$dwin
data$i_dv_c_t1_3 <- data$dv_c_t1_3 * data$dwin
data$i_dv_c_t1_4 <- data$dv_c_t1_4 * data$dwin

data$margin <- abs(data$dv_c_t1)

data$bandwidth <- .25

#Overall incumbency effect
library(Design)
regress1 <- ols(dv_c_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data, subset=margin < bandwidth, method="qr", x=TRUE)
robcov(regress1)

#Incumbency only in midterm years
regress2 <- ols(dv_c_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data, subset=margin < bandwidth & t2_is_midterm==1, method="qr", x=TRUE)
robcov(regress2)

#Randomization Check
data$south <- 0
data$dwint2 <- 0
data$t2_frinc <- max(data$t2_repfrinc, data$t2_demfrinc)
data$t2_margin <- abs(data$dv_c_t2)
data$dwint2[data$dv_c_t2 > 0] <- 1

southern <- c('VA', 'DC', 'WV', 'KY', 'TN', 'NC', 'SC', 'GA', 'FL', 'AL', 'MS', 'LA', 'AR', 'OK', 'TX')

for (i in 1:length(southern)){
	data$south[data$statesabbrev==southern[i]] <- 1
	}
	
#Becomes less and less random as you go further from the threshold
regress3 <- lm(dwint2 ~ dwin + south, data=data, subset=(t2_is_midterm==1 & t2_margin < .01))	
summary(regress3)

regress4 <- lm(dwint2 ~ dwin + south, data=data, subset=(t2_is_midterm==1 & t2_margin < .015))	
summary(regress4)

regress5 <- lm(dwint2 ~ dwin + south, data=data, subset=(t2_is_midterm==1 & t2_margin < .02))	
summary(regress5)

#Midterm years no longer needed, drop; Also the 2008 observations have no presidential info
data$dropflag <- 0
data$dropflag[data$t2_is_midterm==1 | data$t2_year==2008] <- 1
data2 <- subset(data, dropflag==0, select= -dropflag)

#Display how many observations are left from each year
table(data2$t2_year)

#RD incumbency for presidential years
regress6 <- ols(dv_c_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < bandwidth, method="qr", x=TRUE)
robcov(regress6)

#fitted values and standard errors for RD incumbency for presidential years
incumbency1 <- predict(regress6, newdata=subset(data2, dv_c_t1<0), type=c("lp"), se.fit=TRUE)
data2$incumbency_yhat1 <- NA
data2$incumbency_yhat1[data2$dv_c_t1<0] <- incumbency1$linear.predictors
data2$incumbency_stderror1 <- NA
data2$incumbency_stderror1[data2$dv_c_t1<0] <- incumbency1$se.fit

incumbency2 <- predict(regress6, newdata=subset(data2, dv_c_t1>=0), type=c("lp"), se.fit=TRUE)
data2$incumbency_yhat2 <- NA
data2$incumbency_yhat2[data2$dv_c_t1>=0] <- incumbency2$linear.predictors
data2$incumbency_stderror2 <- NA
data2$incumbency_stderror2[data2$dv_c_t1>=0] <- incumbency2$se.fit

#RD reverse coattails
regress7 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < bandwidth, method="qr", x=TRUE)
robcov(regress7)

#fitted values/standard errors for RD reverse coattails
rvsctls1 <- predict(regress7, newdata=subset(data2, dv_c_t1<0), type=c("lp"), se.fit=TRUE)
data2$rvsctls_yhat1 <-NA
data2$rvsctls_yhat1[data2$dv_c_t1<0] <- rvsctls1$linear.predictors
data2$rvsctls_stderror1 <-NA
data2$rvsctls_stderror1[data2$dv_c_t1<0] <- rvsctls1$se.fit

rvsctls2 <- predict(regress7, newdata=subset(data2, dv_c_t1>=0), type=c("lp"), se.fit=TRUE)
data2$rvsctls_yhat2<-NA
data2$rvsctls_yhat2[data2$dv_c_t1>=0] <- rvsctls2$linear.predictors
data2$rvsctls_stderror2<-NA
data2$rvsctls_stderror2[data2$dv_c_t1>=0] <- rvsctls2$se.fit

#Try different bandwidths
regress8 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < .5, method="qr", x=TRUE)
robcov(regress8)

regress9 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < .25, method="qr", x=TRUE)
robcov(regress9)

regress10 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < .2, method="qr", x=TRUE)
robcov(regress10)

regress11 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < .15, method="qr", x=TRUE)
robcov(regress11)

regress12 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < .1, method="qr", x=TRUE)
robcov(regress12)

regress13 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=margin < .05, method="qr", x=TRUE)
robcov(regress13)

#Try different bandwidths - state FE
regress14 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev), data=data2, subset=margin < .5, method="qr", x=TRUE)
robcov(regress14)

regress15 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev), data=data2, subset=margin < .25, method="qr", x=TRUE)
robcov(regress15)

regress16 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev), data=data2, subset=margin < .2, method="qr", x=TRUE)
robcov(regress16)

regress17 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev), data=data2, subset=margin < .15, method="qr", x=TRUE)
robcov(regress17)

regress18 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev), data=data2, subset=margin < .1, method="qr", x=TRUE)
robcov(regress18)

regress19 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev), data=data2, subset=margin < .05, method="qr", x=TRUE)
robcov(regress19)

#Try different bandwidths - stateXyear FE
#
#R and Stata obtain the same coefficient estimates but the robust SEs are different. Due to issues in the R package Design with handling a large number of dummy variables, the estimates of robust standard errors for models including stateXyear fixed effects are extremely large. 
data2$stateyear <- paste(data2$statesabbrev,data2$t2_year,sep="")
data2$stateyear[data2$statesabbrev=='AK' | data2$t2_year==1952] <- "AA.AK.1952.ref"

regress20 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev) + as.factor(t2_year) + as.factor(stateyear), data=data2, method='qr', x=TRUE, subset=margin<.5)
robcov(regress20)

regress21 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev) + as.factor(t2_year) + as.factor(stateyear), data=data2, method='qr', x=TRUE, subset=margin<.25)
robcov(regress21)

regress22 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev) + as.factor(t2_year) + as.factor(stateyear), data=data2, method='qr', x=TRUE, subset=margin<.2)
robcov(regress22)

regress23 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev) + as.factor(t2_year) + as.factor(stateyear), data=data2, method='qr', x=TRUE, subset=margin<.15)
robcov(regress23)

regress24 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev) + as.factor(t2_year) + as.factor(stateyear), data=data2, method='qr', x=TRUE, subset=margin<.1)
robcov(regress24)

regress25 <- ols(dv_p_t2 ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4 + as.factor(statesabbrev) + as.factor(t2_year) + as.factor(stateyear), data=data2, method='qr', x=TRUE, subset=margin<.05)
robcov(regress25)


#Democratic Spending Advantage stats discussed briefly in a footnote
#ICF = inflation conversion factors
data2$t2_dexp <- data2$t2_dexp/data2$t2_icf
data2$t2_rexp <- data2$t2_rexp/data2$t2_icf

data2$t2_dspendadvantage <- data2$t2_dexp - data2$t2_rexp

lm1 <- ols(t2_dspendadvantage ~ dwin + dv_c_t1 + dv_c_t1_2 + dv_c_t1_3 + dv_c_t1_4 + i_dv_c_t1 + i_dv_c_t1_2 + i_dv_c_t1_3 + i_dv_c_t1_4, data=data2, subset=(margin < bandwidth & t2_missingrexp==0 & t2_missingdexp==0), method="qr", x=TRUE)
robcov(lm1)


#Variables used to create the graphs
data2$rvsctls_ciupper1 <- data2$rvsctls_yhat1 + 1.96*data2$rvsctls_stderror1
data2$rvsctls_cilower1 <- data2$rvsctls_yhat1 - 1.96*data2$rvsctls_stderror1
data2$rvsctls_ciupper2 <- data2$rvsctls_yhat2 + 1.96*data2$rvsctls_stderror2
data2$rvsctls_cilower2 <- data2$rvsctls_yhat2 - 1.96*data2$rvsctls_stderror2

data2$incumbency_ciupper1 <- data2$incumbency_yhat1 + 1.96*data2$incumbency_stderror1
data2$incumbency_cilower1 <- data2$incumbency_yhat1 - 1.96*data2$incumbency_stderror1
data2$incumbency_ciupper2 <- data2$incumbency_yhat2 + 1.96*data2$incumbency_stderror2
data2$incumbency_cilower2 <- data2$incumbency_yhat2 - 1.96*data2$incumbency_stderror2

data2$count <- 1
data2$bin   <- trunc(200*(data2$dv_c_t1+.005))/200

#Graph syntax
#fig. 1
belowthresh <- na.omit(cbind(data2$dv_c_t1, data2$dv_c_t2, data2$incumbency_yhat1, data2$incumbency_cilower1, data2$incumbency_ciupper1, data2$count, data2$bin))

margint1 <- belowthresh[,1]
margint2 <- belowthresh[,2]
margint2.1 <- belowthresh[,3]
margint2.1.l <- belowthresh[,4]
margint2.1.u <- belowthresh[,5]
count <- belowthresh[,6]
bin   <- belowthresh[,7]


belowthresh.fig1 <- as.data.frame(cbind(margint1, margint2, margint2.1, margint2.1.l, margint2.1.u, count, bin))

belowthresh.fig1 <- belowthresh.fig1[ sort.list(belowthresh.fig1$margint1) ,]

abovethresh <- na.omit(cbind(data2$dv_c_t1, data2$dv_c_t2, data2$incumbency_yhat2, data2$incumbency_cilower2, data2$incumbency_ciupper2, data2$count, data2$bin))

margint1 <- abovethresh[,1]
margint2 <- abovethresh[,2]
margint2.2 <- abovethresh[,3]
margint2.2.l <- abovethresh[,4]
margint2.2.u <- abovethresh[,5]
count <- abovethresh[,6]
bin   <- abovethresh[,7]

abovethresh.fig1 <- as.data.frame(cbind(margint1, margint2, margint2.2, margint2.2.l, margint2.2.u, count, bin))

abovethresh.fig1 <- abovethresh.fig1[ sort.list(abovethresh.fig1$margint1) ,]

#use bins to determine circle size
binmeans.b1 <- aggregate(belowthresh.fig1$margint2, by=list(belowthresh.fig1$bin), FUN=mean)
bincounts.b1 <- aggregate(belowthresh.fig1$count, by=list(belowthresh.fig1$bin), FUN=sum)
circfig1b <- as.data.frame(cbind(binmeans.b1[,1], binmeans.b1[,2], bincounts.b1[,2]))
names(circfig1b) <- c("bin", "mean", "count")

binmeans.a1 <- aggregate(abovethresh.fig1$margint2, by=list(abovethresh.fig1$bin), FUN=mean)
bincounts.a1 <- aggregate(abovethresh.fig1$count, by=list(abovethresh.fig1$bin), FUN=sum)
circfig1a <- as.data.frame(cbind(binmeans.a1[,1], binmeans.a1[,2], bincounts.a1[,2]))
names(circfig1a) <- c("bin", "mean", "count")

#plot
plot(data2$dv_c_t1, data2$dv_c_t2, type='n', xlim=c(-.15,.15), ylim=c(-.2,.2), main="Fig. 1", xlab="Democratic Congressional Margin of Victory Time 1", ylab="Democratic Congressional Margin of Victory Time 2")
abline(v=0, h=seq(-.2,.2,.05),col = "gray60")
points(circfig1b$bin, circfig1b$mean, col='red', cex=sqrt(circfig1b$count/(pi*10)))
points(circfig1a$bin, circfig1a$mean, col='blue', cex=sqrt(circfig1a$count/(pi*10)))
lines(belowthresh.fig1$margint1, belowthresh.fig1$margint2.1, col='red')
lines(belowthresh.fig1$margint1, belowthresh.fig1$margint2.1.l, col='red', lty=2)
lines(belowthresh.fig1$margint1, belowthresh.fig1$margint2.1.u, col='red', lty=2)
lines(abovethresh.fig1$margint1, abovethresh.fig1$margint2.2, col='blue')
lines(abovethresh.fig1$margint1, abovethresh.fig1$margint2.2.l, col='blue', lty=2)
lines(abovethresh.fig1$margint1, abovethresh.fig1$margint2.2.u, col='blue', lty=2)


#fig 2
belowthresh <- na.omit(cbind(data2$dv_c_t1, data2$dv_p_t2, data2$rvsctls_yhat1, data2$rvsctls_cilower1, data2$rvsctls_ciupper1, data2$count, data2$bin))

margint1 <- belowthresh[,1]
margint2 <- belowthresh[,2]
margint2.1 <- belowthresh[,3]
margint2.1.l <- belowthresh[,4]
margint2.1.u <- belowthresh[,5]
count <- belowthresh[,6]
bin   <- belowthresh[,7]

belowthresh.fig2 <- as.data.frame(cbind(margint1, margint2, margint2.1, margint2.1.l, margint2.1.u, count, bin))

belowthresh.fig2 <- belowthresh.fig2[ sort.list(belowthresh.fig2$margint1) ,]

abovethresh <- na.omit(cbind(data2$dv_c_t1, data2$dv_p_t2, data2$rvsctls_yhat2, data2$rvsctls_cilower2, data2$rvsctls_ciupper2, data2$count, data2$bin))

margint1 <- abovethresh[,1]
margint2 <- abovethresh[,2]
margint2.2 <- abovethresh[,3]
margint2.2.l <- abovethresh[,4]
margint2.2.u <- abovethresh[,5]
count <- abovethresh[,6]
bin   <- abovethresh[,7]

abovethresh.fig2 <- as.data.frame(cbind(margint1, margint2, margint2.2, margint2.2.l, margint2.2.u, count, bin))

abovethresh.fig2 <- abovethresh.fig2[ sort.list(abovethresh.fig2$margint1) ,]

#use bins to determine circle size
binmeans.b2 <- aggregate(belowthresh.fig2$margint2, by=list(belowthresh.fig2$bin), FUN=mean)
bincounts.b2 <- aggregate(belowthresh.fig2$count, by=list(belowthresh.fig2$bin), FUN=sum)
circfig2b <- as.data.frame(cbind(binmeans.b2[,1], binmeans.b2[,2], bincounts.b2[,2]))
names(circfig2b) <- c("bin", "mean", "count")

binmeans.a2 <- aggregate(abovethresh.fig2$margint2, by=list(abovethresh.fig2$bin), FUN=mean)
bincounts.a2 <- aggregate(abovethresh.fig2$count, by=list(abovethresh.fig2$bin), FUN=sum)
circfig2a <- as.data.frame(cbind(binmeans.a2[,1], binmeans.a2[,2], bincounts.a2[,2]))
names(circfig2a) <- c("bin", "mean", "count")

#plot fig 2
plot(data2$dv_c_t1, data2$dv_p_t2, type='n', xlim=c(-.15,.15), ylim=c(-.2,.2), main="Fig. 2", xlab="Democratic Congressional Margin of Victory Time 1", ylab="Democratic Presidential Margin of Victory Time 2")
abline(v=0, h=seq(-.2,.2,.05),col = "gray60")
points(circfig2b$bin, circfig2b$mean, col='red', cex=sqrt(circfig2b$count/(pi*10)))
points(circfig2a$bin, circfig2a$mean, col='blue', cex=sqrt(circfig2a$count/(pi*10)))
lines(belowthresh.fig2$margint1, belowthresh.fig2$margint2.1, col='red')
lines(belowthresh.fig2$margint1, belowthresh.fig2$margint2.1.l, col='red', lty=2)
lines(belowthresh.fig2$margint1, belowthresh.fig2$margint2.1.u, col='red', lty=2)
lines(abovethresh.fig2$margint1, abovethresh.fig2$margint2.2, col='blue')
lines(abovethresh.fig2$margint1, abovethresh.fig2$margint2.2.l, col='blue', lty=2)
lines(abovethresh.fig2$margint1, abovethresh.fig2$margint2.2.u, col='blue', lty=2)