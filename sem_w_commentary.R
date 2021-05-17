#run this every time
read.csv(Grade230.txt)
library(dplyr)
library(car) 
library(Hmisc)
library(psych)
library(scatterplot3d)
library(MASS)
library(lavaan)
library(corrplot)
library(semPlot)
setwd('D:/Downloads/Handout 1 - Regression')
#regression using 2 predictors
grades <- read.fortran("GRADE230.txt", c("F3.0", "F5.0", "F24.0", "7F2.0", "F3.0", "2F2.0", "F4.0", "4F2.0", "F4.0"))
names(grades) <- c("case", "sex", "T1", "PS1", "PS2", "PS3", "PS4", "PS5", "PS6", "PS7", "T2", "PS8", "PS9", "T3", "PS10","PS11", "PS12", "PS13", "T4")
grades.sub <- subset(grades, grades$case < 21 & grades$case != 9)
grades.sub
grades.test <- dplyr::select(grades.sub, c(T1, T2, T4))
describe(grades.test)
grades.cor <- cor(grades.test, use = "pairwise.complete.obs", method = "pearson")
grades.cor 
rcorr(as.matrix(grades.test))
write.csv(grades.cor, "corroutPW.csv")
#deletes the missing data in an easier way than pairwise, leads to less bias
grades.cor <- cor(grades.test, use = "complete.obs", method = "pearson")
grades.cor
write.csv(grades.cor, "corroutLW.csv")
#test 4 is it predicted by 1 & 2
model <- lm(T4 ~ T1 + T2, data = grades.test)
anova(model)
summary(model)
grades.test$Predicted <- predict(model)
grades.test$Residuals <- residuals(model)
grades.test$leverage <- hatvalues(model)
grades.test$distance <- studres(model)
grades.test$dffits <- dffits(model) 
grades.test$dfbetas <- dfbetas(model) 
vif(model)
influence.measures(model)
model.cov <- vcov(model)
grades.cov <- cov(grades.test)
scatterplot(T4 ~ T1, data = grades.test)
scatterplot(Residuals ~ T1, data = grades.test)
model.t4 <- lm(T4 ~ T2, data = grades.test)
grades.test$model.t4resid <- residuals(model.t4)
model.t1 <- lm(T1 ~ T2, data = grades.test)
grades.test$model.t1resid <- residuals(model.t1)
model.final <- lm(model.t4resid ~ model.t1resid, data = grades.test)
anova(model.final)
#above gives df, ss, ms, f-statstic, found that it was signficant
#below is another regression, dependent on predictor, not dependent on dependent variable
model.leverage <- lm(case ~ T1 + T2, data = grades.sub)
grades.sub$leverage <- hatvalues(model.leverage)
anova(model.leverage)
summary(model.leverage)
#results not sig
scatterplot(leverage ~ case, data = grades.sub)
s3d <- scatterplot3d(grades.test$T1, grades.test$T2, grades.test$T4, 
                     pch = 16, highlight.3d = TRUE, type = "h", main = "3D Scatterplot")
s3d$plane3d(model)
cormatlw <- read.csv("corroutLW.csv", row.names = 1)
cormatlw <- data.matrix(cormatlw)
model.cor <- setCor(y = "T4", x = c("T1", "T2"), data = cormatlw, 
                    n.obs = 18, std = FALSE)
summary(model.cor)
#looks like a little loop
# 2ND TUTORIAL: on matrix algebra
install.packages('fortran')

A <- read.fortran("MatrixA.txt", c("6F5.3"))
names(A) <- c("a1", "a2", "a3", "a4", "a5", "a6")
A <- data.matrix(A)
A
B <- matrix(c(1, 2, 3, 4, 5, 6), 3, 2, byrow = TRUE)
C <- matrix(c(5, 3, 5, 4, 9, 6), 3, 2, byrow = TRUE)
D <- matrix(c(5, 4, 4, 9), 2, 2, byrow = TRUE)
#e and f are vectors? 
E <- matrix(c(2, 3, 4, 5, 9, 6), 6, 1, byrow = TRUE)
F <- matrix(c(2, 3, 4, 5, 9, 6), 1, 6, byrow = TRUE)
G <- matrix(c(2, 3, 5, 3, 9, 6, 5, 9, 1), 3, 3, byrow = TRUE)
#algebra part, finding inverse, determinant?, and trace
Ainv <- solve(A)
Adet <- det(A)
Atrace <- sum(diag(A))
Ainv
#tutorial 2 doesn't seem like it is important for what I will be using SEM for
Adet
Atrace
H <- B + C 
I <- B - C  
J <- B %*% D 
Etrans = t(E) 
Etrans
K <- F %*% Ainv %*% t(F)
L <- D * 3
L
Aeigen <- eigen(A) 
O <- kronecker(D, G) 
Aeigen$val

#TUTORIAL 3: PATH ANALYSIS, will be using for my own research
socdata <- read.fortran("pathmodel example 3.txt", c("F12.0", 
                                                     "3F13.0"))
names(socdata) <- c("Behavior", "Snorm", "Belief", "Intent")
describe(socdata)
model.reduced.1 <- lm(Intent ~ Belief + Snorm, socdata)
summary(model.reduced.1)
model.reduced.2 <- lm(Behavior ~ Intent, socdata)
summary(model.reduced.2)
model.full.1 <- lm(Behavior ~ Intent + Snorm + Belief, socdata)
summary(model.full.1)

#tutorial 4: lavaan and path analysis, uses bootstrapping=helps us estimate the indiret effects
socdata <- read.fortran("pathmodel example 3.txt", c("F12.0", 
                                                     "3F13.0"))
socdata <- read.table("pathmodel example 3.txt")  
#naming columns
names(socdata) <- c("Behavior", "Snorm", "Belief", "Intent")
#stats on data
describe(socdata)
#the beta* fixes the parameters..but why are thye being fixed what is wrong with them
#=~ measured by, ~ regressed on, ~~ correlated with, ~1 intercept
#so behavior is regressed on by intenet and storm, intent regressed on by belief and snorm, belief is correlated with snorm
model.reduced <- "Behavior ~ Intent + 0*Snorm
Intent ~ Belief + Snorm
Belief ~~ Snorm"

#why won't it say that model.reduced not found after i ran the line before??, quotation mark was missing
model.reduced.fit <- sem(model.reduced, estimator = "ML", data = socdata)
#print results, all p-values are = 0
summary(model.reduced.fit, fit.measures = TRUE)
semPaths(model.reduced.fit, "par", edge.label.cex = 1.2, fade = FALSE)
model.full <- "Behavior ~ b*Intent + Snorm + Belief
Intent ~ Snorm + a*Belief
Belief ~~ Snorm
Behavior ~ 1
Belief ~ 1
Intent ~ 1
Snorm ~ 1
#calculate indirect effect. a* is where our predictor predicts the mediating variable. 
#b* is where our mediator predicts the final dependent variable.
ab := a*b"

model.full.fit <- sem(model.full, se = "bootstrap", test = "bootstrap", 
                      data = socdata) 
summary(model.full.fit, fit.measures = TRUE)
#final graph that has boostrapping and shows indirect effects
semPaths(model.full.fit, "par", edge.label.cex = 1.2, fade = FALSE)
#starting values for parameters
inspect(model.full.fit, "start")

#free parameters, free means estimated, does not look too different aside from the psi value
inspect(model.full.fit, "free")

#covariance matrix tutorial 4: useful for data that is not raw
socdata.cov <- cov(socdata)
socdata.cov 
lower <- "4.996093 
          4.164750 4.068619 
          1.468777 1.387127 0.9156302 
          1.601469 1.572911 0.3958586 1.0225483"
test.cov <- getCov(lower, names = c("Behavior", "Snorm", "Belief", 
                                    "Intent"))
model.fromcov <- "Behavior ~ b*Intent + Snorm + Belief
Intent ~ Snorm + a*Belief
Belief ~~ Snorm
Behavior ~ 1
Belief ~ 1
Intent ~ 1
Snorm ~ 1
#calculate indirect effect. a* is where our predictor predicts the mediating variable. 
#b* is where our mediator predicts the final dependent variable.
ab := a*b"
fit.fromcov <- sem(model.fromcov, sample.cov = test.cov, sample.nobs = 180)
summary(fit.fromcov)

#TUTORIAL 5: what is a latent factor
library(GPArotation)
efadat <- read.fortran("hbmpre1.txt", c("F5.0", "9F5.0"), na.strings = 99)
names(efadat) <- c("id", "i1", "i2", "i3", "i4", "i5", "i6", 
                   "i7", "i8", "i9")
efamat <- cor(efadat[, 2:10], use = "complete.obs")
#tells us which factors to use and the cutoffs to use
fa.parallel(efamat, n.obs = 615)
#different way to select cutoffs for data, uses MAP: minimum average partial
vss(efamat, n.obs = 615, rotate = "varimax", diagonal = FALSE, 
    fm = "minres")

#factor extraction, PCA which is similar to what we did in the RSQA?
components.norotate <- principal(efamat, nfactors = 2, rotate = "none")
components.norotate
#PAF, assumes errors unlike PCA, looking at 2 different factors
factor.2.norotate <- fa(efamat, nfactors = 2, n.obs = 615, rotate = "none", 
                        fm = "pa")
factor.2.norotate

#orthogonal rotation
#why do we rotate: makes outputs easier to interpret and understand, loading highly >0.5 and low is .2 or lower
factor.2.orthog <- fa(efamat, nfactors = 2, n.obs = 615, rotate = "varimax", 
                      fm = "pa")
factor.2.orthog 
#diff type of oblique rotation, latent factors correlate
factor.2.oblique <- fa(efamat, nfactors = 2, n.obs = 615, rotate = "oblimin", 
                       fm = "pa")

factor.2.oblique 

#TUTORIAL 6: CONFIRMATORY FACTOR ANALYSIS USING LAVAAN
cfadat <- read.fortran("nep modified.dat", c("1X", "6F4.0"))
cfadat <- read.table("nep modified.dat")
names(cfadat) <- c("nep1", "nep2", "nep6", "nep7", "nep11", "nep12")
cfa.model <- "F1 =~ NA*nep1 + nep6 + nep11 #make 3 indicator latent F1
             F2 =~ NA*nep2 + nep7 + nep12 #make 3 indicator latent F2
             F1 ~~ F2 #correlate F1 with F2
             F1 ~~ 1*F1 #fix factor variance to 1
             F2 ~~ 1*F2 #fix factor variance to 1"

cfa.fit <- cfa(cfa.model, data = cfadat)
summary(cfa.fit, fit.measures = TRUE)
semPaths(cfa.fit, "par", edge.label.cex = 1.2, fade = FALSE)
cfa.model.nocorr <- "F1 =~ NA*nep1 + nep6 + nep11
             F2 =~ NA*nep2 + nep7 + nep12
             F1 ~~ 0*F2
             F1 ~~ 1*F1 #fix factor variance to 1
             F2 ~~ 1*F2 #fix factor variance to 1"
cfa.fit.nocorr <- cfa(cfa.model.nocorr, data = cfadat)
summary(cfa.fit.nocorr, fit.measures = TRUE)
semPaths(cfa.fit.nocorr, "par", edge.label.cex = 1.2, fade = FALSE)
anova(cfa.fit, cfa.fit.nocorr)
cfa.model.marker <- "F1 =~ 1*nep1 + nep6 + nep11 #make 3 indicator latent F1 with nep1 as marker
             F2 =~ 1*nep2 + nep7 + nep12 #make 3 indicator latent F2 with nep2 as marker
             F1 ~~ F2 #correlate F1 with F2"
cfa.fit.marker <- cfa(cfa.model.marker, data = cfadat)
summary(cfa.fit.marker, fit.measures = TRUE)
semPaths(cfa.fit.marker, "par", edge.label.cex = 1.2, fade = FALSE) 

#using psych to do this, looking at reliability, this comes after factor structure is established
factor1 <- select(cfadat, nep1, nep6, nep11)
factor2 <- select(cfadat, nep2, nep7, nep12)
alpha(factor1)
alpha(factor2)

#tutorial 7:
semdat <- read.table("EXMP8MATRIX.txt")
names(semdat) <- c("Sex", "Pa_Ed", "Ma_Ed", "Pa_Occ", "Ma_Occ", 
                   "Income", "Accept", "Listen", "Commun", "Openness", "Patience", 
                   "Act_Out", "Agress", "Hostile")
head(semdat)
sem.model.measurement <- "SES =~ 1*Pa_Ed + Ma_Ed + Pa_Occ + Ma_Occ + Income 
#make 5 indicator latent socioeconomic status (SES) factor with parents education variable as the marker
COM =~ 1*Accept + Listen + Commun + Openness + Patience 
#make 5 indicator latent communication factor with accept variable as the marker
Conduct =~ 1*Act_Out + Agress + Hostile 
#make 3 indicator latent conduct factor with acting out variable as the marker"
sem.fit.measurement <- sem(sem.model.measurement, data = semdat)
summary(sem.fit.measurement, fit.measures = TRUE)
semPaths(sem.fit.measurement, "par", edge.label.cex = 1.2, fade = FALSE)
sem.model.full <- "SES =~ 1*Pa_Ed + Ma_Ed + Pa_Occ + Ma_Occ + Income #make 5 indicator latent socioeconomic status (SES) factor with parents education variable as the marker
             COM =~ 1*Accept + Listen + Commun + Openness + Patience; #make 5 indicator latent communication factor with accept variable as the marker
            Conduct =~ 1*Act_Out + Agress + Hostile #make 3 indicator latent conduct factor with acting out variable as the marker
COM ~ Sex + SES #Regress COM on Sex and SES
Conduct ~ Sex + COM #Regress Conduct on Sex and COM"
sem.fit.full <- sem(sem.model.full, data = semdat)
summary(sem.fit.full, fit.measures = TRUE) 
semPaths(sem.fit.full, "par", edge.label.cex = 1.2, fade = FALSE)
semPaths(sem.fit.full, "par", edge.label.cex = 1.2, fade = FALSE) 
sem.model.full.1free <- "SES =~ 1*Pa_Ed + Ma_Ed + Pa_Occ + Ma_Occ + Income #make 5 indicator latent socioeconomic status (SES) factor with parents education variable as the marker
             COM =~ 1*Accept + Listen + Commun + Openness + Patience; #make 5 indicator latent communication factor with accept variable as the marker
            Conduct =~ 1*Act_Out + Agress + Hostile #make 3 indicator latent conduct factor with acting out variable as the marker
COM ~ Sex + SES #Regress COM on Sex and SES
Conduct ~ Sex + COM + SES #Regress Conduct on Sex, COM, and SES"
sem.fit.full.1free <- sem(sem.model.full.1free, data = semdat)
summary(sem.fit.full.1free, fit.measures = TRUE)
semPaths(sem.fit.full.1free, "par", edge.label.cex = 1.2, fade = FALSE) 
anova(sem.fit.full, sem.fit.full.1free)



