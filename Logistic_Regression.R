#
source("Preprocessing.R") 



#remove unused variables, not done in preprocessing because these were needed for graphs
#values with no elements aka no mushroom of that type exisits will introduce bias in the logistic regression model.

which(sapply(mush2[2:74],sum)==0)#these columns have no contents
#gill.color_l 60 ring.type_c 63 ring.type_s 69 ring.type_y 71
#drop by NAME, the index is misleading 

which(colnames(mush2)%in% c("gill.color_l","ring.type_c","ring.type_s","ring.type_y"))
mush2=mush2[,-c(61,64,70,72)]


mush2$class=as.numeric(mush2$class)
(which(lapply(mush2[2:70],sum)==0))


mush2[c(2,(9:70))] = lapply(mush2[c(2,(9:70))], as.factor)

#mush4=mush2[-c(20),]
#Logistic regression

intercept= glm(class~(.),family = binomial(link = "logit"),mush2)
summary(intercept)

m_AIC=MASS::stepAIC(intercept,trace=F, k = 2) 
summary(m_AIC) #model is very large
anova(m_AIC,test="Chisq")
confint(m_AIC)

#BIC tends to produce models with fewer variables, since there are
#several columns, it is advantageous to have the BIC model

m_BIC=MASS::stepAIC(intercept,trace=F, k = log(173)) 

summary(m_BIC)
anova(m_BIC,test="Chisq")
confint(m_BIC)


#BIC model has a slightly higher AIC metric (~+9) but it has far fewer variables
#and included variables have a higher overall significance
#Therefore, BIC model is selected to move forward.

#regression effect with BIC model, p ->1.468e-07, indicates regression effect is present
anova(glm(formula = class ~ 1 , family = binomial(link = "logit"),mush2),
      m_BIC, test="Chi")

#Confidence interval for ring.type_z has no upper limit, estimate as well has
# a massive standard error

#test to drop ring.type_z: test p<-0.0004972 indicates ring.type_z does have a significant difference
#however, more investigation is needed before keeping it
anova(glm(formula = class ~ season_w + cap.shape_b + cap.color_n + 
                cap.color_r + stem.color_w + ring.type_z, family = binomial(link = "logit"), 
              data = mush2),glm(formula = class ~ season_w + cap.shape_b + cap.color_n + 
                                  stem.color_w , family = binomial(link = "logit"), 
                                data = mush2),test= "Chi")
#Ultimately, the graphs from exploratory data analysis indicate bias in ring.type_z
#the value is 
mush2[which(mush2$ring.type_z==1),]
mush2[which(mush2$ring.type_z==1&mush2$class==0),]

m_BIC2=glm(formula = class ~ season_w + cap.shape_b +  cap.color_n+
         cap.color_r + stem.color_w , family = binomial(link = "logit"), mush2)
summary(m_BIC2)

                                                                           
confint(m_BIC2,level=.9,trace=F)

confint(m_BIC,level=.65,trace=F)



res.P = residuals(m_BIC2, type="pearson")
res.D = residuals(m_BIC2, type="deviance") #or residuals(fit), by default
res = cbind(res.P, res.D)
colnames(res)= c("Pearson", "Deviance")


summary(res)

boxplot(res, main="Residuals Boxplots")



par(mfrow=c(1,2))
plot(m_BIC2$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(m_BIC2$fitted.values, res.P, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')
plot(m_BIC2$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(m_BIC2$fitted.values, res.D, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')
library(lawstat)
lawstat::runs.test(res.D, F)





#outliers/ influential point analysis
leverage = hatvalues(m_BIC2)

W = diag(m_BIC2$weights)
X = cbind(rep(1,nrow(mush2)), mush2[['season_w']], mush2[['cap.shape_b']],
          mush2[['cap.color_n']], mush2[['cap.color_r']], mush2[['stem.color_w']])
Hat = sqrt(W) %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% sqrt(W)
all(abs(leverage - diag(Hat)) < 1e-15)

plot(names(leverage), leverage, xlab="Index", type="h")
points(names(leverage), leverage, pch=16, cex=0.6)
text(susPts, leverage[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)

p <- length(coef(m_BIC2))
n <- nrow(mush2)
abline(h=2*p/n,col=2,lwd=2,lty=2)
infPts <- which(leverage>2*p/n)

# ** Cook's Distance ----------------

# high Cook's distance => influential points/outliers
# leverage points with high Cook's distance => suspicious influential points & outliers
#                    may need to be deleted -> check scatterplots

cooks = cooks.distance(m_BIC2)

plot(cooks, ylab="Cook's Distance", pch=16, cex=0.6)
points(infPts, cooks[infPts], pch=17, cex=0.8, col=2)
susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:3]))
text(susPts, cooks[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)

dispersion <- 1
all(abs(cooks - (res.P/(1 - leverage))^2 * leverage/(dispersion * p) < 1e-15))





mush2[c(20,30),c("season_w" ,"cap.shape_b" , "cap.color_n",    "cap.color_r" ,"stem.color_w")]
mush6=mush2[-c(20,30),]

#dropped data model
x_r = glm(formula = class ~ season_w + cap.shape_b +  cap.color_n+
            cap.color_r + stem.color_w , family = binomial(link = "logit"), mush6)

summary(x_r)


res.P = residuals(x_r, type="pearson")
res.D = residuals(x_r, type="deviance") #or residuals(fit), by default
res = cbind(res.P, res.D)
colnames(res)= c("Pearson", "Deviance")


summary(res)

boxplot(res, main="Residuals Boxplots")



par(mfrow=c(1,2))
plot(x_r$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(x_r$fitted.values, res.P, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')
plot(x_r$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(x_r$fitted.values, res.D, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')

#A slight inprovement in fit, nothing drastic







