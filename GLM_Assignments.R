require(dplyr)
library(tidyverse)
require(car)
require(MuMIn)
require(nlme)
options(na.action='na.fail')

EIA<-read.csv('EIA.csv')

EIA$density<-EIA$count/EIA$area
EIA$tidestate <- as.factor(EIA$tidestate)
attach(EIA)

# first group the data by gridcodes and find the mean density for each cell
newdata<-group_by(EIA, GridCode)%>%
  summarise(x.pos=first(x.pos), y.pos=first(y.pos), area=first(area), density=mean(density))
# pick a nice colour scheme
col<-colorRampPalette(rev(rgb(c(231,117,27),c(41,112,158),c(138,179,119),max=255)))(100)
# plot the data
p<-ggplot(newdata)
p<-p + geom_tile(aes(x=x.pos, y=y.pos, fill=density, height=1000, width=1000)) +
  scale_fill_gradientn(colours=col, space="Lab", na.value="grey50", guide="colourbar")
p + theme_bw() + coord_equal()


fit.full<- lm(density ~ tidestate + observationhour + DayOfMonth +
                MonthOfYear + impact + Year + x.pos + y.pos, data=EIA)
# month as a factor
fit.full.fac<- lm(density ~ tidestate + observationhour + DayOfMonth +
                    as.factor(MonthOfYear) + impact + Year +
                    x.pos + y.pos, data=EIA)
summary(fit.full)
summary(fit.full.fac)


select_model2 <- AIC(fit.full, fit.full.fac)
select_model2

summary(fit.full.fac)

#Calculating collinearity using VIF function
vif(fit.full.fac)


#remove the impact term from the model 

fit.fullfac.noimp <- lm(density ~ tidestate + observationhour + DayOfMonth +
                          as.factor(MonthOfYear) + Year +
                          x.pos + y.pos, data=EIA)


fit.interac <- lm(density ~ tidestate + observationhour + DayOfMonth +
                          as.factor(MonthOfYear) + Year +
                          x.pos + y.pos + Year:x.pos + Year:y.pos, data=EIA)


Anova(fit.interac)

#Performing a step wise model optimization using AIC
step(fit.interac,direction="both")

#Now do all possible subsets selection using the dredge function and the default, AICc
options(na.action='na.fail')
head(dredge(fit.interac))


#Using BIC to find the best model.
step(fit.interac, direction = "both", k=log(nrow(EIA)))


Aic.model <- lm(formula = density ~ tidestate + observationhour + as.factor(MonthOfYear) + 
                  Year + x.pos + y.pos + Year:x.pos, data = EIA)

plot(Aic.model)
Aic.model

#Calculating the error variance of the model.
var(Aic.model$residuals)
ncvTest(Aic.model)

durbinWatsonTest(Aic.model)
hist(Aic.model$fitted.values, Aic.model$residuals)
Aic.model$residuals


EIA$sqrtdensity<-sqrt(density)
fit.gls<-gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos +
               MonthOfYear + impact:x.pos + impact:y.pos, data = EIA, method='ML')


fit.exp.gls <- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos +
              MonthOfYear + impact:x.pos + impact:y.pos, data = EIA, weights = varExp(), method='ML')

fit.power.gls <- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos +
                     MonthOfYear + impact:x.pos + impact:y.pos, data = EIA, weights = varPower(), method='ML')



plot(fitted(fit.exp.gls), residuals(fit.exp.gls, type='response'))
cut.fit<-cut(fitted(fit.exp.gls), breaks=quantile(fitted(fit.exp.gls), probs=seq(0,1,length=20)))
means1<- tapply(fitted(fit.exp.gls), cut.fit, mean)
var_exp <- tapply(residuals(fit.exp.gls), cut.fit, var)

fitted_val <-(summary(fit.exp.gls)$sigma^2) * exp(2 *coef(fit.exp.gls$model) * means1)


df <- data.frame(means1, var_exp, fitted_val)
df

df %>% ggplot() + aes(x = means1, y = var_exp) + geom_point() +
                        geom_line(aes(means1, fitted_val), color = "red") + xlab("fitted values") + ylab("Residual Variance") +
                        ggtitle("Mean-Variance Relationship plot") + ylim(0, 4) + theme(plot.title = element_text(hjust=0.5))


## Using ACF plot to visualize model correlation.
par(mfrow=c(1,2))
acf(residuals(fit.exp.gls, type='response'))
acf(residuals(fit.exp.gls, type='normalized'))
par(mfrow=c(1,1))


#
EIA$block<-paste(Year, MonthOfYear, DayOfMonth, GridCode, sep='')
require(dplyr)
EIA2<-arrange(EIA, block, Year, MonthOfYear, DayOfMonth, GridCode)


#Creating an AR1 model
workingModel_Glscorr <- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos +
                             MonthOfYear + impact:x.pos + impact:y.pos, data = EIA, weights = varExp(),
                             correlation=corAR1(form = ~1|block), method='ML')


workingModel_Glscorr_2 <- update(workingModel_Glscorr, 
                                 corr = corARMA(p=2,  q=0, form = ~ 1 | block))


AIC(workingModel_Glscorr, workingModel_Glscorr_2)

acf(residuals(workingModel_Glscorr, type='normalized'))
acf(residuals(workingModel_Glscorr_2, type='normalized'))



#Use hypothesis testing (F-tests) for backwards model for best model.
Anova(workingModel_Glscorr_2)

#Selecting significant data our final variable
final_model <- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos,
                            data = EIA, weights = varExp(),
                            corr=corARMA(p=2, q=0, form = ~ 1| block), method='ML')



#Creating a dataframe for our prediction.

preds <- data.frame(
  tidestate = factor(c("SLACK", "SLACK"), levels = c("EBB", "FLOOD", "SLACK")),
  observationhour = c(10, 10),
  MonthOfYear = c(6, 6),
  x.pos = c(1500, 1500),
  impact = c(0, 1)
)


myprediction<-MuMIn:::predict.gls(final_model, newdata = preds, se.fit=TRUE)
myprediction$fit**2

without_impact <- myprediction$fit[1]
without_impact.se <- myprediction$se.fit[1]
after_impact <- myprediction$fit[2]
after_impact.se <- myprediction$se.fit[2]


#18 95% confidence interval for the before impact
confint.without_impact <- c(without_impact - without_impact.se *qnorm(1- 0.05/2),
                            without_impact + without_impact.se * qnorm(1-0.05/2))

confint.without_impact <- confint.without_impact **2
confint.without_impact


#18 95% confidence interval for the after impact
confint.after_impact <- c(after_impact - after_impact.se *qnorm(1- 0.05/2),
                            after_impact + after_impact.se * qnorm(1-0.05/2))

confint.after_impact <- confint.after_impact **2
confint.after_impact
