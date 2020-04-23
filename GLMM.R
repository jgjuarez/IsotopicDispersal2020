library(lme4)

dispersal$condition<-relevel(wily$condition,ref="Unfed")

model1<-glmer(enriched~condition*distance*Community+(1|week)+(1|houseID),data=wily,family=binomial)

model2<-glmer(enriched~condition*distance+Community+(1|week)+(1|houseID),data=wily,family=binomial)

model3<-glmer(enriched~condition*distance+(1|Community)+(1|week)+(1|houseID),data=wily,family=binomial)

model4<-glmer(enriched~condition*distance+(1|Community:houseID)+(1|week)+(1|houseID),data=wily,family=binomial)

model5a<-glmer(enriched~condition*distance+(1|week)+(1|houseID),data=wily,family=binomial)
model51a<-glmer(enriched~condition*distance+(week)+(1|houseID),data=wily,family=binomial)
model52a<-glmer(enriched~condition*distance+as.factor(week)+(1|houseID),data=wily,family=binomial)

model6<-glmer(enriched~condition*distance+(1|houseID),data=wily,family=binomial)

AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5a)
AIC(model51a)
AIC(model52a)
AIC(model6)

drop1(model51a)

summary(model51a)

> AIC(model1)
[1] 641.5532
> AIC(model2)
[1] 643.5811
> AIC(model3)
[1] 645.5998
> AIC(model4)
[1] 645.5998
> AIC(model5a)
[1] 643.5998
> AIC(model51a)
[1] 632.2852
> AIC(model52a)
[1] 639.7839
> AIC(model6)
[1] 644.1756


> summary(model51a)
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: enriched ~ condition * distance + (week) + (1 | houseID)
   Data: wily

     AIC      BIC   logLik deviance df.resid 
   632.3    673.0   -308.1    616.3     1191 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.3556 -0.2972 -0.2246 -0.1600  8.5261 

Random effects:
 Groups  Name        Variance Std.Dev.
 houseID (Intercept) 0.6589   0.8117  
Number of obs: 1199, groups:  houseID, 56

Fixed effects:
                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)              -8.871248   1.915130  -4.632 3.62e-06 ***
conditionGravid           0.666936   0.744419   0.896 0.370298    
conditionMale            -0.595723   0.562568  -1.059 0.289629    
distance                 -0.004987   0.002637  -1.891 0.058558 .  
week                      0.163749   0.043162   3.794 0.000148 ***
conditionGravid:distance -0.006508   0.004502  -1.446 0.148273    
conditionMale:distance    0.005775   0.002708   2.133 0.032955 *  
---
Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1



