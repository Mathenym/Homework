'''
Problem 2.1 
'''

rm(list=ls(all=TRUE)) 
library(faraway)
data(teengamb)
teengamb$sex <- factor(teengamb$sex)
attach(teengamb)
teengamb[1:3,]


gamb.lm <- lm(gamble ~ sex+status+income+verbal) #how does it treat factor veriables?
summary(gamb.lm)


Call:
lm(formula = gamble ~ sex + status + income + verbal)

Residuals:
Min      1Q  Median      3Q     Max 
-51.082 -11.320  -1.451   9.452  94.252 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  22.55565   17.19680   1.312   0.1968    
sex1        -22.11833    8.21111  -2.694   0.0101 *  
status        0.05223    0.28111   0.186   0.8535    
income        4.96198    1.02539   4.839 1.79e-05 ***
verbal       -2.95949    2.17215  -1.362   0.1803    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 22.69 on 42 degrees of freedom
Multiple R-squared:  0.5267,	Adjusted R-squared:  0.4816 
F-statistic: 11.69 on 4 and 42 DF,  p-value: 1.815e-06
'''

# Part (a)

#What percentage of variation is the response is explained by these predictors?
    # R^2 = 0.5267 

# Part (b)

#Which observation has the largest (positive) residual? Give the case number.

gamb.lm$residuals; max(gam.lm$residuals);
    # the 24th observation has the largest residual.

# Part (c)

#mean and median of the residuals
mean(gamb.lm$residuals)
    # The mean is -3.065e-17. So virually zero.

median(gamb.lm$residuals)
    # The median is -1.4513

# Part (d)

#correlation of the residuals with the fitted values
fitted_value <- gamble - gamb.lm$residuals
cor(gamb.lm$residuals, fitted_value)
    # The correlation between the residuals and the fitted values is: -1.007


# Part (e)

#correlation of the residuals with the income
cor(gamb.lm$residuals, income)
    # The correlation between the residuals and the income is -7.242e-17 (zero)

# Part (f)

#or all other predictors held constant, what would be the difference in predicted expenditure on gambling for a male compared to a female?
    # Since the correlation of the between the sex is -22.11, the result, on average, means that teenage femalepend $21.12 less on gambling than teenage males.
