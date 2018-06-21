#Problem 2.6

library(faraway)
data("cheddar")

# Part (a) 

#Fit refresion model with taste as the response and the three chemical contents as predictors 

fit <- lm(taste~Acetic+H2S+Lactic,data = cheddar)
summary(fit)

'''
lm(formula = taste ~ Acetic + H2S + Lactic, data = cheddar)

Residuals:
  Min      1Q  Median      3Q     Max 
-17.390  -6.612  -1.009   4.908  25.449 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept) -28.8768    19.7354  -1.463  0.15540   
Acetic        0.3277     4.4598   0.073  0.94198   
H2S           3.9118     1.2484   3.133  0.00425 **
  Lactic       19.6705     8.6291   2.280  0.03108 * 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.13 on 26 degrees of freedom
Multiple R-squared:  0.6518,	Adjusted R-squared:  0.6116 
F-statistic: 16.22 on 3 and 26 DF,  p-value: 3.81e-06
'''
 
  # The regression Coefficeints are: 
  # 0.3277(Acetic), 2.9118(H2S), 19.6795(Lactic)

# Part (b)

#Compute the correlation between the fited values and the response. Square it.Identify where this value appears in the reression output. 
# should be R^2 value.  as R^2 = cor(y,y^)^2

cor(fitted(fit), cheddar$taste)^2
# = 0.651, which is the R^2 value


# Part (c) 

#Fit the same regression model but without an intercept term. 
#What is the value of R2 reported in the output? Is it equal to the squared correlation 
#between fitted values and response for this model? 
#Compute a more reasonable measure of the goodness of fit for this model.

fit_no_int <- lm(taste ~ Acetic + H2S + Lactic - 1, data=cheddar)
summary(fit_no_int)
'''
Call:
  lm(formula = taste ~ Acetic + H2S + Lactic - 1, data = cheddar)

Residuals:
  Min       1Q   Median       3Q      Max 
-15.4521  -6.5262  -0.6388   4.6811  28.4744 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
Acetic   -5.454      2.111  -2.583  0.01553 *  
  H2S       4.576      1.187   3.854  0.00065 ***
  Lactic   19.127      8.801   2.173  0.03871 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.34 on 27 degrees of freedom
Multiple R-squared:  0.8877,	Adjusted R-squared:  0.8752 
F-statistic: 71.15 on 3 and 27 DF,  p-value: 6.099e-13
'''

# The R^2 value = 0.887 which is different from the previous regression. Will use the correlation 
# method to compute a better goodness of fit. 

cor(fitted(fit_no_int), cheddar$taste)^2

# = 0.6524 Which is a better measure for the goodness of fit. 