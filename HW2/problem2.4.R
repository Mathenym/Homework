data(prostate, package = "faraway")

prostate = data.frame(prostate)

RSE = rep(0,8) # allows us to store results in list 

R2 = rep(0,8) # allows to store results in list. 

prostate_fit_lcavol = lm(lpsa ~ lcavol , data = prostate)


RSE[1] = summary(prostate_fit_lcavol)$sigma;RSE[1]
# = 0.787499
R2[1] = summary(prostate_fit_lcavol)$r.squared; R2[1] 
# = 0.5394319

prostate_fit_lweight = lm(lpsa ~ lcavol + lweight, data= prostate) #add lweight
RSE[2] = summary(prostate_fit_lweight)$sigma; RSE[2]
# = 0.7506469
R2[2] = summary(prostate_fit_lweight)$r.squared; R2[2] 
# = 0.5859345

prostate_fit_svi = lm(lpsa ~ lcavol + lweight + svi, data= prostate) # add svi
RSE[3] = summary(prostate_fit_svi)$sigma; RSE[3]
# = 0.7168094
R2[3] = summary(prostate_fit_svi)$r.squared; R2[3]
# = 0.624403

prostate_fit_lbph = lm(lpsa ~ lcavol + lweight + svi + lbph, data= prostate) # add lbph
RSE[4] = summary(prostate_fit_lbph)$sigma; RSE[4]
# 0.7108232
R2[4] = summary(prostate_fit_lbph)$r.squared; R2[4]
# = 0.6366035

prostate_fit_age = lm(lpsa ~ lcavol + lweight + svi + lbph + age, data= prostate)
RSE[5] = summary(prostate_fit_age)$sigma; RSE[5] 
R2[5] = summary(prostate_fit_age)$r.squared; R2[5]

prostate_fit_lcp = lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp, data= prostate)
RSE[6] = summary(prostate_fit_lcp)$sigma; RSE[6]
R2[6] = summary(prostate_fit_lcp)$r.squared; R2[6]

prostate_fit_pgg45 = lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45, data= prostate)
RSE[7] = summary(prostate_fit_pgg45)$sigma; RSE[7] 
R2[7] = summary(prostate_fit_pgg45)$r.squared; R2[7]

full_prostate_fit = lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45 + gleason, data= prostate)
RSE[8] = summary(full_prostate_fit)$sigma; RSE[8]
R2[8] = summary(full_prostate_fit)$r.squared; R2[8]


plot(RSE, xlab = "Variables", main="Residual Standard Error")

plot(R2, xlab = "Variables", main="R-Squared")



