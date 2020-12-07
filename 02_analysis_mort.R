### Statistical analysis for baseline mortality in Norwegian salmon farming ###
# Author(s): Victor H. S. Oliveira, Katharine R. Dean, Lars Qviller, Carsten Kirkeby and Britt Bang Jensen

# Loading required packages
library(glmmTMB)
library(performance)
library(DHARMa)

# Final model
m <- glmmTMB(d.count ~ poly(temp, 2) + salinity + zone + poly(w.fish,2) + start.mth +
                 w.start.fish + med.treat + nonmed.treat + (1|location) + offset(log(ar.count)), 
               data= dfanalysis, family = nbinom2)

# Tables of coefficients and significance for m
summary(m) 
Anova (m) # Wald test
LRTm15 <- drop1(m15, test = "Chisq") # LRT test 
round(LRTm15$`Pr(>Chi)`, 3) # as shown in table S2

# checking multicollinearity
check_collinearity(m) 
plot(check_collinearity(m))

# Calculation intra-correlation coefficient 
icc_m <- icc(m) 
iccm_m

# Calculating all profile likelihood CI, as shown in table S2
ci_m <- confint(m, method = "profile") 
ci_m
round(ci_m,3) # rounded ci 
round(ci_m[27,]*50,3) # rounded ci for start weight for every 50g change
round(exp(ci_m),3) 

# Values for a mortality rate ratio tables 
round(exp(confint(m)),4) 

# Model diagnostics

# We refer to the following vignette https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

# Simulating and plotting standardized residuals versus predicted values. 
res <- simulateResiduals(fittedModel = m, n = 1000)

plotQQunif(res, testUniformity = F, testOutliers = F)
plotResiduals(res, rank = T)

plotResiduals(res, dfanalysis$nonmed.treat)
plotResiduals(res, dfanalysis$med.treat)
plotResiduals(res, dfanalysis$temp)
plotResiduals(res, dfanalysis$w.fish)
plotResiduals(res, dfanalysis$salinity)
plotResiduals(res, dfanalysis$zone)
plotResiduals(res, dfanalysis$start.mth)
plotResiduals(res, dfanalysis$w.start.fish)

# Checking confounding effects. Here we only show the cases when potential confounding effects were detected.

# A model without salinity
m_conf <- glmmTMB(d.count ~ poly(temp, 2) + zone + poly(w.fish,2) + start.mth +
                    w.start.fish + med.treat + nonmed.treat + (1|location) + offset(log(ar.count)), 
                  data= dfanalysis, family = nbinom2)
summary(m_conf)
# Cases when changes in the estimates were above 20%
((m$fit$par[5] - m_conf$fit$par[4])/m$fit$par[5])*100 # Potential confounding effects, zone 1 Pr(>|z|) > 0.05
((m$fit$par[27] - m_conf$fit$par[26])/m$fit$par[27])*100 # Potential confounding effects, w.start.fish Pr(>|z|) > 0.05


