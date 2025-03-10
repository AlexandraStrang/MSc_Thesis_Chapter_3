
# Model to predict the number of breeding pairs of Adélie penguins using the area of guano for four colonies in the Ross Sea
# Creator: Alexandra Strang

sessionInfo() # for citing package versions
citation() # for citing packages

#####################################################################################################################
# Read in data
#####################################################################################################################

X5_masterdata <- read.csv("5.0_masterdata.csv")
View(X5_masterdata)
head(X5_masterdata)

Dataset.5.0 <- X5_masterdata

# Add colour to sites 
colours<-c(BIRD="blue",CROZ="red",INEX="orange",ROYD="green")

#####################################################################################################################
# Log transform guano area (GA) and breeding pairs (BP)
#####################################################################################################################

Dataset.5.0$logGuano_area <- log(Dataset.5.0$Guano_area)

Dataset.5.0$logBP <- log(Dataset.5.0$BP)

# Extracting only needed variables
Dataset.5.1 <- Dataset.5.0[,c("logBP", "logGuano_area", "Site_ID", "Colony")]

# Remove NAs
Dataset.5.2 <- na.omit(Dataset.5.1)

View(Dataset.5.2)

#####################################################################################################################
# 1. What factors influence the prediction of breeding pairs using guano area
#####################################################################################################################

# Use GA as the dependent variable to reflect how the data are generated in the system

# Linear model with GA as dependent variable and BP as the predictor variable

New.model <- lm(Dataset.5.2$logGuano_area ~ Dataset.5.2$logBP)

summary(New.model)

#####################################################################################################################
# View relationship between BP and GA
#####################################################################################################################

library(ggplot2)

# Extract R squared
r2 <- round(summary(New.model)$r.squared, 2)

# Plot relationship between BP and GA
Trend_plot <- ggplot(Dataset.5.2, aes(x = logBP, y = logGuano_area, colour = Site_ID)) + 
  geom_point(size=3) + 
  geom_smooth(method="lm", col = "black") +
  annotate("text", x = 8, y = 13, 
           label = paste0("R² = ", r2)) +
  xlab("Log BP") +
  ylab("Log Guano area (m2)") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(color = "Colony") +
  scale_color_manual(values = colours)

Trend_plot 

# Chapter 3/ Results/ Model

#####################################################################################################################
# Plot by site
#####################################################################################################################

# Subset data by site
CROZdf <- subset(Dataset.5.2, Dataset.5.2$Site_ID=="CROZ")
BIRDdf <- subset(Dataset.5.2, Dataset.5.2$Site_ID=="BIRD")
ROYDdf <- subset(Dataset.5.2, Dataset.5.2$Site_ID=="ROYD")
INEXdf <- subset(Dataset.5.2, Dataset.5.2$Site_ID=="INEX")

# Crozier

# Pearson's correlation coefficients and p values
CROZ_cor <- cor.test(CROZdf$logBP, CROZdf$logGuano_area)
CROZ_cor
CROZ_pearsons <- signif(CROZ_cor$estimate, digits = 3)
CROZ_pvalue <- signif(CROZ_cor$p.value, digits = 3)

# Plot
Crozier_plot <- ggplot(CROZdf, aes(x = logBP, y = logGuano_area)) + 
  geom_point(size=3, colour = "#Ff0000") + 
  geom_smooth(method="lm", col = "black") +
  annotate("text", x = 12.69, y = 13.4, 
           label = paste0("Pearsons = ", CROZ_pearsons, "  P value = ", CROZ_pvalue)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

Crozier_plot

# Bird

# Pearson's correlation coefficients and p values
BIRD_cor <- cor.test(BIRDdf$logBP, BIRDdf$logGuano_area)
BIRD_cor
BIRD_pearsons <- signif(BIRD_cor$estimate, digits = 3)
BIRD_pvalue <- signif(BIRD_cor$p.value, digits = 3)

# Plot
Bird_plot <- ggplot(BIRDdf, aes(x = logBP, y = logGuano_area)) + 
  geom_point(size=3, colour = "#0000FF") + 
  geom_smooth(method="lm", col = "black") +
  annotate("text", x = 11.12, y = 12.0, 
           label = paste0("Pearsons = ", BIRD_pearsons, "  P value = ", BIRD_pvalue)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

Bird_plot

# Royds

# Pearson's correlation coefficients and p values
ROYD_cor <- cor.test(ROYDdf$logBP, ROYDdf$logGuano_area)
ROYD_cor
ROYD_pearsons <- signif(ROYD_cor$estimate, digits = 3)
ROYD_pvalue <- signif(ROYD_cor$p.value, digits = 3)

# Plot
Royds_plot <- ggplot(ROYDdf, aes(x = logBP, y = logGuano_area)) + 
  geom_point(size=3, colour = "#00ff00") + 
  geom_smooth(method="lm", col = "black") +
  annotate("text", x = 7.75, y = 10.0, 
           label = paste0("Pearsons = ", ROYD_pearsons, "  P value = ", ROYD_pvalue)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

Royds_plot

# Inexpressible

# Pearson's correlation coefficients and p values
INEX_cor <- cor.test(INEXdf$logBP, INEXdf$logGuano_area)
INEX_cor
INEX_pearsons <- signif(INEX_cor$estimate, digits = 3)
INEX_pvalue <- signif(INEX_cor$p.value, digits = 3)

# Plot
Inexpressible_plot <- ggplot(INEXdf, aes(x = logBP, y = logGuano_area)) + 
  geom_point(size=3, colour = "#FFA500") + 
  annotate("text", x = 10.38, y = 11.4, 
           label = paste0("Pearsons = ", INEX_pearsons, "  P value = ", INEX_pvalue)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_minimal() +
  theme(legend.position = c(1,1)) + 
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

Inexpressible_plot

# Plot together

library(ggpubr)

Correlations <- plot(ggarrange(Crozier_plot, 
                               Bird_plot, 
                               Royds_plot,
                               Inexpressible_plot,
                               ncol = 2, nrow = 2, labels=c("a","b","c","d")))
annotate_figure(Correlations, left = "Log Guano area (m2)", bottom = "Log BP")

# Appendix / Appendix Figure 2

#####################################################################################################################
# Look at residuals for model with GA as dependent variable
#####################################################################################################################

# Extracting only needed variables
ds3 <- Dataset.5.2[,c("logBP", "logGuano_area", "Site_ID")]

# Remove NAs
ds4 <- na.omit(ds3)

ds4$fitted <- New.model$fitted.values
ds4$resid <- New.model$residuals

ds4$Colony <- as.factor(ds4$Site_ID)

# Plot
New.model.resids <- ggplot(ds4, aes(x=fitted, y=resid, colour = Colony, fill = Colony, shape = Colony)) +
  geom_point(size=3 )+
  geom_hline(yintercept=0) +
  xlab("Observed") +
  ylab("Residuals") +
  scale_shape_manual(values = c(21,21,21,21))+
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12),)+
  scale_y_continuous(limits = c(-0.4,0.6), breaks = seq(-0.4,0.6, by=0.2))


New.model.resids # Plot not used in thesis

#####################################################################################################################
# Investigate spatial autocorrelation with linear mixed-effect model (LMM) that includes site as a random effect
#####################################################################################################################

library(lme4)

# LMM with site effect using REML
lmm1 <- lme4::lmer(logGuano_area ~ logBP + (1|Site_ID), data = Dataset.5.2)

anova(lmm1) # just looks at fixed effects
summary(lmm1) # variances came from here

# Compare LMM to LM using ML method
lmm2 <- lme4::lmer(logGuano_area ~ logBP + (1|Site_ID), REML = FALSE, data = Dataset.5.2)

anova(lmm2) # just looks at fixed effects
summary(lmm2)

# using REML = FALSE (or ML method) gives below error
# boundary (singular) fit: see help('isSingular')
# no spatial autocorrelation? - removes variance from site effect due over paramitisation

AIC(New.model, lmm2)
# model without site effect better?

# View residuals of LMM with site effect
ds4$fitted2 <- fitted(lmm1)
ds4$resid2 <- resid(lmm1)

ds4$Colony <- as.factor(ds4$Site_ID)

# Plot
lmm1.resids <- ggplot(ds4, aes(x=fitted2, y=resid2, colour = Colony, fill = Colony, shape = Colony)) +
  geom_point(size=3 )+
  geom_hline(yintercept=0) +
  xlab("Observed") +
  ylab("Residuals") +
  scale_shape_manual(values = c(21,21,21,21))+
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12),)+
  scale_y_continuous(limits = c(-0.4,0.6), breaks = seq(-0.4,0.6, by=0.2))


lmm1.resids # Plot not used in thesis

# Plot together
plot(ggarrange(New.model.resids, 
               lmm1.resids, 
               common.legend = TRUE,
               legend = "right",
               ncol = 1, nrow = 2, labels=c("a","b")))

# Plot not used in thesis

# look very similar
# a is LM without site effect
# b is LMM with site effect

#####################################################################################################################
# Investigate temporal autocorrelation in the residuals
#####################################################################################################################

# ACF on residuals of LM
temporal.residuals <- resid(New.model, type="pearson") # use type = pearson for normalised residuals

par(mfrow = c(1,1))
acf(temporal.residuals) # no temporal autocorrelation

# Appendix / Appendix Figure 1

#####################################################################################################################
# Investigate factors that may influence relationship
#####################################################################################################################

# Calculate northness (use northness instead of aspect due to aspect's circular nature in models)

# Convert degrees to radians
Dataset.5.0$AspectRadians <- pi*Dataset.5.0$Aspect/180

# Calculate northness
Dataset.5.0$Northness <- cos(Dataset.5.0$AspectRadians)

# Extracting extra needed variables
Dataset.5.3 <- Dataset.5.0[,c("logBP", "logGuano_area", "Site_ID", "PAR", "Slope", "Northness")]

# Remove NAs
Dataset.5.4 <- na.omit(Dataset.5.3)

View(Dataset.5.4)

# Investigate correlation between covariates using correlogram

library(corrplot)

covariates <- data.frame(Dataset.5.4$logBP, Dataset.5.4$Slope, Dataset.5.4$Northness, Dataset.5.4$PAR)

cor.matrix <- cor(covariates) # default method is pearsons

corrplot(cor.matrix, method = "number", type = "lower", tl.cex = 1)

# Appendix/ Appendix Table 1

# Can't use northness due to correlation with BP
# northness and BP correlated positively (0.96)
# PAR and BP correlated positively (0.61)
# northness and PAR correlated positively (0.46)

library(car) # for VIF values
library(MuMIn) # for AICc scores

# Run candidate models

# CAN'T USE - Candidate model with northness (correlated with BP)
lm.GA.a1 <- lm(Dataset.5.4$logGuano_area ~ Dataset.5.4$logBP + Dataset.5.4$Northness)

summary(lm.GA.a1)
vif(lm.GA.a1) # well above 2.0

# removed BP
lm.GA.a2 <- lm(Dataset.5.4$logGuano_area ~ Dataset.5.4$Northness)

summary(lm.GA.a2) # northness does have an influence - not as good as BP

# Candidate model with slope
lm.GA.b <- lm(Dataset.5.4$logGuano_area ~ Dataset.5.4$logBP + Dataset.5.4$Slope)

summary(lm.GA.b)
anova(lm.GA.b)
vif(lm.GA.b) # both under 2.0

AIC(New.model, lm.GA.b) # over fit? due to low sample size try AICc
AICc(New.model, lm.GA.b) # model without slope better

# CAN'T USE - Candidate model with northness and slope (have to remove northness)
lm.GA.c <- lm(Dataset.5.4$logGuano_area ~ Dataset.5.4$logBP + Dataset.5.4$Northness + Dataset.5.4$Slope)

summary(lm.GA.c)
vif(lm.GA.c) # only slope under 2.0

# Candidate model with PAR (PAR and BP correlated?)
lm.GA.d <- lm(Dataset.5.4$logGuano_area ~ Dataset.5.4$logBP + Dataset.5.4$PAR)

summary(lm.GA.d)
anova(lm.GA.d)
vif(lm.GA.d) # both under 2.0 (Don't need to remove PAR)

AIC(New.model, lm.GA.d) # model without PAR better
AICc(New.model, lm.GA.d) # model without PAR better

# Candidate model with PAR and slope
lm.GA.e <- lm(Dataset.5.4$logGuano_area ~ Dataset.5.4$logBP + Dataset.5.4$PAR + Dataset.5.4$Slope)

summary(lm.GA.e)
anova(lm.GA.e)
vif(lm.GA.e) # all under 2.0

# CAN'T USE - Candidate model with PAR and northness (predictors correlated)
lm.GA.f <- lm(Dataset.5.4$logGuano_area ~ Dataset.5.4$logBP + Dataset.5.4$PAR + Dataset.5.4$Northness)

summary(lm.GA.f)
vif(lm.GA.f) # all above 2.0

#####################################################################################################################
# 2. How well can we estimate Guano area and BP relationship at a new site (cross-validation)
#####################################################################################################################

# Leave one out cross validation by point

# Used on New.model (lm) - GA as dependent variable

# set up arrays
Dif.array <- array(NA, dim = c(27,2))

# Loop
datSeq <-  1:dim(Dataset.5.2)[1] # to partition the data by every point

# for every point (i) in Dataset.5.2 1-27 ...
for(i in 1:dim(Dataset.5.2)[1]){ 
  mask =  datSeq != i # Leave out one point
  test.dat <- Dataset.5.2[i,] # Testing data is the one point
  train.dat <- Dataset.5.2[mask,] # Training data is the remaining data other than that one point
  
  New.model <- lm(logGuano_area ~ logBP, data = train.dat) # run model
  
  test.predict <- predict(New.model, newdata = test.dat) # use model to predict guano area for testing data
  
  print(test.predict)
  
  train.mean <- mean(train.dat$logGuano_area) # calculate mean guano area of the training data
  
  num.dif <- test.predict-test.dat$logGuano_area # predicted guano area - observed guano area
  dom.dif <- train.mean-test.dat$logGuano_area # mean guano area of the training data - observed guano area
  
  Dif.array[i,1] <- num.dif
  Dif.array[i,2] <- dom.dif
  
  ## calculate the r squared
  r2 = 1 - sum(num.dif^2)/sum(dom.dif^2)
  
  print(dim(test.dat))
  print(dim(train.dat))
  print(r2 )
  
}

# calculate numerator and denominator difference 
r2Total <- 1 - (sum(Dif.array[,1]^2) / sum(Dif.array[,2]^2))

# R squared predicted value
print(r2Total)
# 0.9772684

#####################################################################################################################
# 3. What is the probability of detecting a true change in BP using guano area?
#####################################################################################################################

# sometimes have to clear environment before

# For LM
coeff = coefficients(New.model)
sig = summary(New.model)$sigma

par(mfrow=c(2,2))

#added in below

ADPE_Data <- Dataset.5.2
# sites = ADPE_Data$Site_ID
sites <- c("BIRD", "CROZ", "INEX", "ROYD")

# loop

propReduce = seq(0.05, 0.6, 0.01)
for(i in 1:4)
{
  site_i = sites[i]
  startPop = exp(mean(ADPE_Data$logBP[ADPE_Data$Site_ID == site_i]))
  logPopStart = log(startPop)
  logPopReduce = log(startPop * (1 - propReduce))
  logGAStart = coeff[1] + coeff[2] * logPopStart
  logGAReduce = coeff[1] + coeff[2] * logPopReduce
  zScore = (logGAStart - logGAReduce) / sqrt(sig^2 + sig^2)
  pTrueReduce = pnorm(zScore) # The normal cumulative distribution function
  
  ## proportion change to be 95% confident the reduction is real
  maskPositive = pTrueReduce >= 0.95
  diff = pTrueReduce - 0.95
  minPos = min(diff[maskPositive])
  indx = diff %in% minPos
  print(paste(site_i, " 95% confident this proportion change is real: ", 
              propReduce[indx]))
  
  # print(pTrueReduce) - give same numbers for each site
  # print(propReduce) - give same numbers for each site
  
  plot(propReduce, pTrueReduce, type="l", xlab="Prop. pop. reduced", 
    ylab= "Prob. true reduction", main= site_i)

  abline(h = 0.95, col = "blue", lty = 2) # add a blue dashed line at y = 0.95
  abline(h = 0.80, col = "black", lty = 2) # add a black dashed line at y = 0.80
  abline(h = 0.60, col = "red", lty = 2) # add a black dashed line at y = 0.60
 
}

# Plot relationship on it's own

par(mfrow=c(1,1))

plot(propReduce, pTrueReduce, type="l", xlab="Proportion of population reduced", 
     ylab= "Probability of detecting a true reduction")

abline(h = 0.95, col = "blue", lty = 2) # add a blue dashed line at y = 0.95
abline(h = 0.80, col = "black", lty = 2) # add a black dashed line at y = 0.80
abline(h = 0.60, col = "red", lty = 2) # add a black dashed line at y = 0.60

# Chapter 3/ Results/ Model

# Results 
# "BIRD  95% confident this proportion change is real:  0.45"
# "CROZ  95% confident this proportion change is real:  0.45"
# "INEX  95% confident this proportion change is real:  0.45"
# "ROYD  95% confident this proportion change is real:  0.45"

# What about trying 80% confident
for(i in 1:4)
{
  site_i = sites[i]
  startPop = exp(mean(ADPE_Data$logBP[ADPE_Data$Site_ID == site_i]))
  logPopStart = log(startPop)
  logPopReduce = log(startPop * (1 - propReduce))
  logGAStart = coeff[1] + coeff[2] * logPopStart
  logGAReduce = coeff[1] + coeff[2] * logPopReduce
  zScore = (logGAStart - logGAReduce) / sqrt(sig^2 + sig^2)
  pTrueReduce = pnorm(zScore)
  
  ## proportion change to be 80% confident the reduction is real
  maskPositive = pTrueReduce >= 0.80
  diff = pTrueReduce - 0.80
  minPos = min(diff[maskPositive])
  indx = diff %in% minPos
  print(paste(site_i, " 80% confident this proportion change is real: ", 
              propReduce[indx]))
  
  plot(propReduce, pTrueReduce, type="l", xlab="Prop. pop. reduced", 
       ylab= "Prob. true reduction", main= site_i)
}

# Results
# "BIRD  80% confident this proportion change is real:  0.27"
# "CROZ  80% confident this proportion change is real:  0.27"
# "INEX  80% confident this proportion change is real:  0.27"
# "ROYD  80% confident this proportion change is real:  0.27"

# What about trying 60% confident
for(i in 1:4)
{
  site_i = sites[i]
  startPop = exp(mean(ADPE_Data$logBP[ADPE_Data$Site_ID == site_i]))
  logPopStart = log(startPop)
  logPopReduce = log(startPop * (1 - propReduce))
  logGAStart = coeff[1] + coeff[2] * logPopStart
  logGAReduce = coeff[1] + coeff[2] * logPopReduce
  zScore = (logGAStart - logGAReduce) / sqrt(sig^2 + sig^2)
  pTrueReduce = pnorm(zScore)
  
  ## proportion change to be 60% confident the reduction is real
  maskPositive = pTrueReduce >= 0.60
  diff = pTrueReduce - 0.60
  minPos = min(diff[maskPositive])
  indx = diff %in% minPos
  print(paste(site_i, " 60% confident this proportion change is real: ", 
              propReduce[indx]))
  
  plot(propReduce, pTrueReduce, type="l", xlab="Prop. pop. reduced", 
       ylab= "Prob. true reduction", main= site_i)
}

# Results
# "BIRD  60% confident this proportion change is real:  0.09"
# "CROZ  60% confident this proportion change is real:  0.09"
# "INEX  60% confident this proportion change is real:  0.09"
# "ROYD  60% confident this proportion change is real:  0.09"
