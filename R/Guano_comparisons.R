
# Paired sample t test to compare guano areas extracted using SVM and maximum likelihood classification methods for five VHR images
# Cleaned version
# Creator: Alexandra Strang
# Last edited: 05/02/2024

sessionInfo() # for citing package versions
citation() # for citing packages

#####################################################################################################################
# Read in data
#####################################################################################################################

# ML: Maximum Likelihood classification method
# SVM: Support Vector Machine classification method

df <- data.frame(ML = c(451896.8287, 393073.0548, 188758.9295, 110814.9443, 17794.6638), SVM = c(362620.9025, 352712.8206, 131544.7006, 68654.6981, 14272.3352))

View(df)

#####################################################################################################################
# Checking the assumptions of the paired sample t test
#####################################################################################################################

# Checking normality assumption

# Maximum Likelihood classification method
shapiro.test(df$ML)
# p-value = 0.5979 (greater than 0.05 - data is approximately normally distributed)

# Support Vector Machine classification method
shapiro.test(df$SVM)
# p-value = 0.2139 (greater than 0.05 - data is approximately normally distributed)

# Checking for equal variances
var.test(df$ML, df$SVM)
# p-value = 0.8057 (variances are equal)

#####################################################################################################################
# Run paired sample t test
#####################################################################################################################

t.test(df$ML, df$SVM, paired = TRUE, var.equal = TRUE)
# p-value = 0.0285

#####################################################################################################################
# Run power test
#####################################################################################################################

# Calculating d (effect size)
# Calculates Cohen's d which is the difference between the means divided by the pooled standard deviation

d_means <- abs(mean(df$ML) - mean(df$SVM))
pool_sd <- sqrt((var(df$ML) + var(df$SVM))/2)
d <- d_means/pool_sd

# Use pwr package

library(pwr)

pwr.t.test(n = 5, 
           d = d, 
           sig.level = 0.05, 
           type = "paired",
           alternative = "two.sided")
# power = 0.0755927
