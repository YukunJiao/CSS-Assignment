
## Lab Code to decompose Variance:

# install.packages("haven")

# Load necessary libraries
library(haven)
library(dplyr)

ess <- read_dta("~/Downloads/Lab 1/ESS9e03_2.dta")

# Filter for Sweden (SE) only
ess_se <- ess %>% filter(cntry == "SE")

# Convert eduyrs and edulvlb to numeric, coercing strings to NA
ess_se <- ess_se %>%
  mutate(
    eduyrs = as.numeric(eduyrs),
    edulvlb = as.numeric(edulvlb)
  )

# Filter out rows with NA values in eduyrs or edulvlb (including those that were strings)
ess_se_complete <- ess_se %>% filter(!is.na(eduyrs) & !is.na(edulvlb))

# Run the regression on the filtered dataset
model_edu <- lm(eduyrs ~ factor(edulvlb), data = ess_se_complete)

# Predict Eduyears for the filtered dataset
ess_se_complete <- ess_se_complete %>% mutate(Eduyears = predict(model_edu))

# Preserve the mapping: rename Eduyears to FatherEduyears
father_mapping <- ess_se_complete %>% 
  select(edulvlb, Eduyears) %>%
  rename(FatherEduyears = Eduyears, edulvlfb = edulvlb) %>%
  distinct()

# Merge the father education years into the main dataset based on edulvlfb
ess_se <- ess_se %>%
  left_join(father_mapping, by = c("edulvlfb" = "edulvlfb"))

# hinctnta <- log(hinctnta)

# Run the regression: hinctnta on eduyrs and FatherEduyears
model <- lm(hinctnta ~ eduyrs + FatherEduyears, data = ess_se)

# Results of regression:
summary(model)

# Extract coefficients
b_eduyrs <- coef(model)["eduyrs"]
b_FatherEduyears <- coef(model)["FatherEduyears"]

# Predict Residuals
#ess_se <- ess_se %>% mutate(Residuals = residuals(model))

# Calculate standard deviations
sd_eduyrs <- sd(ess_se$eduyrs, na.rm = TRUE)
sd_FatherEduyears <- sd(ess_se$FatherEduyears, na.rm = TRUE)
#sd_Residual <- sd(ess_se$Residuals, na.rm = TRUE)

# Calculate covariance between eduyrs and FatherEduyears
cov_eduyrs_FatherEduyears <- cov(ess_se$eduyrs, ess_se$FatherEduyears, use = "complete.obs")

# Calibration calculation
Calibration <- b_eduyrs^2 * sd_eduyrs^2 + 
  b_FatherEduyears^2 * sd_FatherEduyears^2 + 
  2 * b_eduyrs * b_FatherEduyears * cov_eduyrs_FatherEduyears

# Calculate the actual variance of hinctnta
sd_hinctnta <- sd(ess_se$hinctnta, na.rm = TRUE)
var_hinctnta <- sd_hinctnta^2

VarE_eduyrs <- b_eduyrs^2 * sd_eduyrs^2
VarE_FatherEduyears <- b_FatherEduyears^2 * sd_FatherEduyears^2
VarE_Cov <- 2 * b_eduyrs * b_FatherEduyears * cov_eduyrs_FatherEduyears

Calibration_r2 <- Calibration/var_hinctnta
cat("Calibrated R2: ", Calibration_r2, "\n")

# Display the results
cat("Calibration: ", Calibration, "\n")
cat("Variance of hinctnta: ", var_hinctnta, "\n")

cat("Variance of VarE_eduyrs: ", VarE_eduyrs, "\n")
cat("Variance of VarE_FatherEduyears: ", VarE_FatherEduyears, "\n")
cat("Variance of VarE_Cov: ", VarE_Cov, "\n")




