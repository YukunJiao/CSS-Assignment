

## Lab code to calculate Lorenz Curve and Gini coefficient:

# install.packages("devtools")
# devtools::install_github("thomasblanchet/wid-r-tool")
devtools::install_github("world-inequality-database/wid-r-tool")

install.packages("ineq")

library(wid)
library(ineq)

#### Load and clean data:

wid_data <-download_wid(
  indicators = "aptinc",
  areas = "SE",
  years = "2022",  
  perc = "all",
  ages = "all",
  pop = "all",
  metadata = FALSE,
  include_extrapolations = TRUE,
  verbose = TRUE
)


pattern <- "^p(\\d{1,2})p(\\d{1,2})$"

# Use grepl with an additional condition to check that the difference between the two numbers is exactly 1
filtered_df <- wid_data[grepl(pattern, wid_data$percentile) & 
                          as.numeric(sub("p(\\d{1,2})p(\\d{1,2})", "\\2", wid_data$percentile)) -
                          as.numeric(sub("p(\\d{1,2})p(\\d{1,2})", "\\1", wid_data$percentile)) == 1, ]



## Graph Lorenz Curve:

# Compute the Lorenz curve
lorenz_curve <- ineq::Lc(filtered_df$value)

# Plot the Lorenz curve
plot(lorenz_curve, main = "Lorenz Curve", xlab = "Cumulative Share of Population", ylab = "Cumulative Share of Income", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2)


#### Calculate Gini:

# Step 1: Sort by income and create the rank (R) variable
filtered_df <- filtered_df[order(filtered_df$value), ]
filtered_df$rank <- seq_along(filtered_df$value) / nrow(filtered_df)

# Step 2: Calculate the mean of the income values (x̄)
mean_income <- mean(filtered_df$value)

# Step 3: Calculate the covariance between income and rank
covar <- cov(filtered_df$value, filtered_df$rank)

# Step 4: Calculate the Gini coefficient using the formula
gini_coefficient <- 2/mean_income * covar

# Print the Gini coefficient
print(gini_coefficient)

# Calculate the Gini coefficient using the ineq::Gini function
gini_coefficient <- ineq::Gini(filtered_df$value)

# Display the Gini coefficient
print(gini_coefficient)

#---


wid_data_cn <-download_wid(
  indicators = "aptinc",
  areas = "CN",
  years = "2022",  
  perc = "all",
  ages = "all",
  pop = "all",
  metadata = FALSE,
  include_extrapolations = TRUE,
  verbose = TRUE
)


pattern <- "^p(\\d{1,2})p(\\d{1,2})$"

# Use grepl with an additional condition to check that the difference between the two numbers is exactly 1
filtered_df_cn <- wid_data_cn[grepl(pattern, wid_data_cn$percentile) & 
                          as.numeric(sub("p(\\d{1,2})p(\\d{1,2})", "\\2", wid_data_cn$percentile)) -
                          as.numeric(sub("p(\\d{1,2})p(\\d{1,2})", "\\1", wid_data_cn$percentile)) == 1, ]



## Graph Lorenz Curve:

# Compute the Lorenz curve
lorenz_curve_cn <- ineq::Lc(filtered_df_cn$value)

# Plot the Lorenz curve
plot(lorenz_curve_cn, main = "Lorenz Curve", xlab = "Cumulative Share of Population", ylab = "Cumulative Share of Income", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2)


plot(lorenz_curve, main = "Lorenz Curve", xlab = "Cumulative Share of Population", ylab = "Cumulative Share of Income", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2)

lines(lorenz_curve_cn, col = "black", lwd = 2)

#### Calculate Gini:

# Step 1: Sort by income and create the rank (R) variable
filtered_df <- filtered_df[order(filtered_df$value), ]
filtered_df$rank <- seq_along(filtered_df$value) / nrow(filtered_df)

# Step 2: Calculate the mean of the income values (x̄)
mean_income <- mean(filtered_df$value)

# Step 3: Calculate the covariance between income and rank
covar <- cov(filtered_df$value, filtered_df$rank)

# Step 4: Calculate the Gini coefficient using the formula
gini_coefficient <- 2/mean_income * covar

# Print the Gini coefficient
print(gini_coefficient)

# Calculate the Gini coefficient using the ineq::Gini function
gini_coefficient <- ineq::Gini(filtered_df$value)

# Display the Gini coefficient
print(gini_coefficient)




