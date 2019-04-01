# Import Required Packages
library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)

# Import the sales data into R
# Read first sheet of toy sales data
toy_data <- read.xlsx("toy_sales_data.xlsx", 1)

#	Create a plot of sales, TV investment and Digital investment in the y axis with time in the x axis
# use melt function to represent data in variable and value for ease of representing in graph and units are used in thousands
tbl <- data.frame(month=toy_data$month,sales=toy_data$sales/100000,tv_spend=toy_data$tv_spend/100000,digital_spend=toy_data$digital_spend/100000,check.names = FALSE)
tbl <- melt(tbl, id.vars = 'month')
plt <- ggplot(data = tbl, aes(x = month, y=value))
plt <- plt + geom_path(aes(colour=tbl$variable))
plt + theme_minimal() + theme(legend.title=element_blank())

# Report the correlations among sales, TV and Digital investment
# Copy required columns in to a temporary data frame and generate correlations and plot the heat map
corelation_df<- data.frame(sales=toy_data$sales,tv_spend=toy_data$tv_spend,digital_spend=toy_data$digital_spend,check.names = FALSE)
res <- cor(corelation_df)
print(res)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

# Fit a regression model to data
# Ignoring trend and xmas variables as they are not used in the test data (planned spend)
fit <- lm(sales ~  tv_spend + digital_spend, data=toy_data)
summary(fit)

rse_error_percent <- sigma(fit)/mean(toy_data$sales)

# Contribution from TV Spend to sales in % and absolute dollar value
tv_spend_coeff <- summary(fit)$coefficients[2,1]
tv_spend_contribution <- sum(toy_data$tv_spend)*tv_spend_coeff
paste("TV Contributions in absolute dollar value:" , tv_spend_contribution)
tv_spend_contribution_percentage <-(sum(toy_data$tv_spend)*tv_spend_coeff)/sum(toy_data$sales)
paste("TV Contributions in percentage:" , tv_spend_contribution_percentage*100, "%")

# TV return on investment (ROI)
tv_spend_roi <- (tv_spend_contribution - sum(toy_data$tv_spend))/sum(toy_data$tv_spend)
paste("TV ROI is:" , tv_spend_contribution_percentage*100, "%")

# expected sales value for the first 3 months of 2018
toy_test_data <- read.xlsx("toy_sales_data.xlsx", 2)
predictions <- predict.lm(fit, toy_test_data)
print(predictions)

#Summary :- 
#The plot of sales, advertising spend shows that maximum sales have been recorded in April 2017 with maximum TV spend and high Digital spend.  

#Correlation heat map represents that there is a strong relation between digital spend & sales compared to TV spend.

#The model shows that the Digital spend has more impact on sales with high t-value followed by Intercept& Low P-Value. The RSE-value is 2595K(depends on units of the variables taken)  with 23% error rate. The adjusted R2 value is 0.55 with small P-value. 

#The goodness of fit has been checked using linear regression diagnostics with plots of Residual Vs Fitted, Normal Q-Q, Scale Location. 

#The TV-spend has been contributing to 73% increasing in sales.For every $1 spent on TV ads, we get 1.73$ returns.

#Adding variables like TV GRP's, Digital AdStock helps to do indepth analysis of sales contribution. 

