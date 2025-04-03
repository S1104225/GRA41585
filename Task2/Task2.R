#| label: load necessary packages
#| include: false
require(dplyr)
# require(ggplot2)

treated.ratio <- 0.8
marketing.cost <- 100 / 1000 # per 1 impression
purchase.contribution <- 300
data <- read.csv('CaseData2025.csv', sep=';')

# Purchase value (300 NOK for each purchase)
data$purchase_value <- ifelse(data$purchase == 1, purchase.contribution, 0)

# 1. Group data based on treated and untreated observations and print results
group_summary <- data %>%
  group_by(test) %>%
  summarise(
    avg_impressions = mean(impressions),
    avg_purchase_rate = mean(purchase),
    total_sales = sum(purchase_value),
    total_impressions = sum(impressions)
  )
print(group_summary)
rate.increase <- group_summary$avg_purchase_rate[2] - group_summary$avg_purchase_rate[1]
cat("Incremental lift in the conversion rate (descriptive):", rate.increase, "\n")
cat("Growth in the conversion rate (descriptive):", 100 * rate.increase / group_summary$avg_purchase_rate[1], "%\n")
# The difference in conversion rates is significant at 5% level
t.test(data[data$test == 1, ]$purchase, data[data$test == 0, ]$purchase, var.equal = F, alternative = 'greater')

# 2. Baseline conversion rate and lift
cat("Baseline conversion rate (control group):", 100 * group_summary$avg_purchase_rate[1], "%\n")
cat("Conversion rate after the campaign (treated group):", 100 * group_summary$avg_purchase_rate[2], "%\n")

# Using logistic regression to estimate the effect and controlling for impressions
model1a <- glm(purchase ~ test, data = data, family = binomial)
cat("Growth in the conversion rate (logit):", 100 * (exp(model1a$coefficients['test']) - 1), "%\n")
summary(model1a)
model1b <- glm(purchase ~ test + impressions, data = data, family = binomial)
cat("Growth in the conversion rate accounting for impressions (logit):", 100 * (exp(model1b$coefficients['test']) - 1), "%\n")
cat("Growth in the conversion rate due to an increase in impressions (logit):", 100 * (exp(model1b$coefficients['impressions']) - 1), "%\n")
summary(model1b)
# 3. Baseline conversion rate and lift
campaign.revenue <- rate.increase * nrow(data[data$test == 1, ]) * purchase.contribution
campaign.cost <- sum(data$impressions[data$test == 1]) * marketing.cost
campaign.profit <- campaign.revenue - campaign.cost
cat("Campaign has earned a profit of:", campaign.profit, "NOK\n")
ROI <- (campaign.revenue - campaign.cost) / campaign.cost
cat("ROI of the campaign is: ", 100 * ROI, "%\n")

# 4. Frequency effect
cat("Difference in average impressions:", group_summary$avg_impressions[2] - group_summary$avg_impressions[1], "\n")
# The average impression for the treated group is significantly larger at 5% confidence
t.test(data[data$test == 1, ]$impressions, data[data$test == 0, ]$impressions, var.equal = F, alternative = 'greater')

# 5. Check for non-compliance in the control group (users who were exposed to ads)
expected.treated <- treated.ratio * nrow(data)
actual.treated <- nrow(data[data$test == 1, ])
# We have more customers in the treated group than expected => 65 users (defiers or always-takers) from the control group were treated 
cat("Number of non-compliers:", abs(actual.treated - expected.treated), "\n")

