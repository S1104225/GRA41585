# Needed for the plots
library(dplyr)
library(ggplot2)
library(sjPlot)
library(plm)
library(tidyr)

library(Synth)
library(tidyverse)
library(synthdid)

# Store our data from the CSV file
data <- read.csv("Store_data_2025.csv", sep=";")

# Display first few rows
head(data)  

# PRE-POST ANALYSIS FOR THE TREATED

# Create a Post dummy variable to indicate the post-treatment period
data$Post <- as.numeric(data$Weekind >= 78)

# Create the new variable
data$Treatment <- ifelse(data$storeNum == 109, 1, 0)

# Create a separate dataset only with data for the treatment group
data.treated <- data[data$storeNum == 109, ]

data.pretreatment.treated <- data.treated %>% filter(Weekind<78)
data.pretreatment <- data %>% filter(Weekind<78)

data.posttreatment.treated <- data.treated %>% filter(Weekind>=78)
data.posttreatment <- data %>% filter(Weekind >= 78)

time_pre <- 1:78 # Pre-treatment periods
print(time_pre)
time_post <- 78:104  # Post-treatment periods
print(time_post)

# Plot the sales data for store 109
ggplot(data.treated) +
    geom_line(aes(x = Weekind, y = p1sales, color = "P1 Sales")) +
    geom_line(aes(x = Weekind, y = p2sales, color = "P2 Sales")) +
    geom_vline(xintercept = 78, linetype = "dotted", color = "black") + # Adding the vertical line
    labs(x = "Observed weeks", y = "Sales", color = "Legend") +
    theme_classic()

# Analysis of the models using prices and the Post variable
model_p1 <- lm(p1sales ~ p1price + Weekind, data = data.pretreatment.treated)
tab_model(model_p1)

model_p2 <- lm(p2sales ~ p2price + Weekind, data = data.pretreatment.treated)
tab_model(model_p2)

# Get estimated coefficients for sales of Product 1
alpha1 <- coef(model_p1)[1]  # Intercept
beta1_price <- coef(model_p1)[2]   # Price coefficient
beta1_trend <- coef(model_p1)[3]  # Trend coefficient (Weekind)
print(paste("Alpha (Intercept):", round(alpha1, 2)))
print(paste("Beta (Trend):", round(beta1_trend, 2)))
predicted_p1sales <- alpha1 + beta1_price * data.posttreatment.treated$p1price + beta1_trend * data.posttreatment.treated$Weekind
print(predicted_p1sales)

atet1 <- data.posttreatment.treated$p1sales - predicted_p1sales
print(atet1)
average_atet1 <- mean(atet1)
print(paste("Average ATET for Product 1:", round(average_atet1, 2)))

# Actual vs. predicted sales for Product 1 plot
ggplot() +
    geom_line(data = data.treated, aes(x = Weekind, y = p1sales, color = "Actual Sales"), size = 1) +
    geom_line(data = data.posttreatment.treated, aes(x = Weekind, y = predicted_p1sales, color = "Predicted Sales"), linetype = "dashed", size = 1) +
    geom_vline(xintercept = 78, linetype = "dotted", color = "black") +  # Vertical line for treatment start
    labs(x = "Week",
         y = "Sales",
         color = "Legend") +
    theme_minimal()

# Get estimated coefficients for sales of Product 2
alpha2 <- coef(model_p2)[1]  # Intercept
beta2_price <- coef(model_p2)[2]   # Price coefficient
beta2_trend <- coef(model_p2)[3]  # Trend coefficient (Weekind)
print(paste("Alpha (Intercept):", round(alpha2, 2)))
print(paste("Beta (Trend):", round(beta2_trend, 2)))
predicted_p2sales <- alpha2 + beta2_price * data.posttreatment.treated$p2price + beta2_trend * data.posttreatment.treated$Weekind
print(predicted_p2sales)

atet2 <- data.posttreatment.treated$p2sales - predicted_p2sales
print(atet2)
average_atet2 <- mean(atet2)
print(paste("Average ATET for Product 2:", round(average_atet2, 2)))

# Actual vs. predicted sales for Product 2 plot
ggplot() +
    geom_line(data = data.treated, aes(x = Weekind, y = p2sales, color = "Actual Sales"), size = 1) +
    geom_line(data = data.posttreatment.treated, aes(x = Weekind, y = predicted_p2sales, color = "Predicted Sales"), linetype = "dashed", size = 1) +
    geom_vline(xintercept = 78, linetype = "dotted", color = "black") +  # Vertical line for treatment start
    labs(x = "Week",
         y = "Sales",
         color = "Legend") +
    theme_minimal()


# DIFFERENCE IN DIFFERENCE

# Create a separate dataset only with data for the control group
data.control <- data[data$storeNum != 109, ]
data.pretreatment.control <- data.control %>% filter(Weekind<78)
data.posttreatment.control <- data.control %>% filter(Weekind>=78)

# Compute average prices over time for store 109
avg_store_109 <- data.treated %>%
    group_by(Weekind) %>%
    summarize(mean_p1sales = mean(p1sales, na.rm = TRUE),
              mean_p2sales = mean(p2sales, na.rm = TRUE))

# Compute average prices over time for all other stores
avg_not_store_109 <- data.control %>%
    group_by(Weekind) %>%
    summarize(mean_p1sales = mean(p1sales, na.rm = TRUE),
              mean_p2sales = mean(p2sales, na.rm = TRUE))

ggplot() +
    geom_line(data = avg_store_109, aes(x = Weekind, y = mean_p1sales, color = "Treated store - Product 1 sales")) +
    geom_line(data = avg_store_109, aes(x = Weekind, y = mean_p2sales, color = "Treated store - Product 2 sales")) +
    geom_line(data = avg_not_store_109, aes(x = Weekind, y = mean_p1sales, color = "Untreated stores - Product 1 sales"), linetype = "dashed") +
    geom_line(data = avg_not_store_109, aes(x = Weekind, y = mean_p2sales, color = "Untreated stores - Product 2 sales"), linetype = "dashed") +
    geom_vline(xintercept = 78, linetype = "dotted", color = "black") + # Adding the vertical line
    labs(x = "Week",
         y = "Average Sales",
         color = "Legend") +
    theme_minimal()

# Test for parallel trends

# Run a linear regression on the pre-treatment data only
# Start with sales for the first product
model1a <- lm(p1sales ~ p1price + p1prom + Weekind, data = data.pretreatment.treated)
tab_model(model1a, show.se = TRUE)

model1b <- lm(p1sales ~ p1price + p1prom + Weekind, data = data.pretreatment.control)
tab_model(model1b, show.se = TRUE)

model1c <- lm(p1sales ~ p1price + p1prom + Weekind*Treatment, data = data.pretreatment)
tab_model(model1c, show.se = TRUE)

# Continue with sales for the second product
model2a <- lm(p2sales ~ p2price + p2prom + Weekind, data = data.pretreatment.treated)
tab_model(model2a, show.se = TRUE)

model2b <- lm(p2sales ~ p2price + p2prom + Weekind, data = data.pretreatment.control)
tab_model(model2b, show.se = TRUE)

model2c <- lm(p2sales ~ p2price + p2prom + Weekind*Treatment, data = data.pretreatment)
tab_model(model2c, show.se = TRUE)

# There is no statistically significant difference in pre-treatment trends between the treatment and control groups.
# This supports the parallel trends assumption, meaning the Difference-in-Differences (DiD) approach is valid.

# Estimate the ATET (average treatment effect on the treated) in the diff-in-diff framework

# Run a linear regression on the data to estimate the treatment effect
model3a <- lm(p1sales ~ Treatment * Post + p1price + p1prom + compind + storesize + citysize + Weekind, data = data)
tab_model(model3a, show.se = TRUE)
# ATET1 = 42.07, p < 0.001
# Interpretation: the marketing strategy showed an increase of 42.07 in sales of the first product.

model3b <- lm(p2sales ~ Treatment * Post + p2price + p2prom + compind + storesize + citysize + Weekind, data = data)
tab_model(model3b, show.se = TRUE)
# ATET2 = -15.13, p < 0.001
# Interpretation: the marketing strategy showed a decrease of 15.13 in sales of the second product.

# SYNTHETIC CONTROL
# P1
# Create a preparatory dataset for the subsequent synthetic control analysis
data.dataprep.p1 <- dataprep(
    foo = data,
    dependent = "p1sales",
    unit.variable = "storeNum",
    time.predictors.prior = 1:77,
    special.predictors = list(
        list("p1price", 1:77, "mean"),
        list("p1prom", 1:77, "mean"),
        list("compind", 1:77, "mean"),
        list("storesize", 1:77, "mean"),
        list("citysize", 1:77, "mean"),
        list("p1sales", 7, "mean"),
        list("p1sales", 15, "mean"),
        list("p1sales", 30, "mean"),
        list("p1sales", 36, "mean"),
        list("p1sales", 45, "mean"),
        list("p1sales", 61, "mean"),
        list("p1sales", 63, "mean"),
        list("p1sales", 67, "mean")),
    time.variable = "Weekind",
    treatment.identifier = 109,
    controls.identifier = c(101:108, 110:121),
    time.optimize.ssr = 1:77,
    time.plot = 1:104)

# Prepare the data for a comparative plot
data.synth.plot.p1 <- data %>% 
    select(storeNum, Weekind, p1sales) %>% 
    mutate(treated = storeNum==109) %>% 
    group_by(treated, Weekind) %>%
    summarize_all(mean, .groups = "drop")

# Compare treated (109) and untreaded shops in terms of p1 sales
# We can see that the sales of p1 jumped after treatment while the sales of untreated shops remained the same
ggplot(data.synth.plot.p1, aes(x = Weekind, y = p1sales, color = treated, group = treated)) +
    geom_line(aes(color = treated), linetype = "solid", linewidth = 0.5) +
    scale_color_manual(values = c("red", "blue")) +
    geom_vline(xintercept = 78, linetype = "dotted", color = "black") +
    labs(title = "P1 Sales: Treated vs. Untreated (Week 1-Week 104)",
         x = "Week",
         y = "P1 Sales",
         color = "Treatment Status") +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
    )

synth.out.p1 = synth(data.prep.obj = data.dataprep.p1)
print(synth.tab(dataprep.res = data.dataprep.p1, synth.res = synth.out.p1))
sum(synth.tab(dataprep.res = data.dataprep.p1, synth.res = synth.out.p1)$tab.w$w.weights > 0.01) # 4 stores are used to construct the synthetic store

# This plot compares sales at the store 109 and the synthetic store
# Sales are matched pretty well before the treatment, however, after the treatment sales of p1 have increased
path.plot(synth.res = synth.out.p1,
          dataprep.res = data.dataprep.p1,
          tr.intake = 77,
          Ylab = c("P1 Sales"),
          Xlab = c("Week"),
          Ylim = c(200,400),
          Main = 'Comparison between stores in terms of Product 1',
          Legend = c("Store 109","Synthetic store 109"))

# This plot exhibits a gap between the store 109 and synthetic store. It complements the previous graph
gaps.plot(synth.res = synth.out.p1,
          dataprep.res = data.dataprep.p1,
          tr.intake = 77,
          Ylab = c("Gap in P1 Sales"),
          Xlab = c("Week"),
          Ylim = c(-75,75),
          Main = 'Gaps in sales between stores in terms of Product 1')

# ATET is 34.95309
diff.sales.p1 <- data.dataprep.p1$Y1plot - (data.dataprep.p1$Y0plot %*% synth.out.p1$solution.w)
post.dum <- 1:104>77
mean(diff.sales.p1[post.dum])

# P2
# Create a preparatory dataset for the subsequent synthetic control analysis
data.dataprep.p2 <- dataprep(
    foo = data,
    dependent = "p2sales",
    unit.variable = "storeNum",
    time.predictors.prior = 1:77,
    special.predictors = list(
        list("p2price", 1:77, "mean"),
        list("p2prom", 1:77, "mean"),
        list("compind", 1:77, "mean"),
        list("storesize", 1:77, "mean"),
        list("citysize", 1:77, "mean"),
        list("p2sales", 5, "mean"),
        list("p2sales", 13, "mean"),
        list("p2sales", 21, "mean"),
        list("p2sales", 39, "mean"),
        list("p2sales", 46, "mean"),
        list("p2sales", 52, "mean"),
        list("p2sales", 64, "mean"),
        list("p2sales", 66, "mean"),
        list("p2sales", 71, "mean"),
        list("p2sales", 75, "mean")),
    time.variable = "Weekind",
    treatment.identifier = 109,
    controls.identifier = c(101:108, 110:121),
    time.optimize.ssr = 1:77,
    time.plot = 1:104)

# Prepare the data for a comparative plot
data.synth.plot.p2 <- data %>% 
    select(storeNum, Weekind, p2sales) %>% 
    mutate(treated = storeNum==109) %>% 
    group_by(treated, Weekind) %>%
    summarize_all(mean, .groups = "drop")

# Compare treated (109) and untreaded shops in terms of p2 sales
# We can see that the sales of p2 dropped after treatment while the sales of untreated shops stayed the same
ggplot(data.synth.plot.p2, aes(x = Weekind, y = p2sales, color = treated, group = treated)) +
    geom_line(aes(color = treated), linetype = "solid", linewidth = 0.5) +
    scale_color_manual(values = c("red", "blue")) +
    geom_vline(xintercept = 78, linetype = "dotted", color = "black") +
    labs(title = "P2 Sales: Treated vs. Untreated (Week 1-Week 104)",
         x = "Week",
         y = "P2 Sales",
         color = "Treatment Status") +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
    )

synth.out.p2 = synth(data.prep.obj = data.dataprep.p2)
# The MSE is much bigger because the sales data for P2 is much more volatile
print(synth.tab(dataprep.res = data.dataprep.p2, synth.res = synth.out.p2))
sum(synth.tab(dataprep.res = data.dataprep.p2, synth.res = synth.out.p2)$tab.w$w.weights > 0.01) # 5 stores are used to construct the synthetic store

# This plot compares sales at the store 109 and the synthetic store
# Sales are matched decently while the sales for the synthetic store are less volatile and not always match the actual sales
# However we can still see that the sales of p2 dropped after treatment
path.plot(synth.res = synth.out.p2,
          dataprep.res = data.dataprep.p2,
          tr.intake = 77,
          Ylab = c("P2 Sales"),
          Xlab = c("Week"),
          Ylim = c(200,400),
          Main = 'Comparison between stores in terms of Product 2',
          Legend = c("Store 109","Synthetic store 109"))

# This plot exhibits a gap between the store 109 and synthetic store. It complements the previous graph
gaps.plot(synth.res = synth.out.p2,
          dataprep.res = data.dataprep.p2,
          tr.intake = 77,
          Ylab = c("Gap in P2 Sales"),
          Xlab = c("Week"),
          Ylim = c(-75,75),
          Main = 'Gaps in sales between stores in terms of Product 2')

# ATET is -14.94119
diff.sales.p2 <- data.dataprep.p2$Y1plot - (data.dataprep.p2$Y0plot %*% synth.out.p2$solution.w)
post.dum <- 1:104>77
mean(diff.sales.p2[post.dum])


# SDID
# P1
# Prepare data further for the analysis
data$treated <- ifelse(data$storeNum == 109 & data$Post == 1, 1, 0)
dataprep.SDID.p1 <- panel.matrices(data, unit = 'storeNum', time = 'Weekind', outcome = 'p1sales', treatment = 'treated', treated.last = T)

# Run synthetic DID approach
sdid.out.p1 <- synthdid_estimate(dataprep.SDID.p1$Y, dataprep.SDID.p1$N0, dataprep.SDID.p1$T0)

# Show main results
summary(sdid.out.p1)
sum(summary(sdid.out.p1)$controls > 0.01)
plot(sdid.out.p1) +
    labs(title = "SDID of Product 1 Sales: Treated vs. Synthetic",
         x = "Week",
         y = "P1 Sales") +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
    )
synthdid_units_plot(sdid.out.p1)
sqrt(vcov(sdid.out.p1, method='placebo'))

# P2
# Prepare data further for the analysis
dataprep.SDID.p2 <- panel.matrices(data, unit = 'storeNum', time = 'Weekind', outcome = 'p2sales', treatment = 'treated', treated.last = T)

# Run synthetic DID approach
sdid.out.p2 <- synthdid_estimate(dataprep.SDID.p2$Y, dataprep.SDID.p2$N0, dataprep.SDID.p2$T0)

# Show main results
summary(sdid.out.p2)
sum(summary(sdid.out.p2)$controls > 0.01)
plot(sdid.out.p2) +
    labs(title = "SDID of Product 2 Sales: Treated vs. Synthetic",
         x = "Week",
         y = "P2 Sales") +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
    )

synthdid_units_plot(sdid.out.p2)
sqrt(vcov(sdid.out.p2, method='placebo'))

sc.out.p1   = sc_estimate(dataprep.SDID.p1$Y, dataprep.SDID.p1$N0, dataprep.SDID.p1$T0)
did.out.p1  = did_estimate(dataprep.SDID.p1$Y, dataprep.SDID.p1$N0, dataprep.SDID.p1$T0)
estimates = list(did.out.p1, sc.out.p1, sdid.out.p1)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates))

synthdid_plot(estimates, facet.vertical = FALSE)

sc.out.p2   = sc_estimate(dataprep.SDID.p2$Y, dataprep.SDID.p2$N0, dataprep.SDID.p2$T0)
did.out.p2  = did_estimate(dataprep.SDID.p2$Y, dataprep.SDID.p2$N0, dataprep.SDID.p2$T0)
estimates.p2 = list(did.out.p2, sc.out.p2, sdid.out.p2)
names(estimates.p2) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates.p2))

synthdid_plot(estimates.p2, facet.vertical = FALSE)