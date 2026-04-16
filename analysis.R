# ============================================================
# IT3011 - Theory and Practices in Statistical Modeling
# Group Assignment: Statistical Analysis
#
# Analytical Statement:
# "Students who feel supported show higher persistence in their studies"
#
# Dataset: Student Performance Factors (6,609 observations)
# ============================================================

# --- Package Setup ---
required_packages <- c("tidyverse", "MASS", "car", "e1071", "corrplot",
                       "nortest", "caret", "fitdistrplus")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, repos = "https://cran.r-project.org")

invisible(lapply(required_packages, library, character.only = TRUE))
set.seed(42)
dir.create("plots", showWarnings = FALSE)


# ============================================================
# PHASE 1: DATA LOADING AND PREPARATION
# ============================================================

data <- read.csv("Student Performance Factors 1.csv", stringsAsFactors = FALSE)

cat("Dataset Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
str(data)

cat("\n--- Missing Values ---\n")
print(colSums(is.na(data)))

# Ordered factor encoding
ordinal_levels <- c("Low", "Medium", "High")
data$Parental_Involvement     <- factor(data$Parental_Involvement, levels = ordinal_levels, ordered = TRUE)
data$Access_to_Resources      <- factor(data$Access_to_Resources,  levels = ordinal_levels, ordered = TRUE)
data$Teacher_Quality           <- factor(data$Teacher_Quality,       levels = ordinal_levels, ordered = TRUE)
data$Motivation_Level          <- factor(data$Motivation_Level,      levels = ordinal_levels, ordered = TRUE)
data$Family_Income             <- factor(data$Family_Income,         levels = ordinal_levels, ordered = TRUE)
data$Peer_Influence            <- factor(data$Peer_Influence,
                                         levels = c("Negative", "Neutral", "Positive"), ordered = TRUE)
data$Parental_Education_Level  <- factor(data$Parental_Education_Level,
                                         levels = c("High School", "College", "Postgraduate"), ordered = TRUE)
data$Distance_from_Home        <- factor(data$Distance_from_Home,
                                         levels = c("Near", "Moderate", "Far"), ordered = TRUE)

data$School_Type                <- factor(data$School_Type)
data$Gender                     <- factor(data$Gender)
data$Extracurricular_Activities <- factor(data$Extracurricular_Activities)
data$Internet_Access            <- factor(data$Internet_Access)
data$Learning_Disabilities      <- factor(data$Learning_Disabilities)

# Numeric encoding for composite index, correlation, and regression
data$Motivation_Num           <- as.numeric(data$Motivation_Level)
data$Extracurricular_Num      <- ifelse(data$Extracurricular_Activities == "Yes", 1, 0)
data$Parental_Involvement_Num <- as.numeric(data$Parental_Involvement)
data$Access_to_Resources_Num  <- as.numeric(data$Access_to_Resources)
data$Teacher_Quality_Num      <- as.numeric(data$Teacher_Quality)
data$Peer_Influence_Num       <- as.numeric(data$Peer_Influence)
data$Family_Income_Num        <- as.numeric(data$Family_Income)
data$Parental_Education_Num   <- as.numeric(data$Parental_Education_Level)
data$Distance_Num             <- as.numeric(data$Distance_from_Home)

# Derived variable
data$Score_Improvement <- data$Exam_Score - data$Previous_Scores

# Composite Persistence Index: row-wise mean of z-scored persistence components
persistence_matrix <- cbind(
  scale(data$Hours_Studied),
  scale(data$Attendance),
  scale(data$Motivation_Num),
  scale(data$Exam_Score),
  scale(data$Score_Improvement),
  scale(data$Extracurricular_Num)
)
data$Persistence_Index <- rowMeans(persistence_matrix)

cat("\n--- Persistence Index Summary ---\n")
print(summary(data$Persistence_Index))
cat("SD:", round(sd(data$Persistence_Index), 4), "\n")


# ============================================================
# PHASE 2: DESCRIPTIVE ANALYTICS (LO1, LO4)
# ============================================================

# --- 2A: Summary Statistics ---

cat("\n\n============ NUMERIC VARIABLE SUMMARIES ============\n")
numeric_vars <- c("Hours_Studied", "Attendance", "Tutoring_Sessions",
                   "Exam_Score", "Previous_Scores", "Score_Improvement",
                   "Persistence_Index")

for (var in numeric_vars) {
  x <- data[[var]]
  cat(sprintf("\n--- %s ---\n", var))
  cat(sprintf("  Mean: %.2f | Median: %.2f | SD: %.2f\n", mean(x), median(x), sd(x)))
  cat(sprintf("  Min: %.2f | Max: %.2f | IQR: %.2f\n", min(x), max(x), IQR(x)))
}

cat("\n\n============ CATEGORICAL VARIABLE FREQUENCIES ============\n")
cat_vars <- c("Parental_Involvement", "Access_to_Resources", "Teacher_Quality",
              "Peer_Influence", "School_Type", "Motivation_Level",
              "Extracurricular_Activities")

for (var in cat_vars) {
  cat(sprintf("\n--- %s ---\n", var))
  tbl <- table(data[[var]])
  pct <- prop.table(tbl) * 100
  print(data.frame(Count = as.numeric(tbl),
                   Percent = round(as.numeric(pct), 1),
                   row.names = names(tbl)))
}

cat("\n\n============ MEAN PERSISTENCE INDEX BY SUPPORT LEVELS ============\n")
support_vars <- c("Parental_Involvement", "Access_to_Resources",
                   "Teacher_Quality", "Peer_Influence", "School_Type")

for (var in support_vars) {
  cat(sprintf("\n--- %s ---\n", var))
  group_stats <- tapply(data$Persistence_Index, data[[var]],
                        function(x) c(Mean = round(mean(x), 4),
                                      SD   = round(sd(x), 4),
                                      N    = length(x)))
  print(do.call(rbind, group_stats))
}


# --- 2B: Visualizations ---

# Histograms for numeric persistence variables
hist_vars <- c("Hours_Studied", "Attendance", "Exam_Score",
               "Score_Improvement", "Persistence_Index")

for (var in hist_vars) {
  p <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  ggsave(sprintf("plots/hist_%s.png", tolower(var)), p, width = 8, height = 6, dpi = 300)
}

# Bar charts for categorical variables
for (var in cat_vars) {
  p <- ggplot(data, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar(alpha = 0.8) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none")
  ggsave(sprintf("plots/bar_%s.png", tolower(var)), p, width = 8, height = 6, dpi = 300)
}

# Box plots: Persistence Index by each support variable
for (var in support_vars) {
  p <- ggplot(data, aes(x = .data[[var]], y = Persistence_Index, fill = .data[[var]])) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    labs(title = paste("Persistence Index by", var),
         x = var, y = "Persistence Index") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none")
  ggsave(sprintf("plots/boxplot_%s.png", tolower(var)), p, width = 8, height = 6, dpi = 300)
}

# Correlation heatmap
cor_vars <- c("Hours_Studied", "Attendance", "Tutoring_Sessions",
              "Exam_Score", "Score_Improvement", "Persistence_Index",
              "Parental_Involvement_Num", "Access_to_Resources_Num",
              "Teacher_Quality_Num", "Peer_Influence_Num")
cor_matrix <- cor(data[, cor_vars], use = "complete.obs")

png("plots/correlation_heatmap.png", width = 1000, height = 800)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7)
dev.off()

# Scatter plot: Tutoring Sessions vs Persistence Index
p <- ggplot(data, aes(x = Tutoring_Sessions, y = Persistence_Index)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Tutoring Sessions vs Persistence Index",
       x = "Tutoring Sessions", y = "Persistence Index") +
  theme_minimal()
ggsave("plots/scatter_tutoring_persistence.png", p, width = 8, height = 6, dpi = 300)


# --- 2C: Probability Distribution Fitting (LO1, LO4) ---

cat("\n\n============ DISTRIBUTION FITTING ============\n")

# ---- Normal Distribution: Exam Score ----
cat("\n--- Normal Distribution: Exam Score ---\n")
fit_norm_exam <- fitdist(data$Exam_Score, "norm")
cat(sprintf("  Estimated Mean: %.2f | SD: %.2f\n",
            fit_norm_exam$estimate["mean"], fit_norm_exam$estimate["sd"]))
ad_exam <- ad.test(data$Exam_Score)
cat(sprintf("  Anderson-Darling: A = %.4f, p-value = %s\n",
            ad_exam$statistic, format.pval(ad_exam$p.value)))

p <- ggplot(data, aes(x = Exam_Score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = fit_norm_exam$estimate["mean"],
                            sd   = fit_norm_exam$estimate["sd"]),
                color = "red", linewidth = 1) +
  labs(title = "Exam Score with Fitted Normal Distribution",
       x = "Exam Score", y = "Density") +
  theme_minimal()
ggsave("plots/fit_normal_exam_score.png", p, width = 8, height = 6, dpi = 300)

# ---- Normal Distribution: Attendance ----
cat("\n--- Normal Distribution: Attendance ---\n")
fit_norm_att <- fitdist(data$Attendance, "norm")
cat(sprintf("  Estimated Mean: %.2f | SD: %.2f\n",
            fit_norm_att$estimate["mean"], fit_norm_att$estimate["sd"]))
ad_att <- ad.test(data$Attendance)
cat(sprintf("  Anderson-Darling: A = %.4f, p-value = %s\n",
            ad_att$statistic, format.pval(ad_att$p.value)))

p <- ggplot(data, aes(x = Attendance)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = fit_norm_att$estimate["mean"],
                            sd   = fit_norm_att$estimate["sd"]),
                color = "red", linewidth = 1) +
  labs(title = "Attendance with Fitted Normal Distribution",
       x = "Attendance", y = "Density") +
  theme_minimal()
ggsave("plots/fit_normal_attendance.png", p, width = 8, height = 6, dpi = 300)

# ---- Normal Distribution: Persistence Index ----
cat("\n--- Normal Distribution: Persistence Index ---\n")
fit_norm_pi <- fitdist(data$Persistence_Index, "norm")
cat(sprintf("  Estimated Mean: %.4f | SD: %.4f\n",
            fit_norm_pi$estimate["mean"], fit_norm_pi$estimate["sd"]))
ad_pi <- ad.test(data$Persistence_Index)
cat(sprintf("  Anderson-Darling: A = %.4f, p-value = %s\n",
            ad_pi$statistic, format.pval(ad_pi$p.value)))

p <- ggplot(data, aes(x = Persistence_Index)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = fit_norm_pi$estimate["mean"],
                            sd   = fit_norm_pi$estimate["sd"]),
                color = "red", linewidth = 1) +
  labs(title = "Persistence Index with Fitted Normal Distribution",
       x = "Persistence Index", y = "Density") +
  theme_minimal()
ggsave("plots/fit_normal_persistence_index.png", p, width = 8, height = 6, dpi = 300)

# QQ Plots for normality assessment
png("plots/qq_plots.png", width = 1200, height = 400)
par(mfrow = c(1, 3))
qqnorm(data$Exam_Score, main = "Q-Q Plot: Exam Score")
qqline(data$Exam_Score, col = "red", lwd = 2)
qqnorm(data$Attendance, main = "Q-Q Plot: Attendance")
qqline(data$Attendance, col = "red", lwd = 2)
qqnorm(data$Persistence_Index, main = "Q-Q Plot: Persistence Index")
qqline(data$Persistence_Index, col = "red", lwd = 2)
dev.off()

# ---- Gamma Distribution: Hours Studied ----
cat("\n--- Gamma Distribution: Hours Studied ---\n")
hours_positive <- data$Hours_Studied[data$Hours_Studied > 0]
if (length(hours_positive) < nrow(data)) {
  cat(sprintf("  Note: %d zero-value observations excluded for Gamma fitting\n",
              nrow(data) - length(hours_positive)))
}
fit_gamma <- fitdist(hours_positive, "gamma")
cat(sprintf("  Estimated Shape: %.4f | Rate: %.4f\n",
            fit_gamma$estimate["shape"], fit_gamma$estimate["rate"]))
gof_gamma <- gofstat(fit_gamma)
cat(sprintf("  KS Statistic: %.4f | CvM Statistic: %.4f | AD Statistic: %.4f\n",
            gof_gamma$ks, gof_gamma$cvm, gof_gamma$ad))

p <- ggplot(data.frame(x = hours_positive), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = dgamma,
                args = list(shape = fit_gamma$estimate["shape"],
                            rate  = fit_gamma$estimate["rate"]),
                color = "red", linewidth = 1) +
  labs(title = "Hours Studied with Fitted Gamma Distribution",
       x = "Hours Studied", y = "Density") +
  theme_minimal()
ggsave("plots/fit_gamma_hours_studied.png", p, width = 8, height = 6, dpi = 300)

# ---- Poisson Distribution: Tutoring Sessions ----
cat("\n--- Poisson Distribution: Tutoring Sessions ---\n")
fit_pois <- fitdist(data$Tutoring_Sessions, "pois")
cat(sprintf("  Estimated Lambda: %.4f\n", fit_pois$estimate["lambda"]))
gof_pois <- gofstat(fit_pois)
cat(sprintf("  Chi-squared Statistic: %.4f\n", gof_pois$chisq))

obs_df <- as.data.frame(table(data$Tutoring_Sessions))
colnames(obs_df) <- c("x", "Observed")
obs_df$x <- as.numeric(as.character(obs_df$x))
obs_df$Observed <- obs_df$Observed / nrow(data)
obs_df$Expected <- dpois(obs_df$x, lambda = fit_pois$estimate["lambda"])

p <- ggplot(obs_df) +
  geom_col(aes(x = x, y = Observed), fill = "steelblue", alpha = 0.7, width = 0.6) +
  geom_line(aes(x = x, y = Expected), color = "red", linewidth = 1) +
  geom_point(aes(x = x, y = Expected), color = "red", size = 2.5) +
  labs(title = "Tutoring Sessions: Observed vs Fitted Poisson Distribution",
       x = "Tutoring Sessions", y = "Probability") +
  theme_minimal()
ggsave("plots/fit_poisson_tutoring.png", p, width = 8, height = 6, dpi = 300)

# ---- Bernoulli Distribution: Extracurricular Activities ----
cat("\n--- Bernoulli Distribution: Extracurricular Activities ---\n")
p_hat <- mean(data$Extracurricular_Num)
cat(sprintf("  Estimated p (participation probability): %.4f\n", p_hat))
cat(sprintf("  Observed: No = %d (%.1f%%) | Yes = %d (%.1f%%)\n",
            sum(data$Extracurricular_Num == 0), (1 - p_hat) * 100,
            sum(data$Extracurricular_Num == 1), p_hat * 100))
# MLE of Bernoulli is the sample proportion, so observed = expected by definition.

binom_df <- data.frame(
  Category = factor(c("No", "Yes"), levels = c("No", "Yes")),
  Observed = c(mean(data$Extracurricular_Num == 0), p_hat),
  Expected = c(1 - p_hat, p_hat)
)
binom_long <- pivot_longer(binom_df, cols = c(Observed, Expected),
                           names_to = "Type", values_to = "Proportion")

p <- ggplot(binom_long, aes(x = Category, y = Proportion, fill = Type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Extracurricular Activities: Observed vs Fitted Bernoulli",
       x = "Participates", y = "Proportion") +
  scale_fill_manual(values = c("Observed" = "steelblue", "Expected" = "tomato")) +
  theme_minimal()
ggsave("plots/fit_bernoulli_extracurricular.png", p, width = 8, height = 6, dpi = 300)


# ============================================================
# PHASE 3: INFERENTIAL ANALYTICS (LO2)
# ============================================================

cat("\n\n============ INFERENTIAL ANALYTICS ============\n")

# --- 3A: Hypothesis Tests for Means (ANOVA) ---

cat("\n--- ANOVA: Persistence Index ~ Parental Involvement ---\n")
cat("H0: mu_Low = mu_Medium = mu_High\n")
cat("H1: At least one group mean differs\n\n")
anova_pi <- aov(Persistence_Index ~ Parental_Involvement, data = data)
print(summary(anova_pi))
cat("\nTukey HSD Post-Hoc:\n")
print(TukeyHSD(anova_pi))

cat("\n\n--- ANOVA: Persistence Index ~ Teacher Quality ---\n")
cat("H0: mu_Low = mu_Medium = mu_High\n")
cat("H1: At least one group mean differs\n\n")
anova_tq <- aov(Persistence_Index ~ Teacher_Quality, data = data)
print(summary(anova_tq))
cat("\nTukey HSD Post-Hoc:\n")
print(TukeyHSD(anova_tq))

cat("\n\n--- ANOVA: Persistence Index ~ Access to Resources ---\n")
cat("H0: mu_Low = mu_Medium = mu_High\n")
cat("H1: At least one group mean differs\n\n")
anova_ar <- aov(Persistence_Index ~ Access_to_Resources, data = data)
print(summary(anova_ar))
cat("\nTukey HSD Post-Hoc:\n")
print(TukeyHSD(anova_ar))

cat("\n\n--- ANOVA: Persistence Index ~ Peer Influence ---\n")
cat("H0: mu_Negative = mu_Neutral = mu_Positive\n")
cat("H1: At least one group mean differs\n\n")
anova_peer <- aov(Persistence_Index ~ Peer_Influence, data = data)
print(summary(anova_peer))
cat("\nTukey HSD Post-Hoc:\n")
print(TukeyHSD(anova_peer))

# --- 3B: Two-Sample t-test ---

cat("\n\n--- Two-Sample t-test: Persistence Index ~ School Type ---\n")
cat("H0: mu_Public = mu_Private\n")
cat("H1: mu_Public != mu_Private\n\n")
t_school <- t.test(Persistence_Index ~ School_Type, data = data)
print(t_school)

# --- 3C: Hypothesis Test for Proportions ---

cat("\n\n--- Two-Proportion z-test: High Persistence by Parental Involvement ---\n")
data$High_Persistence <- ifelse(data$Persistence_Index > median(data$Persistence_Index), 1, 0)

high_inv <- data[data$Parental_Involvement == "High", ]
low_inv  <- data[data$Parental_Involvement == "Low", ]

successes <- c(sum(high_inv$High_Persistence), sum(low_inv$High_Persistence))
totals    <- c(nrow(high_inv), nrow(low_inv))

cat("H0: p_HighInvolvement = p_LowInvolvement (proportion above median persistence)\n")
cat("H1: p_HighInvolvement != p_LowInvolvement\n\n")
cat(sprintf("  High Involvement group: %d/%d = %.1f%% above median\n",
            successes[1], totals[1], successes[1] / totals[1] * 100))
cat(sprintf("  Low  Involvement group: %d/%d = %.1f%% above median\n",
            successes[2], totals[2], successes[2] / totals[2] * 100))

prop_result <- prop.test(successes, totals)
print(prop_result)

# --- 3D: Hypothesis Tests for Variances ---

cat("\n\n--- Levene's Test: Variance of Persistence Index across Parental Involvement ---\n")
cat("H0: sigma^2_Low = sigma^2_Medium = sigma^2_High\n")
cat("H1: At least one group variance differs\n\n")
levene_result <- leveneTest(Persistence_Index ~ Parental_Involvement, data = data)
print(levene_result)

cat("\n--- F-test: Variance of Persistence Index (Public vs Private) ---\n")
cat("H0: sigma^2_Public = sigma^2_Private\n")
cat("H1: sigma^2_Public != sigma^2_Private\n\n")
f_result <- var.test(Persistence_Index ~ School_Type, data = data)
print(f_result)

# --- 3E: Correlation Test ---

cat("\n\n--- Pearson Correlation: Tutoring Sessions vs Persistence Index ---\n")
cat("H0: rho = 0 (no linear correlation)\n")
cat("H1: rho != 0\n\n")
cor_result <- cor.test(data$Tutoring_Sessions, data$Persistence_Index, method = "pearson")
print(cor_result)


# ============================================================
# PHASE 4A: MULTIPLE LINEAR REGRESSION (LO3)
# ============================================================

cat("\n\n============ MULTIPLE LINEAR REGRESSION ============\n")

model_data <- data[, c("Persistence_Index",
                        "Parental_Involvement_Num", "Access_to_Resources_Num",
                        "Tutoring_Sessions", "Teacher_Quality_Num",
                        "Peer_Influence_Num", "School_Type",
                        "Family_Income_Num", "Parental_Education_Num",
                        "Gender", "Distance_Num")]

full_model <- lm(Persistence_Index ~ ., data = model_data)
cat("\n--- Full Model Summary ---\n")
print(summary(full_model))

# Backward elimination using AIC
cat("\n--- Variable Selection (Backward Elimination via AIC) ---\n")
final_model <- step(full_model, direction = "backward", trace = 1)

cat("\n--- Final (Reduced) Model Summary ---\n")
print(summary(final_model))
cat(sprintf("\nR-squared: %.4f\n", summary(final_model)$r.squared))
cat(sprintf("Adjusted R-squared: %.4f\n", summary(final_model)$adj.r.squared))

# Multicollinearity check
cat("\n--- Variance Inflation Factors (VIF) ---\n")
vif_values <- vif(final_model)
print(vif_values)
cat("(VIF > 5 indicates concerning multicollinearity)\n")

# Regression diagnostics plots
png("plots/regression_diagnostics.png", width = 1200, height = 1000)
par(mfrow = c(2, 2))
plot(final_model)
dev.off()

# Cook's Distance for influential observations
cooks_d <- cooks.distance(final_model)
threshold <- 4 / nrow(model_data)
influential <- which(cooks_d > threshold)
cat(sprintf("\n--- Influential Observations (Cook's D > 4/n = %.6f) ---\n", threshold))
cat(sprintf("  Count: %d out of %d (%.1f%%)\n",
            length(influential), nrow(model_data),
            length(influential) / nrow(model_data) * 100))


# ============================================================
# PHASE 4B: NAIVE BAYES CLASSIFICATION (LO6)
# ============================================================

cat("\n\n============ NAIVE BAYES CLASSIFICATION ============\n")

# Persistence categories based on tertiles
tertile_breaks <- quantile(data$Persistence_Index, probs = c(0, 1/3, 2/3, 1))
data$Persistence_Category <- cut(data$Persistence_Index,
                                  breaks = tertile_breaks,
                                  labels = c("Low", "Medium", "High"),
                                  include.lowest = TRUE)

cat("\nPersistence Category Distribution:\n")
print(table(data$Persistence_Category))
cat(sprintf("Tertile Boundaries: [%.4f, %.4f] | (%.4f, %.4f] | (%.4f, %.4f]\n",
            tertile_breaks[1], tertile_breaks[2],
            tertile_breaks[2], tertile_breaks[3],
            tertile_breaks[3], tertile_breaks[4]))

# 70/30 train-test split
train_idx  <- createDataPartition(data$Persistence_Category, p = 0.7, list = FALSE)
train_data <- data[train_idx, ]
test_data  <- data[-train_idx, ]
cat(sprintf("\nTrain: %d rows | Test: %d rows\n", nrow(train_data), nrow(test_data)))

# Naive Bayes model using support variables as predictors
nb_features <- c("Parental_Involvement", "Access_to_Resources", "Tutoring_Sessions",
                  "Teacher_Quality", "Peer_Influence", "School_Type")
nb_formula  <- as.formula(paste("Persistence_Category ~", paste(nb_features, collapse = " + ")))

nb_model <- naiveBayes(nb_formula, data = train_data)
cat("\n--- Naive Bayes A-Priori Probabilities ---\n")
print(nb_model$apriori)

# Predictions and evaluation
nb_pred <- predict(nb_model, newdata = test_data)

cat("\n--- Confusion Matrix ---\n")
cm <- confusionMatrix(nb_pred, test_data$Persistence_Category)
print(cm)

cat(sprintf("\nOverall Accuracy: %.2f%%\n", cm$overall["Accuracy"] * 100))
cat(sprintf("Kappa: %.4f\n", cm$overall["Kappa"]))

# Per-class metrics
cat("\n--- Per-Class Statistics ---\n")
print(cm$byClass[, c("Sensitivity", "Specificity", "Precision", "Recall", "F1")])


# ============================================================
# PHASE 5: SYNTHESIS AND CONCLUSION
# ============================================================

cat("\n\n")
cat("============================================================\n")
cat("  SYNTHESIS: ANSWERING THE ANALYTICAL STATEMENT\n")
cat("  'Students who feel supported show higher persistence'\n")
cat("============================================================\n\n")

cat("1. DESCRIPTIVE FINDINGS\n")
cat("   Mean Persistence Index by support variable levels:\n")
for (var in support_vars) {
  means <- tapply(data$Persistence_Index, data[[var]], mean)
  cat(sprintf("   %s: %s\n", var,
              paste(names(means), "=", round(means, 4), collapse = " | ")))
}

cat("\n2. INFERENTIAL FINDINGS\n")
cat("   ANOVA results (alpha = 0.05):\n")
anova_pvals <- list(
  Parental_Involvement = summary(anova_pi)[[1]][["Pr(>F)"]][1],
  Teacher_Quality      = summary(anova_tq)[[1]][["Pr(>F)"]][1],
  Access_to_Resources  = summary(anova_ar)[[1]][["Pr(>F)"]][1],
  Peer_Influence       = summary(anova_peer)[[1]][["Pr(>F)"]][1]
)
for (name in names(anova_pvals)) {
  sig <- ifelse(anova_pvals[[name]] < 0.05, "SIGNIFICANT", "NOT significant")
  cat(sprintf("   - %s: p = %s (%s)\n", name, format.pval(anova_pvals[[name]]), sig))
}
cat(sprintf("   - School Type (t-test): p = %s (%s)\n",
            format.pval(t_school$p.value),
            ifelse(t_school$p.value < 0.05, "SIGNIFICANT", "NOT significant")))
cat(sprintf("   - Proportion test (High vs Low Involvement): p = %s (%s)\n",
            format.pval(prop_result$p.value),
            ifelse(prop_result$p.value < 0.05, "SIGNIFICANT", "NOT significant")))
cat(sprintf("   - Tutoring-Persistence correlation: r = %.4f, p = %s (%s)\n",
            cor_result$estimate,
            format.pval(cor_result$p.value),
            ifelse(cor_result$p.value < 0.05, "SIGNIFICANT", "NOT significant")))

cat("\n3. PREDICTIVE FINDINGS (Regression)\n")
cat(sprintf("   Adjusted R-squared: %.4f\n", summary(final_model)$adj.r.squared))
coef_tbl <- summary(final_model)$coefficients
sig_predictors <- rownames(coef_tbl)[coef_tbl[, "Pr(>|t|)"] < 0.05 &
                                      rownames(coef_tbl) != "(Intercept)"]
cat("   Significant predictors in final model:\n")
for (v in sig_predictors) {
  cat(sprintf("   - %s (coef = %.4f, p = %s)\n",
              v, coef_tbl[v, "Estimate"], format.pval(coef_tbl[v, "Pr(>|t|)"])))
}

cat("\n4. PREDICTIVE FINDINGS (Naive Bayes)\n")
cat(sprintf("   Classification accuracy: %.2f%%\n", cm$overall["Accuracy"] * 100))

cat("\n============================================================\n")
cat("  Analysis complete. All plots saved to ./plots/\n")
cat("============================================================\n")
