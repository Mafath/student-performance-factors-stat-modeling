# ---- setup ----
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE,
                      fig.width = 8, fig.height = 6, fig.align = "center")

# ---- packages ----
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret", dependencies = TRUE, repos = "https://cran.r-project.org")
}

required_packages <- c("tidyverse", "MASS", "car", "corrplot", "nortest",
                       "caret", "fitdistrplus", "rpart", "rpart.plot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  tryCatch(
    install.packages(new_packages, dependencies = TRUE, repos = "https://cran.r-project.org"),
    error = function(e) message("Package installation skipped in this environment: ", conditionMessage(e))
  )
}

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    library(pkg, character.only = TRUE)
  } else {
    message("Package not available: ", pkg)
  }
}
set.seed(42)

# ---- load-data ----
dataset_file <- "Student Performance Factors.csv"
data <- read.csv(dataset_file, stringsAsFactors = FALSE)

# ---- structure ----
str(data)

# ---- missing-values ----
missing_summary <- data.frame(
  Variable = names(data),
  NA_Count = sapply(data, function(x) sum(is.na(x))),
  Blank_Count = sapply(data, function(x) {
    if (is.character(x)) sum(!is.na(x) & trimws(x) == "") else 0
  }),
  row.names = NULL
)

missing_summary$Total_Missing <- missing_summary$NA_Count + missing_summary$Blank_Count

# convert blank strings to NA for character columns
data <- data %>%mutate(across(where(is.character), ~ na_if(trimws(.x), "")))

rows_before_complete_case <- nrow(data)
data <- data %>% drop_na()
rows_after_complete_case <- nrow(data)
rows_removed_complete_case <- rows_before_complete_case - rows_after_complete_case

# ---- encoding-validation ----
ordinal_levels <- c("Low", "Medium", "High")
peer_levels <- c("Negative", "Neutral", "Positive")
distance_levels <- c("Near", "Moderate", "Far")
education_levels <- c("High School", "College", "Postgraduate")

validate_levels <- function(variable, allowed_levels) {
  actual_levels <- unique(data[[variable]][!is.na(data[[variable]])])
  invalid_levels <- setdiff(actual_levels, allowed_levels)

  if (length(invalid_levels) > 0) {
    stop(paste(
      "Unexpected value(s) in", variable, ":",
      paste(invalid_levels, collapse = ", ")
    ))
  }
}

validate_levels("Parental_Involvement", ordinal_levels)
validate_levels("Access_to_Resources", ordinal_levels)
validate_levels("Motivation_Level", ordinal_levels)
validate_levels("Teacher_Quality", ordinal_levels)
validate_levels("Family_Income", ordinal_levels)
validate_levels("Peer_Influence", peer_levels)
validate_levels("Distance_from_Home", distance_levels)
validate_levels("Parental_Education_Level", education_levels)
validate_levels("Extracurricular_Activities", c("No", "Yes"))
validate_levels("Internet_Access", c("No", "Yes"))
validate_levels("Learning_Disabilities", c("No", "Yes"))
validate_levels("Gender", c("Female", "Male"))
validate_levels("School_Type", c("Public", "Private"))

# ---- ordinal-encoding ----
data$Parental_Involvement <- factor(data$Parental_Involvement,levels = ordinal_levels, ordered = TRUE)
data$Access_to_Resources <- factor(data$Access_to_Resources,levels = ordinal_levels, ordered = TRUE)
data$Motivation_Level <- factor(data$Motivation_Level,levels = ordinal_levels, ordered = TRUE)
data$Teacher_Quality <- factor(data$Teacher_Quality,levels = ordinal_levels, ordered = TRUE)
data$Family_Income <- factor(data$Family_Income,levels = ordinal_levels, ordered = TRUE)
data$Peer_Influence <- factor(data$Peer_Influence,levels = peer_levels, ordered = TRUE)
data$Distance_from_Home <- factor(data$Distance_from_Home,levels = distance_levels, ordered = TRUE)
data$Parental_Education_Level <- factor(data$Parental_Education_Level,levels = education_levels, ordered = TRUE)

# Convert to numeric for modelling
data$Parental_Involvement_Num <- as.numeric(data$Parental_Involvement)
data$Access_to_Resources_Num <- as.numeric(data$Access_to_Resources)
data$Motivation_Level_Num <- as.numeric(data$Motivation_Level)
data$Teacher_Quality_Num <- as.numeric(data$Teacher_Quality)
data$Family_Income_Num <- as.numeric(data$Family_Income)
data$Peer_Influence_Num <- as.numeric(data$Peer_Influence)
data$Distance_from_Home_Num <- as.numeric(data$Distance_from_Home)
data$Parental_Education_Level_Num <- as.numeric(data$Parental_Education_Level)

# ---- binary-encoding ----
data$Extracurricular_Activities_Num <- ifelse(data$Extracurricular_Activities == "Yes", 1,ifelse(data$Extracurricular_Activities == "No", 0, NA))
data$Internet_Access_Num <- ifelse(data$Internet_Access == "Yes", 1,ifelse(data$Internet_Access == "No", 0, NA))
data$Learning_Disabilities_Num <- ifelse(data$Learning_Disabilities == "Yes", 1,ifelse(data$Learning_Disabilities == "No", 0, NA))
data$Gender_Num <- ifelse(data$Gender == "Male", 1,ifelse(data$Gender == "Female", 0, NA))
data$School_Type_Num <- ifelse(data$School_Type == "Private", 1,ifelse(data$School_Type == "Public", 0, NA))

# ---- factor-retention ----
data$Parental_Involvement <- factor(data$Parental_Involvement,levels = ordinal_levels)
data$Access_to_Resources <- factor(data$Access_to_Resources,levels = ordinal_levels)
data$Motivation_Level <- factor(data$Motivation_Level,levels = ordinal_levels)
data$Teacher_Quality <- factor(data$Teacher_Quality,levels = ordinal_levels)
data$Family_Income <- factor(data$Family_Income,levels = ordinal_levels)
data$Peer_Influence <- factor(data$Peer_Influence,levels = peer_levels)
data$Distance_from_Home <- factor(data$Distance_from_Home,levels = distance_levels)
data$Parental_Education_Level <- factor(data$Parental_Education_Level,levels = education_levels)
data$Extracurricular_Activities <- factor(data$Extracurricular_Activities, levels = c("No", "Yes"))
data$Internet_Access <- factor(data$Internet_Access, levels = c("No", "Yes"))
data$Learning_Disabilities <- factor(data$Learning_Disabilities, levels = c("No", "Yes"))
data$Gender <- factor(data$Gender, levels = c("Female", "Male"))
data$School_Type <- factor(data$School_Type, levels = c("Public", "Private"))

# ---- encoding-verification ----
encoded_vars <- c(
  "Parental_Involvement_Num",
  "Access_to_Resources_Num",
  "Motivation_Level_Num",
  "Teacher_Quality_Num",
  "Family_Income_Num",
  "Peer_Influence_Num",
  "Distance_from_Home_Num",
  "Parental_Education_Level_Num",
  "Extracurricular_Activities_Num",
  "Internet_Access_Num",
  "Learning_Disabilities_Num",
  "Gender_Num",
  "School_Type_Num"
)

encoding_check <- data.frame(
  Variable = encoded_vars,
  Encoded_Missing = sapply(data[encoded_vars], function(x) sum(is.na(x))),
  row.names = NULL
)

print(encoding_check)

# ---- score-improvement ----
data$Score_Improvement <- data$Exam_Score - data$Previous_Scores

# ---- persistence-index ----
scale_to_minus1_plus1 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[1], rng[2]))) {
    return(rep(0, length(x)))
  }
  2 * ((x - rng[1]) / (rng[2] - rng[1])) - 1
}

data$Hours_Studied_Scaled <- scale_to_minus1_plus1(data$Hours_Studied)
data$Attendance_Scaled <- scale_to_minus1_plus1(data$Attendance)
data$Score_Improvement_Scaled <- scale_to_minus1_plus1(data$Score_Improvement)

data$Persistence_Index <- rowMeans(
  data[, c("Hours_Studied_Scaled", "Attendance_Scaled", "Score_Improvement_Scaled")]
)

# ---- pi-summary ----
summary(data$Persistence_Index)

# ---- encoded-numeric-preview ----
numeric_vars_current <- names(data)[sapply(data, is.numeric)]
model_input_cols <- unique(c(numeric_vars_current, encoded_vars))
model_input_cols <- setdiff(model_input_cols, c(
  "Previous_Scores", "Exam_Score",
  "Hours_Studied_Scaled", "Attendance_Scaled", "Score_Improvement_Scaled"
))

encoded_numeric_df <- data[, model_input_cols]

# ---- numeric-summary ----
if ("Persistence_Index" %in% names(encoded_numeric_df)) {
  persistence_col <- "Persistence_Index"
} else if ("persistence_index" %in% names(encoded_numeric_df)) {
  persistence_col <- "persistence_index"
} else {
  stop("Persistence index column was not found in encoded_numeric_df.")
}

numeric_summary_df <- data.frame(
  Hours_Studied = data$Hours_Studied,
  Attendance = data$Attendance,
  Sleep_Hours = data$Sleep_Hours,
  Previous_Scores = data$Previous_Scores,
  Tutoring_Sessions = data$Tutoring_Sessions,
  Physical_Activity = data$Physical_Activity,
  Exam_Score = data$Exam_Score,
  Persistence_Index = encoded_numeric_df[[persistence_col]]
)

numeric_vars <- names(numeric_summary_df)

stats_df <- data.frame(
  Variable = numeric_vars,
  Mean   = sapply(numeric_summary_df, mean),
  Median = sapply(numeric_summary_df, median),
  SD     = sapply(numeric_summary_df, sd),
  Min    = sapply(numeric_summary_df, min),
  Max    = sapply(numeric_summary_df, max),
  IQR    = sapply(numeric_summary_df, IQR)
)

print(stats_df)

# ---- categorical-summary ----
cat_vars <- c("Parental_Involvement", "Access_to_Resources", "Teacher_Quality",
              "Peer_Influence", "School_Type", "Motivation_Level",
              "Extracurricular_Activities")

for (var in cat_vars) {
  tbl <- table(data[[var]])
  pct <- prop.table(tbl) * 100
  freq_df <- data.frame(Level = names(tbl), Count = as.numeric(tbl), Percent = round(as.numeric(pct), 1))
  print(sprintf("\n**%s:**\n\n", var))
  print(freq_df)
  print("\n")
}

# ---- persistence-by-support ----
support_vars <- c("Parental_Involvement", "Access_to_Resources","Tutoring_Sessions", "Teacher_Quality", "Peer_Influence")

for (var in support_vars) {
  group_stats <- data %>%
    group_by(.data[[var]]) %>%
    summarise(Mean = round(mean(Persistence_Index), 4),
              SD   = round(sd(Persistence_Index), 4),
              N    = n(), .groups = "drop")
  print(sprintf("\n**%s:**\n\n", var))
  print(group_stats)
  print("\n")
}

# ---- histograms ----
# Numeric variables (histograms)
hist_vars <- c("Hours_Studied", "Attendance","Score_Improvement", "Persistence_Index")

for (var in hist_vars) {
  p <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  print(p)
}

# Motivation level (categorical)
ggplot(data, aes(x = Motivation_Level)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  labs(title = "Distribution of Motivation Level", x = "Motivation Level", y = "Count") +
  theme_minimal()

# ---- bar-charts ----
for (var in cat_vars) {
  p <- ggplot(data, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar(alpha = 0.8) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none")
  print(p)
}

# ---- boxplots ----
support_varss <- c("Parental_Involvement", "Access_to_Resources", "Teacher_Quality", "Peer_Influence")
for (var in support_varss) {
  p <- ggplot(data, aes(x = .data[[var]], y = Persistence_Index, fill = .data[[var]])) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    labs(title = paste("Persistence Index by", var),
         x = var, y = "Persistence Index") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none")
  print(p)
}

# ---- correlation-heatmap ----
cor_vars <- setdiff(
  names(encoded_numeric_df),
  c("Hours_Studied", "Attendance", "Score_Improvement")
)

cor_matrix <- cor(encoded_numeric_df[, cor_vars], use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7)

# ---- correlation-screen-table ----
cor_with_persistence <- cor_matrix["Persistence_Index", -1]

cor_screen_df <- data.frame(
  Variable = names(cor_with_persistence),
  Correlation_With_Persistence = round(as.numeric(cor_with_persistence), 4)
)

cor_screen_df <- cor_screen_df[order(-abs(cor_screen_df$Correlation_With_Persistence)), ]

print(cor_screen_df)

# ---- fit-normal-pi ----
fit_norm_pi <- fitdist(data$Persistence_Index, "norm")
ad_pi <- ad.test(data$Persistence_Index)

# ---- plot-normal-pi ----
ggplot(data, aes(x = Persistence_Index)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = fit_norm_pi$estimate["mean"],
                            sd   = fit_norm_pi$estimate["sd"]),
                color = "red", linewidth = 1) +
  labs(title = "Persistence Index with Fitted Normal Distribution",
       x = "Persistence Index", y = "Density") +
  theme_minimal()

# ---- qq-plots ----
par(mfrow = c(1, 3))
qqnorm(data$Persistence_Index, main = "Q-Q Plot: Persistence Index")
qqline(data$Persistence_Index, col = "red", lwd = 2)

# ---- fit-gamma ----
hours_positive <- data$Hours_Studied[data$Hours_Studied > 0]
fit_gamma <- fitdist(hours_positive, "gamma")
gof_gamma <- gofstat(fit_gamma)

# ---- plot-gamma ----
ggplot(data.frame(x = hours_positive), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  stat_function(fun = dgamma,
                args = list(shape = fit_gamma$estimate["shape"],
                            rate  = fit_gamma$estimate["rate"]),
                color = "red", linewidth = 1) +
  labs(title = "Hours Studied with Fitted Gamma Distribution",
       x = "Hours Studied", y = "Density") +
  theme_minimal()

# ---- fit-poisson ----
fit_pois <- fitdist(data$Tutoring_Sessions, "pois")
gof_pois <- gofstat(fit_pois)

# ---- plot-poisson ----
obs_df <- as.data.frame(table(data$Tutoring_Sessions))
colnames(obs_df) <- c("x", "Observed")
obs_df$x <- as.numeric(as.character(obs_df$x))
obs_df$Observed <- obs_df$Observed / nrow(data)
obs_df$Expected <- dpois(obs_df$x, lambda = fit_pois$estimate["lambda"])

ggplot(obs_df) +
  geom_col(aes(x = x, y = Observed), fill = "steelblue", alpha = 0.7, width = 0.6) +
  geom_line(aes(x = x, y = Expected), color = "red", linewidth = 1) +
  geom_point(aes(x = x, y = Expected), color = "red", size = 2.5) +
  labs(title = "Tutoring Sessions: Observed vs Fitted Poisson Distribution",
       x = "Number of Tutoring Sessions", y = "Probability") +
  theme_minimal()

# ---- fit-bernoulli ----
p_hat <- mean(data$Extracurricular_Activities_Num)

# ---- plot-bernoulli ----
binom_df <- data.frame(
  Category = factor(c("No", "Yes"), levels = c("No", "Yes")),
  Observed = c(mean(data$Extracurricular_Activities_Num == 0), p_hat),
  Expected = c(1 - p_hat, p_hat)
)
binom_long <- pivot_longer(binom_df, cols = c(Observed, Expected),
                           names_to = "Type", values_to = "Proportion")

ggplot(binom_long, aes(x = Category, y = Proportion, fill = Type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Extracurricular Activities: Observed vs Fitted Bernoulli",
       x = "Participates", y = "Proportion") +
  scale_fill_manual(values = c("Observed" = "steelblue", "Expected" = "tomato")) +
  theme_minimal()

# ---- anova-candidate-vars ----
all_anova_candidate_vars <- c(
  "Sleep_Hours",
  "Tutoring_Sessions",
  "Physical_Activity",
  "Parental_Involvement_Num",
  "Access_to_Resources_Num",
  "Motivation_Level_Num",
  "Teacher_Quality_Num",
  "Family_Income_Num",
  "Peer_Influence_Num",
  "Distance_from_Home_Num",
  "Parental_Education_Level_Num",
  "Extracurricular_Activities_Num",
  "Internet_Access_Num",
  "Learning_Disabilities_Num",
  "Gender_Num",
  "School_Type_Num"
)

significant_anova_vars <- c(
  "Tutoring_Sessions",
  "Parental_Involvement_Num",
  "Access_to_Resources_Num",
  "Motivation_Level_Num",
  "Teacher_Quality_Num",
  "Peer_Influence_Num"
)

remaining_anova_vars <- setdiff(all_anova_candidate_vars, significant_anova_vars)

missing_anova_vars <- setdiff(all_anova_candidate_vars, names(encoded_numeric_df))
if (length(missing_anova_vars) > 0) {
  stop(paste("Missing ANOVA variable(s) in encoded_numeric_df:",
             paste(missing_anova_vars, collapse = ", ")))
}

extract_anova_row <- function(df, var_name) {
  tmp_df <- df[, c("Persistence_Index", var_name)]
  group_factor <- factor(tmp_df[[var_name]])
  fit <- aov(tmp_df$Persistence_Index ~ group_factor)
  fit_anova <- summary(fit)[[1]]

  data.frame(
    Variable = var_name,
    Groups = nlevels(group_factor),
    Df = fit_anova$Df[1],
    F_value = fit_anova$`F value`[1],
    P_value = fit_anova$`Pr(>F)`[1],
    stringsAsFactors = FALSE
  )
}

anova_results <- lapply(all_anova_candidate_vars, function(v) {
  extract_anova_row(encoded_numeric_df, v)
}) %>% bind_rows() %>%
  mutate(
    Significant = ifelse(P_value < 0.05, "Yes", "No"),
    F_value = round(F_value, 4),
    P_value = round(P_value, 6)
  ) %>%
  arrange(P_value)

# ---- anova-significant-separate ----
anova_significant_results <- lapply(significant_anova_vars, function(v) {
  extract_anova_row(encoded_numeric_df, v)
}) %>% bind_rows()

# ---- anova-tutoring-sessions ----
anova_tutoring_sessions <- aov(Persistence_Index ~ factor(Tutoring_Sessions), data = encoded_numeric_df)
anova_tutoring_sessions_p <- summary(anova_tutoring_sessions)[[1]]$`Pr(>F)`[1]
summary(anova_tutoring_sessions)

# ---- anova-parental-involvement-num ----
anova_parental_involvement_num <- aov(Persistence_Index ~ factor(Parental_Involvement_Num), data = encoded_numeric_df)
anova_parental_involvement_num_p <- summary(anova_parental_involvement_num)[[1]]$`Pr(>F)`[1]
summary(anova_parental_involvement_num)

# ---- anova-access-to-resources-num ----
anova_access_to_resources_num <- aov(Persistence_Index ~ factor(Access_to_Resources_Num), data = encoded_numeric_df)
anova_access_to_resources_num_p <- summary(anova_access_to_resources_num)[[1]]$`Pr(>F)`[1]
summary(anova_access_to_resources_num)

# ---- anova-motivation-level-num ----
anova_motivation_level_num <- aov(Persistence_Index ~ factor(Motivation_Level_Num), data = encoded_numeric_df)
anova_motivation_level_num_p <- summary(anova_motivation_level_num)[[1]]$`Pr(>F)`[1]
summary(anova_motivation_level_num)

# ---- anova-teacher-quality-num ----
anova_teacher_quality_num <- aov(Persistence_Index ~ factor(Teacher_Quality_Num), data = encoded_numeric_df)
anova_teacher_quality_num_p <- summary(anova_teacher_quality_num)[[1]]$`Pr(>F)`[1]
summary(anova_teacher_quality_num)

# ---- anova-peer-influence-num ----
anova_peer_influence_num <- aov(Persistence_Index ~ factor(Peer_Influence_Num), data = encoded_numeric_df)
anova_peer_influence_num_p <- summary(anova_peer_influence_num)[[1]]$`Pr(>F)`[1]
summary(anova_peer_influence_num)

# ---- anova-remaining-vars ----
anova_remaining_results <- lapply(remaining_anova_vars, function(v) {
  extract_anova_row(encoded_numeric_df, v)
}) %>% bind_rows() %>%
  mutate(
    Significant = ifelse(P_value < 0.05, "Yes", "No"),
    F_value = round(F_value, 4),
    P_value = round(P_value, 6)
  ) %>%
  arrange(P_value)

print(anova_remaining_results)

# ---- anova-combined-results ----
anova_results <- bind_rows(anova_significant_results, anova_remaining_results) %>%
  mutate(
    Significant = ifelse(P_value < 0.05, "Yes", "No"),
    F_value = round(F_value, 4),
    P_value = round(P_value, 6)
  ) %>%
  arrange(P_value)

# ---- t-test-school ----
t_school <- t.test(Persistence_Index ~ School_Type, data = data)
t_school

# ---- proportion-test ----
data$High_Persistence <- ifelse(data$Persistence_Index > median(data$Persistence_Index), 1, 0)

high_inv <- data[data$Parental_Involvement == "High", ]
low_inv  <- data[data$Parental_Involvement == "Low", ]

successes <- c(sum(high_inv$High_Persistence), sum(low_inv$High_Persistence))
totals    <- c(nrow(high_inv), nrow(low_inv))

prop_result <- prop.test(successes, totals)

# ---- prop-result ----
prop_result

# ---- levene-test ----
levene_result <- leveneTest(Persistence_Index ~ Parental_Involvement, data = data)
levene_pval <- levene_result[["Pr(>F)"]][1]
levene_result

# ---- f-test ----
f_result <- var.test(Persistence_Index ~ School_Type, data = data)
f_result

# ---- predictive-train-test-split ----
set.seed(42)

predictive_data <- data[, c("Persistence_Index",
                            "Parental_Involvement_Num", "Access_to_Resources_Num",
                            "Tutoring_Sessions",
                            "Teacher_Quality_Num", "Peer_Influence_Num")]

train_idx_predictive <- createDataPartition(predictive_data$Persistence_Index, p = 0.8, list = FALSE)
train_predictive <- predictive_data[train_idx_predictive, ]
test_predictive  <- predictive_data[-train_idx_predictive, ]

# ---- regression-full ----
model_data <- train_predictive

full_model <- lm(Persistence_Index ~ ., data = model_data)
summary(full_model)

# ---- model-metrics ----
r_sq     <- summary(full_model)$r.squared
adj_r_sq <- summary(full_model)$adj.r.squared

# ---- significant-predictors ----
coef_tbl <- summary(full_model)$coefficients
sig_mask <- coef_tbl[, "Pr(>|t|)"] < 0.05 & rownames(coef_tbl) != "(Intercept)"
if (any(sig_mask)) {
  sig_df <- data.frame(
    Predictor  = rownames(coef_tbl)[sig_mask],
    Coefficient = round(coef_tbl[sig_mask, "Estimate"], 4),
    P_Value    = format.pval(coef_tbl[sig_mask, "Pr(>|t|)"])
  )
  print(sig_df)
}

# ---- diagnostics ----
par(mfrow = c(2, 2))
plot(full_model)

# ---- vif ----
vif_values <- vif(full_model)
vif_df <- data.frame(Variable = names(vif_values), VIF = round(as.numeric(vif_values), 2))
print(vif_df)

# ---- cooks-distance ----ncooks
cooks_d <- cooks.distance(full_model)
threshold <- 4 / nrow(model_data)
influential <- which(cooks_d > threshold)

# ---- tree-train-test-split ----
tree_data <- predictive_data
train_tree <- train_predictive
test_tree  <- test_predictive

# ---- tree-model-training ----
tree_model <- rpart(
  Persistence_Index ~ .,
  data = train_tree,
  method = "anova"
)

printcp(tree_model)

# ---- tree-visualization ----
if (requireNamespace("rpart.plot", quietly = TRUE)) {
  rpart.plot::rpart.plot(
    tree_model,
    type = 2,
    extra = 101,
    fallen.leaves = TRUE,
    main = "Decision Tree Regression for Persistence Index"
  )
} else {
  plot(tree_model)
  text(tree_model, use.n = TRUE, cex = 0.8)
}

# ---- tree-model-evaluation ----
tree_pred <- predict(tree_model, newdata = test_tree)

tree_rmse <- RMSE(tree_pred, test_tree$Persistence_Index)
tree_r2   <- R2(tree_pred, test_tree$Persistence_Index)

tree_metrics <- data.frame(
  Model = "Decision Tree Regression",
  RMSE = round(tree_rmse, 4),
  R_Squared = round(tree_r2, 4)
)

print(tree_metrics)

# ---- tree-vs-linear-comparison ----
lm_pred <- predict(full_model, newdata = test_tree)

lm_rmse <- RMSE(lm_pred, test_tree$Persistence_Index)
lm_r2   <- R2(lm_pred, test_tree$Persistence_Index)
n_test <- nrow(test_tree)
p_pred <- ncol(train_tree) - 1
lm_adj_r2 <- 1 - (1 - lm_r2) * ((n_test - 1) / (n_test - p_pred - 1))
tree_adj_r2 <- 1 - (1 - tree_r2) * ((n_test - 1) / (n_test - p_pred - 1))

comparison_df <- data.frame(
  Model = c("Linear Regression", "Decision Tree Regression"),
  `R^2` = round(c(lm_r2, tree_r2), 4),
  `Adjusted R^2` = round(c(lm_adj_r2, tree_adj_r2), 4),
  RMSE = round(c(lm_rmse, tree_rmse), 4)
)

print(comparison_df)

# ---- conclusion-setup ----
anova_pvals <- setNames(anova_results$P_value, anova_results$Variable)
sig_anova <- names(anova_pvals)[sapply(anova_pvals, function(p) p < 0.05)]
nonsig_anova <- names(anova_pvals)[sapply(anova_pvals, function(p) p >= 0.05)]
cor_result <- cor.test(encoded_numeric_df$Tutoring_Sessions,
                       encoded_numeric_df$Persistence_Index)

print(anova_pvals)
print(sig_anova)
print(nonsig_anova)
print(cor_result)
