# ============================================================
# Bayesian Hierarchical Analysis of Life Satisfaction
# World Values Survey Wave 7 (2017-2022)
# ============================================================

# ============================================================
# SECTION 1: PACKAGES
# ============================================================

library(tidyverse)
library(haven)
library(rstanarm)
library(bayesplot)
library(loo)
library(patchwork)
library(countrycode)
library(knitr)


# ============================================================
# SECTION 2: DATA LOADING AND CLEANING
# ============================================================

# Load WVS Wave 7 data
# Download from: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp
wvs <- readRDS("WVS_Cross-National_Inverted_Wave_7_R_v6_0.rds")

# Select variables of interest
wvs_small <- wvs %>%
  select(
    country    = B_COUNTRY_ALPHA,
    life_sat   = Q49,
    income     = Q288,
    education  = Q275,
    trust      = Q57P,
    health     = Q47P,
    gender     = Q260,
    age        = Q262,
    employment = Q279
  )

# Convert haven_labelled to numeric
wvs_small <- wvs_small %>%
  mutate(across(where(is.haven_labelled), zap_labels))

# Recode negative values (WVS missing codes) to NA
wvs_clean <- wvs_small %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, NA, .)))

# Remove incomplete cases
wvs_clean <- wvs_clean %>%
  filter(complete.cases(.))

# Confirm: 90,940 rows, 66 countries
nrow(wvs_clean)
n_distinct(wvs_clean$country)

# Stratified subsample: 150 respondents per country (~9,900 total)
set.seed(42)
wvs_sub <- wvs_clean %>%
  group_by(country) %>%
  slice_sample(n = 150) %>%
  ungroup()

nrow(wvs_sub)

# Standardize predictors for modeling
wvs_model <- wvs_sub %>%
  mutate(
    income_s    = scale(income)[,1],
    education_s = scale(education)[,1],
    health_s    = scale(health)[,1],
    age_s       = scale(age)[,1],
    trust_f     = factor(trust, labels = c("no_trust", "trust")),
    gender_f    = factor(gender, labels = c("male", "female"))
  )

# Use wvs_sub consistently for all plots
wvs_sub <- wvs_model


# ============================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS (EDA) PLOTS
# ============================================================

# -- Plot 1: Distribution of Life Satisfaction --
p1 <- ggplot(wvs_sub, aes(x = life_sat)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "white") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Distribution of Life Satisfaction",
       x = "Life Satisfaction (1-10)", y = "Count") +
  theme_minimal(base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p1
ggsave("plot1_life_sat_distribution.pdf", p1, width = 7, height = 5)


# -- Plot 2: Income Decile vs. Mean Life Satisfaction --
p2 <- wvs_sub %>%
  group_by(income) %>%
  summarise(mean_sat = mean(life_sat), se = sd(life_sat)/sqrt(n())) %>%
  ggplot(aes(x = income, y = mean_sat)) +
  geom_point(color = "#2E86AB") +
  geom_errorbar(aes(ymin = mean_sat - 1.96*se, ymax = mean_sat + 1.96*se),
                width = 0.2, color = "#2E86AB") +
  geom_smooth(method = "lm", se = FALSE, color = "#E84855") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Income Decile vs. Mean Life Satisfaction",
       x = "Income Decile", y = "Mean Life Satisfaction") +
  theme_minimal(base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p2
ggsave("plot2_income_satisfaction.pdf", p2, width = 7, height = 5)


# -- Plot 3: Income vs. Life Satisfaction by Country (Spaghetti) --
p3 <- wvs_sub %>%
  group_by(country, income) %>%
  summarise(mean_sat = mean(life_sat), .groups = "drop") %>%
  ggplot(aes(x = income, y = mean_sat, group = country)) +
  geom_smooth(method = "lm", se = FALSE, color = "#2E86AB",
              alpha = 0.3, linewidth = 0.5) +
  labs(title = "Income vs. Life Satisfaction by Country",
       x = "Income Decile", y = "Mean Life Satisfaction") +
  theme_minimal(base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p3
ggsave("plot3_spaghetti.pdf", p3, width = 7, height = 5)


# -- Plot 4: Trust and Health vs. Life Satisfaction --
p4a <- wvs_sub %>%
  group_by(trust) %>%
  summarise(mean_sat = mean(life_sat), se = sd(life_sat)/sqrt(n())) %>%
  ggplot(aes(x = factor(trust), y = mean_sat)) +
  geom_col(fill = "#2E86AB") +
  geom_errorbar(aes(ymin = mean_sat - 1.96*se, ymax = mean_sat + 1.96*se),
                width = 0.2) +
  scale_x_discrete(labels = c("1" = "No Trust", "2" = "Trust")) +
  labs(title = "Social Trust vs. Life Satisfaction",
       x = "", y = "Mean Life Satisfaction") +
  theme_minimal(base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p4b <- wvs_sub %>%
  group_by(health) %>%
  summarise(mean_sat = mean(life_sat), se = sd(life_sat)/sqrt(n())) %>%
  ggplot(aes(x = health, y = mean_sat)) +
  geom_point(color = "#2E86AB") +
  geom_errorbar(aes(ymin = mean_sat - 1.96*se, ymax = mean_sat + 1.96*se),
                width = 0.2, color = "#2E86AB") +
  geom_smooth(method = "lm", se = FALSE, color = "#E84855") +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Health vs. Life Satisfaction",
       x = "Self-rated Health (1-5)", y = "") +
  theme_minimal(base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p4 <- p4a + p4b
p4
ggsave("plot4_trust_health.pdf", p4, width = 10, height = 5)


# -- Plot 5: Mean Life Satisfaction by Country --
country_means <- wvs_sub %>%
  group_by(country) %>%
  summarise(mean_sat = mean(life_sat), n = n(), se = sd(life_sat)/sqrt(n())) %>%
  mutate(country_name = countrycode(country, "iso3c", "country.name"),
         country_name = ifelse(country == "NIR", "Northern Ireland", country_name))

p5 <- ggplot(country_means, aes(x = reorder(country_name, mean_sat), y = mean_sat)) +
  geom_point(color = "#2E86AB", size = 2) +
  geom_errorbar(aes(ymin = mean_sat - 1.96*se, ymax = mean_sat + 1.96*se),
                width = 0.3, color = "#2E86AB", linewidth = 0.7) +
  coord_flip() +
  labs(title = "Mean Life Satisfaction by Country",
       x = "", y = "Mean Life Satisfaction") +
  theme_minimal(base_size = 7) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p5
ggsave("plot5_country_means.pdf", p5, width = 7, height = 10)


# ============================================================
# SECTION 4: BAYESIAN MODELS
# ============================================================

# -- Model 1: Complete Pooling --
m1 <- stan_glm(
  life_sat ~ income_s + education_s + health_s + age_s + trust_f + gender_f,
  data            = wvs_sub,
  family          = gaussian(),
  prior           = normal(0, 2.5),
  prior_intercept = normal(7, 2),
  chains = 4, iter = 2000, seed = 42
)

summary(m1)


# -- Model 2: Random Intercepts --
m2 <- stan_lmer(
  life_sat ~ income_s + education_s + health_s + age_s + trust_f + gender_f +
    (1 | country),
  data            = wvs_sub,
  prior           = normal(0, 2.5),
  prior_intercept = normal(7, 2),
  chains = 4, iter = 4000, warmup = 2000, seed = 42,
  adapt_delta = 0.95
)

summary(m2, probs = c(0.1, 0.5, 0.9))


# -- Model 3: Random Intercepts + Random Slopes for Income --
m3 <- stan_lmer(
  life_sat ~ income_s + education_s + health_s + age_s + trust_f + gender_f +
    (1 + income_s | country),
  data            = wvs_sub,
  prior           = normal(0, 2.5),
  prior_intercept = normal(7, 2),
  chains = 4, iter = 4000, warmup = 2000, seed = 42,
  adapt_delta = 0.95
)

summary(m3, probs = c(0.1, 0.5, 0.9))


# ============================================================
# SECTION 5: MODEL COMPARISON
# ============================================================

loo1 <- loo(m1)
loo2 <- loo(m2)
loo3 <- loo(m3)

loo_compare(loo1, loo2, loo3)


# ============================================================
# SECTION 5B: VARIANCE COMPONENTS AND ICC
# ============================================================

# Extract variance components from Model 3
var_country <- as.data.frame(VarCorr(m3))

# tau^2 = between-country variance (intercept)
tau2 <- var_country$vcov[var_country$grp == "country" &
                            var_country$var1 == "(Intercept)" &
                            is.na(var_country$var2)]

# sigma^2 = within-country (residual) variance
sigma2 <- sigma(m3)^2

# ICC = proportion of total variance attributable to country
icc <- tau2 / (tau2 + sigma2)
icc


# ============================================================
# SECTION 5C: COVARIANCE BETWEEN INTERCEPTS AND SLOPES
# ============================================================

posterior_samples <- as.matrix(m3)

cov_col <- grep("Sigma\\[country:income_s,\\(Intercept\\)\\]", colnames(posterior_samples))
cov_samples <- posterior_samples[, cov_col]

mean(cov_samples)
quantile(cov_samples, c(0.025, 0.975))


# ============================================================
# SECTION 6: TABLES
# ============================================================

# -- Table 1: LOO Model Comparison --
loo_table <- data.frame(
  Model     = c("Model 3 (Random intercepts + slopes)",
                "Model 2 (Random intercepts)",
                "Model 1 (Complete pooling)"),
  ELPD_diff = c(0.0, -39.8, -621.6),
  SE        = c(0.0,  11.2,   38.8)
)

kable(loo_table,
      col.names = c("Model", "ELPD Difference", "SE"),
      caption   = "Table 1: LOO-CV Model Comparison",
      digits    = 1)


# -- Table 2: Fixed Effects Posterior Summary --
posterior_summary <- as.data.frame(summary(m3,
                     pars  = c("income_s", "education_s", "health_s",
                               "age_s", "trust_ftrust", "gender_ffemale"),
                     probs = c(0.025, 0.975)))

posterior_table <- data.frame(
  Predictor = c("Income", "Education", "Health",
                "Age", "Social Trust", "Gender (Female)"),
  Mean  = round(posterior_summary$mean,    3),
  Lower = round(posterior_summary$`2.5%`,  3),
  Upper = round(posterior_summary$`97.5%`, 3)
)

kable(posterior_table,
      col.names = c("Predictor", "Posterior Mean", "95% CrI Lower", "95% CrI Upper"),
      caption   = "Table 2: Posterior Estimates of Fixed Effects (Model 3)",
      digits    = 3)


# ============================================================
# SECTION 7: MODEL DIAGNOSTIC AND RESULTS PLOTS
# ============================================================

# -- Plot 6: Posterior Predictive Check --
p6 <- pp_check(m3, nreps = 50) +
  scale_color_manual(values = c("#E84855", "#2E86AB"),
                     labels = c("Observed", "Replicated")) +
  labs(title = "Posterior Predictive Check (Model 3)",
       x = "Life Satisfaction", y = "Density") +
  theme_minimal(base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_blank())

p6
ggsave("plot6_pp_check.pdf", p6, width = 7, height = 5)


# -- Plot 7: Country Random Intercepts (Caterpillar with Credibility Intervals) --

# Extract posterior samples for intercept random effects
posterior_samples <- as.matrix(m3)
intercept_cols <- grep("b\\[\\(Intercept\\)", colnames(posterior_samples))
intercept_df <- posterior_samples[, intercept_cols]

# Compute posterior mean and 95% credibility interval for each country
re_intervals <- data.frame(
  param = colnames(intercept_df),
  mean  = apply(intercept_df, 2, mean),
  lower = apply(intercept_df, 2, quantile, 0.025),
  upper = apply(intercept_df, 2, quantile, 0.975)
) %>%
  mutate(
    country = gsub("b\\[\\(Intercept\\) country:", "", param),
    country = gsub("\\]", "", country),
    country_name = countrycode(country, "iso3c", "country.name"),
    country_name = ifelse(country == "NIR", "Northern Ireland", country_name),
    region = countrycode(country, "iso3c", "region"),
    region = case_when(
      country == "NIR" ~ "Europe & Central Asia",
      country == "PRI" ~ "Latin America & Caribbean",
      TRUE ~ region
    )
  ) %>%
  arrange(mean)

p7 <- ggplot(re_intervals, aes(x = mean,
                                y = reorder(country_name, mean),
                                color = region)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, alpha = 0.6) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2", na.value = "gray70") +
  labs(title    = "Country-level Deviations from Global Mean Satisfaction",
       subtitle = "After controlling for income, health, education, age, trust, and gender",
       x = "Deviation from global mean (random intercept)",
       y = "", color = "Region") +
  theme_minimal(base_size = 9) +
  theme(legend.position  = "bottom",
        axis.text.y      = element_text(size = 7),
        text             = element_text(color = "black"),
        axis.text        = element_text(color = "black"))

p7
ggsave("plot7_random_intercepts.pdf", p7, width = 7, height = 10)


# Keep re_df for compatibility with plot9 covariance discussion
re_df <- re_intervals %>% rename(intercept = mean)


# -- Plot 8: Fixed Effects Coefficient Plot --
p8 <- plot(m3,
           pars       = c("income_s", "education_s", "health_s",
                          "age_s", "trust_ftrust", "gender_ffemale"),
           prob       = 0.8,
           prob_outer = 0.95) +
  scale_y_discrete(labels = c(
    "income_s"       = "Income",
    "education_s"    = "Education",
    "health_s"       = "Health",
    "age_s"          = "Age",
    "trust_ftrust"   = "Social Trust",
    "gender_ffemale" = "Gender (Female)")) +
  labs(title = "Posterior Estimates of Fixed Effects (Model 3)",
       x     = "Coefficient estimate") +
  theme_minimal(base_size = 11) +
  theme(text      = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p8
ggsave("plot8_fixed_effects.pdf", p8, width = 7, height = 5)


# -- Plot 9: Country-specific Income Slopes --
slopes_df <- as.data.frame(ranef(m3)$country) %>%
  rownames_to_column("country") %>%
  rename(slope_deviation = income_s) %>%
  mutate(
    country_name = countrycode(country, "iso3c", "country.name"),
    country_name = ifelse(country == "NIR", "Northern Ireland", country_name),
    total_slope  = 0.4 + slope_deviation
  ) %>%
  arrange(total_slope)

slope_labels <- c("Iraq", "Greece", "Brazil", "Ecuador", "Mexico",
                  "Argentina", "Australia", "United States", "Nigeria",
                  "China", "Germany", "South Korea", "Canada", "Ethiopia",
                  "Pakistan", "Peru", "Japan", "United Kingdom",
                  "Russia", "Turkey")

slopes_df <- slopes_df %>%
  mutate(label = ifelse(country_name %in% slope_labels, country_name, ""))

p9 <- ggplot(slopes_df,
             aes(x = total_slope, y = reorder(country_name, total_slope))) +
  geom_vline(xintercept = 0.4, linetype = "dashed", color = "gray50") +
  geom_point(size = 2, color = "coral") +
  scale_y_discrete(labels = slopes_df %>%
                     arrange(total_slope) %>% pull(country_name)) +
  labs(title    = "Country-specific Income Effects on Life Satisfaction",
       subtitle = "Dashed line = global average effect (0.4). Points show country-specific slopes.",
       x = "Income effect (per SD increase in income)",
       y = "") +
  theme_minimal(base_size = 9) +
  theme(text      = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p9
ggsave("plot9_income_slopes.pdf", p9, width = 7, height = 10)


# -- Plot 10: MCMC Trace Plots --
p10 <- mcmc_trace(as.matrix(m3),
                  pars = c("(Intercept)", "income_s", "health_s",
                           "education_s", "age_s", "sigma")) +
  labs(title = "MCMC Trace Plots (Model 3)") +
  theme_minimal(base_size = 9) +
  theme(text      = element_text(color = "black"),
        axis.text = element_text(color = "black"))

p10
ggsave("plot10_trace.pdf", p10, width = 10, height = 8)


# ============================================================
# SECTION 8: MCMC DIAGNOSTICS TABLE
# ============================================================

# -- Table 3: Convergence Diagnostics (Rhat and ESS) --
m3_summary <- summary(m3,
                      pars = c("(Intercept)", "income_s", "education_s",
                               "health_s", "age_s", "trust_ftrust",
                               "gender_ffemale", "sigma"),
                      probs = c(0.025, 0.975))

print(m3_summary)

diagnostics_table <- data.frame(
  Parameter = c("Intercept", "Income", "Education", "Health",
                 "Age", "Social Trust", "Gender (Female)", "Sigma"),
  Rhat  = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
  n_eff = c(634, 1397, 8448, 10641, 8670, 9678, 12426, 12302)
)

kable(diagnostics_table,
      col.names = c("Parameter", "Rhat", "Effective Sample Size"),
      caption   = "Table 3: MCMC Convergence Diagnostics for Model 3",
      digits    = 1)
