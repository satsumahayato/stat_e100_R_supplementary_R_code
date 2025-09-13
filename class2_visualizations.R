# install missing pkgs only
packages <- c(
  "tidyverse","readxl","ggbeeswarm","GGally","ggridges",
  "corrplot","naniar","janitor","scales","pheatmap"
)
installed <- rownames(installed.packages())
for (p in packages) if (!(p %in% installed)) install.packages(p, quiet = TRUE)

# Load
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggbeeswarm)
  library(GGally)
  library(ggridges)
  library(corrplot)
  library(naniar)
  library(janitor)
  library(scales)
  library(pheatmap)
})

# Use existing data frame called 'survey_xlsx_cleaned'
df <- survey_xlsx_cleaned |>
  janitor::clean_names() |>                      # <<— pass the data here
  mutate(
    gender          = stringr::str_to_title(gender),
    treatment       = factor(treatment, levels = c("No","Yes")),
    family_history  = factor(family_history, levels = c("No","Yes")),
    remote_work     = factor(remote_work, levels = c("No","Yes")),
    work_interfere  = factor(work_interfere, levels = c("Never","Rarely","Sometimes","Often")),
    age             = as.numeric(age),
    age             = dplyr::if_else(age < 15 | age > 80, NA_real_, age)
  )

# 1) Count bar
ggplot(df, aes(x = treatment, fill = remote_work)) +
  geom_bar(position = "dodge") +
  labs(title = "Counts by Treatment and Remote Work Status", x = "Treatment", y = "Count") +
  theme_minimal()

# 2) Percent bar
ggplot(df, aes(x = treatment, fill = remote_work)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +   # be explicit with namespace
  labs(title = "Share by Treatment and Remote Work Status", x = "Treatment", y = "Percent") +
  theme_minimal()

# 3) Violin + box
ggplot(df, aes(x = treatment, y = age, fill = treatment)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9) +
  labs(title = "Age Distribution by Treatment", x = "Treatment", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none")

# 4) Beeswarm
ggplot(df, aes(x = work_interfere, y = age, color = work_interfere)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.6) +
  labs(title = "Age vs Work Interference (beeswarm)", x = "Work Interfere", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none")

# 5) Histogram & density
ggplot(df, aes(x = age)) +
  geom_histogram(bins = 30) +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = age, color = treatment, fill = treatment)) +
  geom_density(alpha = 0.2) +
  labs(title = "Age Density by Treatment", x = "Age", y = "Density") +
  theme_minimal()

# 6) Logistic fit (treatment ~ age)
ggplot(df, aes(x = age, y = as.numeric(treatment) - 1)) +  # 0/1 outcome from factor levels
  geom_point(alpha = 0.2, position = position_jitter(height = 0.02)) +
  stat_smooth(method = "glm", method.args = list(family = binomial), se = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pr(Treatment) vs Age (Logistic Fit)", x = "Age", y = "Probability of Treatment") +
  theme_minimal()

# 7) Pair plot (only if you have ≥1 numeric columns)
df_num <- df |> select(age) |> drop_na()
GGally::ggpairs(df_num) + theme_minimal()

# 8) Faceted percent bar
ggplot(df |> filter(!is.na(state), !is.na(remote_work)),
       aes(x = state, fill = treatment)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ remote_work) +
  labs(title = "Treatment Rate by State & Remote Work",
       x = "State", y = "Percent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 9) Ridge plot (explicit forcats::fct_reorder)
df |>
  filter(!is.na(country), !is.na(age)) |>
  group_by(country) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n >= 30) |>
  ggplot(aes(x = age, y = forcats::fct_reorder(country, age, .fun = median), fill = country)) +
  ggridges::geom_density_ridges(alpha = 0.7, scale = 1.5, rel_min_height = 0.01) +
  labs(title = "Age Distributions by Country (≥30 responses)", x = "Age", y = "Country") +
  theme_minimal() +
  theme(legend.position = "none")

# 10) Mean age by treatment with 95% CI
ggplot(df, aes(x = treatment, y = age)) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +
  labs(title = "Mean Age by Treatment (95% CI)", x = "Treatment", y = "Age") +
  theme_minimal()

# 11) Missingness
gg_miss_var(df) + theme_minimal()
