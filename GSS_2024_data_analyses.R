# GSS 2024 Supplementary R code for STAT E-100

# Function to install and load packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}

# List the libraries you want
my_packages <- c("psych", "dplyr", "ggplot2", "tidyr","forcats")

# Run the function
install_and_load(my_packages)

# Import the dataframe as 'mydata'

####################

# Sample size of the dataset
nrow(mydata)

# Variables in the dataset
colnames(mydata)

# •	iap → Inapplicable
#   Used when the question doesn’t apply (e.g., work hours for someone retired).
# •	no answer / .n → No Answer
#   Respondent refused to answer the question.
# •	d / don't know → Don’t Know
#   Respondent explicitly said they didn’t know.
#	•	skipped on web / .s → Skipped
#	  In self-administered (web) surveys, respondent left it blank.
#	• refused / .r → Refused
#	  Respondent actively refused to answer.
#	•	.u → Uncodeable
#	  Open-ended response could not be coded into a numeric category.
#	•	.x → Not available in this release
#	  Variable exists but wasn’t released yet in this dataset version.
#	•	.y → Not available in this year
#	  Variable exists in cumulative file but not asked in that survey year.

# Frequency table of age responses (including special codes)
freq_age <- mydata %>%
  count(age, sort = TRUE)

print(freq_age)

####################

# Variables of interest
vars <- c("age","childs","sibs","adults","adultsinhh","earnrs","hrs1","hrs2")

# Keep only variables that actually exist in the dataset
vars_existing <- intersect(vars, names(mydata))

# Convert to numeric where needed
clean_data <- mydata %>%
  mutate(across(all_of(vars_existing), ~ suppressWarnings(as.numeric(as.character(.)))))

# Total number of rows
n_total <- nrow(clean_data)

# Missing count + percentage (fixed formatting)
missing_summary <- data.frame(
  Variable   = vars_existing,
  Missing_N  = colSums(is.na(clean_data[vars_existing])),
  Missing_Pct = round(100 * colSums(is.na(clean_data[vars_existing])) / n_total, 2)
)

print(missing_summary)

####################

# choose the categorical income column you want to show
inc_col <- if ("income" %in% names(mydata)) "income" else "rincome"

mydata %>%
  filter(!is.na(.data[[inc_col]]), .data[[inc_col]] != "iap") %>%  # drop missing / inapplicable codes
  ggplot(aes(x = fct_infreq(.data[[inc_col]]))) +
  geom_bar() +
  coord_flip() +  # optional: easier to read
  labs(title = "Income (categories)", x = "Income category", y = "Count") +
  theme_minimal()

# Select only numeric variables
numeric_data <- mydata %>% select(where(is.numeric))

# Descriptive statistics for each numeric variable
desc_stats <- numeric_data %>%
  summarise(across(
    everything(),
    list(
      count = ~ sum(!is.na(.)),
      mean  = ~ mean(., na.rm = TRUE),
      sd    = ~ sd(., na.rm = TRUE),
      min   = ~ min(., na.rm = TRUE),
      q25   = ~ quantile(., 0.25, na.rm = TRUE),
      median= ~ median(., na.rm = TRUE),
      q75   = ~ quantile(., 0.75, na.rm = TRUE),
      max   = ~ max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

# View results
print(desc_stats)

# Optional: create a tidier table (long format)
library(tidyr)
tidy_stats <- numeric_data %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarise(
    count  = sum(!is.na(value)),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    min    = min(value, na.rm = TRUE),
    q25    = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q75    = quantile(value, 0.75, na.rm = TRUE),
    max    = max(value, na.rm = TRUE)
  )

print(tidy_stats)

####################

library(ggplot2)
library(dplyr)
library(forcats)

inc_col <- if ("income" %in% names(mydata)) "income" else "rincome"

mydata %>%
  filter(
    !is.na(.data[[inc_col]]),
    .data[[inc_col]] != "iap",
    !is.na(sex),
    sex != "iap",        # IAP stands for inapplicable
    sex != "no answer"   
  ) %>%
  ggplot(aes(x = fct_infreq(.data[[inc_col]]), fill = sex)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  labs(title = "Income Distribution by Gender",
       x = "Income Category", y = "Count", fill = "Gender") +
  theme_minimal()

####################

library(dplyr)
library(ggplot2)
library(readr)
library(forcats)

# Clean age to a numeric variable
df_age <- mydata %>%
  mutate(
    age = as.character(age),
    age = na_if(age, "iap"),
    age = na_if(age, "no answer"),
    age = na_if(age, "skipped on web"),
    age = na_if(age, "refused"),
    age = na_if(age, "don't know"),
    age_num = parse_number(age)         # safely to numeric
  ) %>%
  filter(!is.na(age_num), age_num > 0, age_num < 120)

# Histogram
ggplot(df_age, aes(x = age_num)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "black") +
  labs(title = "Distribution of Age", x = "Age (years)", y = "Count") +
  theme_minimal()

# (Optional) Density curve
ggplot(df_age, aes(x = age_num)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  labs(title = "Density of Age", x = "Age (years)", y = "Density") +
  theme_minimal()

mydata %>%
  filter(!age %in% c("iap","no answer","skipped on web","refused","don't know")) %>%
  count(age, name = "n", sort = TRUE) %>%
  ggplot(aes(x = fct_reorder(age, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Age (categorical) – Counts", x = "Age", y = "Count") +
  theme_minimal()

####################

mydata %>%
  filter(!is.na(age),
         !is.na(childs),
         !childs %in% c("no answer", "skipped on web", "iap", "refused", "don't know")) %>%
  mutate(childs = as.numeric(childs)) %>%
  ggplot(aes(x = age, y = childs)) +
  geom_jitter(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Age vs Number of Children",
       x = "Age (years)",
       y = "Number of Children") +
  theme_minimal()

####################

# Variables of interest
vars <- c("age", "childs", "sibs", "adults", "adultsinhh",
          "earnrs", "hrs1", "hrs2")

# Clean: convert to numeric while dropping non-numeric entries (iap, no answer, etc.)
clean_data <- mydata %>%
  mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(as.character(.)))))

# Descriptive statistics (central tendency and more)
describe(clean_data[vars])

####################

# Variables of interest
vars <- c("age", "childs", "sibs", "adults", "adultsinhh",
          "earnrs", "hrs1", "hrs2")

# Clean data: convert to numeric, suppress coercion warnings
clean_data <- mydata %>%
  mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  select(all_of(vars))

# Base R correlation matrix (pairwise complete observations)
cor_matrix <- cor(clean_data, use = "pairwise.complete.obs")
print(cor_matrix)

# Psych correlation test (with significance levels and CI)
corr_results <- corr.test(clean_data, use = "pairwise")
print(corr_results$r)   # correlations
print(corr_results$p)   # p-values

####################

# install.packages(c("dplyr","corrplot","psych"))  # if needed
library(dplyr)
library(corrplot)
library(psych)

# variables to use
vars <- c("age","childs","sibs","adults","adultsinhh","earnrs","hrs1","hrs2")

# 1) Make numeric (turns "iap", "no answer", etc. into NA)
numdat <- mydata %>%
  mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  select(all_of(vars))

# 2) Drop columns that are all-NA or have ~0 variance (these cause NA/Inf correlations)
ok_cols <- sapply(numdat, function(x) sum(is.finite(x)) >= 3 && sd(x, na.rm = TRUE) > 0)
numdat  <- numdat[, ok_cols, drop = FALSE]

# 3) Correlation matrix (pairwise NAs allowed), then replace any remaining NA/Inf with 0
cor_mat <- cor(numdat, use = "pairwise.complete.obs")
cor_mat[!is.finite(cor_mat)] <- 0

# (optional) p-values for significance stars
ct <- corr.test(numdat, use = "pairwise")
p_mat <- ct$p
p_mat[!is.finite(p_mat)] <- 1

# 4) Nice visualization
corrplot(cor_mat,
         method = "color",
         type   = "full",       # instead of "upper"
         addCoef.col = "black", # add correlation values
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("red","white","blue"))(200))
