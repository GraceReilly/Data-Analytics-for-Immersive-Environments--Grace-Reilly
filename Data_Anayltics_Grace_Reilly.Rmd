---
output:
  pdf_document: default
  html_notebook: default
editor_options:
  markdown:
    wrap: 72
---

## pdf_document: default editor_options: markdown: wrap: 72 preview: yes

## Data Analytics for Immersive Environments - Grace Reilly

#### Student No. D00262395

### Table of Contents

##### **1. Abstract**

##### **2. Introduction**

##### **3. Method**

##### **4. Results**

##### **5. Discussion**

##### **6. References**

#### **1. Abstract**

The aim of this study was to determine which, if any acts as an
effective treatment for PTSD. The study divided 150 patients (M= 75,
F=75) into 3 groups using random sampling dependent on the type of
treatment they would receive; Control (traditional CBT, no VR), Static
(non-animated model content, VR), Animated (animated model content, VR).
Treatment consisted of a 50 minute session weekly with a therapist over
12 weeks. PTSD measurements for all groups were measured using the
observer-rated and self-report mechanisms. Findings concluded that
patients OR and CPSS-SR measurement were significantly decreased post
treatment. Graph 1a shows that the OR and CPSS-SR measurements are very
closely related. Furthermore the report suggests there is no leading
therapy for PTSD.

#### **2.Introduction**

Post-traumatic stress disorder (PTSD) is observed in humans that have
been exposed to a horrifying event, through direct experience or witness
of such event. Symptoms might include vivid flashbacks, nightmares,
intense distress and severe anxiety (Mayo Clinic, n.d.). Evidently this
mental health issue can be very distressing for those affected.
Therapies utilized were; Control (traditional Cognitive Behavioral
Therapy (CBT), no Virtual Reality (VR)), Static (non-animated model
content, VR), Animated (animated model content, VR). Traditional CBT
involves the therapist and patient working through current day issues.
They will navigate through the patients thoughts, emotions and behaviors
together (HSE, 2022). The rationale behind this study is to understand
what the most effective treatment is for PTSD in young adults. The null
hypothesis states that the treatments will have no effect. The
alternative hypothesis states that all treatments would be effective.

#### **3.Method**

150 patients (aged 18-25) enrolled in the study was conducted over 12
weeks (1st August 2022 - 23rd of October 2022). The patients (M= 75,
F=75) into 3 equal groups using random sampling dependent on the type of
treatment they would receive; Control (traditional CBT, no VR), Static
(non-animated model content, VR), Animated (animated model content, VR).
The three groups underwent therapy with the same therapists to ensure
that all patients results are comparable. Similarly each patient was
treated for 50 minutes, once a week across the 12 weeks. The study aims
to determine the most effective treatment for PTSD through statistical
analysis such as histograms, standard deviation, the T test and box
plots. PTSD measurements for all groups were measured using the
observer-rated (therapist score) and self-report (self-score). These
values assess the extremity of PTSD symptoms in said patient (Foa,
1993). Measurements were taken at the start and end of the study. It was
this data was used for determining effectiveness of PTSD treatments.

#### **4.Results**

1a.

```{r}
# Reading file from destop and storing in data frame called df
library(ggplot2)

data <- read_excel("~/Downloads/data (1).xlsx", sheet = 1)

# Then creates a new data frame called data that contains the 3 test groups and four score values. For each group and store type calculate the sum of the scores from df
data <- data.frame(
    group = c("Static", "Control", "Animated"),
    score_type = c("pre_trial_cpss", "pre_trial_cpss", "pre_trial_cpss",
                   "pre_trial_or", "pre_trial_or", "pre_trial_or",
                   "post_trial_cpss", "post_trial_cpss", "post_trial_cpss",
                   "post_trial_or", "post_trial_or", "post_trial_or"),
    value = c(sum(df$pre_trial_cpss[df$test_group == "Static"]),
              sum(df$pre_trial_cpss[df$test_group == "Control"]),
              sum(df$pre_trial_cpss[df$test_group == "Animated"]),
              sum(df$pre_trial_or[df$test_group == "Static"]),
              sum(df$pre_trial_or[df$test_group == "Control"]),
              sum(df$pre_trial_or[df$test_group == "Animated"]),
              sum(df$post_trial_cpss[df$test_group == "Static"]),
              sum(df$post_trial_cpss[df$test_group == "Control"]),
              sum(df$post_trial_cpss[df$test_group == "Animated"]),
              sum(df$post_trial_or[df$test_group == "Static"]),
              sum(df$post_trial_or[df$test_group == "Control"]),
              sum(df$post_trial_or[df$test_group == "Animated"]))
)

# Convert the group and score_type columns of the data frame to factors. Finally adding in a bart chart using the ggplot2 libary
data$group <- factor(data$group, levels = c("Static", "Control", "Animated"))
data$score_type <- factor(data$score_type, levels = c("pre_trial_cpss", "post_trial_cpss", "pre_trial_or", "post_trial_or"))
ggplot(data, aes(x = group, y = value, color = score_type)) +
    geom_col(aes(fill = score_type), position = "dodge", width = 0.8, color = "#000000", linewidth = 1) +
    labs(x = "Bar Chart Test Group", y = "Sum of Scores") +
    scale_color_manual(values = c("#0000FF", "#FF00FF","#FFFF00","#30FF00")) +
    theme(text = element_text(color = "#000000"))

```

1.b

```{r}
library(readxl)

# read in data from excel file on pc
data <- read_excel("~/Downloads/data (1).xlsx", sheet = 1)

# select the post_trial_cpss column for the Animated group
data_animated <- data[data$test_group == "Animated", "post_trial_cpss"]

# perform t-test on the data_animated and store the result in the object t-test
t_test <- t.test(data_animated)

# create a vector of t-values ranging from -4 to 4 in increments of 0.1
t_values <- seq(from = -4, to = 4, by = 0.1)

# calculate density values for the t-values
density <- dt(t_values, df = t_test$parameter)

# plot t-distribution for the Animated group
plot(t_values, density, type = "l", xlab = "T-test", ylab = "Density", main = "T-test for Animated Group Post Trial - CPSS", lty = 2, lwd = 2, col = "green")
grid()


```

1.c

```{r}
library(readxl)

# read in data from excel file
data <- read_excel("~/Downloads/data (1).xlsx", sheet = 1)

# select the post_trial_cpss column for the Control group
data_control <- data[data$test_group == "Control", "post_trial_cpss"]

# perform t-test on the data_animated and store the result in the object t-test
t_test <- t.test(data_control)

# create a vector of t-values ranging from -4 to 4 in increments of 0.1
t_values <- seq(from = -4, to = 4, by = 0.1)

# calculate density values for the t values
density <- dt(t_values, df = t_test$parameter)

# plot t-distribution for the Control group
plot(t_values, density, type = "l", xlab = "T-test", ylab = "Density", main = "T-Test for Control Group – Post Trial - CPSS", lty = 2, lwd = 2, col = "blue")
grid()


```

1.d

```{r}

library(readxl)

# read in data from excel file
data <- read_excel("~/Downloads/data (1).xlsx", sheet = 1)

# select the post_trial_cpss column for the Static group
data_static2 <- data[data$test_group == "Static", "post_trial_cpss"]

# perform t-test on the data_animated and store the result in the object t-test
t_test <- t.test(data_static2)

# create a vector of t-values ranging from -4 to 4 in increments of 0.1
t_values <- seq(from = -4, to = 4, by = 0.1)

# calculate density values
density <- dt(t_values, df = t_test$parameter)

# plot t-distribution for the Static group
plot(t_values, density, type = "l", xlab = "T-test", ylab = "Density", main = "T-Test for Static Post Trial- CPSS", lty = 2, lwd = 2, col = "blue")
grid()


```

1.e

```{r}
# Load the necessary libraries
library(tidyverse)
library(openxlsx)

# Read in the data from the Excel file
data <- read.xlsx("~/Downloads/data (1).xlsx", sheet = 1)

# Calculate the confidence intervals for the post_trial_cpss for each category in the test_group column, grouped by the test_grouo column.
# Calculating the confidence interval using the mean and standard deviation of the post trial_cpss column and the 1.96 value to calculate the 95% interval.
conf_ints <- data %>%
    group_by(test_group) %>%
    summarize(lower = mean(post_trial_cpss) - 1.96 * sd(post_trial_cpss)/sqrt(n()),
              upper = mean(post_trial_cpss) + 1.96 * sd(post_trial_cpss)/sqrt(n()),
              post_trial_cpss = mean(post_trial_cpss))

# Plot the results using ggplot2
ggplot(data, aes(x = test_group, y = post_trial_cpss)) +
    geom_boxplot() +
    stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.3) +
    geom_errorbar(data = conf_ints, aes(x = test_group, ymin = lower, ymax = upper), width = 0.3)



```

1.f

```{r}
# Load the necessary libraries
library(tidyverse)
library(openxlsx)

# Read in the data from the Excel file on pc
data <- read.xlsx("~/Downloads/data (1).xlsx", sheet = 1)

# Filter the data to only include the "Control" and "Animated" categories
filtered_data <- data %>% filter(test_group %in% c("Control", "Animated"))

# Calculate the difference between the means of the two groups
difference <- mean(filtered_data$post_trial_cpss[filtered_data$test_group == "Animated"]) - mean(filtered_data$post_trial_cpss[filtered_data$test_group == "Control"])

# Plot the results using ggplot2. The ggplot function initializes a plot using the filtered_data. The geom_text function adds a label to the plot showing the calculated difference between the means of the two groups.
ggplot(filtered_data, aes(x = test_group, y = post_trial_cpss)) +
    geom_boxplot() +
    stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.3) +
    geom_text(x = 1, y = max(filtered_data$post_trial_cpss), label = paste("Difference:", difference), size = 4)


```

#### **5.Discussion**

The sum of pre-trial and post-trail OR and CPSS-SR were compared in
figure 1a, which indicates that there is no leading therapy. Figures 1b,
1c and 1d shows that our data collected from each treatment is normally
distributed. This suggests that if a patient is treated with the
described treatments, their OR and cPSS-SR measurements will decrease
post treatment. Figure 1e displays the 95% confidence interval for the
population of Static, Control and Animated. Figure 1f compares the
difference between the mean of the Antimated and Control group results.
There is a negative correlation between level of treatment received by
patient (explanatory variable) and the OR and CPSS-SR measurement
(response variable). In conclusion the results reject the null
hypothesis, and accept the alternative hypothesis. Although there
appears to be no leading treatment for PTSD patients, traditional CBT,
non-animated model content, and animated model content prove to be
successful therapies.

#### **6.References**

Mayo Staff Clinic, Post-traumatic stress disorder (PTSD). Available at:
<https://www.mayoclinic.org/diseases-conditions/post-traumatic-stress-disorder/symptoms-causes/syc-20355967>
(Accessed:n.d.) HSE, Treatment Post-traumatic stress disorder (PTSD).
Available at: <https://www2.hse.ie/conditions/ptsd/> (Accessed: 2022)
Foa, E.B., Riggs, D.S., Dancu, C. v., & Rothbaum, B.O., PTSD Symptom
Scale-Interview Version. Available at:
<https://psycnet.apa.org/doiLanding?doi=10.1037%2Ft05176-000> (Accessed:
1993)
