## Задание 1 
```{r}
set.seed(42)
sample_size <- 30
bp_before <- rnorm(sample_size, mean = 140, sd = 10)
bp_after <- rnorm(sample_size, mean = 130, sd = 10)
```
```{r}
# Визуализация данных
hist(bp_before, col = rgb(1,0,0,0.5), main = "Артериальное давление до и после приема препарата",
     xlab = "Артериальное давление", ylab = "Частота", xlim = c(100, 180), breaks = 10)
hist(bp_after, col = rgb(0,0,1,0.5), add = TRUE, breaks = 10)
legend("topright", legend = c("Давление до", "Давление после"),
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

shapiro.test(bp_before)
shapiro.test(bp_after)
```
```{r}
alpha <- 0.05
```
-  Используем t-тест для зависимых выборок, так как данные распределены нормально 
```{r}
# Выполнение t-теста для зависимых выборок
t_test_result <- t.test(bp_before, bp_after, paired = TRUE)

t_test_result
```

- Оцените и прокомментируйте статистическую значимость.
```{r}
if(t_test_result$p.value < alpha) {
  cat("Есть статистически значимые доказательства того, что новый препарат эффективнее стандартной терапии в снижении артериального давления.\n")
} else {
  cat("Нет статистически значимых доказательств того, что новый препарат эффективнее стандартной терапии в снижении артериального давления.\n")
}
```

Есть статистически значимые доказательства
## Задание 2 (2 балла)
```{r}
library(ggplot2)
library(PropCIs)
prob_smoking <- 0.8
prob_non_smoking <- 0.2
sample_size1 <- 100
sample_size2 <- 30
# Отношение курящих к некурящим 
smokers1 <- rep(1, sample_size1 / 2)
non_smokers1 <- rep(0, sample_size1 / 2)
smokers2 <- rep(1, sample_size2 / 2)
non_smokers2 <- rep(0, sample_size2 / 2)
```
```{r}
alpha <- 0.05
```
- Используем z-тест для сравнения двух пропорций
```{r}
# Выполнение z-теста для обеих выборок
result1 <- prop.test(x = c(sum(smokers1), sum(non_smokers1)), n = c(length(smokers1), length(non_smokers1)), correct = FALSE)
result2 <- prop.test(x = c(sum(smokers2), sum(non_smokers2)), n = c(length(smokers2), length(non_smokers2)), correct = FALSE)

result1
result2
```
наблюдаемые значения статистики значительно превышают критические

## Задание 3 (3 балла)

```{r}
library("exactRankTests")
data_treatment <- rnorm(30, mean = 5, sd = 2)
data_control <- rnorm(30, mean = 4, sd = 2)
```
```{r}
alpha <- 0.05
```
-  тест Манна-Уитни из-за непараметрической природы данных
```{r}
result <- wilcox.test(data_treatment, data_control, alternative = "two.sided", exact = FALSE)
result
```
P-значение меньше 0.05, что указывает на статистическую значимость различий между группами. 

## Задание 4 (4 балла)

```R
tumor <- tibble(
  therapy = c(rep("0", 10), rep("A", 10), rep("B", 10)),
  value = c(rep(3213, 10), rep(2687, 10), rep(2423, 10))
) %>%
  mutate(therapy = factor(therapy, levels = c("0", "A", "B")))
tumor$value <- tumor$value + rnorm(30, 0, 760)
```

```{r}
library(tidyverse)
library(broom)

tumor <- tibble(
  therapy = c(rep("0", 10), rep("A", 10), rep("B", 10)),
  value = c(rep(3213, 10), rep(2687, 10), rep(2423, 10))
) %>%
  mutate(therapy = factor(therapy, levels = c("0", "A", "B")))

tumor$value <- tumor$value + rnorm(30, 0, 760)

ggplot(tumor, aes(x = therapy, y = value, fill = therapy)) +
  geom_boxplot() +
  labs(title = "Размер опухоли в зависимости от терапии", x = "Терапия", y = "Размер опухоли") +
  theme_minimal()
```
```{r}
anova_result <- aov(value ~ therapy, data = tumor)
summary(anova_result)
```
-Различия между группами не являются статистически значимыми на уровне значимости 0.05.

```{r}
tukey_result <- TukeyHSD(anova_result)
tukey_result
```
нет статистически значимых различий между группами терапии (A и B) и группой плацебо. Однако, существует некоторая тенденция к различию между препаратами A и B, что потенциально может указывать на разную эффективность препаратов.