# Linear models

library(readr)
library(dplyr)

person <- read_csv(
  file = 'data/census_pums/sample.csv',
  col_types = cols_only(
    AGEP = 'i',  # Age
    WAGP = 'd',  # Wages or salary income past 12 months
    SCHL = 'i',  # Educational attainment
    SEX = 'f',   # Sex
    OCCP = 'f',  # Occupation recode based on 2010 OCC codes
    WKHP = 'i')) # Usual hours worked per week past 12 months

person <- within(person, {
  SCHL <- factor(SCHL)
  levels(SCHL) <- list(
    'Incomplete' = c(1:15),
    'High School' = 16,
    'College Credit' = 17:20,
    'Bachelor\'s' = 21,
    'Master\'s' = 22:23,
    'Doctorate' = 24)}) %>%
  filter(
    WAGP > 0,
    WAGP < max(WAGP, na.rm = TRUE))

# Formula Notation

fit <- lm(
  formula = WAGP ~ SCHL,
  data = person)

fit <- lm(
  log(WAGP) ~ SCHL,
  person)

# Metadata matters

fit <- lm(
  log(WAGP) ~ AGEP,
  person)

# GLM families

fit <- glm(log(WAGP) ~ SCHL,
  family = gaussian,
  data = person)

# Logistic Regression

fit <- glm(SEX ~ WAGP,
  family = binomial,
  data = person)

anova(fit, update(fit, SEX ~ 1), test = 'Chisq')

# Random Intercept

library(lme4)
fit <- lmer(
  log(WAGP) ~ (1 | OCCP) + SCHL,
  data = person)

# Random Slope

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person)

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person,
  control = lmerControl(optimizer = 'bobyqa'))

ggplot(person,
  aes(x = WKHP, y = log(WAGP), color = SCHL)) +
  geom_point() +
  geom_line(aes(y = predict(fit))) +
  labs(title = 'Random intercept and slope with lmer')


######################################
##Anna McPherran
##July 24, 2019
##Lesson 6 exercises

##Exercise 1
##Regress WKHP against AGEP, adding a second-order interaction to allow for possible curvature 
##in the relationship. Plot the predicted values over the data to verify the coefficients 
##indicate a downward quadratic relationship.
fit1 <- lm(
  formula = WKHP ~ AGEP + I(AGEP^2),
  data = person
)
summary(fit1)

ggplot(person,
  aes(x = AGEP, y = WKHP)) +
  geom_point(shape = 'x') +
  geom_line(aes(y = predict(fit1)))

##Exercise 2
##Controlling for a person’s educational attainment, fit a linear model that addresses the 
##question of wage disparity between men and women in the U.S. workforce. What other predictor 
##from the person data frame would increases the goodness-of-fit of the “control” model, 
##before SEX is considered.
fit2 <- lm(
  formula = WAGP ~ SCHL + WKHP + SEX,
  data = person
)
summary(fit2)

##Exercise 3
##Set up a generalized mixed effects model on whether a person attained an advanced degree 
##(Master’s or Doctorate). Include sex and age as fixed effects, and include a random intercept 
##according to occupation.
levels(person$SCHL) <- c(0, 0, 0, 0, 1, 1)
fit3 <- glmer(
  SCHL ~ SEX + AGEP + (1 | OCCP),
  family = binomial,
  data = person)
summary(fit3)

##Exercise 4
##Write down the formula for a random intercepts model for earned wages with a fixed effect of 
##sex and a random effect of educational attainment.
WAGP ~ SEX + (1 | SCHL)
