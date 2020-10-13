
## load necessary packages
library(brms)
library(bayesplot)
library(rstanarm)
library(rstan)
library(tidyverse)
library(car)

# load datasets
load('data.RData')

##### Model 1: Which grooming bouts face intervention?

# Set priors
m1priors <- c(
  prior(normal(0, 2.5), class = "Intercept"),
  prior(normal(0, 2.5), class = "b")
)

# test for multicollinearity using Variance Inflation Factors
vif(lm(data = test.data.model1,
       total.interventions ~ 1 +
         z.rank.higher +
         z.rank.lower +
         z.dsi.groomer1.groomer2 +
         kin.groomers +
         z.bystanders))

# Bayesian analysis using brms
model1 = brm(
  data = test.data.model1,
  total.interventions ~ 1 +
    z.rank.higher +
    z.rank.lower +
    z.dsi.groomer1.groomer2 +
    kin.groomers +
    z.bystanders +
    (1 + z.rank.lower + z.dsi.groomer1.groomer2||higher) +
    (1 + z.rank.higher + z.dsi.groomer1.groomer2||lower) +
    offset(log(duration)),
  prior = m1priors,
  warmup = 500,
  iter = 3000,
  cores = 5,
  chains = 2,
  family = bernoulli(link = 'logit'),
  seed = 123
)

summary(model1)
r2.intervention = bayes_R2(model1)
odds.model1 = exp(fixef(model1))

###### Model 2: Who intervened?

# Variance Inflation Factor
vif(lm(intervention ~ 1 +
         z.rank.pot.intervener +
         z.rank.diff.higher +
         z.rank.diff.lower +
         z.dsi.higher.intervener +
         z.dsi.lower.intervener +
         kin.higher.intervener +
         kin.lower.intervener, data = test.data.model2))


# conditional logistic regression using stan_clogit

model2 = stan_clogit(
  intervention ~ 1 +
    z.rank.pot.intervener +
    z.rank.diff.higher +
    z.rank.diff.lower +
    z.dsi.lower.intervener +
    z.dsi.higher.intervener +
    kin.higher.intervener +
    kin.lower.intervener +
    (1|pot.intervener),
  strata = gr.bout.id,
  data = test.data.model2,
  prior = normal(),
  QR = TRUE,
  algorithm = 'sampling',
  chains = 2,
  warmup = 500,
  iter = 3000,
  cores = 5,
  sparse = FALSE
)


summary(model2,
        probs = c(0.025, 0.5, 0.975),
        digits = 2)

r2.intervener = mean(bayes_R2(model2))
odds.model2 = exp(fixef(model2))


###### Model 3: which of the two groomers was chosen as target?

# Variance Inflation Factor
vif(lm(choice ~ z.rank.diff +
         z.rank.target +
         z.dsi.target.intervener +
         kin.target.intervener, data = test.data.model3))


# conditional logistic regression using stan_clogit
model3 = stan_clogit(
  choice ~
    z.rank.target +
    z.dsi.target.intervener +
    kin.target.intervener +
    (1 + z.dsi.target.intervener + kin.target.intervener || target),
  strata = bout,
  data = test.data.model3,
  prior = normal(),
  QR = T,
  algorithm = 'sampling',
  chains = 2,
  iter = 3000,
  cores = 5,
  sparse = F
)

summary(model3,
        probs = c(0.025, 0.5, 0.975),
        digits = 2)

r2.target = mean(bayes_R2(model3))
odds.model3 = exp(fixef(model3))


########################### Model 4.1: Which groomers stayed, which ones left

# set priors
m1priors <- c(
  prior(normal(0, 2.5), class = "Intercept"),
  prior(normal(0, 2.5), class = "b")
)
# Variance Inflation Factor
vif(lm(data = test.data.model41,
       stay ~ z.rank.target * z.rank.intervener +
         kin.target.intervener +
         z.dsi.target.intervener))


# Bayesian analysis using brms
model41 = brm(
  data = test.data.model41,
  stay ~ 1 +
    z.rank.target * z.rank.intervener +
    kin.target.intervener +
    z.dsi.target.intervener +
    (1 + z.rank.intervener + z.dsi.target.intervener|| target) +
    (1 + z.rank.target + z.dsi.target.intervener|| intervener),
  prior = m1priors,
  warmup = 500,
  iter = 3000,
  cores = 5,
  chains = 2,
  family = bernoulli(link = 'logit'),
  seed = 123
)

summary(model41)
r2.outcome41 = bayes_R2(model41)
odds.model41 = exp(fixef(model41))

########################### Model 4.2: Did the intervener gain access?

# set priors
m1priors <- c(
  prior(normal(0, 2.5), class = "Intercept"),
  prior(normal(0, 2.5), class = "b")
)

# Bayesian analysis using brms
model42 = brm(
  data = test.data.model42,
  outcome ~
    kin.either +
    z.rank.intervener +
    z.max.dsi +
    (1|intervener) +
    (1|target) +
    (1|competitor),
  warmup = 500,
  iter = 5000,
  prior = m1priors,
  cores = 5,
  chains = 2,
  family = categorical(refcat = 'ignored'),
  seed = 123
)
summary(model42)
odds.model42 = exp(fixef(model42))
