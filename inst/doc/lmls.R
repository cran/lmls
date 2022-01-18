## ----setup, include = FALSE---------------------------------------------------
library(ggplot2)
library(patchwork)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "cairo_pdf",
  fig.align = "center",
  fig.height = 3.5,
  out.width = "80%"
)

theme_set(theme_bw(base_size = 11))

theme_update(
  legend.position = "none",
  axis.text = element_text(color = "black", size = 11),
  axis.title.x.bottom = element_text(margin = margin(t = 16.5)),
  axis.title.y.left = element_text(margin = margin(r = 16.5))
)

set.seed(1337)

## ----abdom-plot, fig.cap = "(ref:abdom-plot)", fig.pos = "H"------------------
library(lmls)

ggplot(abdom, aes(x, y)) +
  geom_point(color = "darkgray", size = 1) +
  xlab("Age [weeks]") +
  ylab("Size [mm]")

## ----abdom-lm-----------------------------------------------------------------
m1 <- lm(y ~ x, data = abdom)
summary(m1)

## ----abdom-resid, fig.cap = "(ref:abdom-resid)", fig.pos = "H"----------------
abdom$resid <- resid(m1)

ggplot(abdom, aes(x, resid)) +
  geom_point(color = "darkgray", size = 1) +
  geom_hline(yintercept = 0, size = 0.5) +
  xlab("Age [weeks]") +
  ylab("Residuals")

## ----abdom-lmls, fig.cap = "(ref:abdom-lmls)", fig.pos = "H"------------------
m2 <- lmls(y ~ x, ~ x, data = abdom)

abdom$mu <- predict(m2, type = "response", predictor = "location")
abdom$sigma <- predict(m2, type = "response", predictor = "scale")
abdom$upper <- abdom$mu + 1.96 * abdom$sigma
abdom$lower <- abdom$mu - 1.96 * abdom$sigma

ggplot(abdom, aes(x, y)) +
  geom_point(color = "darkgray", size = 1) +
  geom_line(aes(y = mu), size = 0.7) +
  geom_line(aes(y = upper), size = 0.3) +
  geom_line(aes(y = lower), size = 0.3) +
  xlab("Age [weeks]") +
  ylab("Size [mm]")

## ----abdom-poly-1-------------------------------------------------------------
m3 <- lmls(y ~ poly(x, 2), ~ x, data = abdom)

## ----abdom-poly-2, echo = FALSE, fig.cap = "(ref:abdom-poly-2)"---------------
abdom$mu <- predict(m3, type = "response", predictor = "location")
abdom$sigma <- predict(m3, type = "response", predictor = "scale")
abdom$upper <- abdom$mu + 1.96 * abdom$sigma
abdom$lower <- abdom$mu - 1.96 * abdom$sigma

ggplot(abdom, aes(x, y)) +
  geom_point(color = "darkgray", size = 1) +
  geom_line(aes(y = mu), size = 0.7) +
  geom_line(aes(y = upper), size = 0.3) +
  geom_line(aes(y = lower), size = 0.3) +
  xlab("Age [weeks]") +
  ylab("Size [mm]")

## ----abdom-mcmc-1-------------------------------------------------------------
m3 <- lmls(y ~ poly(x, 2), ~ x, data = abdom, light = FALSE)
m3 <- mcmc(m3)

summary(m3, type = "mcmc")

## ----abdom-mcmc-2, include = FALSE--------------------------------------------
theme_update(axis.title.y.left = element_text(margin = margin(r = 5.5)))

## ----abdom-mcmc-3, fig.cap = "(ref:abdom-mcmc-3)", fig.pos = "H"--------------
samples <- as.data.frame(m3$mcmc$scale)
samples$iteration <- 1:nrow(samples)

p1 <- ggplot(samples, aes(iteration, x)) +
  geom_line() +
  xlab("Iteration") +
  ylab(expression(gamma[1]))

p2 <- ggplot(samples, aes(x, after_stat(density))) +
  geom_histogram(bins = 15) +
  xlab(expression(gamma[1])) +
  ylab("Density")

p1 + p2

## ----abdom-mcmc-4, include = FALSE--------------------------------------------
theme_update(axis.title.y.left = element_text(margin = margin(r = 16.5)))

## ----abdom-bench-1, eval = FALSE----------------------------------------------
#  library(gamlss)
#  library(mgcv)
#  
#  bench <- microbenchmark::microbenchmark(
#    lmls = lmls(y ~ poly(x, 2), ~ x, data = abdom),
#    mgcv = gam(list(y ~ poly(x, 2), ~ x), family = gaulss(), data = abdom),
#    gamlss = gamlss(y ~ poly(x, 2), ~ x, data = abdom)
#  )

## ----abdom-bench-2, echo = FALSE, fig.cap = "(ref:abdom-bench-2)"-------------
load("abdom-bench.RData")

ggplot(bench, aes(time / 1e6, expr, color = expr, fill = expr)) +
  geom_boxplot(alpha = 0.4) +
  geom_jitter(alpha = 0.4) +
  coord_cartesian(xlim = c(0, NA)) +
  scale_y_discrete(limits = rev(levels(bench$expr))) +
  xlab("Execution time [ms]") +
  ylab("Package")

## ----info-beta, eval = FALSE--------------------------------------------------
#  crossprod(X / scale)

## ----info-gamma, eval = FALSE-------------------------------------------------
#  2 * crossprod(Z)

