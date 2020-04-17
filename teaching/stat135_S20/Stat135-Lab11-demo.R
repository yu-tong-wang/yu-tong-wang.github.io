library(ggplot2)

## Case Study 1: Simple linear regression
x <- seq(1, 10, 0.5)
y <- rnorm(length(x), mean = x, sd = 1)

# method 1: `lm` function
lm_fit <- lm(y ~ x)
lm_fit$coefficients[1]
lm_fit$coefficients[2]
# method 2: (X^TX)^{-1}(X^TY)
# design matrix
X <- cbind(rep(1, length(x)), x)
y <- as.matrix(y)
solve(t(X) %*% X) %*% t(X) %*% y # Notice this is the same as the regression coefficients calculated from `lm`.

# summary result of your linear model
summary_fit <- summary(lm_fit)
summary_fit
# two methods to get your coefficient matrix
summary_fit$coefficients # method 1
coef(summary_fit) # method 2
summary_fit$coefficients[1, 2] # se of the intercept

# two methods to get your confidence intervals
confint(lm_fit) # method 1

n <- length(x) # method 2
mean_beta0 <- coef(summary_fit)[1, 1]
se_beta0 <- coef(summary_fit)[1, 2]
mean_beta1 <- coef(summary_fit)[2, 1]
se_beta1 <- coef(summary_fit)[2, 2]
CI_beta0 <- c(mean_beta0 - qt(0.975, df = n - 2) * se_beta0,
         mean_beta0 + qt(0.975, df = n - 2) * se_beta0)
CI_beta1 <- c(mean_beta1 - qt(0.975, df = n - 2) * se_beta1,
              mean_beta1 + qt(0.975, df = n - 2) * se_beta1)
CI_beta0
CI_beta1


# plot
data_combo <- as.data.frame(cbind(x, y))

ggplot(data_combo, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) # without se being plotted

ggplot(data_combo, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm) +# with se being plotted
  ggtitle("Strong positive linearity") + # add a plot title
  xlab("this is X-axis") + # add axis title
  ylab("this is Y-axis") +
  theme_classic()# add a clean theme without grids

# residual plot
ggplot(lm_fit) +
  geom_point(aes(x = x, y = .resid)) +
  geom_hline(aes(yintercept = 0), col = "red")



## Case Study 2: Multiple linear regression: X_1 = I(ctl), X_2 = I(trt)
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- c(rep("ctl", length(ctl)), rep("trt", length(trt)))
weight <- c(ctl, trt)

# method 1: `lm` function
lm_D9 <- lm(weight ~ group)
summary(lm_D9)
lm_D9$coefficients[1]
lm_D9$coefficients[2]

# method 2: (X^TX)^{-1}(X^TY)

# design matrix
X <- cbind(rep(1, length(ctl) + length(trt)),
           c(rep(0, length(ctl)), rep(1, length(trt))))
y <- as.matrix(weight)

solve(t(X) %*% X) %*% t(X) %*% y


