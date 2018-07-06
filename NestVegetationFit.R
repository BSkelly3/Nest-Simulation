# Fit different nest survival models
# ----------------------------------------------------------------------- #
# No effect models
# Beta = 0 
fit_nullf <- glm(Null ~ veg_fate_null, family = binomial, data = data)
summary(fit_nullf)
fit_nullh <- glm(Null ~ veg_hatch_null, family = binomial, data = data)
summary(fit_nullh)
fit_nullc <- glm(Null ~ veg_cont_null, family = binomial, data = data)
summary(fit_nullc)

AIC(fit_nullf, fit_nullh, fit_nullc)
# ----------------------------------------------------------------------- #
# Negative effect models
# Beta = -0.1
fit_n1f <- glm(n1 ~ veg_fate_n1, family = binomial, data = data)
summary(fit_n1f)
fit_n1h <- glm(n1 ~ veg_hatch_n1, family = binomial, data = data)
summary(fit_n1h)
fit_n1c <- glm(n1 ~ veg_cont_n1, family = binomial, data = data)
summary(fit_n1c)

# Beta = -0.2
fit_n2f <- glm(n2 ~ veg_fate_n2, family = binomial, data = data)
summary(fit_n2f)
fit_n2h <- glm(n2 ~ veg_hatch_n2, family = binomial, data = data)
summary(fit_n2h)
fit_n2c <- glm(n2 ~ veg_cont_n2, family = binomial, data = data)
summary(fit_n2c)

# Beta = - 0.3
fit_n3f <- glm(n3 ~ veg_fate_n3, family = binomial, data = data)
summary(fit_n3f)
fit_n3h <- glm(n3 ~ veg_hatch_n3, family = binomial, data = data)
summary(fit_n3h)
fit_n3c <- glm(n3 ~ veg_cont_n3, family = binomial, data = data)
summary(fit_n3c)

# ----------------------------------------------------------------------- #
# Positive effect models
# Beta = 0.1
fit_p1f <- glm(p1 ~ veg_fate_p1, family = binomial, data = data)
summary(fit_p1f)
fit_p1h <- glm(p1 ~ veg_hatch_p1, family = binomial, data = data)
summary(fit_p1h)
fit_p1c <- glm(p1 ~ veg_cont_p1, family = binomial, data = data)
summary(fit_p1c)

# Beta = 0.2
fit_p2f <- glm(p2 ~ veg_fate_p2, family = binomial, data = data)
summary(fit_p2f)
fit_p2h <- glm(p2 ~ veg_hatch_p2, family = binomial, data = data)
summary(fit_p2h)
fit_p2c <- glm(p2 ~ veg_cont_p2, family = binomial, data = data)
summary(fit_p2c)

# Beta = 0.3
fit_p3f <- glm(p3 ~ veg_fate_p3, family = binomial, data = data)
summary(fit_p3f)
fit_p3h <- glm(p3 ~ veg_hatch_p3, family = binomial, data = data)
summary(fit_p3h)
fit_p3c <- glm(p3 ~ veg_cont_p3, family = binomial, data = data)
summary(fit_p3c)

