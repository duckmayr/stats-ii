## ?wooldridge::traffic2
eqn = formula(
    prcfat ~ t +
        feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec +
        unem + wkends + spdlaw + beltlaw
)
ols = lm(eqn, data = wooldridge::traffic2)
summary(ols)
r = resid(ols)
summary(lm(r[-1] ~ r[-length(r)] - 1))
# \rho \approx 0.2816, evid of autocor since p < 0.01
m = prais::prais_winsten(formula = eqn, data = wooldridge::traffic2, index = "t")
summary(m)
V = sandwich::NeweyWest(ols, lag = 4)
S = summary(ols)$coefficients
policyvars = c("spdlaw", "beltlaw")
round(cbind(
    OLS = 1 - pt(abs(S[ , "Estimate"] / S[ , "Std. Error"]), df = 91),
    HAC = 1 - pt(abs(S[ , "Estimate"] / sqrt(diag(V))), df = 91)
), 3)
