df <- read.csv("PlannedBehavior.csv")
# cat(names(df), sep = " + ")


lm(intention ~ attitude + norms + control, df) |>
  summary()



# Útelemzés
# M. Székely: 302-319

# install.packages("lavaan")
library(lavaan)
library(semPlot)
install.packages("semPlot")

# OLS
specification <- "
intention ~ attitude + norms + control
"

model <- sem(specification, df)
summary(model, fit.measures=T)
lavInspect(model, "rsquare")


# Utak beépítése
# Feltételek:
# mint regressziónál 
# + rekurzív mechanizmus (közvetett, közvetlen és intermediális, közbülső változók)
# + nincs reziduális korreláció
# + lineáris a kapcsolat


specification <- "
intention ~ attitude + norms + control
behavior ~ intention
# intention + behavior ~ attitude + norms + control # felső 2 sor egyben

"

model <- sem(specification, df)
summary(model, fit.measures=T)
lavInspect(model, "rsquare")

# Túlhatározott modellre törekszünk (sem esetén df > 0)
# N * (N + 1) / 2

# Modell illeszkedés diagnosztika -> ha több feltétel sérül -> jól állítottuk fel a modellt?
# Chi^2 test: Nullhipotézis: a modell tökéletesen illeszkedik
# CFI: > 0.90 
# TLI: > 0.95
# RMSEA: < 0.8
# RMSMR: < 0.8

semPlot::semPaths(model, "std")


# HF: útelemzés saját adaton
