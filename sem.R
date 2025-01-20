df <- read.csv("D:/Users/witen/Downloads/PlannedBehavior.csv")

cat(names(df), sep = " + ")

lm(intention ~ attitude + norms + control, df) |> summary()



# Útelemzés
library(lavaan)

# OLS
specification <- "
intention ~ attitude + norms + control
attitude ~~ norms + control
norms ~~ control
"


model <- sem(specification, df)
summary(model, fit.measures=T)
lavInspect(model, "rsquare")


# Utak beépítése
# Feltételek:
# mint regressziónál 
# + rekurzív mechanizmus
# + nincs reziduális korreláció
# + lineáris a kapcsolat
specification <- "
intention ~ attitude + norms + control
behavior ~ intention
# intention + behavior ~ attitude + norms + control # felső 2 sor egyben
attitude ~~ norms + control
norms ~~ control
"

model <- sem(specification, df)
summary(model, fit.measures=T)
lavInspect(model, "rsquare")

# Túlhatérozott modell (sem esetén df > 0) -> van értelme a modellnek
# p * (p + 1) / 2

# Modell illeszkedés diagnosztika -> ha több feltétel sérül -> jól állítottuk fel a modellt?
# Chi^2 test: Nullhipotézis: a modell tökéletesen illeszkedik
# CFI: > 0.90 
# TLI: > 0.95
# RMSEA: < 0.8
# RMSMR: < 0.8


library(lavaan)
library(dplyr)

df <- read.csv("https://osf.io/bu74a/download", header=TRUE, na.strings="NA") 

head(df)

table(df$SEX) %>% prop.table()
names(df)
# Perceived benefits of using Facebook (FB.BEN*)
# Perceived concerns (PRI.CON*)
# Privacy self-efficacy (FB.PRI.SEL.EFF*)
# Self-Disclosure (FB.DIS*)
# Withdrawal (FB.WIT*)
# Check multivariate normal distribution

# normalitás vizsgálat (skewness és kurtusis-szel)
d %>%
  select(PRI.CON_1:PRI.CON_4) %>%
  psych::mardia(plot = FALSE) #
# skewness: -0.5 - 0.5
# curtosis: < 2


model1 <- "
  # látens változók
  priv_concern =~ PRI.CON_1+ PRI.CON_2 + PRI.CON_3 + PRI.CON_4
  self_disclosure =~ FB.DIS_1 + FB.DIS_2 + FB.DIS_3 + FB.DIS_4 
  
  # regresszió
  self_disclosure ~ priv_concern
"

# Illesztés
fit_m1 <- sem(model = model1, 
              estimator = "MLR",
              data = d)

summary(fit_m1, 
        std = T) # standardizált beta -> 0 és 1 között (hatás nagyság)



# Újraspecifikálás
model2 <- "
  # látens változók
  priv_con =~ PRI.CON_P1 + PRI.CON_P2
  perc_ben =~ FB.BEN_P1  + FB.BEN_P1 + FB.BEN_P3
  self_eff =~ FB.PRI.SEL.EFF_P1 + FB.PRI.SEL.EFF_P2
  self_dis =~ FB.DIS_P1 + FB.DIS_P2
  self_wit =~ FB.WIT_P1 + FB.WIT_P2
  
  # regressziók
  self_dis ~ priv_con + perc_ben + self_eff
  self_wit ~ priv_con + perc_ben + self_eff
"

fit_mod2 <- sem(model2, estimator = "MLR", data = d)
summary(fit_mod2, std = T, fit.measures=T)

utils::browseURL(getwd())
