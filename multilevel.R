library(dplyr)
library(ggplot2)

df <- read.csv("complete.csv", encoding = "UTF-8")
names(df)
df$full_name
df$age
unique(df$club) %>% length()

df$eur_wage <- df$eur_wage/1000

aggr <- aggregate(full_name ~ club, df, length)
clubs <- aggr[aggr$full_name >= 30 & aggr$club != "", "club"]
clubs
df <- df[df$club %in% clubs & df$eur_wage > 1.5,]


beta <- c()
b_club <- c()
for (c in df$club){
    if (!c %in% b_club){
        b <- summary(lm(eur_wage ~ age, df[df$club == c,]))$coefficients[2]
            beta <- c(beta, b)
            b_club <- c(b_club, c)
    }
}

b_df <- data.frame("b" = beta, "club" = b_club)
b_df |> View()
sample_clubs <- arrange(b_df, b)[c(1:4, 156:161), "club"]


# df$eur_wage <- log(df$eur_wage)
ggplot(df[df$club %in% sample_clubs,], aes(age, eur_wage)) +
    geom_point()+
    geom_smooth(method = "lm", se = F)

ggplot(df[df$club %in% sample_clubs,], aes(age, eur_wage, colour=club)) +
    geom_point()+
    geom_smooth(method = "lm", se = F)

names(df)
ols <- lm(eur_wage ~ height_cm+ weight_kg+ heading_accuracy+ short_passing+ free_kick_accuracy+
                     acceleration+ age, data = df)
summary(ols)

summary(df[,c("height_cm", "weight_kg", "heading_accuracy", "short_passing",
        "free_kick_accuracy", "acceleration", "age", "eur_wage")])

library(merTools)
library(lme4)
library(jtools)
library(lmerTest)
merTools::ICC("eur_wage", "club", df)

avg_value_in_club <- aggregate(eur_value~club, df, mean)
avg_value_in_club$eur_value <- avg_value_in_club$eur_value %>% log()
names(avg_value_in_club)[2] <- "avg_value"
df <- full_join(df, avg_value_in_club)

model0 <- lmer(eur_wage~1+(1|club), REML=FALSE, data=df)
summ(model0)
ranova(model0)

is.na(df$age) %>% any()
model1 <- lmer(eur_wage~1+ height_cm+ weight_kg+ heading_accuracy+ short_passing+
                 free_kick_accuracy+
                   acceleration + age + (1|club), REML=FALSE, data=df)
ranova(model1)

summ(model1)
anova(model0, model1)

# HF: Saját elemzés többszintű modellel.


model2 <- lmer(eur_wage~1+ height_cm+ weight_kg+ heading_accuracy+ short_passing+
                   free_kick_accuracy+acceleration + age + (1+age|club),
               REML=FALSE, data=df)
summ(model2, digits=4)
ranova(model2)

anova(model2, model1)
