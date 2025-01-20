# R ismétlés

# https://support.posit.co/hc/en-us/articles/200711853-Keyboard-Shortcuts-in-the-RStudio-IDE

# 1. Definiálj két változót és végezz el velük különböző alapműveleteket (összeadás, szorzás)!
# 2. Írj egy if-else szerkezetet, ami vizsgálja, hogy két változó közül melyik a nagyobb, és azt írja ki!
# 3. Hozz létre egy for ciklust, amely kiírja az első 5 egész szám négyzetét!
# 4. Készíts egy függvényt, ami létrehoz egy  0-tól n-ig terjedő vektort, majd plotolja. 
#    Alkalmazd a függvényt egy számra!
# 5. Hozz létre egy saját DataFrame-et, ahol legalább 4 oszlop (például Név, Kor, Súly, Pontszám) található.
#    Addj hozzá 2 sort az adatokhoz!
# 6. Számold ki a DataFrame "Pontszám" oszlopának átlagát és legnagyobb értékét!
# 7. Készíts egy korrelációs mátrixot a DataFrame-hez.
# 8. Lineáris regresszió segítségével modellezd a numerikus változókat.
# 9. ggplot segítségével jelenítsd meg grafikusan a változókat a regressziós egyenessel.

# 1.
x <- 10
y <- 40

sum(c(x, y))
x + y
x * y

# 2.
if(x > y){
  print(x)
}else{
  print(y)
}

# 3.
for(i in 1:5){
  print(i ^ 2)  # vagy **
}

# 4.
sekv <- function(n=20){
  # seq(0, n)
  x <- 1:n
  plot(x)
  return(x)
}
sekv(1)


# 5


# 5
df <- data.frame(
  "Nev" = c("Lajos", 'Mari', 'Géza', 'Irén'),
  "Kor" = c(25, 32, 40, 70),
  "Suly" = c(75, 60, 88, 62),
  "Pontszám" = c(100, 78, 87, 99)
)
df

new.rows <- data.frame(
  "Nev" = c("Anna", "Gábor"),
  "Kor" = c(34, 36),
  "Suly" = c(57, 65),
  "Pontszám" = c(100, 89)
)

df <- rbind(df, new.rows)

# 6
max(df$Pontszám)
mean(df$Pontszám)

df$Pontszám |> max()
df$Pontszám |> mean()


# 7.

matrix <- cor(df[c("Kor", "Suly", "Pontszám")])
matrix

corrplot::corrplot(matrix)

# 8.
names(df)
model <- lm(`Pontszám` ~ Kor+Suly, df)
summary(model)

# 9.
ggiraphExtra::ggPredict(model, interactive = T, se = T)

library(ggplot2)
df$korkat <- sapply(df$Kor, \(x) ifelse(x < 40, "fiatal", "idős"))

ggplot(df, aes(x=Suly, y=Pontszám)) + 
  geom_point() +
  geom_smooth(method = "lm", se=F)

ggplot(df, aes(x=Suly, y=Pontszám, color=korkat)) + 
  geom_point() +
  geom_smooth(method = "lm", se=F)





# Aritmetika
a <- 5
b <- 3
sum_result <- a + b
product_result <- a * b
sum_result
product_result


# Data Structures: Vectors, Matrices, Lists
vector <- c(1, 2, 3, 4, 5)
matrix <- matrix(1:9, nrow = 3, ncol = 3)
list <- list(1, "hello", TRUE)


# Elágazások, Control Flow
print("\nControl Flow:")
if (a > b) {
  print("a nagyobb mint b")
} else {
  print("b nagyobb vagy egyenlő a-val")
}


# Ciklusok
for (i in 1:5) {
  print(paste("iteráció:", i))
}

# Függvények
print("\nFunctions:")
square <- function(x) {
  return(x^2)
}

x <- 10
res <- square(x)
print(paste(x, "^2: ", res))


# Data Frame
df <- data.frame(
  Name = c("Lajos", "Mari", "Károly", "Irén"),
  Age = c(25, 30, 28, 35),
  Score = c(80, 90, 85, 88)
)

df


# 1. Definiálj két változót és végezz el velük különböző alapműveleteket (összeadás, szorzás)!
# 2. Írj egy if-else szerkezetet, ami vizsgálja, hogy két változó közül melyik a nagyobb, és azt írja ki!
# 3. Hozz létre egy for ciklust, amely kiírja az első 5 egész szám négyzetét!
# 4. Készíts egy függvényt, ami létrehoz egy  0-tól n-ig terjedő vektort, majd plotolja. 
#    Alkalmazd a függvényt egy számra!
# 5. Hozz létre egy saját DataFrame-et, ahol legalább 4 oszlop (például Név, Kor, Súly, Pontszám) található. Addj hozzá 2 sort az adatokhoz!
# 6. Számold ki a DataFrame "Pontszám" oszlopának átlagát és legnagyobb értékét!
# 7. Készíts egy korrelációs mátrixot a DataFrame-hez.
# 8. Lineáris regresszió segítségével modellezd a numerikus változókat.
# 9. ggplot segítségével jelenítsd meg grafikusan a változókat a regressziós egyenessel.



# Útmodell:
# A statisztikai modellezésbenben a változók közötti kapcsolatok (utak) grafikus ábrázolása
# A megfigyelt változók közötti közvetlen kapcsolatok meghatározására és becslésére összpontosít, jellemzően lineáris regressziós környezetben
# Nem veszi kifejezetten figyelembe a látens változókat vagy a mérési hibát
# 
# Strukturális egyenletmodellezés (SEM):
# A SEM egy átfogó statisztikai keretrendszer, amely lehetővé teszi mind a megfigyelt, mind a látens változók modellezését
# Speciális esetként magában foglalja az útmodellezést, de azon túlmutatva modellezi a megfigyelt változók,
# a látens változók, a mérési hiba és az adatok mögöttes struktúrája közötti kapcsolatokat
# Több megfigyelt változót, látens változókat, faktorelemzést, hibatermeket foglalhat magában,
# és lehetővé teszi az összetett kapcsolatok becslését, beleértve a közvetlen és közvetett hatásokat
# Lineáris egyenletrendszerek megoldását igényli, és mind a változók közötti kapcsolatokat, mind a megfigyelt és látens változók varianciáit és kovarianciáit becsüli

library(lavaan)


set.seed(123)
n <- 100  # Sample size

latent_variable <- rnorm(n)  

observed_variable1 <- 0.8 * latent_variable + rnorm(n)
observed_variable2 <- 0.5 * latent_variable + rnorm(n)

data <- data.frame(latent_variable, observed_variable1, observed_variable2)

model <- '
  observed_variable1 ~ 0.8 * latent_variable
  observed_variable2 ~ 0.5 * latent_variable
'


fit <- sem(model, data = data)
summary(fit)
