library(modelsummary)
library(tinytable)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url) 
dat$Small <- dat$Pop1831 > median(dat$Pop1831)
dat <- dat[, 
           c("Donations", "Literacy", "Commerce", "Crime_pers", "Crime_prop", "Clergy", "Small")
]

datasummary_skim(dat)
datasummary_balance(~Small, dat)
datasummary_correlation(dat)


models <- list(
  "I" = lm(Donations ~ Literacy + Clergy, data = dat),
  "II" = lm(Crime_pers ~ Literacy + Clergy, data = dat),
  "III" = lm(Crime_prop ~ Literacy + Clergy, data = dat),
  "IV" = glm(Crime_pers ~ Literacy + Commerce, family = poisson, data = dat),
  "V" = glm(Donations ~ Literacy + Commerce, family = poisson, data = dat)
)

modelsummary(models, stars = TRUE, gof_omit = "IC|Adj|F|RMSE|Log") |>
  group_tt(j = list("Linear" = 2:4, "Poisson" = 5:6))

modelsummary(models, output = "table.docx")

ols <- models[1:3]
modelplot(ols, coef_omit = "Intercept")

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv'
penguins <- read.csv(url, na.strings = "")
datasummary_skim(penguins)

library(modelsummary)

url <- "https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv"
dat <- read.csv(url, na.strings = "")

models <- list(
  I = lm(Donations ~ Literacy, data = dat),
  II = lm(Crime_pers ~ Literacy, data = dat),
  III = lm(Crime_prop ~ Literacy + Clergy, data = dat),
  IV = glm(Crime_pers ~ Literacy + Clergy, family = poisson, data = dat),
  V = glm(Donations ~ Literacy + Clergy, family = poisson, data = dat)
)

modelsummary(models) |>
  group_tt(j = list(Linear = 2:4, Poisson = 5:6)) |>
  style_tt(i = 3:4, j = 2, background = "darkblue", color = "white", bold = TRUE)

Density <- function(x) ""

datasummary(mpg + hp + wt ~ Mean + SD + max + min + var + Density, data = mtcars) |>
  plot_tt(
    j = 7,
    fun = "density",
    data = list(mtcars$mpg, mtcars$hp, mtcars$wt),
    color = "#E69F00")
