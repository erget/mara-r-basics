aphasiker <- read.table("data/Aphasiker.csv", sep = ";", dec = ",", header = TRUE, encoding = "UTF-8")
claims <- read.table("data/Auto_Insurance_Claims_Sample.csv", sep = ",", header = T)

aphasiker

head(aphasiker)

tail(aphasiker)

nrow(aphasiker)

colnames(aphasiker)

summary(aphasiker)

str(aphasiker)

42
-5.37
5.2e6
"foobar"
"here's some more character data"
TRUE
FALSE

my.number <- 25
my.characters <- "Wir schaffen das!"
my.truth <- FALSE
my.number
my.characters
my.truth

25 + 42
25 / 42
25 ^ 42

"Wir" + "schaffen"

TRUE + TRUE
TRUE + FALSE
TRUE - FALSE
FALSE * FALSE

25 + TRUE
25 - FALSE

str(my.number)
class(my.number)
str(my.characters)
class(my.characters)
str(my.truth)
class(my.truth)

c(1, 2, 3, 4, 5)
c("these", "are", "exactly", "five", "words")
c(T, T, F, F, T)

n.vector <- c(1, 2, 3, 4, 5)
c.vector <- c("these", "are", "exactly", "five", "words")
b.vector <- c(T, T, F, F, T)

c(n.vector, n.vector)
c(n.vector, 5, 6)
c(n.vector, c(5, 6))
c(n.vector, c.vector)
c(c.vector, b.vector)
c(n.vector, c.vector, b.vector)

n.vector[3]
n.vector[-2]
n.vector[1:2]
n.vector[c(T, T, F, F, F)]
n.vector[b.vector]

n.vector + b.vector
n.vector + 200
n.vector + 1:2

max(100:1)
min(100:1)
?max

na.vector <- 1:10
na.vector[5] <- NA
na.vector

na.vector + 1

na.vector * 2

mean(na.vector)

min(na.vector)

is.na(na.vector)  # Find NAs in vector

na.vector[!is.na(na.vector)]  # ! negates a boolean vector

mean(na.vector, na.rm = TRUE)

sort(n.vector)
sort(c.vector, decreasing = T)
sort(b.vector)
sort(na.vector)
sort(na.vector, na.last = TRUE)
sort(na.vector, na.last = F)

n.vector
filter.vector <- as.logical(!n.vector %% 2)  # Filter odds
filter.vector[n.vector >= 4] <- T  # Keep >= 4
n.vector[filter.vector]  # Did the filter work?
# Modify original vector (actually you should make a new one)
n.vector <- n.vector[filter.vector]
n.vector

filtered.words <- c.vector[c.vector < "l"]
filtered.words
length(filtered.words)

series <- rnorm(100, sd = 200)

sd(series)

var(series)

sort(series)

sort(series, decreasing = TRUE)

series[series > 0]

series <- rexp(100)
sd(series)
var(series)
sort(series)
sort(series, decreasing = TRUE)
series[series > 1]

nrow(aphasiker)  # Number of rows in the table = number of patients
length(aphasiker$Patienten_ID)  # Length of one variable = number of patients
mean(aphasiker$Alter)
table(aphasiker$Geschlecht) 

mean(claims$Total.Claim.Amount)
median(claims$Total.Claim.Amount)
min(claims$Total.Claim.Amount)
max(claims$Total.Claim.Amount)

summary(claims$Total.Claim.Amount)

claims$high.qualified <- FALSE
claims[claims$Education == "Doctor" | claims$Education == "Master", ]$high.qualified <- TRUE
mean(claims[claims$high.qualified == TRUE, ]$Total.Claim.Amount)
mean(claims[claims$high.qualified == FALSE, ]$Total.Claim.Amount)

hist(claims$Income)
hist(claims[claims$Income>0,]$Income, main="Income Distribution", xlab="Income")

barplot(table(claims$State))
barplot(table(claims$Education))
barplot(table(claims$Claim.Reason))

plot(claims$Total.Claim.Amount, claims$Claim.Amount,
     pch=19, col = "darkgrey", cex=0.5,
     xlab="Total Claim Amount",
     ylab="Claim Amount")
abline(lm(claims$Claim.Amount ~ claims$Total.Claim.Amount), col="red")

hist(claims$Claim.Amount)
mean(claims$Claim.Amount)
hist(rexp(10000, rate=1/mean(claims$Claim.Amount)))
ks.test(claims$Claim.Amount, "pexp", rate=1/mean(claims$Claim.Amount))

table(claims$EmploymentStatus)
claims_amount_employed <- claims[claims$EmploymentStatus == "Employed", ]$Claim.Amount
claims_amount_unemployed <- claims[claims$EmploymentStatus == "Unemployed", ]$Claim.Amount
t.test(claims_amount_employed, claims_amount_unemployed)
wilcox.test(claims_amount_employed, claims_amount_unemployed)

library(dplyr)

arrange(filter(select(claims, 
                      Customer,
                      Claim.Amount,
                      Number.of.Open.Complaints), 
               Number.of.Open.Complaints >= 1), 
        -Claim.Amount)

claims |>
    select(Customer, Claim.Amount, Number.of.Open.Complaints) |>
    filter(Number.of.Open.Complaints >= 1) |>
    arrange(-Claim.Amount)

cor.test(claims$Claim.Amount, claims$Total.Claim.Amount, method="spearman")
cor.test(claims$Claim.Amount, claims$Months.Since.Policy.Inception, method="spearman")
cor.test(claims$Claim.Amount, claims$Income, method="spearman")

cor(claims[, c(5, 12, 17)], method = "spearman")

numeric.aphasiker <- aphasiker[, c(3,4, 6:14)]
apply(numeric.aphasiker, 2, min, na.rm = T)
apply(numeric.aphasiker, 2, max, na.rm = T)
apply(numeric.aphasiker, 2, sd, na.rm = T)

# Repeat this with different values for hungry
# Also try using conditions that have to be evaluated, like "n - 4"
hungry <- TRUE
if (hungry) {
  print("I'll cook something.")
  print("And then I'll eat it.")
  print("Afterwards I need to wash the dishes.")
} else {
  print("I think I'll go running instead.")
  print("Oh, my MP3 player isn't charged.")
  print("Maybe I'll just read a book?")
}

# Each member is assigned to the variable you specify
nums <- runif(10, 0, 100)  # 10 random numbers between 0 and 100
for (i in nums) {  # Each member of nums is assigned to i for the block's body
  print(i)
}

i <- 0
while (i < 10) {
  print(i)
  i <- i + 1
}

mittel <- function(numbers) {
  # This returns the mean of the numbers passed to the function.
  sum(numbers) / length(numbers)  # The result is returned to the caller
}
nums <- 1:10  # Generate a series of numbers
mean(nums) == mittel(nums)  # Result is true: it worked!
