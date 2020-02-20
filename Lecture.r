1 + 3  # Addition

8 %% 3  # Remainder division

5 ^ 2

1:10  # Generate a series

mean(1:10)  # Use a function

x <- mean(1:10) # Capture results by saving it to a variable

x <- c(5,4,1,6,3,1,5)
x

?c  # Get description for function c

Mean(x)  # Produces an error because it's called "mean", not "Mean"!

mean(x

mean(x
    )

?read.table

getwd()

claims <- read.table("data/Auto_Insurance_Claims_Sample.csv", sep=",", header=T)

head(claims)

str(claims)

hist(claims$Claim.Amount)

table(claims$Education)

claims$Education.Level <- "low"
claims$Education.Level[claims$Education %in% c("Master", "Doctor", "Bachelor")] <- "high"

claims_basic <- claims[claims$Coverage == "Basic", ]

fit_claims <- glm(Claim.Amount ~ Monthly.Premium.Auto + Income + Education.Level + Claim.Reason,
                  data=claims_basic, family=Gamma(link='log'))

summary(fit_claims)

plot(fit_claims)

# Load a library that contains functions we want to use
library(ggplot2)

p <- ggplot(claims,
       aes(x=Monthly.Premium.Auto, y=Claim.Amount, color=Coverage))
p + geom_point(size=0.5,alpha=0.3) + facet_wrap(~Coverage) +
    geom_smooth(method="lm", color="orange", linetype=2) +
    labs(title="Claim Analyses", x="Monthly Premium", y="Claim Amount") +
    scale_colour_brewer(palette = "Set1") 

getwd()  # Find out current working directory

# Define the path where the data is stored on your computer. Some examples:
#work.dir <- "D:/BEN/my_name"
#work.dir <- "~/Dropbox/Bio-Workshop"
work.dir <- "C:/Users/n351384/OneDrive - Munich Re/Privat/R basics 2days"
# Set this to directory with your data
setwd(work.dir)

?read.table  # overview on all parameters

claims <- read.table("data/Auto_Insurance_Claims_Sample.csv", sep=",", header=T)

aphasiker <- read.table("data/aphasiker.csv", sep=";", dec=",", header=TRUE)

read.table("data/Aphasiker.csv", sep=";", dec=",", header=TRUE)

read.table("data/aphasiker.csv", sep=",")

head(claims)  # get only first rows of table

tail(claims)  # get only last rows of table

nrow(claims)  # how many rows does the table have?

colnames(claims)  # what are the column names of the table?

summary(claims)  # a quick summary of the table 

str(claims)  # deeper information on the table 

0  # Numeric

"zero"  # Character

FALSE  # Logical

"a string" + 2

numbers <- c(5, 79, 234, 150, 0, -986)
numbers

numbers[1]

numbers[-1]

numbers[-5]

numbers[2:4]

numbers[c(1:3, 5)]

numbers2 <- -2:-10
new.numbers <- c(numbers, numbers2)

some.strings <- c("This", "is", "a", "bunch", "of", "strings")
some.strings[2:4]
some.strings[-(2:4)]

?mean  # How do I use the function "mean"?

mean(numbers)  # Mean

sd(numbers)  # Standard deviation

var(numbers)  # Variance

not.available <- NA

1 + NA

mean(c(numbers, not.available))

mean(c(numbers, not.available), na.rm = TRUE)

sort(numbers)
sort(numbers, decreasing = TRUE)

numbers <- sort(numbers)

numbers[numbers < 100]
numbers[numbers > 0]

numbers[numbers < 100 & numbers > 0]
numbers[numbers > 100 | numbers < 0]

length(numbers[numbers %% 2 == 0])

?distributions

random.numbers <- runif(min = -500, max = 500, n = 100)

a <- c(2, 3, 5) 
b <- c("aa", "bb", "cc") 
c <- c(TRUE, FALSE, TRUE) 
df <- data.frame(a, b, c)  # create new data.frame
df

class(claims)  # get information on class of object

str(claims)  # get single elements/vectors of a data.frame

claims$Claim.Amount  # By variable name

claims[["Claim.Amount"]]  # by name string

claims[,5]  # by column number

colnames(claims)  # find the column numbers

head(claims[-5])  # the reverse: all columns except 5

mean(claims$Claim.Amount)

attach(claims)
mean(Claim.Amount)

detach(claims)
mean(Claim.Amount)

claims[42,]  # Row 42

claims[37, 8]  # Row 37, column 8

claims[claims$State == "Kansas" & claims$Months.Since.Policy.Inception <= 1, ]

claims$State == "Kansas" & claims$Months.Since.Policy.Inception <= 1

subset(aphasiker, State == "Kansas" & Months.Since.Policy.Inception <= 1)

claims$id <- 1:nrow(claims)  # Assign an ID number to each row
head(claims)

claims$Large.Loss <- "No"
claims$Large.Loss[claims$Claim.Amount > 1000] <- "Yes"
table(claims$Large.Loss)

?apply

head(aphasiker)

apply(aphasiker, 2, summary)  # Apply "summary" to each column

summary(aphasiker)  # For comparison

apply(aphasiker[, c(3,4, 6:14)], 2, mean, na.rm = T)

write.table(claims, "claims_new.txt", sep=";")

write.csv(object.name, "claims_new.csv")

?write.table

?par

hist(claims$Claim.Amount)

hist(claims$Claim.Amount, breaks=20, col="blue")

hist(claims$Claim.Amount, breaks=20, col=3)
hist(claims$Claim.Amount, breaks=20, col="#31a354")
hist(claims$Claim.Amount, breaks=20,
     col=rgb(117, 107, 177, maxColorValue = 255))

hist(claims$Claim.Amount, breaks=20, col="#31a354", 
     main="Histogram",
     xlab="Claims Amount")

hist(claims$Claim.Amount, breaks=20, col="#31a354", 
     main="Histogram",
     xlab="Claims Amount")
abline(v = mean(claims$Claim.Amount, na.rm=T))

hist(claims$Claim.Amount, breaks=20, col="#31a354", 
     main="Histogram",
     xlab="Claims Amount")
abline(v = mean(claims$Claim.Amount, na.rm=T), col = "red", lwd = 2)
abline(v = median(claims$Claim.Amount, na.rm=T), col = "blue", lwd = 2)

hist(claims$Claim.Amount, breaks=20, col="#31a354", 
     main="Histogram",
     xlab="Claims Amount")
rug(claims$Claim.Amount)

hist(claims$Claim.Amount, freq=F, breaks=20, col="#31a354", 
     main="Histogram",
     xlab="Claims Amount")
lines(density(claims$Claim.Amount), lwd=2, col="red")

hist(claims$Claim.Amount, breaks=20, col="#31a354", 
     main="Histogram",
     xlab="Claims Amount")
dev.copy2pdf(file = "histogram.pdf", width = 7, height = 5)

gender.table <- table(aphasiker$Geschlecht)
barplot(gender.table, col=rainbow(2), main="Gender")

pie(gender.table) 

boxplot(aphasiker$Alter ~ aphasiker$Aphasie, col="gold")

plot(claims$Total.Claim.Amount, claims$Claim.Amount)

x <- 2001:2010
y <- c(4,1,6,3,8,5,4,7,3,9)
plot(x, y, type="l", lwd=2, col=2)

?heatmap

x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")

aphasiker <- read.table("Aphasiker.csv", sep=";", dec=",", header=TRUE)
claims <- read.table("data/Auto_Insurance_Claims_Sample.csv", sep=",", header=T)

table(aphasiker$Geschlecht)

prop.table(table(aphasiker$Geschlecht))

chisq.test(table(aphasiker$Geschlecht))

chi_gender <- chisq.test(table(aphasiker$Geschlecht))

chi_gender

str(chi_gender)

chi_gender$expected

chisq.test(table(aphasiker$Geschlecht), p=c(0.3,0.7))

table_Gender_Aphasie <- table(aphasiker$Geschlecht, aphasiker$Aphasie)
table_Gender_Aphasie
chisq.test(table_Gender_Aphasie)

fisher.test(table_Gender_Aphasie)

matrix_Gender_Aphasie <- matrix(c(3,4,4,5,6,5,5,4), nrow=2, byrow=T)
matrix_Gender_Aphasie
chisq.test(matrix_Gender_Aphasie)

shapiro.test(aphasiker$Lex_Dec)

ks.test(aphasiker$Lex_Dec, "pnorm", mean=mean(aphasiker$Lex_Dec, na.rm=T),
         sd=sd(aphasiker$Lex_Dec, na.rm=T))

mean(aphasiker$Lex_Dec)
mean(aphasiker$Lex_Dec, na.rm=T)

Lex_Dec_new <- na.omit(aphasiker$Lex_Dec)
ks.test(Lex_Dec_new, "pnorm", mean=mean(Lex_Dec_new), sd=sd(Lex_Dec_new))

hist(aphasiker$Lex_Dec, freq=F)
lines(density(aphasiker$Lex_Dec, na.rm=T))
curve(dnorm(x, mean=mean(aphasiker$Lex_Dec, na.rm=T), sd=sd(aphasiker$Lex_Dec, na.rm=T)),
      col="darkblue", lwd=2, add=TRUE)

aphasiker_BW <- subset(aphasiker, Aphasie == "B" | Aphasie == "W")
aphasiker_BW <- aphasiker[aphasiker$Aphasie == "W" | aphasiker$Aphasie == "B", ]
dim(aphasiker)
dim(aphasiker_BW)

t.test(aphasiker_BW$Lex_Dec ~ aphasiker_BW$Aphasie)

aphasiker_B <- subset(aphasiker, Aphasie == "B")
aphasiker_W <- subset(aphasiker, Aphasie == "W")
t.test(aphasiker_B$Lex_Dec, aphasiker_W$Lex_Dec)

shapiro.test(aphasiker[aphasiker$Aphasie == "B", 14])
shapiro.test(aphasiker[aphasiker$Aphasie == "W", 14])

var.test(aphasiker_BW$Lex_Dec ~ aphasiker_BW$Aphasie)
t.test(aphasiker_BW$Lex_Dec ~ aphasiker_BW$Aphasie, var.equal=TRUE)

# One-sided t-tests: greater or less
t.test(aphasiker_BW$Lex_Dec ~ aphasiker_BW$Aphasie, alternative="greater")  
t.test(aphasiker_BW$Lex_Dec ~ aphasiker_BW$Aphasie, alternative="less") 

# Test against given mean value
t.test(aphasiker$Lex_Dec, mu=1200) 

# Paired t-test. Note that the formula notation doesn't make much sense here
t.test(aphasiker_BW$Syntax, aphasiker_BW$Wortfindung, paired=TRUE)

wilcox.test(aphasiker_BW$Lex_Dec ~ aphasiker_BW$Aphasie)

library(dplyr)

aphasiker <- tbl_df(aphasiker)

aphasiker

class(aphasiker)

aphasiker[aphasiker$Aphasie == "B" & aphasiker$Alter >= 50, ]

filter(aphasiker, Aphasie == "B", Alter >= 50) # "&" for AND would also work

select(aphasiker, Patienten_ID, Aphasie, Lex_Dec)

select(aphasiker, Patienten_ID:Artikulation)

select(aphasiker, Patienten_ID, Aphasie, contains("Wert"))

arrange(aphasiker, Lex_Dec)

arrange(aphasiker, -Artikulation, -Syntax, -Wortfindung)

arrange(filter(select(aphasiker, Patienten_ID, Aphasie, Lex_Dec), Lex_Dec > 1600), -Lex_Dec)

aphasiker %>%
    select(Patienten_ID, Aphasie, Lex_Dec) %>%
    filter(Lex_Dec > 1600) %>%
    arrange(-Lex_Dec)

aphasiker$Lex_Dec_2 <- round(aphasiker$Lex_Dec, 0)

aphasiker <- mutate(aphasiker, Lex_Dec_2 = round(Lex_Dec, 0))

aphasiker_grouped <- group_by(aphasiker, Aphasie)

summarise(aphasiker_grouped, avg_Lex_Dec = mean(Lex_Dec, na.rm=T))

summarise(aphasiker_grouped, 
          avg_Lex_Dec = mean(Lex_Dec, na.rm=T),
          sd_Lex_Dec = sd(Lex_Dec, na.rm=T),
          count = n(),
          distinct_Syntax = n_distinct(Syntax)
         )

n_distinct(aphasiker$Aphasie)

sample_frac(aphasiker_grouped, 0.2)

fit <- aov(aphasiker$Lex_Dec ~ aphasiker$Aphasie)
fit

summary(fit)

fit2 <- aov(aphasiker$Lex_Dec ~ aphasiker$Aphasie + aphasiker$Geschlecht)
summary(fit2)

fit3 <- aov(aphasiker$Lex_Dec ~ aphasiker$Aphasie + aphasiker$Geschlecht +
              aphasiker$Aphasie:aphasiker$Geschlecht)
summary(fit3)

# Short form version
fit3 <- aov(aphasiker$Lex_Dec ~ aphasiker$Aphasie*aphasiker$Geschlecht)
summary(fit3)

#install.packages("car")
library(car)

leveneTest(aphasiker$Lex_Dec ~ aphasiker$Aphasie*aphasiker$Geschlecht)

oneway.test(aphasiker$Lex_Dec ~ aphasiker$Aphasie + aphasiker$Geschlecht)

cor(aphasiker$Alter, aphasiker$Lex_Dec, use="complete.obs")

plot(aphasiker$Alter, aphasiker$Lex_Dec)

# Test 1: Pearson's cor
cor.test(aphasiker$Alter, aphasiker$Lex_Dec)

# Test 2: Spearman's rho
cor.test(aphasiker$Alter, aphasiker$Lex_Dec,
         method="spearman")

# Test 3: Kendall's tau
cor.test(aphasiker$Alter, aphasiker$Lex_Dec,
         method="kendall")

lm1 <- lm(Lex_Dec ~ Alter, data=aphasiker)
summary(lm1)

lm2 <- lm(Lex_Dec ~ Aphasie + Geschlecht, data=aphasiker)
summary(lm2)

str(summary(lm2))

summary(lm2)$adj.r.squared

AIC(lm1)
AIC(lm2)

lm2_res <- rstandard(lm2)

ks.test(lm2_res, "pnorm", mean=mean(lm2_res), sd=sd(lm2_res))

hist(lm2_res, breaks=30)

library(car)
vif(lm2)

plot(lm2)

fit_claims <- glm(Claim.Amount ~ Monthly.Premium.Auto + Income + Claim.Reason + Coverage, data=claims, family=Gamma(link='log'))

summary(fit_claims)

plot(fit_claims)

?glm

# Get a list of all files in input directory
input.folder <- "data/Mannobi/"
files <- list.files(input.folder)
files <- paste(input.folder, files, sep = "")

# Initialize a data frame of mean intensities with 1 row
mean.intensities <- data.frame(1)
# Loop over all files
for (file in files) {
  # Read each file, skipping the first 18 lines
  read.file <- read.table(file, skip = 18)
  # Assume that observations of 999.999 mean that the sensor was oversaturated
  read.file$V2[read.file$V2 == 999.999] <- NA
  # Construct a file name to save to by concatenating ".png" to the end of the
  # text file's name
  filename <- paste(file, ".png", sep = "")
  # Open the file as a PNG so you can save the plot to it
#  png(filename)
  # Create the plot from the two columns in the table we read
  plot(read.file$V1, read.file$V2,
    type = "l",  # Plot the observations as a line
    xlab = "Nanometers",  # Label X axis
    ylab = "Intensity")  # Label Y axis
  dev.off()  # Save and close plot file
  # Record mean intensities to the data frame
  mean.intensities[[file]] <- mean(read.file$V2, na.rm = TRUE)
}

# Clean up outputs
mean.intensities <- mean.intensities[-1]
mean.intensities <- t(mean.intensities)
mean.intensities <- data.frame(source.file = rownames(mean.intensities), 
                               mean.intensities = mean.intensities, 
                               row.names = NULL)
# Write to disk
write.csv(mean.intensities, "mean_intensities.csv")

library("ggplot2")

p <- ggplot(data=mtcars, aes(factor(cyl), mpg)) + geom_boxplot(aes(fill = cyl))
p

ggplot(claims, aes(x=Coverage)) +
  geom_bar(stat="count") 

ggplot(claims, aes(x=Coverage, fill=Education)) +
  geom_bar(stat="count")

ggplot(claims, aes(x=Coverage, fill=Education)) +
  geom_bar(position="dodge", stat="count")

ggplot(claims, aes(x=Vehicle.Class, fill=Education)) +
  geom_bar(stat="count", width = 0.9) + 
  coord_polar(theta = "y") 

ggplot(claims, aes(x=Claim.Amount)) +
  geom_histogram()

ggplot(claims, aes(x=Claim.Amount)) +
  geom_histogram(stat="bin", binwidth=100)

ggplot(claims, aes(x=Monthly.Premium.Auto, y=Claim.Amount)) +
  geom_point() 

ggplot(claims, aes(x=Monthly.Premium.Auto, y=Claim.Amount)) +
  geom_point(color="darkblue", size=0.5, alpha=0.3) +
  theme_bw() +
  labs(title="Claim Analyses", x="Monthly Premium", y="Claim Amount")

ggplot(claims, aes(x=Monthly.Premium.Auto, y=Claim.Amount, color=Coverage)) +
  geom_point(size=0.5, alpha=0.3) +
  scale_colour_brewer(palette = "Set1") +
  labs(title="Claim Analyses", x="Monthly Premium", y="Claim Amount")

# ... facetting by coverage and adding a regression line
ggplot(claims, aes(x=Monthly.Premium.Auto, y=Claim.Amount, color=Coverage)) +
  geom_point(size=0.5,alpha=0.3) +
  facet_wrap(~Coverage) +
  geom_smooth(method="lm", color="orange", linetype=2) +
  labs(title="Claim Analyses", x="Monthly Premium", y="Claim Amount") +
  scale_colour_brewer(palette = "Set1") 

# Slow!
results <- c()
for (i in 1:10) {
    results <- c(results, i)
}
results

# Faster!
results <- rep(NA, 10)
for (i in 1:10) {
    results[i] <- i
}
results

my.variable <- "foo"
my.function <- function() {
    my.variable <<- "bar"
    cat("I just overwrote the external variable that I know nothing about!")
}
my.function()
my.variable  # What? Isn't the variable supposed to have value "foo"?
