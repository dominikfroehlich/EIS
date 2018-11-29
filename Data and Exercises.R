###### SETUP
#install.packages("truncnorm")
#install.packages("MASS")

#options(digits=3)


# for (i in 10:12){

# generateDataAnalysis(88)
# 
# generateDataAnalysis <- function(seed) {
    seed <- 199#i
    set.seed(seed)
    N <- round(runif(n = 1, min=65, max=705),0)

    require(truncnorm)
    require(psych)
    require(haven)
    require(MASS) # for Chi Square, table()
    
    MOT1 <- round(rtruncnorm(n= N, a=0, b=5, mean=runif(n = 1, 2, 4), sd=runif(n = 1, 0.2, 2.2)), 0)
    MOT2 <- round(rtruncnorm(n= N, a=0, b=5, mean=runif(n = 1, 2, 4), sd=runif(n = 1, 0.2, 2.2)), 0)
    MOT3 <- round(rtruncnorm(n= N, a=0, b=5, mean=runif(n = 1, 2, 4), sd=runif(n = 1, 0.2, 2.2)), 0)
    MOT <- rowMeans(x = data.frame(MOT1, MOT2, MOT3))
    
    SEX <- rbinom(n = N, 1, .3)+1 #binominale Variable--z.B. Geschlecht
    
    EX1 <- round(rtruncnorm(n= N, a=0, b=5, mean=runif(n = 1, 4, 8), runif(n = 1, 1.2, 3.2)), 0)
    EX2 <- round(rtruncnorm(n= N, a=0, b=5, mean=runif(n = 1, 2, 5), runif(n = 1, 1.2, 3.2)), 0)
    EX <- rowMeans(x = data.frame(EX1, EX2))
    
    GROUP <- round(rtruncnorm(n= N, a=1, b=4, mean=2, sd=1.5), 0)
    
    #Build Model
    b1 <- runif(1, min = 0.1, max = 1.2) + .9*rnorm(N)
    b2 <- runif(1, min = 0.2, max = 1.3) + .2*rnorm(N)
    b3 <- (1 + .3*rnorm(N))*-1
    b4 <- runif(1, min = 0.6, max = 1.5) + .6*rnorm(N)
    
    PERF.raw <- 3 +
      MOT * b1 +
      SEX * b2 +
      EX * b3 +
      GROUP * b4 + 
      .3*rnorm(N)
   PERF <- round(PERF.raw, 2)
    
    
    PERF.max <- describe(PERF)$max
    PERF <- round(PERF/PERF.max*100,2)
    
    GROUP[GROUP == 4] <- 0
    Grp <- dummy.code(GROUP)
    
    
    
    data <- data.frame(SEX, GROUP, MOT1, MOT2, MOT3, MOT, EX1, EX2, EX, Grp, PERF)
    data$GROUP2[GROUP==0 | GROUP==2] <- 1 
    data$GROUP2[GROUP==1 | GROUP==3] <- 2 
    write.csv2(x = data[, c(1:5, 7:8, 14)], file = paste0("Data DE ", seed, ".csv"))
    write.csv(x = data[, c(1:5, 7:8, 14)], file = paste0("Data EN ", seed, ".csv"))
    haven::write_sav(data[, c(1:5, 7:8, 14)], paste0("Data EN ", seed, ".sav"))


sink(paste0("Data ", seed, " Results.txt"))

data$SEX <- as.factor(data$SEX)

cat("t-Test (ungepaart)")
print("Gibt es einen Unterschied in PERF basierend auf SEX?")

#Shapiro-Wilk's method is widely recommended for normality test and it provides better power than K-S. It is based on the correlation between the data and the corresponding normal scores.
normalityCheck <- function(data){
  print("Test for Normality (Shapiro)")
  p <- shapiro.test(data)$p.value
  print("p-Wert:")
  print(p)
  print("Folgerung:")
  if(p>0.05){print("normality confirmed!")}else{print("normality NOT confirmed!")}
}

require(car)

homogeneityCheck <- function(var, group){
  print("Test homogeneity of variance")
   p <- car::leveneTest(var ~ as.factor(group))$Pr[1]
   print("p-Wert:")
   print(p)
  print("Folgerung:")
  if(p>0.05){print("homogeneity confirmed!")}else{print("homogeneity NOT confirmed!")}
}


normalityCheck(data$PERF[data$SEX=="1"])
normalityCheck(data$PERF[data$SEX=="2"])
homogeneityCheck(data$PERF,data$SEX)

print("Test OHNE homogene varianzen angenommen")
tt <- t.test(x = data$PERF[data$SEX=="1"], y = data$PERF[data$SEX=="2"], paired = FALSE)

print("Teststatistik")
tt$statistic
p <- tt$p.value
print("p-Wert")
print(p)
print("Folgerung:")
if(p>0.05){print("Eher kein Unterschied in Population, H0 beibehalten")}else{print("Eher ein Unterschied in Population, H0 verwerfen")}

print("U test")
p <- wilcox.test(data$PERF~data$SEX)$p.value 
print("p-Wert")
print(p)
print("Folgerung:")
if(p>0.05){print("Eher kein Unterschied in Population, H0 beibehalten")}else{print("Eher ein Unterschied in Population, H0 verwerfen")}


print("ANOVA")

print("Normality")

normalityCheck(data$PERF[data$GROUP=="0"])
normalityCheck(data$PERF[data$GROUP=="1"])
normalityCheck(data$PERF[data$GROUP=="2"])
normalityCheck(data$PERF[data$GROUP=="3"])

homogeneityCheck(data$PERF, data$GROUP)

print("Gibt es einen Unterschied in PERF basierend auf GROUP?")
anova <- summary(aov(formula = PERF ~ GROUP, data = data))
print("F value")
anova[[1]]$"F value"[1]
print("p-Value")
p <- anova[[1]]$"Pr(>F)"[1]
print(p)
if(p>0.05){print("Eher kein Unterschied in Population, H0 beibehalten")}else{print("Eher ein Unterschied in Population, H0 verwerfen")}


# print("Chi Square")
# print("Gibt es einen Zusammenhang zwischen SEX und GROUP? Teste auch für GROUP2 wobei hier die Gruppen 0 und 3 bzw. 1 und 2 zusammengefasst werden.")
# (tbl <- table(data$SEX, data$GROUP))
# chisq.test(tbl)
# 
# (tbl2 <- table(data$SEX, data$GROUP2))
# chisq.test(tbl2)

print("Correlation")
print("Gibt es einen Zusammenhang zwischen MOT, EX und PERF? Teste sowohl nach Pearson als auch nach Spearman.")
corr.test(x = data[, c("MOT", "EX", "PERF")], method = "pearson")
corr.test(x = data[, c("MOT", "EX", "PERF")], method = "spearman")

print("Regression")
print("Gibt es einen Zusammenhang zwischen PERF (AV) und MOT, SEX, EX, GROUP (UV, GROUP als dummy!)")
summary(lm(formula = PERF ~ MOT + SEX + EX +Grp, data = data))

sink(NULL)


# }
