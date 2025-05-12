library(tidyverse)
library(ggplot2)
library(olsrr)
library(dplyr)
library(lmtest)
library(corrplot)

#Cleaning Data
mlb1 <- read.csv("/Users/uzairahmed/Downloads/mlb1.csv")
mlb2 <- read.csv("/Users/uzairahmed/Downloads/mlb2.csv")
mlb3 <- merge(mlb1, mlb2, by = "Name")
mlb3 <- mlb3 %>% rename(K = K.) 
mlb3 <- mlb3 %>% rename(BB = BB.)
mlb3 <- mlb3 %>% rename(OSwing = O.Swing.)
mlb3 <- mlb3 %>% rename(ZSwing = Z.Swing.)
mlb3 <- mlb3 %>% rename(Contact = Contact.)
mlb3 <- mlb3 %>% rename(Zone = Zone.)
mlb3 <- mlb3 %>% rename(SwStr = SwStr.)
mlb3$BB <- as.numeric(gsub("%", "", mlb3$BB))
mlb3$K  <- as.numeric(gsub("%", "", mlb3$K))
mlb3$OSwing <- as.numeric(gsub("%", "", mlb3$OSwing))
mlb3$ZSwing <- as.numeric(gsub("%", "", mlb3$ZSwing))
mlb3$Contact <- as.numeric(gsub("%", "", mlb3$Contact))
mlb3$Zone <- as.numeric(gsub("%", "", mlb3$Zone))
mlb3$SwStr <- as.numeric(gsub("%", "", mlb3$SwStr))

mlb3 <- mlb3 %>%
  mutate(
    BB = BB / 100,
    K = K / 100,
    OSwing = OSwing / 100,
    ZSwing = ZSwing / 100,
    Contact = Contact / 100,
    Zone = Zone / 100,
    SwStr = SwStr / 100
  )

#Statistical Summaries
ggplot(mlb3, aes(x=K)) +
  geom_histogram(binwidth=0.01, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player SO% 2024", x="SO%", y="Frequency")
summary(mlb3$K)

ggplot(mlb3, aes(x=SB)) +
  geom_histogram(binwidth=3, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player Stolen Bases 2024", x="SB", y="Frequency")
summary(mlb3$SB)

ggplot(mlb3, aes(x=OBP)) +
  geom_histogram(binwidth=0.01, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player OBP 2024", x="OBP", y="Frequency")
summary(mlb3$OBP)

ggplot(mlb3, aes(x=SLG)) +
  geom_histogram(binwidth=0.02, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player SLG% 2024", x="SLG", y="Frequency")
summary(mlb3$SLG)

ggplot(mlb3, aes(x=OSwing)) +
  geom_histogram(binwidth=0.01, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player OSwing% 2024", x="OSwing%", y="Frequency")
summary(mlb3$OSwing)

ggplot(mlb3, aes(x=ZSwing)) +
  geom_histogram(binwidth=0.01, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player ZSwing% 2024", x="ZSwing%", y="Frequency")
summary(mlb3$ZSwing)

ggplot(mlb3, aes(x=Zone)) +
  geom_histogram(binwidth=0.01, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player Zone% 2024", x="Zone%", y="Frequency")
summary(mlb3$Zone)

ggplot(mlb3, aes(x=SwStr)) +
  geom_histogram(binwidth=0.01, fill="skyblue", color="black", alpha=0.7) +
  labs(title="MLB Player SwStr% 2024", x="SwStr%", y="Frequency")
summary(mlb3$SwStr)

#SLR Relationships
m1 <- lm(K~SB, data=mlb3)
summary(m1)

m2 <- lm(K~OBP, data=mlb3)
summary(m2)

m3 <- lm(K~SLG, data=mlb3)
summary(m3)

m4 <- lm(K~OSwing, data=mlb3)
summary(m4)

m5 <- lm(K~ZSwing, data=mlb3)
summary(m5)

m6 <- lm(K~Zone, data=mlb3)
summary(m6)

m7 <- lm(K~SwStr, data=mlb3)
summary(m7)

m8 <- lm(K~Position, data=mlb3)
summary(m8)

ggplot(mlb3, aes(x = OBP, y = K)) +
  geom_point() +
  labs(title = "Strikeout Rate vs OBP", x = "OBP", y = "SO Rate") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(mlb3, aes(x = Zone, y = K)) +
  geom_point() +
  labs(title = "Strikeout Rate vs Zone%", x = "Zone%", y = "SO Rate") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red")


ggplot(mlb3, aes(x = SwStr, y = K)) +
  geom_point() +
  labs(title = "Strikeout Rate vs SwStr%", x = "SwStr%", y = "SO Rate") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red")

#MLR Relationships
mlb_fit <- lm(K ~ SB + OBP + SLG + OSwing + ZSwing + Zone + SwStr + Position, data = mlb3)
summary(mlb_fit)
ols_vif_tol(mlb_fit)

ols_step_best_subset(mlb_fit)

mlb_optimal <- lm(K ~ OBP + SLG + OSwing + ZSwing + SwStr, data = mlb3)
summary(mlb_optimal)
ols_vif_tol(mlb_optimal)

scatter.smooth(mlb_optimal$fitted.values, mlb_optimal$residuals,
               main="Residuals vs Fitted",
               xlab="Fitted Values", ylab="Residuals")
abline(h = 0, col = "red")

qqnorm(mlb_optimal$residuals)
qqline(mlb_optimal$residuals, col = "red")

shapiro.test(residuals(mlb_optimal))
bptest(mlb_optimal)

MSE <- summary(mlb_optimal)$sigma^2
outlier_check <- round(data.frame(Residuals=mlb_optimal$residuals,
                                  "Standardized Res"=mlb_optimal$residuals/sqrt(MSE),
                                  "Studentized Res"=rstandard(mlb_optimal),
                                  "Press"=rstandard(mlb_optimal,type='predictive'),
                                  "R-student"=rstudent(mlb_optimal),
                                  "Hat-Values"=hatvalues(mlb_optimal)),2)
outlier_check


influence <- round(data.frame(Cooks=cooks.distance(mlb_optimal),
                              dffits=dffits(mlb_optimal),
                              dfbeta=dfbetas(mlb_optimal),
                              cov_ratio=covratio(mlb_optimal)),3)
influence


mlb_3 <- mlb3 %>% 
  mutate(Cooks = cooks.distance(mlb_optimal)) %>% 
  mutate(StuRes = rstandard(mlb_optimal)) %>% 
  mutate(dffits = dffits(mlb_optimal)) %>% 
  mutate(covratio = covratio(mlb_optimal))
mlb_3


filter(mlb_3, abs(StuRes) > 2)
filter(mlb_3, Cooks > 0.031)
filter(mlb_3, Cooks > 0.031 & abs(dffits) > (2*sqrt(5/129)))
filter(mlb_3, covratio > (1 + (15/129)) | covratio < (1 - (15/129)))
filter(mlb_3, abs(dffits) > (2*sqrt(5/129)))


interact1 <- lm(K ~ OBP + SLG + OSwing + ZSwing + SwStr + SLG:ZSwing, data = mlb3)
summary(interact1)

#Testing Model
mlb3 %>% 
  sample_n(7)

players_to_test <- c("Will Smith", "Isaac Paredes", "Francisco Lindor",
                     "Brendan Rodgers", "Ryan McMahon", "Wyatt Langford", "Trea Turner")

test_data <- mlb3 %>%
  filter(Name %in% players_to_test)

test_data$predicted_K <- predict(interact1, newdata = test_data)

test_data %>%
  select(Name, K, predicted_K)