# HMWK1
houses <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/houses.csv")
# Q1
View(houses)
model_all <- lm(Price ~ Lot.Size+Age+Land.Value+Living.Area+Pct.College+Bedrooms+Bathrooms+Rooms, data = houses)
summary(model_all)
# Q5
model_signif <- lm(Price ~ Lot.Size+Age+Land.Value+Living.Area+Bedrooms+Bathrooms+Rooms, data = houses)
model_insignif <- lm(Price ~ Pct.College, data = houses)
summary(model_signif)
summary(model_insignif)
# Q10
model_LandLiveBath <- lm(Price ~ Land.Value+Living.Area+Bathrooms, data = houses)
summary(model_LandLiveBath)
# Q11
plot(predict(model_LandLiveBath), resid(model_LandLiveBath))
qqnorm(resid(model_LandLiveBath), main='')
hist(resid(model_LandLiveBath))
# Q12
predict.lm(model_LandLiveBath, list(Land.Value=20000, Living.Area=1800, Bathrooms=1))
predict.lm(model_LandLiveBath, list(Land.Value=20000, Living.Area=1800, Bathrooms=2))
203796.2-176693
# Q14
confint(model_LandLiveBath)
# Q16
predict.lm(model_LandLiveBath, list(Land.Value=21200, Living.Area=1416, Bathrooms=1.5))
164737.2-122900

# HMWK 2
state <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/state.csv", header=T)
View(state)
mod1 <- lm(Income ~ Illiteracy+Murder, data = state)
summary(mod1)
mod2 <- lm(Income ~ Illiteracy+Murder+Illiteracy*Murder, data = state)
summary(mod2)
# Q4
146.82/117.10
# Q5
3822.61+(617.34*4)+(146.82*1)-(117.1*1*4)
3822.61+(617.34*4)+(146.82*6)-(117.1*6*4)
4362.49-5970.39
predict.lm(mod2, list(Illiteracy=4, Murder=6)) - predict.lm(mod2, list(Illiteracy=4, Murder=1))
predict.lm(mod2, list(Illiteracy=1, Murder=6)) - predict.lm(mod2, list(Illiteracy=1, Murder=1))
# Q8
predict.lm(mod2, list(Illiteracy=4, Murder=10), interval="prediction")
# Q9
boxplot(Income~Size, data=state)
# Q10
mod3 <- lm(Income ~ Size, data = state)
summary(mod3)
anova(mod3)
# Q12
mod4 <- lm(Income~ Size+Population+Size*Population, data = state)
summary(mod4)
5.668e-02+-1.919e-03
1.134e-01+-1.919e-03
anova(mod4)

# HMWK3
monet <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/monet.csv")
View(monet)
# Q1
model1 <- lm(PRICE ~ HEIGHT+WIDTH+SIGNED+HOUSE, data = monet)
summary(model1)
anova(model1) # House is insignificant so remove it
model2 <- lm(PRICE ~ HEIGHT+WIDTH+SIGNED, data = monet)
summary(model2)
plot(predict(model2), residuals(model2))
qqnorm(resid(model2), main='')
# Q2
model3 <- lm(log(PRICE) ~ HEIGHT+WIDTH+SIGNED, data = monet)
plot(predict(model3), residuals(model3))
# Q4
summary(model3)
predict.lm(model3, list(HEIGHT=20, WIDTH=30, SIGNED=1), interval = "prediction")
exp(-1.798774)
predict.lm(model3, list(HEIGHT=20, WIDTH=30, SIGNED=0), interval = "confidence")
predict.lm(model3, list(HEIGHT=20, WIDTH=30, SIGNED=1), interval = "confidence")
exp(-1.326609) - exp(0.1835512)
# Q5
model4 <- lm(log(PRICE) ~ HEIGHT+WIDTH+SIGNED+HEIGHT*WIDTH, data = monet)
summary(model4)
anova(model4)
# Q6
bollywood <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/bollywood_boxoffice.csv")
View(bollywood)
mod5 <- lm(Gross ~ Budget, data = bollywood)
predict.lm(mod5, list(Budget=20))
plot(predict(mod5), resid(mod5))
mod6 <- lm(log(Gross) ~ Budget, data = bollywood)
summary(mod6)
exp(0.030084)
plot(predict(mod6), resid(mod6))
mod7 <- lm(log(Gross) ~ log(Budget), data = bollywood)
mod8 <- lm(log(Gross) ~ sqrt(Budget), data = bollywood)
plot(predict(mod7), resid(mod7))
plot(predict(mod8), resid(mod8))
summary(mod7)
summary(mod8)

# HMWK 4
calschool <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/calschool.csv")
# Q1
View(calschool)
boxplot(calschool$testscr)
calschool$testscr[300] = calschool$testscr[300]/100
calschool$testscr[271] = calschool$testscr[271]/100
calschool$testscr[134] = calschool$testscr[134]/100
calschool$testscr[98] = calschool$testscr[98]/100
mean(calschool$testscr)
# Q2
calschool[is.na(calschool$expnstu),]
mean(calschool$expnstu, na.rm = TRUE)
# Q3
model_exp <- lm(expnstu ~ grspan+enrltot+teachers+calwpct+mealpct+computer+compstu+str+avginc+elpct+testscr, data = calschool)
predict.lm(model_exp, list(grspan='KK-08', enrltot=195, teachers=10.9, calwpct=0.5102, mealpct=2.0408, computer=67, compstu=0.3435898, str=17.88991, avginc=22.69, elpct=0, testscr=690.8))
View(calschool)
# Q4
calschool$expnstu[1] = 5820.78
model_all <- lm(testscr ~ grspan+enrltot+teachers+calwpct+mealpct+computer+compstu+expnstu+str+avginc+elpct, data = calschool)
summary(model_all)
AIC(model_all)
# Q5
null <- lm(testscr ~ 1, data = calschool)
full <- lm(testscr ~ grspan+enrltot+teachers+calwpct+mealpct+computer+compstu+expnstu+str+avginc+elpct, data = calschool)
forward.model <- step(null, scope = list(lower=null, upper=full), direction = "forward") 
summary(forward.model)
AIC(forward.model)
# Q6
backward.model <- step(full, scope = list(lower=null, upper=full), direction = "backward")
summary(backward.model)
AIC(backward.model)
# Q7
model.manual <- lm(testscr ~ grspan+mealpct+expnstu+avginc+elpct, data = calschool)
summary(model.manual)
AIC(model.manual)
# Q8
library(leaps)
regsubsets.output <- regsubsets(testscr ~ grspan+mealpct+expnstu+avginc+elpct, data = calschool)
plot(regsubsets.output, scale = 'bic')
# Q9
cor(calschool[4:13], calschool[4:13])
cor(calschool[,4:13])
install.packages('car')
library(car)
vif(model_all)
