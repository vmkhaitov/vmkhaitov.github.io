# Абра кадабра
# 
# 

2 + 2


sqrt(9)

"это текст"

-1 / 2 #Просто деление 

(88 * 2 / 44)^2 #Прмер со скобками

sqrt(100/50*2) #используем функцию


#Работа с переменными #####

Gala <- (88 * 2 / 44)^2 

MarinA <- sqrt(100/50*2)

Tanya <- MarinA + Gala

Renata <- (sqrt(Tanya))^Gala

#Работа с векторами ######

letters
LETTERS

c("MarinA", Gala, Tanya, Renata)

numers <- c(6, 18, 9, 5, 14, 4)

which(letters == "f")

which(letters != "f")


letters[numers]


1:10

0:-10

letters[5:10]

seq(from = 5, to = 10, by = 2)

#Пользовательские функции#####

uravn <- function(a, b) -b/a



uravn <- function(a, b) {
  x <- -b/a
  cat("Ответ: x = ", x, "\n")
}

uravn(10, 3)


a = 1
b = 2
c = 4

sq_uravn <- function(a, b, c){
  D <- b^2 - 4 * a * c
  if(D < 0) cat("Корней нет")
  if(D == 0) cat("Один корень x = ", -b/(2*a))
  if(D > 0) cat("Два корня \n x1 = ", (-b + sqrt(D))/(2*a), "\n x2 = ", (-b - sqrt(D))/(2*a))
}

sq_uravn(2, 0, -1)



#Датафреймы ##########


# install.packages("MASS")

library(MASS)

data(package = "MASS")

data("birthwt")

birthwt

str(birthwt)

?birthwt

bwt <- birthwt

bwt$smoke

bwt$smoker [bwt$smoke == 1] <- "smoke" 
bwt$smoker [bwt$smoke == 0] <- "none" 

bwt$smoker <- ifelse(test = bwt$smoke == 1, yes = "smoke", no = "none")


bwt$race2 <- ifelse(test = bwt$race == 1, yes = "white", no = ifelse(test = bwt$race == 2, yes = "black", no = "other"))

bwt$race2[bwt$race == 1] <- "white"
bwt$race2[bwt$race == 2] <- "black"
bwt$race2[bwt$race == 3] <- "other"

str(bwt)


bwt$race2 <- factor(bwt$race2)
str(bwt)

bwt$smoker <- factor(bwt$smoker)
str(bwt)


## Визуализация данных #######

## Базовая графика
plot(x = bwt$age, y = bwt$bwt)


## Пакет ggplot2
library(ggplot2)

ggplot(data = bwt, mapping = aes(x = age, y = bwt))

ggplot(bwt, aes(x = age, y = bwt, color = smoker)) + 
  geom_point(aes(size = lwt, shape = race2)) + 
  labs(x = "Возраст матери", y = "Вес младенца", color = "Курение", size = "Вес матери") + 
  theme_bw() + 
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position =  "right") +
  facet_wrap( ~ race2)



ggplot(bwt, aes(x = age, y = bwt, shape = smoker)) + 
  geom_point(aes(color = lwt), size = 4) + 
  labs(x = "Возраст матери", y = "Вес младенца", color = "вес матери", size = "Вес матери") + 
  theme_bw() + 
  theme(legend.position =  "right") +
  facet_wrap( ~ race2)


#Конвейерный способ обработки данных

library("dplyr")

mean_weght <- bwt %>% group_by(age) %>% summarise(mean_bwt = mean(bwt), n = n())

ggplot(mean_weght, aes(x = age, y = mean_bwt)) + geom_line()



mean_weght <- bwt %>% group_by(age, smoker) %>% summarise(mean_bwt = mean(bwt), n = n())

ggplot(mean_weght, aes(x = age, y = mean_bwt)) + geom_line(aes(color = smoker))


ggplot(bwt, aes(x = age, y = bwt)) +  stat_summary(fun.y = mean, geom = "line") + aes(color = smoker)



ggplot(bwt, aes(x = bwt)) + geom_histogram(binwidth = 500)

ggplot(bwt, aes(x = bwt)) + stat_bin(geom = "col", binwidth = 500)


ggplot(bwt, aes(x = bwt)) + geom_histogram(bins = 10, aes(fill = smoker)) + facet_wrap(~smoker, dir = "v")


ggplot(bwt, aes(x = bwt)) + geom_histogram(bins = 10, aes(fill = smoker)) + facet_wrap(~smoker, ncol = 1)


ggplot(bwt, aes(x = bwt)) + geom_density(aes(fill = smoker), alpha = 0.5) 


# Бокс-плоты

ggplot(bwt, aes(x = smoker, y = bwt)) + geom_boxplot()


bwt %>% group_by(smoker) %>% summarise(median(bwt))





library(ggplot2)

ggplot(data = iris, aes(x = Sepal.Length , y = Petal.Length, color = Species )) + geom_point() 


ggplot(data = iris, aes(x = Species, y = Petal.Length)) + geom_boxplot()


setosa <- iris$Petal.Length[iris$Species == "setosa"]

setosa_sorted <- sort(setosa, decreasing = FALSE)

length(setosa_sorted)

median(setosa_sorted)

quantile(setosa_sorted, probs = seq(0, 1, 0.05))




credits <- read.table(file = "data/credits_sept.csv", sep = ",", header = TRUE)

ggplot(data = credits, aes(x = Discipline, y = Credit)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

ggplot(data = credits, aes(x = Discipline, y = Credit)) + geom_violin() + theme(axis.text.x = element_text(angle = 90))



ggplot(data = credits, aes(x=1, y = Credit)) + geom_violin() + theme(axis.text.x = element_text(angle = 90))


ggplot(data = credits, aes( y = Credit)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))


###################################################

#Читаем данные

cred_sept <- read.table("data/credits_sept.csv", sep = ",", header = TRUE ) 

cred_sept <- cred_sept[ , -c(5:8)]

names(cred_sept)
  
cred_sept$Month <- "Sept"


cred_oct <- read.table("data/credits_oct.csv", sep = ",", header = TRUE ) 

cred_oct$Month <- "Oct"


credit <- rbind(cred_sept, cred_oct)

gender <- read.table("data/gender.csv", sep = ";", header = TRUE)

gender <- gender[-21, ]

credit2 <- merge(credit, gender, by = "Subject" )


ggplot(credit2, aes(x = Gender, y = Credit, fill = Month)) + geom_boxplot()

ggplot(credit2, aes(x = Gender, y = Credit, fill = Month)) + geom_boxplot() + facet_wrap(~Discipline)



ggplot(credit2, aes(y = Data, x = Credit)) + geom_point()

str(credit2)

unique(credit2$Data)

inform <- credit2[credit2$Discipline == "Информатика", ]

ggplot(inform, aes(x = Credit)) + geom_histogram() 


ggplot() + geom_abline() + geom_abline(slope = 2, color = "blue") + geom_abline(color = "red", slope = 0.5) + geom_abline(slope = 2, intercept = 0.5, color = "yellow")


set.seed(1234567)
cred_rnd <- data.frame(cred = round(rnorm(1000, mean = 4, sd = 0.5),0))

ggplot(cred_rnd, aes(x = cred)) + geom_histogram() + xlim(0, 7)



sum(inform$Credit)/length(inform$Credit)


mean(inform$Credit)
median(inform$Credit)


sd(inform$Credit)

round(rnorm(100, mean = 3.5, sd = 0.9 ), 0)

cred_rnd <- data.frame(cred = round(rnorm(100, mean = 3.9, sd = 0.9 ), 0))

ggplot(cred_rnd, aes(x = cred)) + geom_histogram() + xlim(0, 7)



library(ggplot2)

elves <- data.frame(L = rnorm(1000, mean = 2.1, sd = 0.1),EL = rnorm(1000, mean = 0.1, sd = 0.001))




ggplot(elves, aes(x = L)) + geom_histogram()

ggplot(elves, aes(x = L, y = EL)) + geom_point()
 


elves_sample <- elves[sample(1:1000, size = 5), ]

mean(elves_sample$L)

means_L <- data.frame(L = rep(NA, 100))

for(i  in 1:100){
  elves_sample <- elves[sample(1:1000, size = 50), ]
  means_L$L[i] <- mean(elves_sample$L)
}
  
pl_means <- ggplot(means_L, aes(x = L)) + geom_histogram() + xlim(1.5, 2.5)
p_total <- ggplot(elves, aes(x = L)) + geom_histogram()+ xlim(1.5, 2.5)


library(gridExtra)

grid.arrange(pl_means, p_total)




elves_sample <- elves[sample(1:1000, size = 5), ]

M <- mean(elves_sample$L)
SD <- sd(elves_sample$L)

SE <- SD/sqrt(5)

M - 2*SE

M + 2*SE
 

