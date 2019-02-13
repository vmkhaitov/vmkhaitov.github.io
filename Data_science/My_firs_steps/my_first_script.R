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








