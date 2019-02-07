# Абра кадабра
# 
# 

2 + 2


sqrt(9)

"это текст"

-1 / 2 #Просто деление 

(88 * 2 / 44)^2 #Прмер со скобками

sqrt(100/50*2) #используем функцию


#Работа с переменными

Gala <- (88 * 2 / 44)^2 

MarinA <- sqrt(100/50*2)

Tanya <- MarinA + Gala

Renata <- (sqrt(Tanya))^Gala

#Работа с векторами

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

#Пользовательские функции

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



#Датафреймы

install.packages("MASS")

library(MASS)





