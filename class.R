Atonic object

characters(string)
integers
double(real number)
boolean

vectors are the simplest non-atomic objects type in R

They consist of the antomic objects of the same type
x = "dadsf"
typeof(x)
y = 26
typeof(y)

x = c(1,2,3,4,5)
x[-3]

typeof(x)

x = c(2,3,-5,7,-2)
x[x > 0]

table(x>0)
2 %in% x
11 %in% x

y = c(0,-9,1)
x +y
x*y

x = c(1,2,0)
y = c(-3,1,7)

x %*% y
multiply and sum
x - y

matrix(data, nrow, ncol, byrow = F)

matrix(data = c("me","z"))
matrix(data = 1:6)

matrix(1:6, nrow = 2)
matrix(1:6, nrow = 2, byrow =T)

a <- c(9:105)
b = a[a %% 2 == 1]
length(b)
a[a < 95]

x = seq(from = 9, to = 105, by = 3)
foo = vector("integer", length = 10)

z = "everybody"
typeof(z)
length(z)
nchar(z)

x1 = c("me", "you", "everybody")
length(x1)
nchar(x1)

x = c(1,2,3)
y = c(4,5,6)

M1 = rbind(x,y)
M2 = cbind(x,y)

typeof(M1)

rownames(M1)
colnames(M1)

M1[ ,1]
M1[1, ]
M2[ ,1]
M2[1, ]
M2[x]

dim(M1)
dim(M2)
M2[x] + M2[y]
M2[x]*M2[y]
M1%*%M2
M2%*%M1

t = M2%*%M1
str(t)
rowSums(M1)
colSums(M2)

x = c('first', 2.3)
nchar(x)

y = c(7, F, TRUE)
typeof(y)

z = as.numeric(y)
z1 = as.logical(z)

typeof(z1)

y = c(z, -2.3, 1.4, 8)
z2 = as.logical(y)

x1 = list(17,7L, 17., c('foo', 22, F), c(7, "two"), 2-3i)

x1[[1]]

str(x1[1])

NA = not available
NaN : not a number
Inf : infinity

is.finite(x)
is.na(x)
is.nan(x)
is.finite(x)
table(is.na(X))

1+2 == 3
0.1+0.2==.3
x = 0.3
y = .1 + .2
x == y

format(y, digits = 18)

abs(x - y) < 0.001

tail(iris)
head(iris , 3)
str(iris)
colnames(iris)
unique(iris[, "Species"])
dim(iris)
nrow(iris)
ncol(iris)
length(iris)

table(iris$Species)


length(unique(iris$Sepal.Length))
head(iris[ ,1:3])
head(iris[ ,c(1,4)])
iris[iris[ ,5] == 'versicolor', ]

data('iris')
attach(iris)
head(Species)

head(iris[,c(1,3)])
head(iris[c(1,3)])
head(iris[,c('Sepal.Length', 'Petal.Length')])

head(iris[iris$Species == 'versicolor', ])
head(iris[(iris$Species == 'versicolor')&(iris$Sepal.Length <=7),])
head(iris[(iris$Species == 'versicolor')|(iris$Sepal.Length <=7),])

x = 3
y = -1
if(x == y) {
  print('x and y is equal')
} else {
  print(' x and y is not equal')
}

if(y>0){
  cat(sprintf('%d is a positive number. \n', y))
} else if (y<0){
  cat(sprintf('%d is a negative number. \n', y))
} else{
  cat(sprintf('%d is neither a positive nor negative number. \n', y))
}

for(i in 1:5){
  print(i)
}

#set.seed(123)
y = sample(-10:10, 5, replace = T)

for(i in 1 : length(y)){
  if(y[i]>0){
    cat(sprintf('%d is a positive number. \n', y[i]))
  } else if (y[i]<0){
    cat(sprintf('%d is a negative number. \n', y[i]))
  } else{
    cat(sprintf('%d is neither a positive nor negative number. \n', y[i]))
  }
  }
}

source("~/. active-rstudio-document", echo = T)

i = 5

bla = 3
myfun <- function(x){
  x^2 + bla
}
myfun(2)

while(i > 0){
  print(i)
  i = i-1
}

iris[(iris$Species == 'setosa')&(iris$Sepal.Length >= 6), ]

for(i in 1:100){
  if(i %% 2 == 0){
    cat(sprintf('%d is a even \n', i))
  } else{
    cat(sprintf('%d is a odd \n', i))
  }
}

for(i in 1:20){
  if((i %% 3 == 0)&(i %% 5 == 0)){
    print("FizzBUZZ")
  } else if(i %% 5 == 0){
    print("Buzz")
  } else if(i %% 3 == 0) {
    print("Fizz")
  } else{
    print(i)
  }
}

sum_n <- function(x){
  j = 0
  for(i in 1 : x){
    j = j+i
  }
  return (j)
}

sum_n(5)

factorial <- function(x){
  i = 1
  while(x != 1){
    i = x*i
    x = x-1
  }
  return (i)
}

factorial(5)

x = seq(-10,10, by = 0.1)
y1 = sin(x)
y2 = cos(x)
plot(x , y1, type = 'l', col = 'blue')
lines(x, y2, col = 'red', lty = 2)
abline(a =0, b = 0.1, col = 'darkgreen')
abline(v = 2, col = 'magenta', lwd = 4, lty = 3)
points(x = seq(-10,10,by = 1), y = exp(seq(-10,10,by = 1)), col = 'cyan', pch = 4)

data("mtcars")
head(mtcars)
str(mtcars)
hist(mtcars$mpg, breaks = 10, col = 'red')

#set.seed(123)
x = rnorm(n = 10000000, mean = 0, sd = 1)
hist(x, breaks = 20, xlim = c(-4,4), col = 'blue')

t = (-400:400)/100
set.seed(12)
par(mfrow = c(2,2))
for(n in c(100,500,1000,1500)){
  x = rnorm(n = n, mean = 0, sd = 1)
  hist(x, breaks = 50, freq = F, main = paste('n=', n), xlim = c(-4,4), ylim = c(0,0.8))

  lines(t, dnorm(t, 0.1), lwd = 2, col = 'blue')
}

mean(mpg)
var(mpg)
sd(mpg)
x = mtcars$mpg
hist(x, freq = F, col = 'red', xlab = NULL, ylab = NULL, main = NULL)
dsty = density(mtcars$mpg, kernel = 'gaussian')
lines(dsty$x, dsty$y, type = 'l', lwd = 2)
str(dsty)
air = airquality
unique(air$Month)
par(mfrow = c(2,1), mai = c(0.5,.6,.5,.6))
hist(air$Solar.R, xlim = c(0,350))
boxplot(air$Solar.R, horizontal = T, main = 'adsf', ylim = c(0,350), axes = F)
axis(1)
median(air$Solar.R, na.rm = T)
mean(air$Solar.R, na.rm = T)
quantile(air$Solar.R, probs = seq(0,1,by = .25), na.rm = T)

count = as.vector(table(mtcars$cyl))
barplot(mtcars$cyl)
xx = barplot(count, main = 'asdf', xlab = 'asdf', col = c('deeppink', 'darkblue'), ylim = c(0,max(count) + 3), legend = rownames(count), xlim = c(0,4.5))

pie(count)

text(x = xx, y = count, labels = as.character(count), col = 'darkblue')

lbls = c('4','6','8')
pct = round(count/sum(count)*100)
lbls = paste(lbls, pct, sep = ':')
lbls = paste(lbls, '%', sep = "")
pie(count, labels = lbls, col = rainbow(length(lbls)))

pie3D(count, labels = lbls, col = rainbow(length(lbls)))

#apply, lapply, sapply, mapply, tapply
m = matrix(1:9, nrow = 3, byrow = T)
apply(X = m, MARGIN = 1, FUN = sum)

colMeans()
rnorm()
apply(x, 1, quantile, probs = c(0.25, 0.75))
a = array(sample(2*4*3, replace = T), dim = c(2,4,3))

apply(a, MARGIN = c(1,2), sum)

install.packages('plotrix')
library(plotrix)

lapply(list apply)
set.seed(1234)
x = list(a = 1:4, b = rnorm(10))
x

lapply(x, mean)
x = 1:5
lapply(x, runif)
lapply(x, runif, min=0, max=10)

x = list(a = matrix(1:4, 2,2), b = matrix(1:6,3,2))
lapply(x, function(bla){
  bla[ ,1]
})

sapply(simply apply)

x = list(a = 1:4, b = rnorm(10), c = rnorm(20,1), d = rnorm(100,5))

lapply(x, mean) -> list
sapply(x, mean) -> vector

mapply(rep, 1:4, 4:1)
mapply(rep, 1:100,100:1)

tapply -> giving subdataframe by grouping

df = data.frame(c(7,1,3,4,2,17), c('feb', 'apr', 'may', 'feb', 'apr', 'apr'))
colnames(df) = c('hours', 'month')
tapply(df$hours, df$month, sum)

split -> giving subdataframe
x = c(rnorm)
x = c(rnorm(10), runif(10), rnorm(10,1))

lvs = gl(3,10,labels = c('a', 'b', 'c'))
split(x, lvs)
lapply(split(x, lvs), mean)

head(airquality)
unique(airquality$Month)
lapply(split(airquality, airquality$Month), function(x) colMeans(x[, c(1,2,3)]))

       
