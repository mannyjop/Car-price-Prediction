setwd("I:/R Data/141")
load("vehicles.rda")
###1 How many observations are there in the data set?
car = vposts
dim(car)

### total 34677 obs
test = car[!(is.na(car$price)),]
fuck_what_it_is = test[test$price == 600030000, ]

###2 What are the names of the variables? and what is the class of each variable?
## split
names = names(car)
class = sapply(car,class)

###3 What is the average price of all the vehicles? the median price? and the deciles? Displays these on a plot of the distribution of vehicle prices.

mean(car$price, na.rm = T)
median(car$price, na.rm = T)
quantile(car$price, 0.9, na.rm = T)
hist(car$price, na.rm = T)

h = mean(car$price, na.rm = T)
i = median(car$price, na.rm = T)
j = quantile(car$price, 0.9, na.rm = T)
hist(car$price[car$price<=70000], na.rm = T)

abline(v = h, col = "blue", lwd = 2)
abline(v = i, col = "red", lwd = 2)
abline(v = j, col = "green", lwd = 2)
max(car$price, na.rm = T)

###4 What are the different categories of vehicles, i.e. the type variable/column? What is the proportion for each category ?
a = table(car$type)
prop = a/sum(a)
barplot(prop) 

###5 Display the relationship between fuel type and vehicle type. Does this depend on transmission type?
### mosaicplot  las = 1i
library(ggplot2)
ggplot(na.omit(car), aes(x=type, fill=fuel)) + geom_bar()

ggplot(na.omit(car), aes(fuel,fill = type),) + geom_bar() +
  facet_grid(. ~ transmission)

###6 How many different cities are represented in the dataset?
length(levels(car$city))

###7 Visually how the number/proportion of "for sale by owner" and "for sale by dealer" varies across city?

temp = car$byOwner
citybyo = car[temp,]$city
to = table(citybyo)

citybyd = car[!temp,]$city
td = table(citybyd)
value = rbind(as.vector(to), as.vector(td))
ylim <- c(0, 1.5*max(value))
p = barplot(value, beside = T, names = names(to), ylim = ylim)
text(x =p, y = value, label = value, pos = 3, cex = 0.8, col = "red")

###8 What is the largest price for a vehicle in this data set? Examine this and fix the value. Now examine the new highest value for price.
p = car$price
p [p == max(p,na.rm = TRUE)]=25000
p [p == max(p,na.rm = TRUE)]=3500
p [p == max(p,na.rm = TRUE)]=NA
p [p == max(p,na.rm = TRUE)]=5730
p [p == max(p,na.rm = TRUE)]=5730
p [p == max(p,na.rm = TRUE)]=NA

###9 What are the three most common makes of cars in each city for "sale by owner" and for "sale by dealer"? Are they similar or quite different?
temp = car$byOwner
citybyo = car[temp,]$city
makerbyo = car[temp,]$maker
table = table(citybyo, makerbyo)
dim(table)
makerbyo=list(sort(table[1,], decreasing = T)[1:3])

for(i in 2:7)
{
  temp = sort(table[i,], decreasing = T)[1:3]
  makerbyo = c(makerbyo, list(temp))
}
names(makerbyo)=c("boston","chicago","denver", "lasvegas", "nyc", "sac", "sfbay") 

citybyd = car[!temp,]$city
makerbyd = car[!temp,]$maker
table = table(citybyd, makerbyd)
dim(table)
makerbyd=list(sort(table[1,], decreasing = T)[1:3])

for(i in 2:7)
{
  temp = sort(table[i,], decreasing = T)[1:3]
  makerbyd = c(makerbyd, list(temp))
}
names(makerbyd)=c("boston","chicago","denver", "lasvegas", "nyc", "sac", "sfbay") 

sort(table(sapply(makerbyo, names)), decreasing = TRUE)
sort(table(sapply(makerbyd, names)), decreasing = TRUE)

###10 Visually compare the distribution of the age of cars for different cities and for "sale by owner" and "sale by dealer". Provide an interpretation of the plots, i.e., what are the key conclusions and insights?

plot(car[!car$year == 4,]$city, abs(car[!car$year == 4,]$year-2015), main = "Age for different city")
plot(subcar$year)

par(mfrow=c(1,1))
temp = car$byOwner
citybyo = car[temp,]$city
agebyo = abs(car[temp,]$year-2015)
citybyo = citybyo[-3435]
agebyo = agebyo[-3435]
plot(citybyo,agebyo, main = "Age for sale by owner")

citybyd = car[!temp,]$city
agebyd = abs(car[!temp,]$year-2015)
plot(citybyd,agebyd, main = "Age for sale by dealer")
#

###11 Plot the locations of the posts on a map? What do you notice?
## how to make it biger
library(maps)
map(regions = "USA")
points(car$long, car$lat, col = "red")

### 12 Summarize the distribution of fuel type, drive, transmission, and vehicle type. Find a good way to display this information.
library(ggplot2)
library(gridExtra)

plots = list()
 p1 = ggplot(na.omit(car[car$transmission == "automatic",]), aes(drive,fill = fuel)) + geom_bar() +
    facet_grid(. ~ type) + ggtitle("Automatic")
  plots[[1]] <- p1  

p2 = ggplot(na.omit(car[car$transmission == "manual",]), aes(drive,fill = fuel)) + geom_bar() +
  facet_grid(. ~ type) + ggtitle("manual")
plots[[2]] <- p2  

p3 = ggplot(na.omit(car[car$transmission == "other",]), aes(drive,fill = fuel)) + geom_bar() +
  facet_grid(. ~ type) + ggtitle("Other")
plots[[3]] <- p3

###13 Plot odometer reading and age of car? Is there a relationship? Similarly, plot odometer reading and price? Interpret the result(s). Are odometer reading and age of car related?
### quad regression
odoage = data.frame(cbind(car$odometer, car$year))
smoothScatter(odoage$X2[odoage$X1<=300000],odoage$X1[odoage$X1<=300000], xlab = "year", ylab = "odometer")

a = car[car$odometer <= 300000, ]
b = a[a$price <= 70000,]
smoothScatter(b$price, b$odometer, xlab = "price", ylab = "odometer")

age = a$year
odo = a$odometer
age2 = age^2
y = lm(odo~age)
anova(y)
plot(odoage$X2[odoage$X1<=300000],odoage$X1[odoage$X1<=300000], xlab = "year", ylab = "odometer")
abline(y)

###14 Identify the "old" cars. What manufacturers made these? What is the price distribution for these?.
a = car[(car$year>=1935 & car$year <=1975),]
table(a$maker)
b = a$price
b [b == max(b,na.rm = TRUE)]=NA
plot(a$year, b, xlab = 'year', ylab = 'price', main = "The price distribution")

### 15 I have omitted one important variable in this data set. What do you think it is? Can we derive this from the other variables? If so, sketch possible ideas as to how we would compute this variable.
sum(str_count(car$body, "VIN"))
###16 Display how condition and odometer are related. Also how condition and price are related. And condition and age of the car. Provide a brief interpretation of what you find.

### move out the outliers

new1 = na.omit(car)
new2 = new1[new1$odometer<=300000,]

con = new2$condition
odo = new2$odometer
i = names(table(new2$condition)[table(new2$condition)>0])
con1 = con[con==i]
con1 = factor(con1)
odo1 = odo[con==i]
plot(con1,odo1, main = "Condition VS Odometer")

new3 = new1[new1$price<=80000,]
con = new3$condition
price = new3$price
i = names(table(new3$condition)[table(new3$condition)>0])
con1 = con[con==i]
con1 = factor(con1)
price1 = price[con==i]
plot(con1,price1,  main = "Condition VS Price")

con = new1$condition
year= new1$year
i = names(table(new1$condition)[table(new1$condition)>0])
con1 = con[con==i]
con1 = factor(con1)
year1 = year[con==i]
plot(con1,abs(year1-2015),  main = "Condition VS Age")
