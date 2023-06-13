library(imager)

#Question a
data(iris)
par(mfrow = c(1,4))
boxplot(Sepal.Length ~ Species, data = iris, main="Sepal Length")
boxplot(Sepal.Width ~ Species, data = iris, main="Sepal Width")
boxplot(Petal.Length ~ Species, data = iris, main="Petal Length")
boxplot(Petal.Width ~ Species, data = iris, main="Petal Length")

plot(Sepal.Length ~ Petal.Length, data = iris, xlab = "Petal Length", ylab = "Sepal Length", col = iris$Species)
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 1)

#the species sentosa has the smallest petal and sepal length 
#while virginica has the largest petal and sepal length and versicolor lies in the middle
#ratio of sepal to petal length is quite similar across species

#Question b
flip <- function(image){
  
  col.mat <- as.array(image[, ,1, ])
  dims <- dim(col.mat)
  flipped <- array(0, dim = dims)
  for(i in 1:dims[1])
    {
      flipped[ i, , ] <- col.mat[dims[1] - i + 1, , ]
    }
  return(as.cimg(flipped))
}
dog <- load.image("dog.jpeg")
flipped_dog <- flip(dog)
dog
flipped_dog
par(mfrow = c(1,2))
plot(dog)
plot(flipped_dog)

#question c

library(MASS)
data(ships)
boxplot(incidents ~ type, data = ships, 
        xlab = "Ship Type", ylab = "Damage Incidents",
        main = "Damage Incidents by Ship Type")
#so the boxplot proves the hypothesis

#question d
library(tidyverse)
library(rvest)
html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
titles_of_q <- html %>% html_elements(".s-post-summary--content-title .s-link") %>% html_text()
views <- html %>% html_elements(".is-supernova .s-post-summary--stats-item-number") %>% html_text()
answers <- html %>% html_elements(".has-answers .s-post-summary--stats-item-number") %>% html_text()
votes <- html %>% html_elements(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text() 
statistical_data <- data.frame("Title" = titles_of_q, "Views" = views, "Answers" = answers, "Votes" = votes )
statistical_data

#question e
days <- function()
{
  tab_in_bottle = rep(1,100)
  total_days <- 0
  while(TRUE){
    total_days = total_days+1
    tablet <- sample(tab_in_bottle,1)
    if(tablet==1){
      tab_in_bottle = c(tab_in_bottle,0.5,0.5)
    }
    #tablet==0.5 then return the number of days
    else {
      return(total_days)
    }
  }
}
#can change the number of trials
numsOfTrials <- 1000
totalcount <- 0
for(i in 1:numsOfTrials){
  totalcount = totalcount + days();
}
avg = totalcount/numsOfTrials
avg
