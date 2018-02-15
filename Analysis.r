library(ggplot2)
library(VIM)
library(mice)
library(vcd)
require(car)
library(tabplot)
library(PerformanceAnalytics)
library(MASS)
library(glmnet)


#movies = read.csv("movie_metadata.csv")
movies = movie_metadata


summary(movies)
sapply(movies, sd)
#num_critic_for_reviews=movies[,3]
#num_critic_for_reviews=as.numeric(num_critic_for_reviews)
hist(movies$num_critic_for_reviews)
#hist(num_critic_for_reviews)


length(complete.cases(movies))
aggr(movies, numbers=T, prop=F, sortVars=T, labels=names(movies)) # change 'prop' to T to see ratio



matrixplot(movies, interactive=T, sortby="imdb_score")
#matrixplot(movies, interactive=T, sortby=NULL)


marginplot(movies[,c("gross", "imdb_score")])

md.pattern(movies) # hard to interpret due to too many variables

scattmatrixMiss(movies[, 1:5], interactive = F)

imdb_score=movies[,26]
imdb_score=as.numeric(imdb_score)
hist(movies$imdb_score, breaks=30)
hist(imdb_score)
plot(density(imdb_score))
abline(v=mean(imdb_score), lty=2)

hist(movies$title_year)

boxplot(imdb_score ~ title_year, data=movies, col="bisque", las=2)
title("IMDB score vs movie year")

#boxplot(imdb_score ~ actor_1_facebook_likes, data=movies, col="bisque", las=2)
#title("IMDB score vs actor 1 facebook likes")
formatter1000 <- function(x){ 
  x
}
plotter = movies[,c("actor_1_facebook_likes","imdb_score" )]
plotter  = na.omit(plotter)
p = ggplot(data = plotter, aes(x = imdb_score, y = actor_1_facebook_likes))
p + geom_point() + scale_y_continuous(labels = formatter1000) 

plotter2 = movies[,c("director_facebook_likes","imdb_score" )]
plotter2  = na.omit(plotter2)
p = ggplot(data = plotter2, aes(x = imdb_score, y = director_facebook_likes))
p + geom_point() #+ scale_y_continuous(labels = formatter1000) 

plotter1 = movies[,c("movie_facebook_likes","imdb_score" )]
plotter1  = na.omit(plotter1)
p = ggplot(data = plotter1, aes(x = imdb_score, y = movie_facebook_likes))
p + geom_point() + scale_y_continuous(labels = formatter1000) 


reordered_country = with(movies, reorder(country, -imdb_score, median))
boxplot(imdb_score ~ reordered_country, data=movies, lwd=0.5, col="bisque", las=2)
#stripchart(imdb_score ~ reordered_country, data=movies, vertical=T, add=T, pch=1, col='grey')
title("IMDB score vs country")

reordered_language = with(movies, reorder(language, -imdb_score, median))
boxplot(imdb_score ~ reordered_language, data=movies, lwd=0.5, col="bisque", las=2)
#stripchart(imdb_score ~ reordered_language, data=movies, vertical=T, add=T, pch=1, col='grey')
title("IMDB score vs language")

#boxplot(imdb_score ~ aspect_ratio, data=movies, lwd=0.5, col="bisque", las=2)
#stripchart(imdb_score ~ aspect_ratio, data=movies, vertical=T, add=T, pch=1, col='grey')
#title("IMDB score vs aspect ratio")

boxplot(imdb_score ~ color, data=movies, lwd=0.5, col="bisque", las=2)
stripchart(imdb_score ~ color, data=movies, vertical=T, add=T, pch=1, col='grey')
title("IMDB score vs color")

reordered_content_rating = with(movies, reorder(content_rating, -imdb_score, median))
boxplot(imdb_score ~ reordered_content_rating, data=movies, lwd=0.5, col="bisque", las=2)
stripchart(imdb_score ~ reordered_content_rating, data=movies, vertical=T, add=T, pch=1, col='grey')
title("IMDB score vs content rating")

library(googleVis)
library(dplyr)
m1 = movies %>% select(actor_1_name, actor_1_facebook_likes) %>% 
    group_by(actor_1_name) %>% summarize(appear.count=n())

m2 = left_join(movies, m1, by="actor_1_name")
m3 = m2 %>% select(actor_1_name, actor_1_facebook_likes, appear.count) %>%
    distinct %>% arrange(desc(appear.count))

hist(m3$appear.count, breaks=30)

Bubble <- gvisBubbleChart(m3, idvar="actor_1_name", 
                          xvar="appear.count", yvar="actor_1_facebook_likes",
                          sizevar="appear.count",
                          #colorvar="title_year",
                          options=list(
                              #hAxis='{minValue:75, maxValue:125}',
                              width=1000, height=800
                          )
                          )
plot(Bubble)


ms_all_rows = movies[, c("imdb_score",
                "director_facebook_likes", 
                #"cast_total_facebook_likes", 
                "actor_1_facebook_likes",
                #"actor_2_facebook_likes",
                #"actor_3_facebook_likes",
                #"movie_facebook_likes", 
                "facenumber_in_poster",
                "gross",
                "budget")]
ms = na.omit(ms_all_rows)




cor(ms)
plot(ms, pch='.')
chart.Correlation(ms) 


msc = movies[, c("color",
    "duration",
    "content_rating",
    "language",
    "country",
    "aspect_ratio",
    "title_year",
    "imdb_score"
)]


scatterplotMatrix(ms, pch=".")

hist(movies$director_facebook_likes, breaks=30)
plot(density(na.omit(movies$director_facebook_likes)))


tableplot(ms, sortCol="imdb_score") 
tableplot(msc, sortCol="country") 

hist(movies$facenumber_in_poster, breaks=50)

view_model = function(model) {
    par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
    plot(model)
    par(mfrow=c(1,1)) # Change back to 1 x 1
}

trainingIndex <- sample(1:nrow(ms), 0.7*nrow(ms))
trainingData <- ms[trainingIndex, ]
testData <- ms[-trainingIndex, ]

model.saturated = lm(imdb_score ~ ., data = trainingData)
predicted <- predict (model.saturated, testData)
compare <- cbind (actual=testData$imdb_score, predicted)

mean((testData$imdb_score - predicted)^2)

mean (apply(compare, 1, min)/apply(compare, 1, max))

summary(model.saturated)
view_model(model.saturated)
influencePlot(model.saturated)
vif(model.saturated) #Assessing the variance inflation factors for the variables in our model.
avPlots(model.saturated)

AIC(model.saturated)
BIC(model.saturated)

model.empty = lm(imdb_score ~ 1, data = ms) #The model with an intercept ONLY.
model.full = lm(imdb_score ~ ., data = ms) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)

summary(forwardAIC)
view_model(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC) 

bc = boxCox(forwardAIC)
lambda = bc$x[which(bc$y == max(bc$y))] # Extracting the best lambda value
imdb_score.bc = (ms$imdb_score^lambda - 1)/lambda
model.bc = lm(imdb_score.bc ~ gross + director_facebook_likes + facenumber_in_poster + 
                  cast_total_facebook_likes, data=ms)

summary(model.bc)
view_model(model.bc)
influencePlot(model.bc)
vif(model.bc)
avPlots(model.bc)
confint(model.bc) 

movies_with_good_variables = movies[, c("imdb_score",
                 "director_facebook_likes", 
                 "cast_total_facebook_likes", 
                 "actor_1_facebook_likes",
                 "actor_2_facebook_likes",
                 "actor_3_facebook_likes",
                 "movie_facebook_likes", 
                 "facenumber_in_poster",
                 "gross",
                 "budget")]
mvs = na.omit(movies_with_good_variables)
x = as.matrix(mvs[, -1])
y = mvs[, 1]

####Ridge Regression#
library(tidyverse)
library(broom)


grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.models)) 
coef(ridge.models) 

plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)

set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge


log(bestlambda.ridge)

ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)

compare <- cbind (actual=y.test, ridge.bestlambdatrain)
mean (apply(compare, 1, min)/apply(compare, 1, max))

ridge.out = glmnet(x, y, alpha = 0)
predict(ridge.out, type = "coefficients", s = bestlambda.ridge)
ridge.bestlambda = predict(ridge.out, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda - y)^2)


#Lasso Regression
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)

dim(coef(lasso.models)) 
coef(lasso.models) 


set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)



lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

compare <- cbind (actual=y.test, lasso.bestlambdatrain)
mean (apply(compare, 1, min)/apply(compare, 1, max))

lasso.out = glmnet(x, y, alpha = 1)

predict(lasso.out, type = "coefficients", s = bestlambda.lasso)

lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)

