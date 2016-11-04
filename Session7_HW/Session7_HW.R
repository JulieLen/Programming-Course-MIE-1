### Session 7 - Homework ###

# I've choosed the movie dataset

colnames(movie)

summary(movie[,"facenumber_in_poster"])

# The number of faces on the poster was computed using a
# algorithm for face recognition. Besides the strange value of 43, 
# it seems that most of the movies had posters displaying only 
# one face. 

summary(movie[,"title_year"])

#there is a great deal of movie here, but 50% where produced in the 
# 10 past years. 

summary(movie[,"imdb_score"])
#scores are over 10 so obviously all the grades are used. 
# Median and mean are around 6 and 50% of movies were attributed a grade
# between 5.8 and 7.2. 

type <- rep(0, 5043)
i<-1

while(i<5044) {
if(movie[i,"imdb_score"]<5.8){
  type[i] <- 0
} else if(movie[i,"imdb_score"]>7.2) {
  type[i] <- 2
} else {
  type[i] <- 1
}
i <- i+1
}
head(type)

# Now we have 3 types of movies : the "bad" ones (score 0), the "good ones"
# (score 1) and the "excellent ones" (score 2). 

movies <- cbind(movie, type)

# I tried various things to calculate group average but none of the solutions
#were fully satisfying…

mean(movies[,"imdb_score"], by="type")

aggregate(movies[, "budget"], list(movies$type), mean)

ddply(movies, .(type), summarize, budget=mean(budget), imdb_score=mean(imdb_score))

# Plots : 


# Correlations : 





