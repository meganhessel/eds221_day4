# adds up the number of birds and dogs 

# defined the function
birddog_sum <- function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

# use the function 
total_pets <- birddog_sum(bird = 2, dog = 5)

# create a function to double values 
double_it <- function(x) {
  print(2 * x)
}

double_it(24)

# write a function with conditionals 
# example is converting animal ages 

animal_age <- function(animal, age) { 
  if(animal == "dog") {
    print(age * 7)
  } else if(animal == "goat") {
    print(age * 4.7)
  }
}

# write an updated version of the animal age with error messages 
animal_age_stop <- function(animal, age) {
  if(!animal %in% c("dog", "goat")) {
    stop("animal must be a dog or goat")
  }
  if(is.numeric(age) == FALSE) {
    stop("The age must be a number")
  }
  if(age <= 0 | age > 50) {
    warning("are you sure about your animals age?")
  }
  if(animal == "dog") {
    print(age * 7)
  } 
  else if(animal == "goat") {
    print(age * 4.7)
  }
}

animal_age_stop("dog", 100)
animal_age_stop("elephant", 20)
animal_age_stop("dog", 12)

# functions need for loops 
# all the data frames in the fucntoinare called df ---> argument df

df_means <- function(df) {
  for(i in 1:ncol(df)) {
    if(is.numeric(df[[i]])) {
      col_name <- colnames(df[i])
      col_mean <- mean(df[[i]], na.rm = TRUE)
      print(paste("the mean value of", col_name, "is", round(col_mean, 2))) 
    }
  } 
}

df_means(df <- penguins)


# function for logistic growth  

logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K-N0)/N0) * exp(-r * time))
  print(Nt)
}

# try it with one set of values
logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

for(i in seq_along(time_vec)) {
  logistic_growth <- function(N0, K, r, time) {
    Nt <- K / (1 + ((K-N0)/N0) * exp(-r * time))
    print(Nt)
  }
}

# working on an example just dealing with time 
time_vec <- seq(from = 0, to = 35, by = 0.1)

pop_35 <- logistic_growth(N0 = 100, K = 6000, r = .27, time = time_vec)

pop_time_35 <- data.frame(time_vec, pop_35)

ggplot(data = pop_time_35,
       aes(x = time_vec, 
           y = pop_35)) + 
  geom_line(size = 0.5)

# alternatively with an internal for loop 

#storage for output vector
pop_35_vec <- vector(mode = "numeric", length = length(time_vec))

# for loop for tepping through time steps
for(i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = .27, time = time_vec[i])
  pop_35_vec[i] <- population
}

# now buliding to estimating across growthrates 
# create a series of growth rates 
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

# create a matrix to store output values
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for(j in seq_along(r_seq)) {
  for(i in seq_along(time_vec)) {
    population <- logistic_growth(N0 = 100, K = 6000, r = r_seq[j], time = time_vec[i])
    out_matrix[i,j] <- population
  }
}
  
# data wrangling
# make a time a vairable 
out_df <- data.frame(out_matrix, time = time_vec)

# add column names for growth rates 
colnames(out_df) <- c(paste0("gr_", r_seq), "time_vec")

#pivot longer 
out_df_long <- out_df %>% 
  pivot_longer(cols = -time_vec, 
               names_to = "growth rate", 
               values_to = "population")



?pivot_longer


# plot it 
ggplot(data = out_df_long, 
       aes(x = time_vec, y = population)) +
  geom_line(aes(color = `growth rate`)) + 
  theme_minimal()
