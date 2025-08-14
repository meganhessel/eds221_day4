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
