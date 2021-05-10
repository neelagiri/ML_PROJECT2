# gradient descent optimization with nesterov momentum for a two-dimensional test function



objective<-function(x,y,x)
{
  return (x**2.0 + y**2.0 + z**2.0)
}

derivative<- function(a,b,d)
{
  return (c(a*2.0, b*2.0, d*2.0))
}


nesterov<- function(objective, derivative, bounds, n_iter, step_size, momentum)
{
  # generate an initial point
  solution<-bounds[,1] + runif(nrow(bounds)) * (bounds[, 1] - bounds[, 0])
  solution_eval=0
  # list of changes made to each variable
  change = rep(0, nrow(bounds))
  # run the gradient descent
  for(i in 1:n_iter) {
    # calculate the projected solution
    projected = solution[i] + momentum * (rep(change[i], nrow(bounds)))
    gradient = derivative(projected[0], projected[1], projected[2])
    # build a solution one variable at a time  
    new_solution = list()
    for(it in 1:nrow(solution)) {
      # calculate the change 
      change[i] = (momentum * change[i]) - step_size * gradient[i]
      # calculate the new position in this variable
      value = solution[i] + change[i]
      # store this variable
      append(new_solution, value)
      # evaluate candidate point
      solution = c(new_solution)
      solution_eval = objective(solution[0], solution[1], solution[2])
      print(it)
      print(solution)
      print(solution_eval)
    }
    
    return (solution, solution_eval)
  }
}



# training dataset for animal which is converted to martix (image converted to grayscale)

datafram_animal_train <- "/Users/chayan/Desktop/Project/Final Data sheets/Animals/Images/ImageAnimalData"
datafram_animal_train_read <- read.csv(datafram_animal_train)


retval <- subset(data, "Image:")

for(i in 1:retval) {
  print("starting netserov Momentum Method")
  t1 = Sys.time()
  # define the total iterations
  n_iter = 40
  # define the step size
  step_size = 0.1
  # define momentum
  momentum = 0.3
  # perform the gradient descent search with nesterov momentum
  finalValues = nesterov(objective, derivative, test_x, n_iter, step_size, momentum)
  t2 = Sys.time()
  print ("Total Time", (t2 - t1))
  print(finalValues.best)
  print(finalValues.score)
  print('Done!')
  }



