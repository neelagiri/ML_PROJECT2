

conjugateGrad<-function(A, b, x=None)
{
  n=length(b)
  if (!x) {
    x = matrix(1,1, 3)
  }
  
  r<-(A*x - b)
  p = -r
  r_k_norm = (r*r)
  
  iterationFinal<-0
  for (x in 1:2*n) {
    Ap = A*p
    alpha = r_k_norm/p*Ap
    x = x + (alpha * p)
    r = r + (alpha * Ap)
    r_kplus1_norm = XA
    beta = r_kplus1_norm / r_k_norm
    r_k_norm = r_kplus1_norm
    iterationFinal = i+1
    if(r_kplus1_norm < 1.0e-9) {
      break
    }
    p=beta * p - r
  }
  print(iterationFinal)
  return(x)
}



# training dataset for animal which is converted to martix (image converted to grayscale)

datafram_animal_train <- "/Users/chayan/Desktop/Project/Final Data sheets/Animals/Images/ImageAnimalData"
datafram_animal_train_read <- read.csv(datafram_animal_train)

retval <- subset(data, "Image:")

for(i in 1:retval) {
  b = matrix(1,1, 3)
  print("starting conjugate Gradient")
  t1 = Sys.time()
  print(t1)
  CG <- conjugateGrad(i, b)
  t2= Sys.time()
  print(t2-t1)
}



