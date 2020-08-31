#Q11

a <- 200 #Sample mean
s <- 30
n <- 2000 #sample size
#conf int=94%

#alpha level
(1-0.94)/2 

#step 2
error <- qnorm(0.03)*s/sqrt(n)
error
left <- a-error
right <- a+error
left
right

#for 98%
  #at alpha level
(1-0.98)/2

error <- qnorm(0.01)*s/sqrt(n)
error
left <- a-error
right <- a+error
left
right

#for 96%
#at alpha level
(1-0.96)/2

error <- qnorm(0.02)*s/sqrt(n)
error
left <- a-error
right <- a+error
left
right> a <- 200 #Sample mean
> s <- 30
> n <- 2000 #sample size
> #alpha level
> (1-0.94)/2 
[1] 0.03
> #step 2
> error <- qnorm(0.03)*s/sqrt(n)
> error
[1] -1.261675
> left <- a-error
> right <- a+error
> left
[1] 201.2617
> right
[1] 198.7383
> #for 98%
>   #at alpha level
> (1-0.98)/2
[1] 0.01
> error <- qnorm(0.01)*s/sqrt(n)
> error
[1] -1.560562
> left <- a-error
> right <- a+error
> left
[1] 201.5606
> right
[1] 198.4394
> #for 96%
> #at alpha level
> (1-0.96)/2
[1] 0.02
> error <- qnorm(0.02)*s/sqrt(n)
> error
[1] -1.377697
> left <- a-error
> right <- a+error
> left
[1] 201.3777
> right
[1] 198.6223
