x = c(2,4,3,6,5,8,7,9,10)
y = c(10,13,7,8,5,6,3,11,2)

mean1 = sum(x)/length(x)
print(mean1)
mean2 = sum(y)/length(y)
print(mean2)

n = length(x)
cov = (1/n)*(x-mean1)*(y-mean2)
print(cov)

std = function(data) {
  if (n < 2) {
    stop("Atleast give two data points to calculate std deviation.")
  }
  sum_squared_diff = sum((data - mean)^2)
  variance = sum_squared_diff / (n - 1)
  std_dev = sqrt(variance)
  return(std_dev)
}
std_x = std(x)
std_y = std(y)
print(std_x)
print(std_y)


cor = cov/(std_x*std_y)
print(cor)