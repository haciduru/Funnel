# This function an array of numbers as input. Optionally, the numbers have names.


funnel = function(x, title = '') {
  
  reverse = function(x) {
    x_ = c()
    while (length(x)>1) {
      x_ = c(x_, x[length(x)])
      x = x[1:(length(x)-1)]
    }
    x_ = c(x_, x)
    return(x_)
  }
  
  if (!length(names(x))) names(x) = x
  
  x = data.frame(vals = x, nams = names(x))
  x = x[order(x$vals), ]
  
  nams = reverse(x$nams)
  
  x = x$vals
  x = c(x[1], x[2:length(x)] - x[1:(length(x)-1)])
  
  y = matrix(x, ncol = 1)
  i = 1
  while (i < length(x)) {
    y = cbind(y, c(x[1:(length(x)-i)], rep(0, i)))
    i = i + 1
  }
  
  barplot(y, space = .1, names.arg = nams, ylab = 'Frequency', main = title)
}
