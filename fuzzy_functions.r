# Functions plot
#-------------------------------------------------------------------------------

# Linear rescale function
linear_rescale <-
  function(x, range_min, range_max, lower_bound, upper_bound, target_min, target_max) {
    if (lower_bound < upper_bound) {
      vals <- ifelse(x < lower_bound, 0, 
                     ifelse(x > upper_bound, 1,
                            (x - lower_bound) / (upper_bound - lower_bound)
                     ))
    } else{
      vals <- ifelse(x > lower_bound, 0,
                     ifelse(x < upper_bound, 1,
                            (x - lower_bound) / (upper_bound - lower_bound)
                     ))
    }
    
    vals <- vals * (target_max - target_min) + target_min
    return(vals)
  }

linres <-
  function(r, s, lower_bound, upper_bound, target_min, target_max) {
    if (lower_bound < upper_bound) {
      s[r < lower_bound] = target_min
      s[r > upper_bound] = target_max
      s[r >= lower_bound & r <= upper_bound] = 
        ((r - lower_bound) / (upper_bound - lower_bound) * 
           (target_max - target_min) + target_min)
      return(s)
    } else {
      s[r > lower_bound] = target_min
      s[r < upper_bound] = target_max
      s[r <= lower_bound & r >= upper_bound] = 
        ((r - lower_bound) / (upper_bound - lower_bound) * 
           (target_max - target_min) + target_min)
      return(s)      
    }
  }

power_rescale <-
  function(x, lower_bound, upper_bound, a, target_min, target_max) {
    if (lower_bound < upper_bound) {
      vals <- ifelse(x < lower_bound, 0,
                     ifelse(x > upper_bound, 1,
                            ((x - lower_bound) / (upper_bound - lower_bound)
                            ) ^ a))
    } else{
      vals <- ifelse(x > lower_bound, 0,
                     ifelse(x < upper_bound, 1,
                            ((x - lower_bound) / (upper_bound - lower_bound)
                            ) ^ a))
    }
    vals <- vals * (target_max - target_min) + target_min
    return(vals)
  }

powres <-
  function(r, s, lower_bound, upper_bound, target_min, target_max, a) {
    if (lower_bound < upper_bound) {
      s[r < lower_bound] = target_min
      s[r > upper_bound] = target_max
      s[r >= lower_bound & r <= upper_bound] = 
        (((r - lower_bound) / (upper_bound - lower_bound))^a * 
           (target_max - target_min) + target_min)
      return(s)
    } else {
      s[r > lower_bound] = target_min
      s[r < upper_bound] = target_max
      s[r <= lower_bound & r >= upper_bound] = 
        (((r - lower_bound) / (upper_bound - lower_bound))^a * 
           (target_max - target_min) + target_min)
      return(s)      
    }
  }

# Gaussian rescale function
gaussian_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- exp(-f1 * (x - f2)^2)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

gaures <- function(r, f1, f2, target_min, target_max) {
  s = exp(-f1 * (r - f2)^2) * (target_max - target_min) + target_min
  return(s)
}

# Small membership rescale function
small_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- 1 / (1 + (x/f2)^f1)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

smares <- function(r, f1, f2, target_min, target_max) {
  s = (1 / (1 + (r/f2)^f1)) * (target_max - target_min) + target_min
  return(s)
}

# Large membership rescale function
large_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- 1 / (1 + (x/f2)^-f1)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

larres <- function(r, f1, f2, target_min, target_max) {
  s = (1 / (1 + (r/f2)^-f1)) * (target_max - target_min) + target_min
  return(s)
}

# Near membership rescale function
near_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- 1 / (1 + f1*(x - f2)^2)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

neares <- function(r, f1, f2, target_min, target_max) {
  s = (1 / (1 + f1*(r - f2)^2)) * (target_max - target_min) + target_min
  return(s)
}