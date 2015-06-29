library(dplyr)
library(tidyr)
library(ggplot2)

unconditionedError <- function(null_prob = 0.5, ...){
  error_prob <- expand.grid(alpha = seq(0.01, 0.99, by=0.01),
                            power = seq(0.01, 0.99, by=0.01),
                            null_prob = null_prob)
  
  error_prob <- error_prob %>%
    mutate(u_type1 = alpha * null_prob,
           u_type2 = (power) * (1-null_prob)) %>%
    gather(type, prob, u_type1, u_type2, -alpha, -power, -null_prob) %>%
    mutate(type = factor(type, 
                         c("u_type1", "u_type2"),
                         c("Unconditional alpha", "Unconditional beta")),
           null_prob = paste0("P(H0)=", null_prob))
  
  p <- ggplot(error_prob, aes(x=alpha, y=1-power, fill=type, alpha=prob)) + 
         geom_tile() + 
         scale_alpha_continuous(limits=c(0, 1))
  if (length(null_prob) > 1)
    p <- p + facet_wrap(~ null_prob, ...)

  return(list(plot = p, error_prob = error_prob))
}

unconditionedError(c(.1, .3, .5, .7, .9))
