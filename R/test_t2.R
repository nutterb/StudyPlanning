test_t2 <- function(delta = NULL, n=NULL, sd=NULL, alpha=.05,
                    power=NULL, delta0=0, weights=list(c(1, 1)),
                    two_tail=TRUE, 
                    df_type=c("default", "equal", "satterthwaite"),
                    mu1_null = NA, mu1_alt = NA, 
                    mu2_null = NA, mu2_alt = NA, 
                    s1 = NA, n1=NA, 
                    s2 = NA, n2=NA,
                    var_equal=TRUE){
  
  n1_est <- quote({k / (1-k) * n2})
  
  s_star <- function(ve){
    switch(as.numeric(ve),
           '1' = quote({sqrt(((n1 - 1) * s1^2 + n2 + s2^2) / 
                               (n1 + n2 - 2))}),
           '0' = quote({sqrt(s1^2/n1 + s2^2/n2)}))
  }
  
  S1 <- expand.grid(s1 = s1,
                    n1 = n1)
  S2 <- expand.grid(s2 = s2,
                    n2 = n2)
  S <- expand.grid.df(S1, S2, var_equal=var_equal)
  S$sd <- with(S, eval(s_star(var_qual)))
                   
  
  df <- switch(dft,
               "equal" = eval(n1) + n2 - 2,
               "satterthwaite" = )
  
  k <- sapply(weights, function(w) w[1] / sum(w))
  
}