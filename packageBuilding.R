devtools::document(".")
devtools::test(".")
# devtools::check(".")
devtools::install_local(getwd())

p_example <-
  test_binomial(n = NULL, 
                p0 = .15, 
                p1 = .2, 
                alpha = .05,
                power = seq(0.10, 0.90, by = .01),
                conservative = c(FALSE, TRUE),
                tail='right')

test_binomial(n = 2:30, 
              p0 = .15, 
              p1 = .25, 
              alpha= .05, 
              power = NULL,
              tail = 'right')

library(ggplot2)
ggplot(data = p_example,
       mapping = aes(x = power_actual,
                     y = n,
                     colour= factor(conservative))) + 
  geom_line()

test_binomial(n=20, p0=.5, p1=.8, alpha=c(.01),
              tail='right')

test_binomial(n=NULL, p0=.5, p1=.8, alpha=c(.01), power = 0.6296483,
              tail='right')



source("https://gist.githubusercontent.com/nutterb/f9fb89d35766022b0f75c90b24d45033/raw/e48041dcccee51f7cfada65df13b844c69833114/directorySearch")
directorySearch("R", "remove_limit")
