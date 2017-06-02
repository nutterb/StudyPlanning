#' @name test_t2 
#' @title Study Parameter Estimation for Two Sample T-Tests
#' 
#' @description The two sample t-test may be used to evaluate if a difference
#'   exists between two population means.  It does not assume knowledge of 
#'   the population standard deviations.  \code{test_t2} is similar in function
#'   to \code{power.t.test}, but allows greater flexibility over the 
#'   input values and includes support for estimating study parameters 
#'   with unequal sample sizes.
#'   
#' @param delta Numeric vector of proposed observed differences between 
#'   the two sample means.
#' @param n  Numeric vector of the total sample size.
#' @param se Numeric vector of the estimated joint standard error between
#'   the two samples.  Must be greater than 0.
#' @param alpha Numeric vector of significance levels for the test.
#'   Must be on the interval \code{(0, 1)}.
#' @param power Numeric vector of statistical power for the test.  Must be 
#'   on the interval \code{(0, 1)}.
#' @param delta0 Numeric vector of true differences between the two population 
#'   means.  Defaults to 0, but may be changed to propose a non-zero null
#'   hypothesis.
#' @param weights A list of vectors. Each list must have length 2.  The vectors
#'   specify the weighting ratio of sample 1 to sample 2.  \code{c(1, 1)} 
#'   indicates equal sample sizes;  \code{c(2, 1)} indicates group 1 has 
#'   twice as many subjects as group 2.  Proportions may also be used, but 
#'   will be normalized to establish the weight.
#' @param two_tail Logical, indicating if the test should be evaluated as a
#'   one tail or two tail test.  Only distinct values will be used, and 
#'   permuted with each of the other study inputs.
#' @param df_type Character vector indicating the type of degrees 
#'   of freedom to use.  Only one value will be used.  \code{"equal_variance"},
#'   will result in the degrees of freedom \code{n1 + n2 - 2}. 
#'   \code{"satterthwaite"} calculates the degrees of freedom using 
#'   the Welch-Satterthwaite equation. The behavior of \code{"default"}
#'   depends on the value of the \code{var_equal} argument.
#' @param mu1_null,mu2_null Numeric vectors giving the null hypothesis
#'   values for the population means of groups 1 and 2, respectively.
#' @param mu1_alt,mu2_alt Numeric vectors giving the alternative hypothesis
#'   values for the population means of groups 1 and 2, respectively.
#' @param n1,n2 Numeric vectors of sample sizes for the two groups. This 
#'   allows the sample sizes to be entered directly instead of using the 
#'   \code{weights} approach.
#' @param s1,s2 Numeric vectors of standard deviations for the first 
#'   and second groups. Allows for standard deviations to be specified 
#'   directly instead of providing the joint standard error for 
#'   \code{se}.
#' @param var_equal Logical vector that directs the calculation of degrees
#'   of freedom when \code{df_type = "default"}.  When \code{var_equal = TRUE}, 
#'   the default behavior of \code{df_type = "default"} is \code{n1 + n2 - 2}.
#'   Otherwise, the Welch-Satterthwaite equation is used. The default value 
#'   is \code{FALSE}
#' 
#' @section Argument Groups:
#' A design choice has been made to provide multiple arguments that can 
#' accomplish similar designs.  Thus, it is not necessary to provide a 
#' value for every argument to the function, but if certain arguments are 
#' provided values at the same time, conflicts will need to be resolved.
#' 
#' For example, \code{n} is used to specify the total sample size, with 
#' group allocation determined by \code{weights}.  Meanwhile, \code{n1} and
#' \code{n2} may be used to specify exact sample sizes without having to 
#' determine the weight allocated to each group.  If all four arguments are 
#' provided, a conflict exists.  The following paragraphs identify these
#' argument groups and describe their behavior toward each other.
#' 
#' \strong{Sample Size}: \code{n} and \code{weights} form a pair that describe
#' the size of each group.  \code{n1} and \code{n2} form another pair that 
#' perform the same functionality.  \code{n1} and \code{n2} are ignored so long
#' as both values are non-missing.  If argument is given a value, both must be
#' given a value, and these values will override the \code{n}/\code{weight} 
#' pair.
#' 
#' \strong{Standard Deviation}: \code{se} may be used by itself to indicate the
#' joint standard error.  
#' In situations where the joint value is not provided, but the
#' individual values are, \code{s1} and \code{s2} may be provided and the 
#' joint standard error will be calculated.  If either \code{s1} or \code{s2} has 
#' a value, they must both have a value. The values calculated from \code{s1}
#' and \code{s2} override any values passed to \code{se}.
#' 
#' \strong{Null Hypotheses}: \code{delta0} may be used to
#' indicate the value of the difference under the null hypothesis. Alternatively,
#' \code{mu1_null} and \code{mu2_null} may be specified.  If either one is 
#' specified, both must be specified, and the values calculated from those 
#' override any values given to \code{delta0}
#' 
#' \strong{Alternative Hypotheses}: \code{delta} may be used to
#' indicate the value of the difference under the alternative hypothesis. 
#' Alternatively, \code{mu1_alt} and \code{mu2_alt} may be specified.  
#' If either one is specified, both must be specified, and the values 
#' calculated from those override any values given to \code{delta}
#' 
#' @section Building the Results Frame:
#' Results are returned as a data frame.  Prior to calculating any results,
#' the entire data frame is constructed to hold all of the results and the
#' inputs.  When only the primary arguments \code{delta, n, sd, alpha, 
#' power, delta0} and \code{weights} are used, the results frame is a 
#' simple matter of \code{expand.grid}
#' 
#' When the alternate arguments are used, the principle is the same, but
#' some intermediary work is done.  There is no requirement that \code{s1} 
#' and \code{s2}, for example, have the same length.  In order to construct
#' the values that will ultimately act as \code{se}, then, \code{expand.grid} 
#' is used on \code{s1} and \code{s2}, returning all of the permutations 
#' of the two vectors, which will then permute with all of the primary 
#' arguments. In other words, vectors of unequal length do not use recycling, 
#' but use permutation.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Exactly one of \code{delta}, \code{n}, \code{s}, \code{alpha}, or
#'     \code{power} must be \code{NULL}.
#'   \item If either of \code{mu1_null} or \code{mu2_null} is non missing, 
#'     the must both be non missing.
#'   \item Values for \code{delta0} may be calculated from \code{mu1_null} 
#'     and \code{mu2_null}.
#'   \item If either of \code{mu1_alt} or \code{mu2_alt} is non missing, 
#'     the must both be non missing.
#'   \item Values for \code{delta} may be calculated from \code{mu1_alt} 
#'     and \code{mu2_alt}.
#'   \item Values for \code{n} may be calculated from \code{n1} and \code{n2}
#'   \item Values for \code{se} may be calculated from \code{s1} and \code{s2}
#'   \item Values for \code{delta0} may be calculated from \code{mu1_null}
#'     and \code{mu2_null}
#'   \item \code{alpha} strips 0 and 1 from its inputs quietly.
#'   \item \code{power} strips 0 and 1 from its inputs quietly.
#'   \item \code{two_tail} is a logical vector, but is reduced to only distinct
#'     values. (at most \code{TRUE} and \code{FALSE}).
#'   \item \code{var_equal} is a logical vector, but is reduced to only 
#'     distinct values (at most \code{TURE} and \code{FALSE}).
#' }
#' 
#' @export

test_t2 <- function(delta = NULL, n=NULL, se=NULL, alpha=.05,
                    power=NULL, delta0=0, weights=list(c(1, 1)),
                    two_tail=TRUE, 
                    df_type=c("default", "equal_variance", "satterthwaite"),
                    mu1_null = NA, mu1_alt = NA, 
                    mu2_null = NA, mu2_alt = NA, 
                    s1 = NA, n1=NA, 
                    s2 = NA, n2=NA,
                    var_equal=FALSE)
{
  #******************************************************************
  #* Argument Checks - First pass
  #* 1. Both mu1_null and mu2_null are missing or they are both 
  #*    non-missing
  #* 2. Both mu1_alt and mu2_alt are missing or they are both
  #*    non-missing
  #* 3. Both s1 and s2 are missing or they are both non-missing
  #* 4. Both n1 and n2 are missing or they are both non-missing
  #* 5. Check the types of each of the variables
  #******************************************************************
  coll <- checkmate::makeAssertCollection()
  
  if (!identical(any(is.na(mu1_null)), any(is.na(mu2_null))))
  {
    coll$push(
      "If either of `mu1_null` or `mu2_null` is given, they must both be given"
    )
  }
  
  if (!identical(any(is.na(mu1_alt)), any(is.na(mu2_alt))))
  {
    coll$push(
      "If either of `mu1_alt` or `mu2_alt` is given, they must both be given"
    )
  }
  
  if (!identical(any(is.na(s1)), any(is.na(s2))))
  {
    coll$push(
      "If either of `s1` or `s2` is given, they must both be given"
    )
  }
  
  if (!identical(any(is.na(n1)), any(is.na(n2))))
  {
    coll$push(
      "If either of `n1` or `n2` is given, they must both be given"
    )
  }
  
  if (!is.null(delta))
  {
    checkmate::assert_numeric(x = delta,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (!is.null(n))
  {
    checkmate::assert_numeric(x = n,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (!is.null(se))
  {
    checkmate::assert_numeric(x = se,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (!is.null(alpha))
  {
    checkmate::assert_numeric(x = alpha,
                              any.missing = FALSE,
                              lower = 0,
                              upper = 1,
                              add = coll)
  }
  
  if (!is.null(power))
  {
    checkmate::assert_numeric(x = power,
                              any.missing = FALSE,
                               lower = 0,
                               upper = 1,
                               add = coll)
  }
  
  if (!is.null(delta0))
  {
    checkmate::assert_numeric(x = delta0,
                              any.missing = FALSE,
                              add = coll)
  }
  
  checkmate::assert_class(x = weights,
                          classes = "list")
  
  if ("list" %in% class(weights))
  {
    weight_len <- 
      vapply(weights,
             FUN = length,
             FUN.VALUE = numeric(1))
    
    weight_class <- 
      vapply(weights, 
             FUN = is.numeric,
             FUN.VALUE = logical(1))
    
    if (!all(weight_len == 2) & all(weight_class))
    {
      coll$push("Each element in `weights` must be numeric with length 2")
    }
  }
  
  checkmate::assert_logical(x = two_tail,
                            any.missing = FALSE,
                            add = coll)
  
  checkmate::assert_logical(x = var_equal,
                            any.missing = FALSE,
                            add = coll)

  df_type <- checkmate::matchArg(x = df_type,
                                 choices = c("default", "equal_variance",
                                             "satterthwaite"),
                                 add = coll)
  
  if (any(!is.na(mu1_null)))
  {
    checkmate::assert_numeric(x = mu1_null,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (any(!is.na(mu2_null)))
  {
    checkmate::assert_numeric(x = mu1_null,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (any(!is.na(mu1_alt)))
  {
    checkmate::assert_numeric(x = mu1_alt,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (any(!is.na(mu2_alt)))
  {
    checkmate::assert_numeric(x = mu2_alt,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (any(!is.na(s1)))
  {
    checkmate::assert_numeric(x = s1,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (any(!is.na(s2)))
  {
    checkmate::assert_numeric(x = s2,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (any(!is.na(n1)))
  {
    checkmate::assert_numeric(x = n1,
                              any.missing = FALSE,
                              add = coll)
  }
  
  if (any(!is.na(n2)))
  {
    checkmate::assert_numeric(x = n2,
                              any.missing = FALSE,
                              add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  #******************************************************************
  #* Build the results data frame
  #* 1. delta
  #* 2. delta0
  #* 3. n
  #* 4. s
  #* 5. alpha, power, two_tail, var_equal
  
  #* Construct delta
  if (any(!is.na(mu1_alt)))
  {
    .result <- expand.grid(mu1_alt = mu1_alt,
                           mu2_alt = mu2_alt) %>%
      dplyr::mutate(delta = mu1_alt - mu2_alt)
  }
  else 
  {
    .result <- expand.grid(delta = if (is.null(delta)) NA else delta)
  }
  
  #* Construct delta0
  if (any(!is.na(mu1_null)))
  {
    .result <- 
      expand.grid.df(
        .result,
        expand.grid(mu1_null = mu1_null,
                    mu2_null = mu2_null) %>%
          dplyr::mutate(delta0 = mu1_null - mu2_null)
      )
  }
  else
  {
    .result <- 
      expand.grid.df(
        .result,
        data.frame(delta0 = if (is.null(delta0)) NA else delta0)
      )
  }
  
  #* Construct s
  if (any(!is.na(s1)))
  {
    .result <- 
      expand.grid.df(
        .result,
        expand.grid(s1 = s1,
                    s2 = s2)
      )
  }
  else
  {
    .result <- 
      expand.grid.df(
        .result,
        expand.grid(se = if (is.null(se)) NA else se)
      )
  }
  
  #* Construct n
  if (any(!is.na(n1)))
  {
    .result <- 
      expand.grid.df(
        .result,
        expand.grid(n1 = n1,
                    n2 = m2) %>%
          dplyr::mutate(n = n1 + n2,
                        w = n1 / n)
      )
  }
  else
  {
    w <- vapply(weights,
                FUN = function(wt) wt[1] / sum(wt),
                FUN.VALUE = numeric(1))
    .result <- 
      expand.grid.df(
        .result,
        expand.grid(n = if (is.null(n)) NA else n,
                    w = w) %>%
          dplyr::mutate(n1_est = n * w)
      )
  }
  
  #* Construct with alpha, power, two_tail, var_equal, df_type
  .result <-
    expand.grid.df(
      .result,
      expand.grid(alpha = if (is.null(alpha)) NA else alpha,
                  power = if (is.null(power)) NA else power,
                  two_tail = unique(two_tail),
                  var_equal = unique(var_equal),
                  df_type = df_type,
                  stringsAsFactors = FALSE)
    )
  
  
  power_fn <- function()
  {
    power - qt(1 - alpha / 2, 
               df = t2_df_calc(se = se,
                               s1 = s1, 
                               s2 = s2, 
                               n = n, 
                               w = w, 
                               df_type = df_type, 
                               var_equal = var_equal),
               ncp = (delta0 - delta) / se)
  }
  
  
  .result
  
}


t2_se_calc <- function(se, s1, s2, n, w, var_equal = FALSE)
{
  #* When se was given as an argument, its value is assumed to be 
  #* exact.  There is no need to calculate it. 
  if (!is.na(se)) return(se)
  
  n1 <- n * w
  n2 <- n * (1 - w)
  
  if (var_equal)
  {
    sqrt(s1^2 / n1 + s2^2 / n2)
  }
  else
  {
    sqrt((((n1-1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2)) * 
      sqrt(1 / n1 + 1 / n2)
  }
}

t2_df_calc <- function(se, s1, s2, n, w, df_type, var_equal)
{
  if (df_type == "default")
  {
    df_type <- if (var_equal) "equal_variance" else "satterthwaite"
  }
  
  n1 <- n * w
  n2 <- n * (1 - w)
  if (is.null(s2)) s2 <- s1
  
  if (df_type == "equal_variance" | !is.na(se))
  {
    n1 + n2 - 2
  }
  else
  {
    (s1^2 / n1 + s2^2 / n2)^2 / 
      ((s1^2 / n1) / (n1 - 1) + (s2^2 / n2) / (n2 - 1))
  }
}

test_t2(n = 20, delta = 1, se = 1, s1 = 3, s2 = 4)
test_t2(n = 20, mu1_alt = 1:3, mu2_alt = 3:2, 
        mu1_null = 2, mu2_null = 3, se = 1)
