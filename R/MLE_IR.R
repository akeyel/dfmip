#' Calculate Maximum Likelihood Estimate of Infection Rate using R
#'
#' Calculate the MLE using the dprev function
#'
#' @author C. Williams <chrisw at uidaho.edu> and C. Moffitt <cmoffitt at uidaho.edu>
#'
#' @details Citation: C. Williams and C. Moffitt 2005. Estimation of pathogen
#' prevalence in pooled samples using maximum likelihood methods and
#' open source software. Journal of Aquatic Animal Health 17: 386 - 391
#'
#' Documentation and minor reformatting performed by
#' A. Keyel <akeyel at albany.edu> Permission to distribute under a GPL 3
#' license granted on 2019 March 5 by Chris Williams via email to A. Keyel
#'
#' @name MLE_IR
NULL

#' llprevr
#'
#' Internal function used in calculating infection rate
#'
#' @details Citation: C. Williams and C. Moffitt 2005. Estimation of pathogen
#' prevalence in pooled samples using maximum likelihood methods and
#' open source software. Journal of Aquatic Animal Health 17: 386 - 391
#'
#' @name llprevr
llprevr <- function(p, yes = c(0), no = c(0)){

  sumcheck <- sum(yes) + sum(no)

  if (sumcheck == 0) stop('Data must be entered for yes or no')

  if (sum(yes) == 0) { llmck <- sum(no) * log(1-p) }

  else { llmck <- sum(log(1-(1-p)^yes)) + sum(no) * log(1-p) }

  llmck

}

#' dprev
#'
#' Calculate mosquito infection rates
#'
#' @details Citation: C. Williams and C. Moffitt 2005. Estimation of pathogen
#' prevalence in pooled samples using maximum likelihood methods and
#' open source software. Journal of Aquatic Animal Health 17: 386 - 391
#'
#' @param yes a vector of positive pools, with the elements indicating how many
#' mosquitoes were in each positive pool
#' @param no a vector of negative pools, with the elements indicating how many
#' mosquitoes were in each negative pool
#' @param disp 'y' indicates to print results to dipslay
#' @param conf The confidence interval to return. Default is 0.95 to yield a
#' 95 percent confidence interval
#'
#' @examples
#' dprev(y = c(15), n = c(15,15,16))
#' dprev(c(5,10), c(5))
#'
#' @examples
#' dprev(y = c(15), n = c(15,15,16))
#' dprev(c(5,10), c(5))
#'
#' @export dprev
dprev <- function(yes = c(0),no = c(0),disp = 'y',conf = .95){

  sumcheck <- sum(yes) + sum(no)

  if (sumcheck == 0) stop('Data must be entered for yes or no')

  if (sum(yes) == 0)

  {

    ucl <- 1 -exp(-qchisq(conf,1)/(2*sum(no)))

    result <- c(0., 0., ucl)

    if (disp == 'y') print('Lower 95% limit, MLE, Upper 95% limit = ')

    result

  }

  else if (sum(no) == 0)

  {

    tfct <- function(p)

    {

      sum(log(1-(1-p)^yes)) + qchisq(conf,1)/2

    }

    lcl <- uniroot(tfct, interval = c(0.0001, 1.))

    result <- c(lcl$root, 1., 1.)

    if (disp == 'y') print('Lower 95% limit, MLE, Upper 95% limit = ')

    result

  }

  else

  {

    #print(yes) ; #print(no)

    llpmax <- optimize(llprevr, c(0., 1.), maximum = TRUE, yes = yes, no = no)

    #print(llpmax)

    mval <- llprevr(llpmax$maximum, yes = yes, no = no)

    tfct <- function(p)

    {

      llprevr(p, yes, no)-(mval-qchisq(conf,1)/2)

    }

    lcl <- uniroot(tfct, interval = c(0.00000001, llpmax$maximum))

    ucl <- uniroot(tfct, interval = c(llpmax$maximum, 0.99999999))

    result <- c(lcl$root, llpmax$maximum, ucl$root)

    if (disp == 'y') print('Lower 95% limit, MLE, Upper 95% limit = ')

    result

  }

}

# Test Example to show how the function works
#
#
