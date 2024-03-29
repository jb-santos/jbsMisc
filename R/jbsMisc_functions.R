




#' Calculate SE of proportion
#'
#' @param prop Proportion for which SE is to be calculated (in decimal format)
#' @param n Sample size
#' @param ... Other arguments (not currently implemented)
#'
#' @return A scalar denoting the SE
#' @export
#'
seProp <- function(prop, n, ...) {
  se <- sqrt( (prop * (1-prop)) / n)
  return(se)
}





#' Title
#'
#' @param a.prop Proportion for Group A (in decimal format)
#' @param a.n Size of Group A
#' @param b.prop Proportion for Group B (in decimal format)
#' @param b.n Size of Group B
#' @param ... Other arguments (not currently implemented)
#'
#' @return A data frame
#' @export
#'
testProps <- function(a.prop, a.n, b.prop, b.n, ...) {
  diff <- a.prop - b.prop
  diff.se <- sqrt( ((a.prop * (1-a.prop)) / a.n) +
                     ((b.prop * (1-b.prop)) / b.n) )
  diff.lwr95 <- diff - (1.96 * diff.se)
  diff.upr95 <- diff + (1.96 * diff.se)

  exp.prop <- ( (a.prop * a.n) + (b.prop * b.n) ) / (a.n + b.n)
  z.val <- (diff - 0) /
     sqrt( (exp.prop * (1-exp.prop)) * ((1/a.n) + (1/b.n)) )
  p.val <- 2 * pnorm(z.val, lower.tail = FALSE)

  # pstar <- (a.prop + b.prop) / (a.n + b.n)
  # z.val <- (diff.se - 0) /
  #   sqrt( (pstar * (1-pstar)) * ((1/a.n) + (1/b.n)) )
  # p.val <- pnorm(z.val, lower.tail = FALSE)

  out <- data.frame(
      mean.diff = diff,
      se.diff = diff.se,
      lwr95 = diff.lwr95,
      upr95 = diff.upr95,
      exp.prop = exp.prop,
      z.val = z.val,
      p.val = p.val
  )
  return(out)
}






#' Margin of error of proportion
#'
#' @param prop Proportion for which margin of error is to be calculated  (in decimal format)
#' @param n Sample size
#' @param ... Other arguments (not currently implemented)
#'
#' @return A scalar denoting the SE
#' @export
#'
moeProp <- function(prop, n, ci = .95, ...) {
  se <- sqrt( (prop * (1-prop)) / n)
  moe <- se * 1.96
  out <- data.frame(
    prop = prop,
    n = n,
    se = se,
    moe = moe,
    lwr = prop - moe,
    upr = prop + moe
  )
  return(out)
}


#' range01
#'
#' @param x A numerical vector or column of a data frame/tibble to re-range from 0 to 1
#'
#' @export
#'
range01 <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}




#' ggfontsize
#'
#' @param basesize Scalar denoting the base size (in points) of plot text
#' @param angle.labs Boolean indicating whether to angle x-axis labels
#'
#' @importFrom ggplot2 theme element_text margin
#'
#' @export
#'
ggfontsize <- function(basesize = 11,
                       angle.labs = FALSE) {

  ggfontsize <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = basesize * 1.2),
    plot.subtitle = ggplot2::element_text(size = basesize * 1.1),
    axis.title = ggplot2::element_text(size = basesize * 1),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8, b = 0.1)),
    axis.text = ggplot2::element_text(size = basesize * .9),
    strip.text = ggplot2::element_text(size = basesize * 1),
    legend.box.margin = ggplot2::margin(t = 0.1),
    legend.title = ggplot2::element_text(size = basesize, margin = ggplot2::margin(t = 0)),
    legend.margin = ggplot2::margin(t = 0),
    legend.text = ggplot2::element_text(size = basesize * .9, margin = ggplot2::margin(t = 0)),
    plot.caption = ggplot2::element_text(size = basesize * .8)
  )

  ggfontsize
  if(angle.labs){
    if(isTRUE(angle.labs)){
      ggfontsize + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1))
    }else{}
  }

}






#' Unweight survey design
#'
#' Sets sampling weights to 1. This "unweights" a survey design object, but
#' keeps it as a survey design object. Written to help with develop the
#' svyEffects package.
#'
#' @param obj A survey design object
#' @param ... Other arguments (not currently implemented)
#'
#' @return A survey design object
#'
#' @export
#'
unWeight <- function(obj, ...) {
  mysvy <- obj
  l <- nrow(mysvy$variables)
  mysvy$prob <- rep(1, l)
  mysvy$allprob[[1]] <- rep(1, l)
  mysvy$variables[[ncol(mysvy$variables)]] <- rep(1, l)

  return(mysvy)
}






#' Detailed summary statistics
#'
#' @param x Vector on which to calculate summary statistics
#' @param digits Scalar indicating how many decimal points to return
#' @param convertFactors Boolean indicating whether to convert factors to numeric
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom stats as.formula median sd var
#'
#' @export
#'
describe <- function(x,
                     digits = 4,
                     convertFactors = FALSE,...) {
  if(is.character(x)){
    x <- factor(x)
  }
  if(is.ordered(x)){
    x <- factor(x, ordered=FALSE)
  }
  if(is.factor(x) & isFALSE(convertFactors)){
    freqs <- table(x)
    props <- prop.table(table(x))
    marginals <- cbind(Frequency = round(freqs),
                       Prop = (round(props*100, digits)))
    modal <- which.max(marginals[,1])
    statnames <- c("Type",
                   # "n NA",
                   # "% NA",
                   "Modal category",
                   "Modal n",
                   "Modal %",
                   "Ind. Qual. Var.")
    stats <- tibble(Value=c(class(x),
                            # round(sum(is.na(x))),
                            # paste0(round(sum(is.na(x)) / length(x) * 100, 2), "%"),
                            names(modal),
                            (marginals)[modal,1],
                            paste0(round((marginals)[modal,2], 2), "%"),
                            round(prod(marginals[,1]) /
                                    ( ( sum(marginals[,1]) / dim(marginals)[1] ) ^ dim(marginals)[1] ), digits)
    )
    )
    stats <- add_column(stats, Statistic = statnames, .before=1)
    marginals <- as_tibble(marginals) %>%
      add_column(Category = rownames(freqs), .before=.1) %>%
      add_row(Frequency = sum(marginals[,1]),
              Prop = sum(marginals[,2]))
    marginals <- as_tibble(marginals) %>%
      add_row(Frequency = sum(is.na(x))) %>%
      add_row(Frequency = length(x))
    marginals <- as_tibble(marginals) %>%
      add_column("% total" = round(marginals$Frequency / length(x) * 100, 2)) %>%
      rename("% valid" = Prop)
    marginals$Category[dim(marginals)[1]-2] <- "Valid total"
    marginals$Category[dim(marginals)[1]-1] <- "NA n"
    marginals$Category[dim(marginals)[1]] <- "Total"
    descriptives <- list(marginals = marginals, stats = stats)
    descriptives
  }else{
    x <- as.numeric(x)
    statnames <- c("Type",
                   "Mean",
                   "SD",
                   "Variance",
                   #"Skew",
                   "Min",
                   "Median",
                   "Max",
                   "Range",
                   "Valid n",
                   "NA n",
                   "NA % of total",
                   "Total n",
                   "Mean SE",
                   "Mean 95%CI lower",
                   "Mean 95%CI upper")
    stats <- tibble(
      Value = c(class(x),
                round(mean(x, na.rm=T), digits),
                round(sd(x, na.rm=T), digits),
                round(var(x, na.rm=T), digits),
                #round(skew(x, na.rm=T), digits),
                round(min(x, na.rm=T), digits),
                round(median(x, na.rm=T), digits),
                round(max(x, na.rm=T), digits),
                round(max(x, na.rm=T) - min(x, na.rm=T), digits),
                round(sum(!is.na(x))),
                round(sum(is.na(x))),
                paste0(round(sum(is.na(x)) / length(x) * 100, 2), "%"),
                length(x),
                round(sd(x, na.rm=T)/sqrt(sum(!is.na(x))), digits),
                round(mean(x, na.rm=T) - (1.96*(sd(x, na.rm=T)/sqrt(sum(!is.na(x))))), digits),
                round(mean(x, na.rm=T) + (1.96*(sd(x, na.rm=T)/sqrt(sum(!is.na(x))))), digits)
      )
    )
    stats <- add_column(stats, Statistic = statnames, .before=1)
    descriptives <- list(stats = stats)
    descriptives
  }
}






