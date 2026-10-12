



#' Cut quantiles
#'
#' @param x Vector
#' @param n Number of quantiles to make
#'
#' @returns A vector
#' @export
#'
cutQuants <- function(x, n = 3) {
  cut(x,
      breaks = quantile(x, probs = seq(0, 1, by = 1/n)),
      include.lowest = TRUE)
}






#' Generate a code list out of a labelled data frame
#'
#' @param labelleddf A labelled data frame (e.g. generated from {haven})
#' @param ... Other options (not currently implemented)
#'
#' @importFrom tibble tibble
#'
#' @returns A list that can be exported to Excel openxlsx::write.xlsx(output, file = "output.xlsx")
#' @export
#'
#'
makeCodeList <- function(labelleddf, ...) {

  out <- lapply(1:ncol(labelleddf), function(i) {
    tibble::tibble(
      !!sym(paste0(names(labelleddf)[[i]], "..", "value")) := unname(attributes(labelleddf[[i]])$labels),
      !!sym(paste0(names(labelleddf)[[i]], "..", "label")) := names(attributes(labelleddf[[i]])$labels),
    )
  })

  names(out) <- names(labelleddf)

  return(out)

}














#' Recode Likert item with AGREE scale
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeAgree <- function(x) {
  car::recode(x,
              '1="Strongly agree";  2="Somewhat agree";
               3="Somewhat disagree"; 4="Strongly disagree";
               else="DKNA"',
              as.factor = TRUE,
              levels = c("Strongly agree", "Somewhat agree",
                         "Somewhat disagree", "Strongly disagree",
                         "DKNA"))
}


#' Recode Likert item with AGREE scale (totals)
#'
#' @param x A vector or column of a data frame
#'
#' @importFrom car recode
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeAgreeNets <- function(x) {
  car::recode(x,
              '1:2="Agree";3:4="Disagree"; else="DKNA"',
              as.factor = TRUE,
              levels = c("Agree","Disagree", "DKNA"))
}


#' Recode Likert item with APPROVE scale
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeApprove <- function(x) {
  car::recode(x,
              '1="Strongly approve";  2="Somewhat approve";
               3="Somewhat disapprove"; 4="Strongly disapprove";
               else="DKNA"',
              as.factor = TRUE,
              levels = c("Strongly approve", "Somewhat approve",
                         "Somewhat disapprove", "Strongly disapprove",
                         "DKNA"))
}


#' Recode Likert item with APPROVE scale (totals)
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeApproveNets <- function(x) {
  car::recode(x,
              '1:2="Approve";3:4="Disapprove"; else="DKNA"',
              as.factor = TRUE,
              levels = c("Approve","Disapprove", "DKNA"))
}


#' Recode Likert item with SUPPORT scale
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeSupport <- function(x) {
  car::recode(x,
              '1="Strongly support";  2="Somewhat support";
               3="Somewhat oppose"; 4="Strongly oppose";
               else="DKNA"',
              as.factor = TRUE,
              levels = c("Strongly support", "Somewhat support",
                         "Somewhat oppose", "Strongly oppose",
                         "DKNA"))
}


#' Recode Likert item with SUPPORT scale (totals)
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeSupportNets <- function(x) {
  car::recode(x,
              '1:2="Support";3:4="Oppose"; else="DKNA"',
              as.factor = TRUE,
              levels = c("Support","Oppose", "DKNA"))
}



#' Recode Likert item with CONFIDENT scale (totals)
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeConfident <- function(x) {
  car::recode(x,
              '1="Very confident";  2="Somewhat confident";
               3="Not very confident"; 4="Not at all confident";
               else="DKNA"',
              as.factor = TRUE,
              levels = c("Very confident", "Somewhat confident",
                         "Not very confident", "Not at all confident",
                         "DKNA"))
}




#' Recode Likert item with CONFIDENT scale (totals)
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
makeConfidentNets <- function(x) {
  car::recode(x,
              '1:2="Very or somewhat";3:4="Not very or not at all"; else="DKNA"',
              as.factor = TRUE,
              levels = c("Very or somewhat", "Not very or not at all", "DKNA"))
}



#' Recode numerical 0 to 10 item
#'
#' @param x A vector or column of a data frame
#'
#' @returns A vector or column of a data frame
#' @export
#'
make0to10 <- function(x) {car::recode(x, 'NA=0; 99=NA')}





















#' Calculate SE of proportion
#'
#' @param prop Proportion for which SE is to be calculated (in decimal format)
#' @param n Sample size
#' @param ... Other arguments (not currently implemented)
#'
#' @returns A scalar denoting the SE
#' @export
#'
seProp <- function(prop, n, ...) {
  se <- sqrt( (prop * (1-prop)) / n)
  return(se)
}








#' Margin of error of proportion
#'
#' @param prop Proportion for which margin of error is to be calculated  (in decimal format)
#' @param n Sample size
#' @param ci Confidence level
#' @param ... Other arguments (not currently implemented)
#'
#' @returns A scalar denoting the SE
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







#' Significance of difference test for two column proportions
#'
#' @param a.prop Proportion for Group A (in decimal format)
#' @param a.n Size of Group A
#' @param b.prop Proportion for Group B (in decimal format)
#' @param b.n Size of Group B
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom stats na.omit pnorm
#'
#' @returns A data frame
#' @export
#'
testProps <- function(a.prop, a.n, b.prop, b.n, ...) {
  diff <- abs(a.prop - b.prop)
  diff.se <- sqrt( ((a.prop * (1-a.prop)) / a.n) +
                       ((b.prop * (1-b.prop)) / b.n) )
  diff.lwr95 <- diff - (1.96 * diff.se)
  diff.upr95 <- diff + (1.96 * diff.se)

  exp.prop <- ( (a.prop * a.n) + (b.prop * b.n) ) / (a.n + b.n)
  z.val <- (diff - 0) /
    sqrt( (exp.prop * (1-exp.prop)) * ((1/a.n) + (1/b.n)) )
  p.val <- 2 * stats::pnorm(abs(z.val), lower.tail = FALSE)
  # note: {R} pnorm(abs(x), lower.tail=F) == {Excel} norm.s.dist(x, TRUE)

  # pstar <- (a.prop + b.prop) / (a.n + b.n)
  # z.val <- (diff.se - 0) /
  #   sqrt( (pstar * (1-pstar)) * ((1/a.n) + (1/b.n)) )
  # p.val <- pnorm(z.val, lower.tail = FALSE)


  yates = ( ( (1 / a.n) + (1 / b.n) ) ) / 2

  zscore_yates = ( diff -  yates ) / sqrt( (exp.prop * (1-exp.prop)) * ((1/a.n) + (1/b.n)) )

  pvalue_yates = 2 * stats::pnorm(abs(zscore_yates), lower.tail = FALSE)


  out <- data.frame(
    difference = diff,
    se = diff.se,
    lwr95 = diff.lwr95,
    upr95 = diff.upr95,
    #expected = exp.prop,
    z = z.val,
    p = p.val,
    yates = yates,
    yates.z = zscore_yates,
    yates.p = pvalue_yates
  )
  return(out)
}




#' Difference of proportions test (unused)
#'
#' @param x1 Frequency of Group 1
#' @param n1 Total of Group 1
#' @param x2 Frequency of Group 2
#' @param n2 Total of Group 2
#' @param ... Other arguments passed through the function (not currently implemented)
#'
#' @importFrom stats na.omit pnorm
#'
#' @returns A data frame
#'
zTest <- function(x1, n1, x2, n2, ...) {

  diff <- abs((x1 / n1) - (x2 / n2))

  phatpooled <- (x1 + x2) / (n1 + n2)

  #denom <- sqrt( phatpooled * ( (1/n1) + (1/n2) ) )

  denom <- sqrt( (phatpooled * (1 - phatpooled)) *  ((1/n1) + (1/n2)) )

  zscore = diff / denom

  yates = ( ( (1/n1) + (1/n2) ) / 2 )

  zscore_yates = ( diff -  yates ) / denom

  pvalue = 2 * stats::pnorm(abs(zscore), lower.tail = FALSE)

  pvalue_yates = 2 * stats::pnorm(abs(zscore_yates), lower.tail = FALSE)

  out <- data.frame(
    difference = diff,
    #phatpooled = phatpooled,
    #denom = denom,
    zscore = zscore,
    pvalue = pvalue,
    yates = yates,
    zscore_yates = zscore_yates,
    pvalue_yates = pvalue_yates
  )

  return(out)

}


#zTest(138, 300, 165, 300)
#testProps(138/300, 300, 165/300, 300)




#sampleMoe <- function(n, ci = .95, prop = .5, ...) {
#
#  alpha = qnorm(1  - ((1-ci) / 2) )
#  # note: {R} qnorm() == {Excel} norm.s.inv()
#
#  moe = alpha * sqrt( (prop*(1-prop)) / n )
#
#  out <- data.frame(
#    n = n,
#    ci = ci,
#    prop = prop,
#    moe = moe
#  )
#
#  return(out)
#
#}
#
#sampleMoe(900)
#
#
#
#
#sampleMoe_ <- function(x) {
#
#  a1 =  0.254829592
#  a2 = -0.284496736
#  a3 =  1.421413741
#  a4 = -1.453152027
#  a5 =  1.061405429
#  p  =  0.3275911
#
#  t = 1.0 / (1.0 + p * abs(x))
#
#  y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-x*x)
#
#  return(y)
#
#}
#
#sampleMoe_(900/sqrt(2)/2)





#' range01
#'
#' @param x A numerical vector or column of a data frame/tibble to re-range from 0 to 1
#'
#'
#' @export
#'
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
#' @returns A survey design object
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





descTidysvy <- function(design,
                     varnames,
                     digits = 4,
                     ...) {

  # non-survey version
  # design %>%
  #   dplyr::summarise(
  #     mean = mean(!!sym(varname), na.rm = TRUE),
  #     sd = sd(!!sym(varname), na.rm = TRUE),
  #     se = sd(!!sym(varname), na.rm = TRUE) / sqrt(sum(!is.na(!!sym(varname)))),
  #     lower95 = mean - (1.96*se),
  #     upper95 = mean + (1.96*se))


  if(length(varnames)==1) {

    out <- survey::svymean(as.formula(paste0("~", varnames)),
                           design, na.rm = TRUE, round = TRUE) %>%
      as.data.frame() %>%
      rename("se" = 2) %>%
      mutate(lower95 = mean - (1.96*se),
             upper95 = mean + (1.96*se))

  } else {

      out <- lapply(1:length(varnames), function(i) {
        survey::svymean(as.formula(paste0("~", varnames[[i]])),
                        design, na.rm = TRUE, round = TRUE) %>%
          as.data.frame() %>%
          rename("se" = 2) %>%
          mutate(lower95 = mean - (1.96*se),
                 upper95 = mean + (1.96*se))
        })

      out <- dplyr::bind_rows(out)

    }

  return(out)

}




#' Detailed summary statistics of a vector
#'
#' @param x Vector on which to calculate summary statistics
#' @param digits Scalar indicating how many decimal points to return
#' @param convertFactors Boolean indicating whether to convert factors to numeric
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr rename
#' @importFrom tibble add_column add_row tibble
#' @importFrom stats as.formula median sd var
#'
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
    stats <- tibble::tibble(Value=c(class(x),
                            # round(sum(is.na(x))),
                            # paste0(round(sum(is.na(x)) / length(x) * 100, 2), "%"),
                            names(modal),
                            (marginals)[modal,1],
                            paste0(round((marginals)[modal,2], 2), "%"),
                            round(prod(marginals[,1]) /
                                    ( ( sum(marginals[,1]) / dim(marginals)[1] ) ^ dim(marginals)[1] ), digits)
    )
    )
    stats <- tibble::add_column(stats, Statistic = statnames, .before=1)
    marginals <- tibble::as_tibble(marginals) %>%
      tibble::add_column(Category = rownames(freqs), .before=.1) %>%
      tibble::add_row(Frequency = sum(marginals[,1]),
              Prop = sum(marginals[,2]))
    marginals <- as_tibble(marginals) %>%
      tibble::add_row(Frequency = sum(is.na(x))) %>%
      tibble::add_row(Frequency = length(x))
    marginals <- tibble::as_tibble(marginals) %>%
      tibble::add_column("% total" = round(marginals$Frequency / length(x) * 100, 2)) %>%
      dplyr::rename("% valid" = Prop)
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
    stats <- tibble::tibble(
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
    stats <- tibble::add_column(stats, Statistic = statnames, .before=1)
    descriptives <- list(stats = stats)
    descriptives
  }
}






