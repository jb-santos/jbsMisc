

#' ctab
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvar String denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @return A tibble
#'
#' @author John Santos
#'
#' @export
#'
#' @examples
#' library(survey)
#' data("ab")
#' absvy <- svydesign(~1, data = ab, weights = ~weight)
#' ctab(absvy, yvar = "dvote3", xvar = "region")
#'
ctab <- function(design,
                 yvar,
                 xvar,
                 ...) {UseMethod("ctab")}





#' ctab.survey.design
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvar String denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @return A tibble
#'
#' @author John Santos
#'
#' @export
#'
#' @examples
#' library(survey)
#' data("ab")
#' absvy <- svydesign(~1, data = ab, weights = ~weight)
#' ctab(absvy, yvar = "dvote3", xvar = "region")
#'
ctab.survey.design <- function(design,
                               yvar,
                               xvar, ...) {

  xt <- survey::svytable(as.formula(paste0("~", yvar, "+", xvar)),
                         design, round = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!sym(xvar)) %>%
    dplyr::mutate(pct =  round(n/sum(n), 4))

  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(!!sym(xvar) := "Total", .after = 1)

  out <- bind_rows(xt, totals)
  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))
  out[[yvar]] <- factor(out[[yvar]], levels = ylabs)
  out[[xvar]] <- factor(out[[xvar]],
                        levels = c(levels(droplevels(as.factor(design$variables[[xvar]]))),
                                   "Total"))
  out <- out %>% filter(!is.na(!!sym(yvar)))

  if(isFALSE(is.null(attr(design$variables[[yvar]], "label")))) {
    attr(out, "label") <- sjlabelled::get_label(design$variables[[yvar]])
  }
  attr(out, "yvar") <- yvar
  attr(out, "xvar") <- xvar
  return(out)

}




#' ctab.data.frame
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvar String denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @return A tibble
#'
#' @author John Santos
#'
#' @export
#'
#' @examples
#' data("ab")
#' ctab(ab, yvar = "dvote3", xvar = "region")
#'
ctab.data.frame <- function(design,
                            yvar,
                            xvar,
                            ...) {

  design <- srvyr::as_survey(design)

  xt <- survey::svytable(as.formula(paste0("~", yvar, "+", xvar)),
                         design, round = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!sym(xvar)) %>%
    dplyr::mutate(pct =  round(n/sum(n), 4))

  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(!!sym(xvar) := "Total", .after = 1)

  out <- bind_rows(xt, totals)
  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))
  out[[yvar]] <- factor(out[[yvar]], levels = ylabs)
  out[[xvar]] <- factor(out[[xvar]],
                        levels = c(levels(droplevels(as.factor(design$variables[[xvar]]))),
                                   "Total"))
  out <- out %>% filter(!is.na(!!sym(yvar)))

  if(isFALSE(is.null(attr(design$variables[[yvar]], "label")))) {
    attr(out, "label") <- sjlabelled::get_label(design$variables[[yvar]])
  }
  attr(out, "yvar") <- yvar
  attr(out, "xvar") <- xvar
  return(out)

}





#' ctabs
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvars String (or vector of strings) denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @return A list of class \code{ctabs} with dataframes:
#'   \describe{
#'     \item{\code{$Summary}}{The summary crosstab of every column variable
#'     against the row variable}
#'     \item{\code{$Total}}{The marginal frequencies and proportions of the row variable}
#'     \item{\code{$(name)}}{A grouped dataframe of the crosstab of the respective
#'     column variable against the row variable}
#'     }
#'
#' @author John Santos
#'
#' @export
#'
#' @examples
#' library(survey)
#' data("ab")
#' absvy <- svydesign(~1, data = ab, weights = ~weight)
#' table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "female", "age"))
#' table_vote1
#'
ctabs <- function(design,
                  yvar,
                  xvars, ...) {UseMethod("ctabs")}






#' ctabs.survey.design
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvars String (or vector of strings) denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @return A list of class \code{ctabs} with dataframes:
#'   \describe{
#'     \item{\code{$Summary}}{The summary crosstab of every column variable
#'     against the row variable}
#'     \item{\code{$Total}}{The marginal frequencies and proportions of the row variable}
#'     \item{\code{$(name)}}{A grouped dataframe of the crosstab of the respective
#'     column variable against the row variable}
#'     }
#'
#' @author John Santos
#'
#' @export
#'
#' @examples
#' library(survey)
#' data("ab")
#' absvy <- svydesign(~1, data = ab, weights = ~weight)
#' table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "female", "age"))
#' table_vote1
#'
ctabs.survey.design <- function(design,
                                yvar,
                                xvars, ...) {

  # Setup
  ylabs <- levels(as.factor(design$variables[[yvar]]))
  xlabs <- lapply(1:length(xvars), function(i) {
    levels(as.factor(design$variables[[xvars[[i]]]]))
  })
  myforms <- lapply(1:length(xvars), function(i) {
    as.formula(paste0("~", yvar, "+", xvars[[i]]))
  })

  # Marginals table
  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(Total := "Total", .after = 1) %>%
    dplyr::mutate(Total = factor(Total, levels = "Total"))
  totals[[yvar]] <- ylabs
  totals[[yvar]] <- factor(totals[[yvar]], levels = ylabs)

  # Crosstabs
  xt_list <- lapply(1:length(xvars), function(i) {
    survey::svytable(myforms[[i]], design, round = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::group_by(!!sym(xvars[[i]])) %>%
      dplyr::mutate(pct =  round(n/sum(n), 4),
                    !!sym(yvar) := ylabs,
                    !!sym(xvars[[i]]) := !!sym(xvars[[i]])) %>%
      dplyr::mutate(!!sym(yvar) := factor(!!sym(yvar), levels = ylabs),
                    !!sym(xvars[[i]]) := factor(!!sym(xvars[[i]]), levels = xlabs[[i]]))
  })
  names(xt_list) <- xvars

  # Create omnibus summary table by pivoting all crosstabs and then add marginals
  omnixt <- lapply(1:length(xt_list), function(i) {
    xt_list[[i]] <- xt_list[[i]] %>%
      dplyr::select(-n) %>%
      tidyr::pivot_wider(names_from = !!sym(xvars[[i]]), values_from = pct)
  })
  omnixt <- do.call("cbind", omnixt)
  omnixt <- omnixt[!duplicated(as.list(omnixt))] %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Total = totals$pct, .after = 1)

  # Assemble output
  out <- list(
    "Summary" = omnixt,
    "Total" = totals)
  out <- append(out, xt_list)
  out[[1]] <- out[[1]] %>% filter(Total != 0)
  for(i in 2:length(out)) {out[[i]] <- out[[i]] %>% filter(n != 0)}

  if(isFALSE(is.null(attr(design$variables[[yvar]], "label")))) {
    attr(out, "label") <- sjlabelled::get_label(design$variables[[yvar]])
  }

  attr(out, "class") <- "ctabs"
  attr(out, "yvar") <- yvar
  attr(out, "xvars") <- xvars
  return(out)

}




# print.grouped_df <- function(x, tblno = NULL, digits = 0, ...) {
#
#   out <- x[[tblno]]
#   xlevs <- unique(out[[1]])
#   out <- out %>%
#     dplyr::select(-n) %>%
#     tidyr::pivot_wider(names_from = 2, values_from = pct) %>%
#     select(-1)
#   out <- round(out * 100, digits)
#   out <- apply(out, c(1,2), function(x) paste0(x,"%")) %>%
#     tibble::as_tibble() %>%
#     mutate(!!sym(names(x[[tblno]][1])) := xlevs, .before = 1)
#
#   print(out)
#
# }




#' ctabs.data.frame
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvars String (or vector of strings) denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @return A list of class \code{ctabs} with dataframes:
#'   \describe{
#'     \item{\code{$Summary}}{The summary crosstab of every column variable
#'     against the row variable}
#'     \item{\code{$Total}}{The marginal frequencies and proportions of the row variable}
#'     \item{\code{$(name)}}{A grouped dataframe of the crosstab of the respective
#'     column variable against the row variable}
#'     }
#'
#' @author John Santos
#'
#' @export
#'
#' @examples
#' data("ab")
#' table_vote1 <- ctabs(ab, yvar = "dvote3", xvars = c("region", "female", "age"))
#' table_vote1
#'
ctabs.data.frame <- function(design,
                             yvar,
                             xvars, ...) {

  # The ctabs.data.frame is actually the same as ctabs.survey.design
  # To avoid having to write new code (because I'm lazy AF), we'll use srvyr::as_survey()
  # to create a survey design object with (weight==1) and then re-use the same code.
  design <- srvyr::as_survey(design)

  # Setup
  ylabs <- levels(as.factor(design$variables[[yvar]]))
  xlabs <- lapply(1:length(xvars), function(i) {
    levels(as.factor(design$variables[[xvars[[i]]]]))
  })
  myforms <- lapply(1:length(xvars), function(i) {
    as.formula(paste0("~", yvar, "+", xvars[[i]]))
  })

  # Marginals table
  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(Total := "Total", .after = 1) %>%
    dplyr::mutate(Total = factor(Total, levels = "Total"))
  totals[[yvar]] <- ylabs
  totals[[yvar]] <- factor(totals[[yvar]], levels = ylabs)

  # Crosstabs
  xt_list <- lapply(1:length(xvars), function(i) {
    survey::svytable(myforms[[i]], design, round = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::group_by(!!sym(xvars[[i]])) %>%
      dplyr::mutate(pct =  round(n/sum(n), 4),
                    !!sym(yvar) := ylabs,
                    !!sym(xvars[[i]]) := !!sym(xvars[[i]])) %>%
      dplyr::mutate(!!sym(yvar) := factor(!!sym(yvar), levels = ylabs),
                    !!sym(xvars[[i]]) := factor(!!sym(xvars[[i]]), levels = xlabs[[i]]))
  })
  names(xt_list) <- xvars

  # Create omnibus summary table by pivoting all crosstabs and then add marginals
  omnixt <- lapply(1:length(xt_list), function(i) {
    xt_list[[i]] <- xt_list[[i]] %>%
      dplyr::select(-n) %>%
      tidyr::pivot_wider(names_from = !!sym(xvars[[i]]), values_from = pct)
  })
  omnixt <- do.call("cbind", omnixt)
  omnixt <- omnixt[!duplicated(as.list(omnixt))] %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Total = totals$pct, .after = 1)

  # Assemble output
  out <- list(
    "Summary" = omnixt,
    "Total" = totals)
  out <- append(out, xt_list)
  out[[1]] <- out[[1]] %>% filter(Total != 0)
  for(i in 2:length(out)) {out[[i]] <- out[[i]] %>% filter(n != 0)}

  if(isFALSE(is.null(attr(design$variables[[yvar]], "label")))) {
    attr(out, "label") <- sjlabelled::get_label(design$variables[[yvar]])
  }

  attr(out, "class") <- "ctabs"
  attr(out, "yvar") <- yvar
  attr(out, "xvars") <- xvars
  return(out)

}



#' Print method for ctabs objects
#'
#' @description Prints the summary crosstab table from a ctabs list object. It
#' automatically converts decimals to percentages.
#'
#' @param x An object of class "ctabs" produced by `ctabs()`
#' @param digits Scalar indicating how many decimal places to display (default 0)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export
#'
print.ctabs <- function(x,
                        digits = 0,
                        ...) {

  out <- x[[1]] %>% as.data.frame()
  rownames(out) <- out[[1]]
  out <- out %>% select(-1)
  out <- round(out * 100, digits)
  out <- apply(out, c(1,2), function(x) paste0(x,"%")) %>% as.data.frame()

  if(isFALSE(is.null(attr(x, "label")))) attr(x, "label")
  out <- tibble::as_tibble(out, rownames = attr(x, "yvar"))

  print(out)

}



#' plot method for ctabs objects
#'
#' @description Plots outputs from `ctabs()` using `ggplot()`. By default, it
#' plots the summary table. But, using the `tblno` option, you can print out any
#' of the tables in a "ctabs" list object.
#'
#' Because this is a wrapper function for `ggplot()`, you can add additional
#' plot options using the + command.
#'
#' @param x Object of class "ctabs" created by `ctabs()`
#' @param tblno Integer indicating which table from the ctabs list should be printed
#' @param dodge Boolean indicating whether bars should be dodged (default FALSE)
#' @param txt Boolean indicating whether text labels for percentages should be dodged (default FALSE)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs scale_y_continuous position_stack position_dodge
#' @importFrom scales percent
#'
#' @export
#'
#' @examples
#' library(survey)
#' data("ab")
#' absvy <- svydesign(~1, data = ab, weights = ~weight)
#' table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "female", "age"))
#' table_vote1
#' library(ggplot2)
#' plot(table_vote1, dodge = FALSE, txt = TRUE) +
#' scale_fill_manual(values = c("#184484", "#f37221", "#bbbbbb")) +
#'   labs(title = "Alberta vote intention",
#'        y = "",
#'        x = "Sub-group",
#'        fill = "Party") +
#'   theme_classic()
#'
plot.ctabs <- function(x,
                       tblno = 1,
                       dodge = FALSE,
                       txt = FALSE,
                       ...) {

  if(tblno == 1) {

    plotdat <- x[[tblno]]
    plotdat <- plotdat %>%
      pivot_longer(-1)
    plotdat$name <- factor(plotdat$name,
                           levels = unique(plotdat$name))
    plotdat

    p <- ggplot2::ggplot(plotdat) +
      aes(x = name,
          y = value,
          fill = .data[[attr(x, "yvar")]],
          label = paste0(round(value*100), "%")) +
      labs(title = paste("Summary crosstab for", attr(x, "yvar")),
           x = "Sub-groups",
           y = "%")

  }

  if(tblno == 2) {

    plotdat <- x[[tblno]]
    p <- ggplot2::ggplot(plotdat) +
      aes(x = Total,
          y = pct,
          fill = .data[[attr(x, "yvar")]],
          label = paste0(round(pct*100), "%")) +
      labs(title = paste("Frequency distribution for", attr(x, "yvar")),
           x = " ",
           y = "%")

  }

  if(tblno > 2) {

    plotdat <- x[[tblno]]
    p <- ggplot2::ggplot(plotdat) +
      aes(x = !!sym(names(plotdat[2])),
          y = pct,
          fill = .data[[attr(x, "yvar")]],
          label = paste0(round(pct*100), "%")) +
      labs(title = paste(attr(x, "yvar"), "by", names(plotdat[2])),
           x = "Sub-groups",
           y = "%")

  }

  if(isTRUE(dodge)) {
    p <- p +
      geom_bar(stat = "identity", position = position_dodge(width = .9)) +
      scale_y_continuous(labels = scales::percent)

    if(isTRUE(txt)) {
      p <- p +
        geom_text(size = 3.5, position = position_dodge(width = .9), vjust = -.5)
    }

  } else {
    p <- p +
      geom_bar(stat = "identity", position = position_stack()) +
      scale_y_continuous(labels = NULL, breaks = NULL)

    if(isTRUE(txt)) {
      p <- p +
        geom_text(size = 3.5, position = position_stack(vjust = .5))
    }

  }

  return(p)

}











likertnets <- function(obj, ...) {

  xvars <- names(obj[2:length(obj)])
  yvar <- names(obj[[1]][1])

  xt_list <- obj[2:length(obj)]
  xt_list <- lapply(1:length(xt_list), function(i){
    xt_list[[i]] %>%
      dplyr::select(-pct) %>%
      tidyr::pivot_wider(names_from = !!sym(yvar),
                         values_from = n) %>%
      dplyr::mutate(`Agree` = `Strongly agree` + `Somewhat agree`,
                    Disagree = `Strongly disagree` + `Somewhat disagree`) %>%
      dplyr::select(-c(`Strongly agree`,`Somewhat agree`, `Strongly disagree`, `Somewhat disagree`)) %>%
      tidyr::pivot_longer(-1) %>%
      dplyr::rename(n = value) %>%
      dplyr::group_by(!!sym(xvars[[i]])) %>%
      dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
      dplyr::rename(!!sym(yvar) := name) %>%
      dplyr::relocate(2, .before = 1) # %>% dplyr::select(-n) %>% tidyr::pivot_wider(names_from = 1, values_from = pct)
  })
  names(xt_list) <- xvars

  # pivoting to create omnibus summary crosstab
  omnixt <- lapply(1:length(xt_list), function(i) {
    xt_list[[i]] <- xt_list[[i]] %>%
      dplyr::select(-n) %>%
      tidyr::pivot_wider(names_from = !!sym(xvars[[i]]),
                         values_from = pct)
  })
  omnixt <- do.call("cbind", omnixt)
  omnixt <- omnixt[!duplicated(as.list(omnixt))] %>% tibble::as_tibble()
  total_pct <- xt_list[[1]] %>%
    dplyr::select(-n)  %>% tidyr::pivot_wider(names_from = 2, values_from = pct)
  omnixt <- omnixt %>%
    dplyr::mutate(Total = total_pct$Total, .after = 1) %>%
    dplyr::rename(!!sym(yvar) := 1)

  out <- list("Summary Table" = omnixt)
  out <- append(out, xt_list)

  for(i in 1:length(out)) {
    out[[i]] <- dplyr::bind_rows(obj[[i]], out[[i]]) %>%
      unique()
  }
  for(i in 2:length(out)) {
    out[[i]] <- out[[i]] %>%
      dplyr::arrange(!!sym(names(out)[[i]]))
  }

  class(out) <- "ctabs"
  return(out)
}




#' testcols
#'
#' @param obj An object of class "ctabs" created with the `ctabs()` function
#' @param adj.p Boolean indicating whether to apply Holm correction to p-values (default TRUE)
#' @param siglevel Numeric indicating significance level to use (default .05)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @return A list of class \code{ctabs.test} with one dataframe per crosstab of
#' the respective column variable against the row variable. Each crosstab has
#' frequencies, column proportions, and letters indicating significant differences
#' of proportions between pairs of columns.
#'
#' @author John Santos
#'
#' @export
#'
#' @examples
#' library(survey)
#' data("ab")
#' absvy <- svydesign(~1, data = ab, weights = ~weight)
#' table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "female", "age"))
#' table_vote1_tests <- testcols(table_vote1)
#' table_vote1_tests
#'
testcols <- function(obj,
                      adj.p = FALSE,
                      siglevel = .05,
                      ...)  {UseMethod("testcols")}



#' testcols.ctabs
#'
#' @description This is (mainly) a wrapper around \code{rstatix::row_wise_prop_test}.
#' It takes a \code{ctabs} list of crosstabs, tests for all pairwise differences
#' and outputs a list of data frames that merge the frequencies/percentages with
#' significantly different column proportions being marked with letters.
#'
#' @param obj An object of class "ctabs" created with the `ctabs()` function
#' @param adj.p Boolean indicating whether to adjust for multiple comparisons
#' @param siglevel Numeric indicating significance level to use (default .05)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom stringr str_detect str_remove_all str_split_i
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @return A list of class \code{ctabs.test} with one dataframe per crosstab of
#' the respective column variable against the row variable. Each crosstab has
#' frequencies, column proportions, and letters indicating significant differences
#' of proportions between pairs of columns.
#'
#' @author John Santos
#'
#' @export
#'
testcols.ctabs <- function(obj,
                           adj.p = TRUE,
                           siglevel = .05,
                           ...) {

  # Setup ----------------------------------------------------------------------

  if(isTRUE(adj.p)) {
    pval <- "p.adj"
  } else {
    pval <- "p"
  }

  xvars <- names(obj[3:length(obj)])
  yvar <- names(obj[[1]][1])
  ylevs <- levels(obj[[3]][[1]])

  xt_list <- obj[3:length(obj)]
  names(xt_list) <- xvars


  # Build list of pairwise column tests ----------------------------------------

  out <- NULL
  testdfs <- NULL
  finaltbls <- NULL

  for(x in 1:length(xt_list)) {


    # Create data frame for one bivariate crosstab

    dat <- xt_list[[x]] %>%
      dplyr::select(-pct) %>%
      tidyr::pivot_wider(names_from = 2, values_from = n) %>%
      as.data.frame()
    rownames(dat) <- dat[[1]]
    dat <- dat %>% dplyr::select(-1)


    # Create list of pairwise comparisons

    pairlist <- combn(names(dat), 2, function(i) {
      d <- dat[i]
      names(d) <- i
      d
    },
    simplify = FALSE)

    names(pairlist) <- sapply(1:length(pairlist), function(i) {
      paste(colnames(pairlist[[i]][1]),"-",colnames(pairlist[[i]][2]))
    })


    # Test differences

    pairlist_tests <- lapply(1:length(pairlist), function(i) {
      rstatix::row_wise_prop_test(pairlist[[i]], correct = TRUE, detailed = TRUE)
    })

    pairlist_pvals <- lapply(1:length(pairlist), function(i) {
      pairlist_tests[[i]][[pval]]
    })

    pairlist_sig <- NULL
    for(i in 1:length(pairlist_pvals)) {
      pairlist_sig[[i]] <- ifelse(pairlist_pvals[[i]] < siglevel, 1, 0)
    }
    names(pairlist_sig) <- names(pairlist)

    # # Old output function
    # sigdiffs <- do.call("cbind", pairlist_sig)
    # rownames(sigdiffs) <- rownames(dat)
    # out[[x]] <- as.data.frame(sigdiffs)


    ## Assemble single output table --------------------------------------------

    xlevs <- levels(xt_list[[x]][[2]])


    # Create column letter scheme

    combs <- combn(LETTERS[1:length(xlevs)], 2) %>%
      as.data.frame()

    pairnames <- sapply(1:ncol(combs), function(i) {
      paste0((combs[[i]][1]),(combs[[i]][2]))
    })
    names(pairlist_sig) <- pairnames


    # Identify significant differences and collapse to list

    pairlist_sig <- lapply(1:length(pairlist_sig), function(k) {
      sapply(1:length(pairlist_sig[[k]]), function(j) {
        ifelse(pairlist_sig[[k]][[j]] == 1,
               pairlist_sig[[k]][[j]] <- names(pairlist_sig)[[k]],
               "")
      })
    })
    names(pairlist_sig) <- names(pairlist)

    sigdiffs <- do.call("cbind", pairlist_sig)
    rownames(sigdiffs) <- rownames(dat)


    # Create list of significant differences; will be used to populate summary table later

    siglist <- lapply(1:nrow(dat), function(i) {
      as.list(sigdiffs[i,])
    })
    names(siglist) <-  rownames(dat)


    # Create blank summary table

    diffs_summary <- dat
    rownames(diffs_summary) <- rownames(dat)
    colnames(diffs_summary) <- LETTERS[1:length(xlevs)]
    diffs_summary <- apply(diffs_summary, c(1,2), function(x) x <- "")


    # Loop to mark all pairs of significant differences

    i <- 1
    j <- 1
    k <- 1
    for(i in 1:nrow(diffs_summary)) {
      while(j <= ncol(diffs_summary)) {
        while(k <= length(siglist[[i]])) {
          ifelse(str_detect(siglist[[i]][[k]], colnames(diffs_summary)[[j]]),
                 diffs_summary[i,j] <- paste0(diffs_summary[i,j], siglist[[i]][[k]]),
                 "")
          k <- k + 1
        }
        k <- i
        j <- j + 1
      }
      j <- 1
      i <- i + 1
    }
    diffs_summary <- as.data.frame(diffs_summary)


    # Above marks a column with its own letter; below removes that redundant letter

    diffs_summary1 <- sapply(1:ncol(diffs_summary), function(j) {
      stringr::str_remove_all(diffs_summary[[j]], colnames(diffs_summary)[[j]])
    })
    diffs_summary1 <- as.data.frame(diffs_summary1)


    # Add rownames to facilitate merging with table of freqs/pcts
    rownames(diffs_summary1) <- sapply(1:nrow(dat), function(i) {
      paste0(rownames(dat)[[i]], "__sig")
    })
    colnames(diffs_summary1) <- xlevs


    ## Simple output
    #dat_pct <- as.matrix(dat) %>%
    #  prop.table(margin = 2) %>%
    #  round(., digits = 4)
    #dat_pct <- apply(dat_pct, c(1,2), function(x) paste0(round(x*100),"%")) %>%
    #  as.data.frame()
    #
    #finaltbl <- rbind(dat_pct, diffs_summary1)
    #finaltbl <- finaltbl[order(row.names(finaltbl)), ]


    # First create output table with n AND pct

    temp <- xt_list[[x]] %>%
      dplyr::mutate(pct = paste0(round(pct*100),"%"),
                    n = as.character(n)) %>%
      tidyr::pivot_longer(c(n,pct)) %>%
      tidyr::pivot_wider(names_from = 2, values_from = value) %>%
      dplyr::mutate(rwname = paste0(!!sym(yvar), "__", name)) %>%
      dplyr::select(-c(1,2)) %>% as.data.frame()
    rownames(temp) <- temp$rwname
    temp <- temp %>% dplyr::select(-rwname)


    # Merge n/pct table with significant differences table

    finaltbl <- dplyr::bind_rows(temp, diffs_summary1) %>%
      tibble::rownames_to_column("Response") %>%
      dplyr::mutate(Stat = stringr::str_split_i(Response, "__", 2), .after = Response,
             Response = stringr::str_split_i(Response, "__", 1))
    finaltbl$Response <- factor(finaltbl$Response, levels = ylevs)
    finaltbl <- finaltbl %>%
      dplyr::arrange(Response)
    finaltbl$Response <- as.character(finaltbl$Response)
    finaltbl <- finaltbl %>%
      dplyr::group_by(Response) %>%
      dplyr::mutate(Response = replace(Response, duplicated(Response), " "))

    out[[x]] <- finaltbl
    attr(out[[x]], "xvar") <- xvars[[x]]
    attr(out[[x]], "xlevs") <- xlevs
    attr(out[[x]], "siglabs") <- colnames(diffs_summary)
  }

  # Assemble final output object -----------------------------------------------

  attr(out, "ctabs.test")  <- TRUE
  attr(out, "yvar") <- names(obj[[1]][1])
  attr(out, "ylevs") <- levels(obj[[3]][[1]])
  attr(out, "siglabs") <- sapply(1:length(out), function(x) attr(out[[x]], "siglabs"))
  names(out) <- xvars
  return(out)
}




#' Print method for ctabs.test objects
#'
#' @description Prints dataframes within ctabs.test list object and adds column
#' letters marking significantly different column proportions.
#'
#' @param x An object of class "ctabs.test" produced by `ctabs()`
#' @param ... Other arguments (not currently implemented)
#'
#' @export
#'
print.ctabs.test <- function(x,
                             ...) {

  x <- lapply(1:length(x), function(i) {
    mycols <- c("", "", paste0("(",LETTERS[1:(ncol(x[[i]])-2)],")"))
    names(mycols) <- colnames(x[[i]])
    x[[i]] <- bind_rows(mycols, x[[i]])
  })
  for(i in 1:length(x)) print(as.data.frame(x[[i]]))
}








#' format_ctabs
#'
#' @param obj An object of class "ctabs.test" created with the \code{testcols} function.
#' @param ... Other arguments (not currently implemented)
#'
#' @return A tibble
#'
#' @importFrom dplyr bind_cols select
#' @importFrom magrittr %>%
#' @importFrom tidyselect starts_with
#'
#' @return A tibble
#'
#' @author John Santos
#'
#' @export
#'
ctabs_format <- function(obj, ...) {

  labelcols <- obj[[1]] %>% select(c(1,2))
  siglabs <- sapply(1:length(obj), function(x) attr(obj[[x]], "siglabs"))
  xvars <- sapply(1:length(obj), function(x) attr(obj[[x]], "xvar"))
  xlevs <- sapply(1:length(obj), function(x) attr(obj[[x]], "xlevs"))


  out <- dplyr::bind_cols(obj) %>%
    dplyr::select(-c(tidyselect::starts_with("Response..."), tidyselect::starts_with("Stat...")))

  out <- dplyr::bind_cols(labelcols, out)

  temp <- c(unlist(siglabs))
  temp <- c("", "", paste0("(",temp,")"))
  names(temp) <- colnames(out)
  out <- rbind(temp, out)

  attr(out, "ctabs.fmt") <- TRUE
  attr(out, "yvar") <- attr(obj, "yvar")
  attr(out, "ylevs") <- attr(obj, "ylevs")
  attr(out, "xvars") <- xvars
  attr(out, "xlevs") <- xlevs
  attr(out, "siglabs") <- siglabs

  return(out)
}





#' Print objects outputted by \code{testcols} and \code{format_ctabs} using \code{kable}
#'
#' @param obj An list created with \code{testcols} or a data frame created with
#' \code{format_ctabs}.
#' @param xvarnames A vector of character strings that replaces the names of your
#' column variables. (Default \code{NULL}; i.e. the function uses the variable
#' names as they appear in \code{R}.) Useful if you want to specify nicely
#' formatted variable names..
#' @param ... Other arguments (not currently implemented).
#'
#' @return A formatted \code{kable} table
#'
#' @author John Santos
#'
#'
#' @importFrom dplyr slice
#' @importFrom kableExtra add_header_above column_spec kable_classic kbl row_spec
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom tibble rownames_to_column
#' @importFrom utils combn
#'
#' @export
#'
ctabs_kbl <- function(obj, xvarnames = NULL, ...) {UseMethod("ctabs_kbl")}



#' Print objects outputted by \code{format_ctabs} using \code{kable}
#'
#' @param obj An list created with \code{testcols} or a data frame created with
#' \code{format_ctabs}.
#' @param xvarnames A vector of character strings that replaces the names of your
#' column variables. (Default \code{NULL}; i.e. the function uses the variable
#' names as they appear in \code{R}.) Useful if you want to specify nicely
#' formatted variable names..
#' @param ... Other arguments (not currently implemented).
#'
#' @return A formatted \code{kable} table
#'
#' @author John Santos
#'
#'
#' @importFrom dplyr slice
#' @importFrom kableExtra add_header_above column_spec kable_classic kbl row_spec
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom tibble rownames_to_column
#' @importFrom utils combn
#'
#' @export
#'
ctabs_kbl.data.frame <- function(obj, xvarnames = NULL, ...)  {

  if(is.null(attr(obj, "ctabs.fmt"))) {
    stop(print("This function only works with data frames created with format_ctabs(), or lists of data frames creates with testcols()."))}


  # if lengths don't match, you're gonna have a bad time
  if(!is.null(xvarnames) &
     length(xvarnames) != length(attr(obj, "xvars"))) {
    stop(print("xvarnames must have the same length the number of column variables in your ctabs"))}

  # boring setup crap
  siglabs <- unlist(attr(obj, "siglabs"))
  siglabsb <- sapply(1:length(siglabs), function(x) paste0("(",siglabs[[x]],")"))
  xlevs <- attr(obj, "xlevs")

  ifelse(!is.null(xvarnames),
         xvars <- xvarnames,
         xvars <- attr(obj, "xvars")
  )

  # main function starts here
  obj <- dplyr::slice(obj, -1)   # b/c input table for this function has column letters for sigdiffs
  out <- kableExtra::kbl(obj,
                         col.names = c("", "", siglabsb),
                         align = c("l", "c", rep("r", ncol(obj)-2))) %>%
    kableExtra::kable_classic() %>%
    # kableExtra::row_spec(rep(3, length(attr(obj, "ylevs"))), extra_css = "border-bottom: 1px solid") %>%
    kableExtra::column_spec(c(2,2+cumsum(lengths(xlevs))), border_right = TRUE) %>%
    kableExtra::row_spec(0, align = "c") %>%
    kableExtra::add_header_above(data.frame(names = c("", "", unlist(xlevs)),
                                            colspan = rep(1, ncol(obj)))) %>%
    kableExtra::add_header_above(data.frame(names = c("", xvars),
                                            colspan = c(2, lengths(xlevs))))

  # assemble output
  # attr(out, "ctabs.kbl") <- TRUE
  attr(out, "siglabs") <- attr(obj, "siglabs")
  attr(out, "xvars") <- attr(obj, "xvars")
  attr(out, "xlevs") <- xlevs
  return(out)
}



#' Print objects outputted by \code{testcols} using \code{kable}
#'
#' @param obj An list created with \code{testcols} or a data frame created with
#' \code{format_ctabs}.
#' @param xvarnames A vector of character strings that replaces the names of your
#' column variables. (Default \code{NULL}; i.e. the function uses the variable
#' names as they appear in \code{R}.) Useful if you want to specify nicely
#' formatted variable names..
#' @param ... Other arguments (not currently implemented).
#'
#' @return A formatted \code{kable} table
#'
#' @author John Santos
#'
#'
#' @importFrom dplyr slice
#' @importFrom kableExtra add_header_above column_spec kable_classic kbl row_spec
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom tibble rownames_to_column
#' @importFrom utils combn
#'
#' @export
#'
ctabs_kbl.list <- function(obj, xvarnames = NULL, ...)  {

  if(is.null(attr(obj, "ctabs.test"))) {
    stop(print("This function only works with lists of data frames creates with testcols() or data frames created with format_ctabs()."))}


  # First, recreate format_ctabs() function ------------------------------------

  labelcols <- obj[[1]] %>% select(c(1,2))
  siglabs <- sapply(1:length(obj), function(x) attr(obj[[x]], "siglabs"))
  xvars <- sapply(1:length(obj), function(x) attr(obj[[x]], "xvar"))
  xlevs <- sapply(1:length(obj), function(x) attr(obj[[x]], "xlevs"))


  out <- dplyr::bind_cols(obj) %>%
    dplyr::select(-c(tidyselect::starts_with("Response..."), tidyselect::starts_with("Stat...")))

  out <- dplyr::bind_cols(labelcols, out)

  temp <- c(" ", " ", unlist(siglabs))
  names(temp) <- colnames(out)
  out <- rbind(temp, out)


  # Second, duplicate process from kbl_ctabs.data.frame() method ---------------

  # if lengths don't match, you're gonna have a bad time
  if(!is.null(xvarnames) &
     length(xvarnames) != length(obj)) {
    stop(print("xvarnames must have the same length the number of column variables in your ctabs"))}

  # boring setup crap
  siglabs <- unlist(attr(obj, "siglabs"))
  siglabsb <- sapply(1:length(siglabs), function(x) paste0("(",siglabs[[x]],")"))

  ifelse(!is.null(xvarnames),
         xvars <- xvarnames,
         xvars <- names(obj)
  )

  # main function starts here
  out <- dplyr::slice(out, -1)   # b/c input table for this function has column letters for sigdiffs
  out2 <- kableExtra::kbl(out,
                         col.names = c("", "", siglabsb),
                         align = c("l", "c", rep("r", ncol(out)-2))) %>%
    kableExtra::kable_classic() %>%
    # kableExtra::row_spec(rep(3, length(attr(obj, "ylevs"))), extra_css = "border-bottom: 1px solid") %>%
    kableExtra::column_spec(c(2,2+cumsum(lengths(xlevs))), border_right = TRUE) %>%
    kableExtra::row_spec(0, align = "c") %>%
    kableExtra::add_header_above(data.frame(names = c("", "", unlist(xlevs)),
                                            colspan = rep(1, ncol(out)))) %>%
    kableExtra::add_header_above(data.frame(names = c("", xvars),
                                            colspan = c(2, lengths(xlevs))))

  # assemble output
  # attr(out, "ctabs.kbl") <- TRUE
  attr(out2, "yvar") <- attr(obj, "yvar")
  attr(out2, "ylevs") <- attr(obj, "ylevs")
  attr(out2, "xvars") <- xvars
  attr(out2, "xlevs") <- xlevs
  attr(out2, "siglabs") <- siglabs
  return(out2)
}




















#' testcols_
#'
#' @param obj An object of class "ctabs" created with the `ctabs()` function
#' @param adj.p Boolean indicating whether to apply Holm correction to p-values (default TRUE)
#' @param siglevel Numeric indicating significance level to use (default .05)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @export
#'
#' @examples
#' library(survey)
#' data("ab")
#' absvy <- svydesign(~1, data = ab, weights = ~weight)
#' table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "female", "age"))
#' table_vote1_tests <- testcols_(table_vote1)
#' table_vote1_tests
#'
testcols_ <- function(obj,
                     adj.p = FALSE,
                     siglevel = .05,
                     ...)  {UseMethod("testcols_")}



#' testcols_.ctabs
#'
#' @param obj An object of class "ctabs" created with the `ctabs()` function
#' @param adj.p Boolean indicating whether to adjust for multiple comparisons
#' @param siglevel Numeric indicating significance level to use (default .05)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @export
#'
testcols_.ctabs <- function(obj,
                            adj.p = TRUE,
                            siglevel = .05,
                            ...) {

  if(isTRUE(adj.p)) {
    pval <- "p.adj"
  } else {
    pval <- "p"
  }

  xvars <- names(obj[3:length(obj)])
  yvar <- names(obj[[1]][1])

  xt_list <- obj[3:length(obj)]
  names(xt_list) <- xvars

  out <- NULL
  details <- NULL
  for(x in 1:length(xt_list)) {

    dat <- xt_list[[x]] %>%
      dplyr::select(-pct) %>%
      tidyr::pivot_wider(names_from = 2, values_from = n) %>%
      as.data.frame()
    rownames(dat) <- dat[[1]]
    dat <- dat %>% dplyr::select(-1)

    pairlist <- combn(names(dat), 2, function(i) {
      d <- dat[i]
      names(d) <- i
      d
    },
    simplify = FALSE)

    names(pairlist) <- sapply(1:length(pairlist), function(i) {
      paste(colnames(pairlist[[i]][1]),"-",colnames(pairlist[[i]][2]))
    })
    pairlist_tests <- lapply(1:length(pairlist), function(i) {
      rstatix::row_wise_prop_test(pairlist[[i]], correct = TRUE, detailed = TRUE)
    })
    # names(pairlist_tests) <- names(pairlist)
    pairlist_pvals <- lapply(1:length(pairlist), function(i) {
      pairlist_tests[[i]][[pval]]
    })
    # names(pairlist_pvals) <- names(pairlist)
    pairlist_sig <- NULL
    for(i in 1:length(pairlist_pvals)) {
      pairlist_sig[[i]] <- ifelse(pairlist_pvals[[i]] < siglevel, "*", "")
    }
    names(pairlist_sig) <- names(pairlist)
    sigdiffs <- do.call("cbind", pairlist_sig)
    rownames(sigdiffs) <- rownames(dat)

    out[[x]] <- as.data.frame(sigdiffs)
    details[[x]] <- pairlist_tests
  }

  names(out) <- xvars
  names(details) <- xvars

  output <- list(
    Summary = out,
    Details = details
  )
  # attr(output, "class")  <- "ctabs.test_"
  return(output)
}




#' Print method for ctabs.test_ objects
#'
#' @description Prints only the summary tables from a ctabs.test_ list object.
#'
#' @param x An object of class "ctabs.test" produced by `ctabs()`
#' @param ... Other arguments (not currently implemented)
#'
#' @export
#'
print.ctabs.test_ <- function(x,
                              ...) {
  print(x[[1]])
}
