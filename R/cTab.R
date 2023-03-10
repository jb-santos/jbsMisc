

#' ctab
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvar String denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange as_tibble bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @export
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
#' @export
#'
ctab.survey.design <- function(design,
                               yvar,
                               xvar, ...) {

  xt <- survey::svytable(as.formula(paste0("~", yvar, "+", xvar)),
                         design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(!!sym(xvar)) %>%
    dplyr::mutate(pct =  round(n/sum(n), 4))

  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(!!sym(xvar) := "Total", .after = 1)

  out <- bind_rows(xt, totals)
  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))
  out[[yvar]] <- factor(out[[yvar]], levels = ylabs)
  out[[xvar]] <- factor(out[[xvar]],
                        levels = c(levels(droplevels(as.factor(design$variables[[xvar]]))),
                                   "Total"))

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
#' @export
#'
ctab.data.frame <- function(design,
                            yvar,
                            xvar,
                            ...) {

  design <- srvyr::as_survey(design)

  xt <- survey::svytable(as.formula(paste0("~", yvar, "+", xvar)),
                         design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(!!sym(xvar)) %>%
    dplyr::mutate(pct =  round(n/sum(n), 4))

  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(!!sym(xvar) := "Total", .after = 1)

  out <- bind_rows(xt, totals)
  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))
  out[[yvar]] <- factor(out[[yvar]], levels = ylabs)
  out[[xvar]] <- factor(out[[xvar]],
                        levels = c(levels(droplevels(as.factor(design$variables[[xvar]]))),
                                   "Total"))

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
#' @importFrom dplyr arrange as_tibble bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @export
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
#' @export
#'
ctabs.survey.design <- function(design,
                                yvar,
                                xvars, ...) {

  # Setup
  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))
  xlabs <- lapply(1:length(xvars), function(i) {
    levels(as.factor(design$variables[[xvars[[i]]]]))
  })
  myforms <- lapply(1:length(xvars), function(i) {
    as.formula(paste0("~", yvar, "+", xvars[[i]]))
  })

  # Marginals table
  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(Total := "Total", .after = 1) %>%
    dplyr::mutate(Total = factor(Total, levels = "Total"))
  totals[[yvar]] <- ylabs
  totals[[yvar]] <- factor(totals[[yvar]], levels = ylabs)

  # Crosstabs
  xt_list <- lapply(1:length(xvars), function(i) {
    survey::svytable(myforms[[i]], design, round = TRUE) %>%
      dplyr::as_tibble() %>%
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
    as_tibble() %>%
    dplyr::mutate(Total = totals$pct, .after = 1)

  # Assemble output
  out <- list(
    "Summary Table" = omnixt,
    "Total" = totals)
  out <- append(out, xt_list)

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
#     as_tibble() %>%
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
#' @export
#'
ctabs.data.frame <- function(design,
                             yvar,
                             xvars, ...) {

  # The ctabs.data.frame is actually the same as ctabs.survey.design
  # To avoid having to write new code (because I'm lazy AF), we'll use srvyr::as_survey()
  # to create a survey design object with (weight==1) and then re-use the same code.
  design <- srvyr::as_survey(design)

  # Setup
  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))
  xlabs <- lapply(1:length(xvars), function(i) {
    levels(as.factor(design$variables[[xvars[[i]]]]))
  })
  myforms <- lapply(1:length(xvars), function(i) {
    as.formula(paste0("~", yvar, "+", xvars[[i]]))
  })

  # Marginals table
  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(Total := "Total", .after = 1) %>%
    dplyr::mutate(Total = factor(Total, levels = "Total"))
  totals[[yvar]] <- ylabs
  totals[[yvar]] <- factor(totals[[yvar]], levels = ylabs)

  # Crosstabs
  xt_list <- lapply(1:length(xvars), function(i) {
    survey::svytable(myforms[[i]], design, round = TRUE) %>%
      dplyr::as_tibble() %>%
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
    as_tibble() %>%
    dplyr::mutate(Total = totals$pct, .after = 1)

  # Assemble output
  out <- list(
    "Summary Table" = omnixt,
    "Total" = totals)
  out <- append(out, xt_list)

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
#' @importFrom dplyr as_tibble select
#' @importFrom magrittr %>%
#' @importFrom rlang sym
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
  out <- as_tibble(out, rownames = attr(x, "yvar"))

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
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs scale_y_continuous
#' @importFrom scales percent
#'
#' @export
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
  omnixt <- omnixt[!duplicated(as.list(omnixt))] %>% as_tibble()
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
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange as_tibble bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @export
#'
testcols <- function(obj,
                     adj.p = FALSE,
                     ...)  {UseMethod("testcols")}



#' testcols.ctabs
#'
#' @param obj An object created with the ctab or Ctabs functions
#' @param adj.p Boolean indicating whether to adjust for multiple comparisons
#' @param ... Other arguments (not currently implemented)
#'
#' @importFrom dplyr arrange as_tibble bind_cols bind_rows filter group_by mutate rename relocate select summarise tibble
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom rstatix row_wise_prop_test
#' @importFrom sjlabelled get_label get_labels
#' @importFrom stats as.formula
#' @importFrom srvyr as_survey
#' @importFrom survey svydesign svymean svyquantile svytable
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn relist
#'
#' @export
#'
testcols.ctabs <- function(obj,
                           adj.p = TRUE,
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
      pairlist_sig[[i]] <- ifelse(pairlist_pvals[[i]] < .05, "*", "")
    }
    names(pairlist_sig) <- names(pairlist)
    sigdiffs <- do.call("cbind", pairlist_sig)
    rownames(sigdiffs) <- rownames(dat)

    out[[x]] <- sigdiffs

  }

  names(out) <- xvars
  return(out)
}
