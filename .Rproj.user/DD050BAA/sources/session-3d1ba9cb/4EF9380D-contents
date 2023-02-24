

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
cTab <- function(design,
                 yvar,
                 xvar,
                 ...) {UseMethod("cTab")}





#' cTab.survey.design
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvar String denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @export
#'
cTab.survey.design <- function(design,
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

  return(out)

}




#' cTab.data.frame
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvar String denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @export
#'
cTab.data.frame <- function(design,
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

  return(out)

}





#' cTabs
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
cTabs <- function(design,
                  yvar,
                  xvars, ...) {UseMethod("cTabs")}






#' cTabs.survey.design
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvars String (or vector of strings) denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @export
#'
cTabs.survey.design <- function(design,
                                yvar,
                                xvars, ...) {

  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))

  xlabs <- lapply(1:length(xvars), function(i) {
    levels(as.factor(design$variables[[xvars[[i]]]]))
  })

  myforms <- lapply(1:length(xvars), function(i) {
    as.formula(paste0("~", yvar, "+", xvars[[i]]))
  })

  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(Total := "Total", .after = 1)
  totals[[yvar]] <- ylabs
  totals[[yvar]] <- factor(totals[[yvar]], levels = ylabs)

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

  # pivoting to create omnibus summary crosstab
  omnixt <- lapply(1:length(xt_list), function(i) {
    xt_list[[i]] <- xt_list[[i]] %>%
      dplyr::select(-n) %>%
      tidyr::pivot_wider(names_from = !!sym(xvars[[i]]), values_from = pct)
  })
  omnixt <- do.call("cbind", omnixt)
  omnixt <- omnixt[!duplicated(as.list(omnixt))] %>% as_tibble()
  total_pct <- totals %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = !!sym(yvar), values_from = pct)
  omnixt <- omnixt %>%
    dplyr::mutate(Total = total_pct$Total, .after = 1)

  out <- list(
    "Summary Table" = omnixt,
    "Total" = totals)
  out <- append(out, xt_list)
  class(out) <- "cTabs.list"
  return(out)

}





#' cTabs.data.frame
#'
#' @param design Survey design object or data frame
#' @param yvar String denoting name of row variable (in quotes)
#' @param xvars String (or vector of strings) denoting name of column variable (in quotes)
#' @param ... Other arguments (not currently implemented)
#'
#' @export
#'
cTabs.data.frame <- function(design,
                             yvar,
                             xvars, ...) {

  design <- srvyr::as_survey(design)

  ylabs <- levels(droplevels(as.factor(design$variables[[yvar]])))

  xlabs <- lapply(1:length(xvars), function(i) {
    levels(as.factor(design$variables[[xvars[[i]]]]))
  })

  myforms <- lapply(1:length(xvars), function(i) {
    as.formula(paste0("~", yvar, "+", xvars[[i]]))
  })

  totals <- survey::svytable(as.formula(paste0("~", yvar)),
                             design, round = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(pct =  round(n/sum(n), 4)) %>%
    dplyr::mutate(Total := "Total", .after = 1)
  totals[[yvar]] <- ylabs
  totals[[yvar]] <- factor(totals[[yvar]], levels = ylabs)

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

  # pivoting to create omnibus summary crosstab
  omnixt <- lapply(1:length(xt_list), function(i) {
    xt_list[[i]] <- xt_list[[i]] %>%
      dplyr::select(-n) %>%
      tidyr::pivot_wider(names_from = !!sym(xvars[[i]]), values_from = pct)
  })
  omnixt <- do.call("cbind", omnixt)
  omnixt <- omnixt[!duplicated(as.list(omnixt))] %>% as_tibble()
  total_pct <- totals %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = !!sym(yvar), values_from = pct)
  omnixt <- omnixt %>%
    dplyr::mutate(Total = total_pct$Total, .after = 1)

  out <- list(
    "Summary Table" = omnixt,
    "Total" = totals)
  out <- append(out, xt_list)
  class(out) <- "cTabs.list"
  return(out)

}






likertNets <- function(obj, ...) {

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

  class(out) <- "cTabs.list"
  return(out)
}





#' testCols
#'
#' @param obj An object created with the cTab or Ctabs functions
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
testCols <- function(obj,
                     adj.p = FALSE,
                     ...)  {UseMethod("testCols")}



#' testCols.cTabs.list
#'
#' @param obj An object created with the cTab or Ctabs functions
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
testCols.cTabs.list <- function(obj,
                                adj.p = FALSE,
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
