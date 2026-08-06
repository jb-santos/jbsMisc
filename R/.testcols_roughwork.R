

library(survey)
library(srvyr)
library(jbsMisc)
library(tidyverse)


data("ab")
absvy <- svydesign(~1, data = ab, weights = ~weight)
table_vote1 <- ctabs(absvy, yvar = "dvote3", xvars = c("region", "female", "age"))
table_vote1



# Setup ----------------------------------------------------------------------


xvars <- names(table_vote1[3:length(table_vote1)])
yvar <- names(table_vote1[[1]][1])
ylevs <- levels(table_vote1[[3]][[1]])

xt_list <- table_vote1[3:length(table_vote1)]
names(xt_list) <- xvars



# Build list of pairwise column tests ----------------------------------------

out <- NULL
testdfs <- NULL
finaltbls <- NULL





# Create data frame for one bivariate crosstab

dat <- xt_list[[1]] %>%
  dplyr::select(-pct) %>%
  tidyr::pivot_wider(names_from = 2, values_from = n) %>%
  as.data.frame()
rownames(dat) <- dat[[1]]
#dat <- dat %>% dplyr::select(-1)    # Causes error when NAs encountered
dat <- dat %>% dplyr::select(-1) %>% replace(is.na(.), 0)




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
  pairlist_tests[[i]]$p
})

pairlist_sig <- NULL
for(i in 1:length(pairlist_pvals)) {
  pairlist_sig[[i]] <- ifelse(pairlist_pvals[[i]] < .05, 1, 0)
}
names(pairlist_sig) <- names(pairlist)



xlevs <- levels(xt_list[[1]][[2]])


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





# First create output table with n AND pct

temp <- xt_list[[1]] %>%
  dplyr::mutate(pct = paste0(round(pct*100),"%"),
                n = as.character(round(n))) %>%
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
  dplyr::group_by(Response) # %>% dplyr::mutate(Response = replace(Response, duplicated(Response), " "))

out[[1]] <- finaltbl
attr(out[[1]], "xvar") <- xvars[[1]]
attr(out[[1]], "xlevs") <- xlevs
attr(out[[1]], "siglabs") <- colnames(diffs_summary)






# Keep output table long

diffs_summary1$rwname <- sapply(1:nrow(dat), function(i) {
  paste0(rownames(dat)[[i]], "__sig")
})
diffs_summary1 <- as_tibble(diffs_summary1)

temp <- xt_list[[1]] %>%
  dplyr::mutate(n = round(n)) %>%
  tidyr::pivot_longer(c(n,pct)) %>%
  tidyr::pivot_wider(names_from = 2, values_from = value) %>%
  dplyr::mutate(rwname = paste0(!!sym(yvar), "__", name)) %>%
  dplyr::select(-c(1,2)) %>% as.data.frame()
#rownames(temp) <- temp$rwname
#temp <- temp %>% dplyr::select(-rwname)

tbl_n_pct <- temp %>%
  pivot_longer(1:3, names_to = "region", values_to = "value") %>%
  mutate(value = as.character(value))


tbl_sig <- diffs_summary1 %>%
  pivot_longer(1:3, names_to = "region")

tbl_assembled <- bind_rows(tbl_n_pct, tbl_sig)

tbl_assembled_wide <- tbl_assembled %>%
  pivot_wider(names_from = region, values_from = value) %>% as.data.frame()





# append column of marginals
tbl_marginals <- table_vote1[[2]] %>%
  select(-Total) %>%
  mutate(sig = NA) %>%
  pivot_longer(c(n, pct, sig), names_to = "Stat", values_to = "Total") %>%
  mutate(rwname = paste0(dvote3, "__", Stat)) %>% as.data.frame()

row.names(tbl_marginals) <- tbl_marginals$rwname
tbl_marginals <- tbl_marginals %>% select(-rwname)

row.names(tbl_assembled_wide) <- tbl_assembled_wide$rwname
tbl_assembled_wide <- tbl_assembled_wide %>% select(-rwname)



tbl_assembled_wide_wmarginals <- left_join(tbl_assembled_wide, tbl_marginals)
tbl_assembled_wide_wmarginals







testfun <- function(obj,
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


    ### ------
    ### THIS IS WHERE TO put dat <- dat %>% replace(is.na(.), 0)
    ### ------




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









# alternate approach: use survey::svyttest() -----------------------------------


# general format:
svyttest(ideology01~female, absvy)

svychisq(educ~dvote3, absvy)



levelpairs <- combn(names(dat), 2)
levelpairs <- combn(xlevs, 2, simplify = FALSE)

levelpairs[[1]]

as_survey_design(absvy)

