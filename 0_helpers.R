#' # Helper functions used throughout {.tabset .tabset-sticky}
#' Documentation on the functions is interspersed through code comments
#'
#' ## Set some options
#' do net set strings as factors
options(stringsAsFactors = FALSE)
#' show four significant digits
options(digits = 4)
#' tend not to show scientific notation
options(scipen = 7)
#' make output a bit wider
options(width = 110)
#' set a seed to make analyses depending on random number generation reproducible
set.seed(1710)

#' ## Load packages
#' generate the site
library(rmarkdown)
#' set options for chunks
library(knitr)
#' formr utility package to generate e.g. the bibliography
library(formr)
#' pretty-printed output
library(pander)
#' tidyverse date times
library(lubridate)
#' tidyverse strings
library(stringr)
#' extractor functions for models
library(broom)
#' marginal effects plots for regressions
library(effects)
#' grammar of graphics plots
library(ggplot2)
#' enhanced version of data.frame
library(data.table)
#' linear mixed effects models
library(lme4)
#' interpret linear mixed effects models output
library(lmerTest)
#' effect displays for all sort of models
library(effects)
#' simple, consistent wrappers for common string operations
library(stringr)
#' tools for working with categorical variables
library(forcats)
#' helps with fancier graphs
library(cowplot)
#' provides a funtion to format R source corde
library(formatR)
#' loads specific r tools
library(devtools)
#' imports IFLS data
library(haven)
#' package to perform factor analysis
library(GPArotation)
#' another package to perform factor analysis
library(psych)
#' yet another package to perform factor analysis
library(lavaan)
#' many usefull functions for data analysis
library(Hmisc)
#' all colors are from here
library(RColorBrewer)
#' tidyverse: transform data wide to long
library(tidyr)
#' tidyverse-style data wrangling. has a lot of naming conflicts, so always load last
library(dplyr)


#' ## Spin R files
#' R scripts can be documented in markdown using Roxygen comments, as demonstrated here
#' This function turns all R files (that don't have an Rmd file of the same name and that don't start with an underscore _) into HTML pages
spin_R_files_to_site_html = function() {
  library(knitr)
  all_Rs = c(list.files(pattern = "^[^_].+\\.R$"), ".Rprofile")
  component_Rmds = list.files(pattern = "^_.+\\.Rmd$")
  temporary_Rmds = c()
  for (i in seq_along(all_Rs)) {
    if(all_Rs[i] == ".Rprofile") {
      Rmd_file = ".Rprofile.Rmd"
    } else {
      Rmd_file = paste0(all_Rs[i], "md")
    }
    if (!file.exists(Rmd_file)) {
      next_document = length(temporary_Rmds) + 1
      temporary_Rmds[next_document] = spin(all_Rs[i], knit = FALSE, envir = new.env(), format = "Rmd")
      prepended_yaml = paste0(c("---
output:
  html_document:
    code_folding: 'show'
---

", readLines(temporary_Rmds[next_document])), collapse = "\n")
      cat(prepended_yaml, file = temporary_Rmds[next_document])
    }
  }
  components_and_scripts = c(temporary_Rmds, component_Rmds)
  for (i in seq_along(components_and_scripts)) {
    opts_chunk$set(eval = FALSE, cache = FALSE)
    # if we call render_site on the .R file directly it adds a header I don't like
    rmarkdown::render_site(components_and_scripts[i], quiet = TRUE)
  }
  opts_chunk$set(eval = TRUE, cache = TRUE)
  unlink(temporary_Rmds)
}

#' ## Output options
#' use pander to pretty-print objects (if possible)
pander_handler = function(x, ...) {
  anyS3method = function(x) {
    classes = class(x)
    any(sapply(classes, FUN = function(classes) { !is.null(getS3method('pander',classes,TRUE)) } ))
  }
  if ("knit_asis" %in% class(x)) {
    x
  } else if (is.data.table(x)) {
    ""
    # don't ever print stupid data tables
  } else if (anyS3method(x)) {
    pander(x, row.names = F, ...)
  } else if (isS4(x)) {
    show(x)
  } else {
    print(x)
  }
}

opts_chunk$set(
  render = pander_handler
)


#' don't split tables, scroll horizontally
panderOptions("table.split.table", Inf)

#' ## Functions used
#' set apa-conform theme for all graphs
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())

#' function used to do all birth order effect plots
plot_birthorder = function(model, ylabel = NULL, title = "", bo_var = "birth_order", separate = TRUE) {
  if(inherits(model, "merMod")) {
    varnames = names(model@frame)
  } else {
    varnames = names(model$model)
  }
  outcome = varnames[1]
  if(is.null(ylabel)) ylabel = outcome
  library(effects)
  library(tidyr)
  emm = allEffects(model)
  bo_var = names(emm)[names(emm) %contains% bo_var]
  cemm = as.data.frame(emm[[bo_var]])
  if (separate != TRUE) {
    cemm = cemm %>% rename_("Birth order" = bo_var) %>% mutate(Sibship = "across")
  } else {
    cemm = cemm %>%
      separate_(bo_var, into = c("Birth order", "Sibship"), sep = "/")
    number = spread(as.data.frame(table(model.frame(model)[`bo_var`])), Var1, Freq)
    n2 = paste0("2 (", sum(number$`1/2`, number$`2/2`), ")", seperate="")
    n3 = paste0("3 (", sum(number$`1/3`, number$`2/3`, number$`3/3`), ")",
                seperate="")
    n4 = paste0("4 (", sum(number$`1/4`, number$`2/4`, number$`3/4`,
                           number$`4/4`), ")", seperate="")
    n5 = paste0("5 (", sum(number$`1/5`, number$`2/5`, number$`3/5`,
                           number$`4/5`, number$`5/5`), ")", seperate="")
    n5more = paste0("5+ (", sum(number$`1/5+`, number$`2/5+`, number$`3/5+`,
                                number$`4/5+`, number$`5/5+`, number$`5+/5+`), ")",
                    seperate="")
    cemm = cemm %>%
      mutate(Sibship = recode_factor(Sibship, "2" = `n2`, "3" = `n3`, "4" = `n4`,
                                     "5" = `n5`, "5+" = `n5more`))
  }
  plotx = ggplot(cemm, aes(`Birth order`, y = fit, ymax = upper, ymin = lower,
                           colour = `Sibship`, group = `Sibship`)) +
    geom_pointrange(stat = "identity", position = position_dodge(width = 0.5)) +
    geom_line(position = position_dodge(width = 0.5)) +
    scale_y_continuous(name=ylabel, limits = c(-0.8, 0.8), breaks = c(-0.8, -0.6, -0.4, -0.2, 0,
                                                                      0.2, 0.4, 0.6, 0.8)) +
    labs(title= title) +
    apatheme +
    theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          plot.title = element_text(hjust = 0)) +
    guides(colour=guide_legend(title = "Sibship")) +
    scale_fill_brewer(palette = "Set2") +
    scale_colour_brewer(palette = "Set2")
  print(plotx)
  assign(paste0("plot_", outcome, seperate=""),plotx,.GlobalEnv)
}

plot_birthorder_nonz = function(model, ylabel = NULL, title = "", bo_var = "birth_order", separate = TRUE) {
  if(inherits(model, "merMod")) {
    varnames = names(model@frame)
  } else {
    varnames = names(model$model)
  }
  outcome = varnames[1]
  if(is.null(ylabel)) ylabel = outcome
  library(effects)
  library(tidyr)
  emm = allEffects(model)
  bo_var = names(emm)[names(emm) %contains% bo_var]
  cemm = as.data.frame(emm[[bo_var]])
  if (separate != TRUE) {
    cemm = cemm %>% rename_("Birth order" = bo_var) %>% mutate(Sibship = "across")
  } else {
    cemm = cemm %>%
      separate_(bo_var, into = c("Birth order", "Sibship"), sep = "/")
    number = spread(as.data.frame(table(model.frame(model)[`bo_var`])), Var1, Freq)
    n2 = paste0("2 (", sum(number$`1/2`, number$`2/2`), ")", seperate="")
    n3 = paste0("3 (", sum(number$`1/3`, number$`2/3`, number$`3/3`), ")",
                seperate="")
    n4 = paste0("4 (", sum(number$`1/4`, number$`2/4`, number$`3/4`,
                           number$`4/4`), ")", seperate="")
    n5 = paste0("5 (", sum(number$`1/5`, number$`2/5`, number$`3/5`,
                           number$`4/5`, number$`5/5`), ")", seperate="")
    n5more = paste0("5+ (", sum(number$`1/5+`, number$`2/5+`, number$`3/5+`,
                                number$`4/5+`, number$`5/5+`, number$`5+/5+`), ")",
                    seperate="")
    cemm = cemm %>%
      mutate(Sibship = recode_factor(Sibship, "2" = `n2`, "3" = `n3`, "4" = `n4`,
                                     "5" = `n5`, "5+" = `n5more`))
  }
  plotx = ggplot(cemm, aes(`Birth order`, y = fit, ymax = upper, ymin = lower,
                           colour = `Sibship`, group = `Sibship`)) +
    geom_pointrange(stat = "identity", position = position_dodge(width = 0.5)) +
    geom_line(position = position_dodge(width = 0.5)) +
    scale_y_continuous(name=ylabel, limits = c(0,26), breaks = c(0:26)) +
    labs(title= title) +
    apatheme +
    theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          plot.title = element_text(hjust = 0)) +
    guides(colour=guide_legend(title = "Sibship")) +
    scale_fill_brewer(palette = "Set2") +
    scale_colour_brewer(palette = "Set2")
  print(plotx)
  assign(paste0("plot_", outcome, seperate=""),plotx,.GlobalEnv)
}


#' functions used to compare models
compare_models_markdown = function(m1_covariates_only) {
  formr::asis_knit_child('_test_outcome.Rmd')
  }

compare_models_markdown_nonz = function(m1_covariates_only) {
  formr::asis_knit_child('_test_outcome_nonz.Rmd')
}

pad_month = function(x) { str_pad(x, width = 2, side = "left", pad = "0")}

#' Function to calculate the birthdate out of all available informations for one individual
all_available_info_birth_date = function(byear, bmonth, bday = NULL) {
  if(!is.null(bday)) {
    bday = paste0("-", pad_month(bday))
  } else {
    bday = ""
  }
  ifelse(is.na(byear), NA,
         paste0(byear, "-", pad_month(bmonth), pad_month(bday)))
  # can yield 2016-NA-NA
  #           2016-01-NA
  #           2016-01-01
  #           2016-01
}

#' Function to calculate the birthorder based on the siblings still alive at the time of birth
older_sibs_alive_and_dependent = function(byear, dyear) {
  sibs = length(byear)
  older_sibs_alive_and_dependent = integer(length=sibs) + 1
  for(i in 1:sibs) {
    older_sibs = byear <= byear[i] # not using < because of twins
    older_sibs[i] = F # minus self
    my_sibs = sum(older_sibs,na.rm = T) # minus self
    if(my_sibs > 0) {
      sib_births = byear[ which(older_sibs) ]
      sib_deaths = dyear[ which(older_sibs) ]
      my_sibs = my_sibs -
        sum(
          # sib_births < (byear[i] - 5) | # others born more than 5y earlier than me  # 10 seconds of 17
          (sib_deaths <= byear[i]) # died before my birth
          ,na.rm=T)
      older_sibs_alive_and_dependent[i] = my_sibs
    }
  }
  older_sibs_alive_and_dependent
}


