apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Arial', size = 35))

m2_birthorder_linear = update(g_factor_m1, formula = . ~ . + birth_order)

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
                           colour = "#8DA0CB", group = `Sibship`)) +
    geom_smooth(stat = "identity", fill = "#8DA0CB") +
    scale_y_continuous((expression(paste("Intelligenz"))), limits = c(-0.5, 0.5), breaks = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
    scale_x_continuous("Geburtenposition", breaks = c(0, 2, 4, 6, 8, 10, 12, 14)) +
    apatheme +
    scale_colour_manual(values = "#8DA0CB") +
    scale_fill_manual(values = "#8DA0CB") +
    theme(legend.position="none")
  print(plotx)
  assign(paste0("plot_", outcome, seperate=""),plotx,.GlobalEnv)
}

plot = plot_birthorder(m2_birthorder_linear, separate = FALSE)
plot

m3_birthorder_nonlinear = update(big5_ext_m1, formula = . ~ . + birth_order_nonlinear)

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
    geom_pointrange(stat = "identity", fill = c(brewer.pal(8, "Set2")[c(7)]),  size=2) +
    geom_line(size=2) +
    scale_y_continuous("Extraversion", limits = c(-0.5, 0.5), breaks = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
    scale_x_discrete("Geburtenposition") +
    apatheme +
    scale_colour_manual(values = "#66C2A5") +
    scale_fill_manual(values ="#66C2A5") +
    theme(legend.position="none")
  print(plotx)
  assign(paste0("plot_", outcome, seperate=""),plotx,.GlobalEnv)
}

plot = plot_birthorder(m3_birthorder_nonlinear, separate = FALSE)


m3_birthorder_nonlinear = update(big5_open_m1, formula = . ~ . + birth_order_nonlinear)

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
    geom_pointrange(stat = "identity", fill = c(brewer.pal(8, "Set2")[c(7)])) +
    geom_line() +
    scale_y_continuous("Openness", limits = c(-0.5, 0.5), breaks = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
    scale_x_discrete("Maternal birth order") +
    apatheme +
    scale_colour_manual(values = rev(brewer.pal(7,"Set2"))) +
    scale_fill_manual(values = rev(brewer.pal(7,"Set2"))) +
    theme(legend.position="none")
  print(plotx)
  assign(paste0("plot_", outcome, seperate=""),plotx,.GlobalEnv)
}

plot = plot_birthorder(m3_birthorder_nonlinear, separate = FALSE)

library(grid)
library(gridExtra)

### Within
## Intelligence
g_factor_m1 = lmer(g_factor ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                   data = birthorder %>%
                     rename(sibling_count = sibling_count_uterus_alive_factor,
                            birth_order_nonlinear = birthorder_uterus_alive_factor,
                            birth_order = birthorder_uterus_alive,
                            count_birth_order = count_birthorder_uterus_alive))

m4_interaction = update(g_factor_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)


plot_g_factor = plot_birthorder(m4_interaction, "Intelligence", "a")


## Personality
#Extraversion
big5_ext_m1 = lmer(big5_ext ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                   data = birthorder %>%
                     rename(sibling_count = sibling_count_uterus_alive_factor,
                            birth_order_nonlinear = birthorder_uterus_alive_factor,
                            birth_order = birthorder_uterus_alive,
                            count_birth_order = count_birthorder_uterus_alive))
m4_interaction = update(big5_ext_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)

plot_big5_ext = plot_birthorder(m4_interaction, "Extraversion", "b")

# Neuroticism
big5_neu_m1 = lmer(big5_neu ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                   data = birthorder %>%
                     rename(sibling_count = sibling_count_uterus_alive_factor,
                            birth_order_nonlinear = birthorder_uterus_alive_factor,
                            birth_order = birthorder_uterus_alive,
                            count_birth_order = count_birthorder_uterus_alive))
m4_interaction = update(big5_neu_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)

plot_big5_neu = plot_birthorder(m4_interaction, "Neuroticism", "c")

#Conscientiuosness
big5_con_m1 = lmer(big5_con ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                   data = birthorder %>%
                     rename(sibling_count = sibling_count_uterus_alive_factor,
                            birth_order_nonlinear = birthorder_uterus_alive_factor,
                            birth_order = birthorder_uterus_alive,
                            count_birth_order = count_birthorder_uterus_alive))
m4_interaction = update(big5_con_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)

plot_big5_con = plot_birthorder(m4_interaction, "Conscientiousness", "d")

#Agreeableness
big5_agree_m1 = lmer(big5_agree ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                   data = birthorder %>%
                     rename(sibling_count = sibling_count_uterus_alive_factor,
                            birth_order_nonlinear = birthorder_uterus_alive_factor,
                            birth_order = birthorder_uterus_alive,
                            count_birth_order = count_birthorder_uterus_alive))
m4_interaction = update(big5_agree_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)

plot_big5_agree = plot_birthorder(m4_interaction, "Agreeableness", "e")

#Openness
big5_open_m1 = lmer(big5_open ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                     data = birthorder %>%
                       rename(sibling_count = sibling_count_uterus_alive_factor,
                              birth_order_nonlinear = birthorder_uterus_alive_factor,
                              birth_order = birthorder_uterus_alive,
                              count_birth_order = count_birthorder_uterus_alive))
m4_interaction = update(big5_open_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)

plot_big5_open = plot_birthorder(m4_interaction, "Openness", "f")

##Risk
#Risk A
riskA_m1 = lmer(riskA ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                     data = birthorder %>%
                       rename(sibling_count = sibling_count_uterus_alive_factor,
                              birth_order_nonlinear = birthorder_uterus_alive_factor,
                              birth_order = birthorder_uterus_alive,
                              count_birth_order = count_birthorder_uterus_alive))
m4_interaction = update(riskA_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)

plot_riskA = plot_birthorder(m4_interaction, "Risk A", "g")


# Risk B
riskB_m1 = lmer(riskB ~ birthyear + male + sibling_count + (1 | mother_pidlink),
                     data = birthorder %>%
                       rename(sibling_count = sibling_count_uterus_alive_factor,
                              birth_order_nonlinear = birthorder_uterus_alive_factor,
                              birth_order = birthorder_uterus_alive,
                              count_birth_order = count_birthorder_uterus_alive))
m4_interaction = update(riskB_m1, formula = . ~ . - birth_order_nonlinear - sibling_count + count_birth_order)

plot_riskB = plot_birthorder(m4_interaction, "Risk B", "h")



plots = grid.arrange(plot_g_factor, plot_big5_ext, plot_big5_neu, plot_big5_con, plot_big5_agree,
                     plot_big5_open, plot_riskA, plot_riskB,
                     ncol = 2)
