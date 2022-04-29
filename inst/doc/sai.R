## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----load-data, eval = TRUE---------------------------------------------------
library(sf)
library(smile)
library(ggplot2)

data(nyc_surv)
data(nyc_comd)

## ----var_calc-----------------------------------------------------------------
#  nyc_surv <- transform(nyc_surv,
#                        my_var = moe / qnorm(p = .975))
#  nyc_surv <- transform(nyc_surv, my_var = my_var * my_var)

## ----est-1, eval = FALSE------------------------------------------------------
#  estimate_comd <-
#      ai(source = nyc_surv, target = nyc_comd,
#         vars = "estimate")

## ----est-2--------------------------------------------------------------------
#  estimate_comd <-
#      ai_var(source = nyc_surv, target = nyc_comd,
#             vars = "estimate", vars_var = "my_var",
#             var_method = "MI")

## ----ggplot-obs-est-----------------------------------------------------------
#  viz_dt <-
#      rbind(
#          transform(nyc_surv, source = "Observed", est = estimate)[c("source", "est")],
#          transform(estimate_comd, source = "Predicted")[c("source", "est")]
#      )
#  
#  ggplot(data = viz_dt,
#         aes(fill = est / 1000)) +
#      geom_sf(color = 1, lwd = .1) +
#      scale_fill_viridis_c(option = "H",
#                           name = "median \n income",
#                           label = \(x) paste0(x, "K")) +
#      theme_bw() +
#      facet_wrap( ~ source) +
#      theme(axis.text = element_blank(),
#            axis.ticks = element_blank())

## ----ggplot-obs-est-viz, eval = TRUE, echo = FALSE----------------------------
viz_dt <-
    rbind(
        transform(nyc_surv, source = "Observed",
                  est = estimate)[c("source", "est")],
        transform(nyc_comd, source = "Predicted")[c("source", "est")]
    )

ggplot(data = viz_dt,
       aes(fill = est / 1000)) +
    geom_sf(color = 1, lwd = .1) +
    scale_fill_viridis_c(option = "H",
                         name = "median \n income",
                         label = \(x) paste0(x, "K")) +
    theme_bw() +
    facet_wrap( ~ source) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())

