## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(smile)
library(ggplot2)
library(sf)

## ----load-data----------------------------------------------------------------
data(liv_msoa)
data(liv_lsoa)

## workaround for compatibility with different PROJ versions
st_crs(liv_msoa) <-
    st_crs(liv_msoa)$input

st_crs(liv_lsoa) <-
    st_crs(liv_lsoa)$input

## msoa from sf to spm
spm_msoa <-
    sf_to_spm(sf_obj = liv_msoa,
              n_pts  = 500,
              type   = "regular",
              by_polygon = FALSE,
              poly_ids = "msoa11cd",
              var_ids  = "leb_est")

## ----fit-pred-1---------------------------------------------------------------
theta_st_msoa <- c("phi" = 1)

## 1) it is important to NAME the initial values for each parameter
## 2) to estimate "nu" from the data we only need to provide an initial value for such
## parameter
## 3) uncomment the code below to do so.
## 4) Note that it is possible to set the boundaries for the parameter space on
## which we want to optmize the likelihood.
## theta_st_msoa <- c("phi" = 1, "nu" = 1)
## fit_msoa1 <-
##     fit_spm(x = spm_msoa,
##             theta_st = theta_st_msoa,
##             model = "matern",
##             nu = .5,
##             lower = c(1e-16, 1e-16),
##             upper = c(Inf, 1),
##             opt_method = "L-BFGS-B",
##             control   = list(maxit = 500))

fit_msoa1 <-
    fit_spm(x = spm_msoa,
            theta_st = theta_st_msoa,
            model = "matern",
            nu = .5,
            apply_exp = TRUE,
            opt_method = "L-BFGS-B",
            control   = list(maxit = 500))

fit_msoa2 <-
    fit_spm(x = spm_msoa,
            theta_st = theta_st_msoa,
            model = "matern",
            nu = 1.5,
            apply_exp = TRUE,
            opt_method = "L-BFGS-B",
            control    = list(maxit = 500))

fit_msoa3 <-
    fit_spm(x = spm_msoa,
            theta_st = theta_st_msoa,
            model = "matern",
            nu = 2.5,
            apply_exp = TRUE,
            opt_method = "L-BFGS-B",
            control   = list(maxit = 500))

## ----AIC, echo = TRUE, eval = TRUE--------------------------------------------
c("m1" = AIC(fit_msoa1), "m2" = AIC(fit_msoa2), "m3" = AIC(fit_msoa3))

## ----summary-fit1-------------------------------------------------------------
summary_spm_fit(fit_msoa1, sig = .05)

## ----predict-fit1-------------------------------------------------------------
pred_lsoa <- predict_spm(x = liv_lsoa, spm_obj = fit_msoa1, id_var = "lsoa11cd")

## ----manip-pred-1-------------------------------------------------------------
ggplot(data = pred_lsoa$pred_agg,
       aes(fill  = mu_pred)) +
    geom_sf(color = 1,
            lwd = .1) +
    scale_fill_viridis_c(option = "H") +
    guides(fill = "none") +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank())

## ----se-ggplot2---------------------------------------------------------------
ggplot(data = pred_lsoa$pred_agg,
       aes(fill  = se_pred)) +
    geom_sf(color = 1,
            lwd = .1) +
    scale_fill_viridis_c(option = "H") +
    guides(fill = "none") +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank())

