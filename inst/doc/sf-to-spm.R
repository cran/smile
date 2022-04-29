## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(smile)
library(ggplot2)
library(sf)

## ----load-data, eval = TRUE, echo = TRUE--------------------------------------
data(liv_lsoa) # loading the LSOA data
data(liv_msoa) # loading the MSOA data

## workaround for compatibility with different PROJ versions
st_crs(liv_msoa) <-
    st_crs(liv_msoa)$input
st_crs(liv_lsoa) <-
    st_crs(liv_lsoa)$input

## ----read-data, fig.cap="LEB in Liverpool at the MSOA."-----------------------
ggplot(data = liv_msoa,
       aes(fill = leb_est)) +
    geom_sf(color = "black",
            lwd   = .1) +
    scale_fill_viridis_b(option = "H") +
    theme_minimal()

## ----sf-to-spm1---------------------------------------------------------------
msoa_spm <-
    sf_to_spm(sf_obj = liv_msoa, n_pts = 1000,
              type = "regular", by_polygon = FALSE,
              poly_ids = "msoa11cd", var_ids = "leb_est")

## ----sf-to-spm2, echo = FALSE, warning = FALSE, message = FALSE---------------
set.seed(123)

msoa_spm1 <-
    sf_to_spm(sf_obj = liv_msoa, n_pts = 305,
              type = "random", by_polygon = FALSE,
              poly_ids = "msoa11cd", var_ids = "leb_est")

msoa_spm2 <-
    sf_to_spm(sf_obj = liv_msoa, n_pts = 305,
              type = "regular", by_polygon = FALSE,
              poly_ids = "msoa11cd", var_ids = "leb_est")

msoa_spm3 <-
    sf_to_spm(sf_obj = liv_msoa, n_pts = 305,
              type = "hexagonal", by_polygon = FALSE,
              poly_ids = "msoa11cd", var_ids = "leb_est")

msoa_spm4 <-
    sf_to_spm(sf_obj = liv_msoa, n_pts = 305/61,
              type = "random", by_polygon = TRUE,
              poly_ids = "msoa11cd", var_ids = "leb_est")

msoa_spm5 <-
    sf_to_spm(sf_obj = liv_msoa, n_pts = 305/61,
              type = "regular", by_polygon = TRUE,
              poly_ids = "msoa11cd", var_ids = "leb_est")

msoa_spm6 <-
    sf_to_spm(sf_obj = liv_msoa, n_pts = 305/61,
              type = "hexagonal", by_polygon = TRUE,
              poly_ids = "msoa11cd", var_ids = "leb_est")

to_plot <- rbind(
    transform(msoa_spm1$grid, type = "random", by_polygon = FALSE),
    transform(msoa_spm2$grid, type = "regular", by_polygon = FALSE),
    transform(msoa_spm3$grid, type = "hexagonal", by_polygon = FALSE),
    transform(msoa_spm4$grid, type = "random", by_polygon = TRUE),
    transform(msoa_spm5$grid, type = "regular", by_polygon = TRUE),
    transform(msoa_spm6$grid, type = "hexagonal", by_polygon = TRUE),
    deparse.level = 1
    )

rm(list = ls(pattern = "^msoa_spm")); invisible(gc(verbose = FALSE, full = TRUE))

ggplot(data = to_plot,
       aes(color = msoa11cd)) +
    guides(color = "none") +
    geom_sf(pch = 15) +
    scale_color_viridis_d(option = "H") +
    facet_grid(by_polygon ~ type, labeller = label_both) +
    theme_bw() +
    theme(axis.text = element_blank())

