## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

## ----load-data----------------------------------------------------------------
library(sf)
library(smile)
library(ggplot2)

data(nl_ct)
nl_ct <- st_transform(nl_ct, 20823)

## ----synthetic-data-----------------------------------------------------------
set.seed(2024)

## outer polygon
nl_outer <- nl_ct |>
  st_geometry() |>
  st_union() |>
  smile:::st_remove_holes()

## creating `target` data
points_nl <- st_sample(x = nl_outer,
                       size = 40)

nl_vor <- do.call(c, points_nl) |>
  ## voronoi tesselation
  st_voronoi(envelope = nl_outer) |> 
  st_collection_extract(type = "POLYGON") |>
  st_set_crs(st_crs(nl_ct)) |>
  st_intersection(nl_outer) |>
  st_as_sf()

## creating id variable
nl_vor <- transform(nl_vor, id = seq_len(NROW(nl_vor)))

## ----est-1--------------------------------------------------------------------
nl_ests <-
    ai(source = nl_ct,
       target = nl_vor,
       vars = c("hh_density",
                "avg_income",
                "avg_age"))

## displaying the result
nl_ests |>
  st_drop_geometry() |>
  head()

## ----est-2--------------------------------------------------------------------
nl_est <-
    ai_var(source = nl_ct,
           target = nl_vor,
           vars = "hh_density",
           vars_var = "var_hhd",
           var_method = "MI")

## renaming geometry
st_geometry(nl_est) <- "geometry"

## ----ggplot-obs-est-----------------------------------------------------------
viz_dt <-
    rbind(
        transform(nl_ct, source = "Observed",
                  est = hh_density)[c("source", "est")],
        transform(nl_est, source = "Estimated")[c("source", "est")]
    )

ggplot(data = viz_dt,
       aes(fill = est)) +
    geom_sf(color = 1, lwd = .1) +
    scale_fill_viridis_c(option = "H",
                         name = "Household \n density") +
    theme_bw() +
    facet_wrap( ~ source) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())

## ----ggplot-obs-est-viz, eval = TRUE, echo = FALSE----------------------------
ggplot(data = nl_est,
       aes(fill = se_est)) +
    geom_sf(color = 1, lwd = .1) +
    scale_fill_viridis_c(option = "H",
                         name = "SE(Household \n density)") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())

