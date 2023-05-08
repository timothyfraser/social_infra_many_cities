#' @name `05_models.R`
#'
#' @description Script for analyzing dataset.

## Data #########################################

library(sf)
library(dplyr)
library(DBI)
library(readr)
library(purrr)
library(tidyr)
library(broom)

source("keys.R")
data("meta")
data("connect")
geo = connect()
geo %>% dbListTables()

original = geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  filter(pop_density_block > 0) %>%
  collect() %>%
  select(name, cell, social_capital, bonding:linking,
         total, community_space, place_of_worship, social_business, park,
         pop_density_block, white_block, black_block, hisplat_block, asian_block, income_0_60K_bg, median_household_income_bg,
         some_college_bg, over_65_bg, unemployment_bg) %>%
  mutate(nonwhite_block = 1 - white_block) %>%
  na.omit()

.vars = c("pop_density_block", "median_household_income_bg", "some_college_bg", "nonwhite_block")

c = read_rds("viz/models_correction.rds")

nthroot = nthroot = function(x,n) { (abs(x)^(1/n))*sign(x) }

yhat = function(var, trans = TRUE){
  x = original %>%
    select(api = all_of(var), all_of(.vars)) %>%
    predict(object = c[[var]], newdata = .)
  if(trans == TRUE){ x = nthroot(x, n = 10) }
    # Clip values at zero.
  x = if_else(x < 0, 0, x)
  return(x)
}

adj = original %>%
  mutate(total = yhat("total"),
         community_space = yhat("community_space"),
         place_of_worship = yhat("place_of_worship"),
         social_business = yhat("social_business"),
         park = yhat("park", trans = FALSE))

work = function(data){
  dat = data %>%
    group_by(name) %>%
    mutate_at(vars(pop_density_block,
                   black_block, hisplat_block,asian_block, median_household_income_bg,income_0_60K_bg,
                   some_college_bg, over_65_bg, unemployment_bg), list(~scale(.))) %>%
    ungroup() %>%
    na.omit() %>%
    pivot_longer(
      cols = c(social_capital, bonding, bridging, linking),
      names_to = "outcome", values_to = "value")

  list(
    total = dat %>%
      split(.$outcome) %>%
      map(~lm(formula = value ~ log(total + 1) +
                pop_density_block + black_block + hisplat_block + asian_block +
                median_household_income_bg + income_0_60K_bg +
                some_college_bg + over_65_bg + unemployment_bg + name, data = .),
          .id = "outcome"),

    subtypes = dat %>%
      split(.$outcome) %>%
      map(~lm(formula = value ~ log(community_space + 1) +
                log(place_of_worship + 1) +
                log(social_business + 1) + log(park + 1) +
                pop_density_block +
                black_block + hisplat_block + asian_block +
                median_household_income_bg + income_0_60K_bg +
                some_college_bg + over_65_bg + unemployment_bg + name, data = .),
          .id = "outcome")
  )

}

original %>% work() %>% saveRDS("viz/models_main.rds")
adj %>% work() %>% saveRDS("viz/models_main_adj.rds")


get_stat = function(m){


  # Write a function to get vif
  get_vif = function(m){ x = (car::vif(m)^2)[,3]; tibble(term = names(x), vif = x) }


  tidier = function(m){
    broom::tidy(m) %>%
      rename(se = std.error, stat = statistic, p_value = p.value) %>%
      mutate(stars = gtools::stars.pval(p_value))
  }

  sci = function(x, digits = 3){
    require(stringr)
    require(dplyr)
    # Get scientific notation
    x = format(x, digits = digits, scientific = TRUE, big.mark = ",")
    # Make scientific notation prettier
    x = case_when(
      str_detect(x, "e[+]00|e[+]01|e[+]02|e[-]00|e[-]01|e[-]02") ~ as.character(as.numeric(x)),
      TRUE ~ paste0(
        str_remove(x, "e[+][0-9]{2}|e[-][0-9]{2}"),
        " x 10<sup>",
        as.integer(str_extract(str_extract(x, "e[+].*|e[-].*"), "[0-9]+")),
        "</sup>") )
  }

  # Check max vif
  myvif = m %>% map_dfr(. %>% map_dfr(~get_vif(.), .id = "model"), .id = "group") %>%
    group_by(group, model) %>%
    summarize(vif = max(vif)) # all are fine

  beta = read_rds("viz/models_main.rds") %>%
    map_dfr(. %>% map_dfr(~tidier(.), .id = "model"), .id = "group") %>%
    # Get scientific notation
    mutate(across(.cols = c(estimate, se), .fns = ~sci(.x))) %>%
    mutate(value = paste0(estimate, stars, "<br>(",se,")"))

  stat = read_rds("viz/models_main.rds") %>%
    map_dfr(. %>% map_dfr(~broom::glance(.), .id = "model"), .id = "group") %>%
    left_join(by = c("group", "model"), y = myvif) %>%
    mutate(
      across(.cols = c(r.squared, adj.r.squared, statistic, vif),
             .fns = ~scales::number(.x, accuracy = 0.01) %>% str_trim(side = "both")),
      sigma = sci(sigma),
      df = as.character(df),
      nobs = as.character(nobs)) %>%
    mutate(statistic = paste0(statistic, gtools::stars.pval(p.value))) %>%
    select(group, model, all_of(c("sigma", "r.squared", "adj.r.squared", "vif", "statistic", "df", "nobs"))) %>%
    pivot_longer(cols = c(sigma, r.squared, adj.r.squared, vif, statistic, df, nobs), names_to = "term", values_to = "value")


  .levels = c("total" = "Total Social<br>Infrastructure",
              "community_space" = "Community<br>Spaces","place_of_worship" = "Places<br>of Worship",
              "social_business" = "Social<br>Businesses",  "park" = "Parks")

  trans = tibble(term = "trans", model = c("social_capital", "bridging", "bonding", "linking")) %>%
    expand_grid(group = c("total", "subtypes")) %>%
    mutate(value = "log(y + 1)")


  table = bind_rows(trans, beta, stat, myvif) %>%
    mutate(model = paste0(group, "-", model)) %>%
    pivot_wider(id_cols = c(term), names_from = model, values_from = value, values_fill = list(value = ""))  %>%
    select(Term = term,
           "Social Capital" = "total-social_capital",
           "Social Capital " = "subtypes-social_capital",
           "Bonding Social Capital" = "total-bonding",
           "Bonding Social Capital " = "subtypes-bonding",
           "Bridging Social Capital" = "total-bridging",
           "Bridging Social Capital " = "subtypes-bridging",
           "Linking Social Capital" = "total-linking",
           "Linking Social Capital " = "subtypes-linking") %>%
    mutate(group = case_when(
      Term == "trans" ~ "trans",
      str_detect(Term, "name") ~ "fe",
      Term %in% c("sigma", "(Intercept)", "r.squared", "adj.r.squared","vif", "statistic", "df", "nobs") ~ "gof",
      TRUE ~ "cov")) %>%
    filter(!is.na(Term))

  # Goodness of Fit Stats
  table0 = table %>%
    filter(group == "gof") %>%
    mutate(Term = Term %>% dplyr::recode_factor(
      "(Intercept)" = 'Constant',
      "sigma" = "Sigma (Avg. Error)",
      "r.squared" = "R<sup>2</sup>",
      "adj.r.squared" = "Adj. R<sup>2</sup",
      "vif" = "Max VIF",
      "statistic" = "F statistic (df)",
      "df" = "Degrees of Freedom",
      "nobs" = "N"
    )) %>%
    arrange(Term) %>% select(-group)

  table3 = table %>%
    filter(group == "trans") %>%
    mutate(Term = Term %>% dplyr::recode_factor(
      "trans" = "Transformation")) %>%
    select(-group)

  # Drop fixed effects
  table1 = table %>%
    filter(group == "cov") %>%
    mutate(Term = Term %>% dplyr::recode_factor(
      "log(total + 1)" = "log(Total SI)",
      "log(community_space + 1)" = "log(Community Spaces)",
      "log(place_of_worship + 1)" = "log(Places of Worship)",
      "log(social_business + 1)" = "log(Social Businesses)",
      "log(park + 1)" = "log(Parks)",
      "pop_density_block" = "Pop Density",
      "black_block"= "% Black",
      "hisplat_block" ="% Hispanic/Latino",
      "asian_block" = "% Asian",
      "some_college_bg" = "% Some College",
      "over_65_bg" = "% Over Age 65",
      "unemployment_bg" = "Unemployment Rate",
      "income_0_60K_bg" = "% Income Under 60K",
      "median_household_income_bg" = "Median Household Income"
    )) %>%
    arrange(Term) %>% select(-group)

  # Get Fixed Effects
  table2 = table %>%
    filter(group == "fe") %>%
    rowwise() %>%
    mutate(Term = Term %>% str_remove("name") %>%
             str_replace_all("[_]", " ") %>% str_trim("both") %>%
             str_split(" ") %>% unlist() %>%
             map(~paste0(str_sub(.x, 0,1) %>% toupper(), str_sub(.x, 2, -1))) %>%
             paste0(collapse = " ")) %>%
    mutate(Term = case_when(Term == "Dc" ~ "DC", Term == "La" ~ "LA", TRUE ~ Term)) %>%
    arrange(Term) %>% select(-group)


  list(
    table_a = bind_rows(table3, table1, table0),
    table_b = bind_rows(table3, table2, table0)
  ) %>%
    return()
}



viz = function(path = "viz/table_main.html", path_fe ="viz/table_fe.html", stats){
  library(knitr)
  library(kableExtra)

  table_a = stats$table_a


  kable(table_a, format = "html", caption = "Table X: Ordinary Least Squares Models of Social Capital Index Scores",
        #      col.names = rep(c(""), 6),
        align = c('l', rep('c', 8)), escape = FALSE) %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    kableExtra::add_header_above(header = c(" ", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8")) %>%
    kableExtra::pack_rows(group_label = "Dependent Variable", start_row = 1, end_row = 1) %>%
    kableExtra::pack_rows(group_label = "Independent Variable", start_row = 2, end_row = 6) %>%
    kableExtra::pack_rows(group_label = "Census Covariates", start_row = 7, end_row = 15) %>%
    kableExtra::pack_rows(group_label = "Constants", start_row = 16, end_row = 17) %>%
    kableExtra::pack_rows(group_label = "Goodness of Fit", start_row = 18, end_row = 23) %>%
    {for(i in 2:9){ . = kableExtra::column_spec(., column = i, bold = str_detect(unlist(table_a[,i]), "[*]")) }; .} %>%
    kableExtra::footnote(threeparttable = TRUE,
                         general = paste0(
                           "<b>Statistical Significance</b>: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10. <b>Bold</b> highlights p < 0.05.",
                           "<br>",
                           "<b>Sigma</b>: Residual Standard Error of model. Can be used to approximate error term in model.",
                           "<br>",
                           "<b>Covariates:</b> Sourced from 2020 Decennial Census estimates, at lowest level possible per variable. Calculated as the median value among blocks/block groups overlapping each grid cell.",
                           " Covariates derived from Block-level include Pop Density & % Nonwhite; from Block Group-level include % Some College & Income.",
                           "<br>",
                           "<b>Fixed Effects</b>: Each model includes fixed effects for each city, with Atlanta as the baseline. Omitted from table."),
                         escape = FALSE) %>%
    cat(file = path, sep = "\n")


  table_b = stats$table_b

  kable(table_b, format = "html", caption = "Table X: Ordinary Least Squares Models of Social Capital Index Scores",
        #      col.names = rep(c(""), 6),
        align = c('l', rep('c', 8)), escape = FALSE) %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    kableExtra::add_header_above(header = c(" ", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8")) %>%
    kableExtra::pack_rows(group_label = "Dependent Variable", start_row = 1, end_row = 1) %>%
    kableExtra::pack_rows(group_label = "Fixed Effects", start_row = 2, end_row = 25) %>%
    kableExtra::pack_rows(group_label = "Constants", start_row = 26, end_row = 27) %>%
    kableExtra::pack_rows(group_label = "Goodness of Fit", start_row = 28, end_row = 33) %>%
    {for(i in 2:9){ . = kableExtra::column_spec(., column = i, bold = str_detect(unlist(table_b[,i]), "[*]")) }; .} %>%
    kableExtra::footnote(threeparttable = TRUE,
                         general = paste0(
                           "<b>Statistical Significance</b>: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10. <b>Bold</b> highlights p < 0.05.",
                           "<br>",
                           "<b>Sigma</b>: Residual Standard Error of model. Can be used to approximate error term in model.",
                           "<br>",
                           "<b>Covariates:</b> Sourced from 2020 Decennial Census estimates, at lowest level possible per variable. Calculated as the median value among blocks/block groups overlapping each grid cell.",
                           " Covariates derived from Block-level include Pop Density & % Nonwhite; from Block Group-level include % Some College & Income.",
                           "<br>",
                           "<b>Fixed Effects</b>: Each model includes fixed effects for each city, with Atlanta as the baseline. This table shows only fixed effects; other effects shown elsewhere."),
                         escape = FALSE) %>%
    cat(file = path_fe, sep = "\n")

}

# Load in
read_rds("viz/models_main.rds") %>%
  get_stat() %>%
  viz(path = "viz/table_main.html", path_fe = "viz/table_fe.html", stats = .)

read_rds("viz/models_main_adj.rds") %>%
  get_stat() %>%
  viz(path = "viz/table_main_adj.html", path_fe = "viz/table_fe_adj.html", stats = .)

#browseURL("viz/table_fe.html")

rm(list = ls()); gc()


