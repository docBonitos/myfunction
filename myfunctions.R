#regression model to multiple columns and subsections
fit_fun <- function(df){
  summary(lm(y_var ~ x_var, data = df)) #depending on the data
}
by(df, df$group, fit_fun)


fit_fun <- function(df, y){ #for changing y variables
  lm_mod <- substitute(
    y ~ Census_Interval,
    list(y = substitute(y))
  )
  eval(summary(lm(lm_mod, data = df)))
}
by(df, y = y_var, df$group, fit_fun)


fit_fun <- function(df, x, y){ #for changing y and x variables
  lm_mod <- substitute(
    y ~ x,
  list(y = substitute(y), x = substitute(x))
  )
eval(summary(lm(lm_mod, data = df)))
}
by(df, y = y_var, x = x_var, df$group, fit_fun)


#Data summary
data_summary <- function(x, num_var, ...){
  group_var <- quos(...)
  num_var <- enquo(num_var)
  x %>%
    group_by(!!!group_var) %>%
    summarize(mean = mean(!!num_var), n = n(), 
              sd = sd(!!num_var), se = sd/sqrt(n))
}
