##### log-binomial model #####
# not using survey package


# complete-case analysis

model_cc <- glm( identity_change ~ sexual_identity_2010, 
                 family = binomial( link = "log" ),
                 data = d_cc )

tidy_cc <- tidy( model_cc, conf.int = TRUE, conf.level = 0.95 )

tidy_cc %>%
  slice( 2 ) %>%
  mutate(
    `RR [95% CI]` = paste0(
      round( exp( estimate ), 2 ), " [",
      round( exp( conf.low ), 2 ), ", ",
      round( exp( conf.high ), 2 ), "]"
    ),
    `Imputation Model` = "Complete-case analysis"
    ) %>%
  select( `RR [95% CI]`, `Imputation Model` )


# imputation analysis

long_data <- complete( imp, action = "long", include = TRUE )
summary( long_data )

fit_model <- with( as.mids( long_data ),
                   glm( identity_change ~ sexual_identity_2010, family = binomial( link = "log" ) ) )

pooled_summary <- summary( pool( fit_model ), conf.int = TRUE )
pooled_summary

tibble(
  "RR [95% CI]" = paste0(
    round( exp( pooled_summary[[ 2, "estimate" ]] ), 2 ), " [",
    round( exp( pooled_summary[[ 2, "conf.low" ]] ), 2 ), ", ",
    round( exp( pooled_summary[[ 2, "conf.high" ]] ), 2 ), "]" )
)
