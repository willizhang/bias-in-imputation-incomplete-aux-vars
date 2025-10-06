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
impute_data_4 <- d_2010_selected_final %>%
  select( sexual_identity_2010, identity_change, age_2010, sex, country_of_birth ) %>%
  mutate( identity_change = as.factor( identity_change ) )
summary( impute_data_4 )
str( impute_data_4 )
miss_var_summary( impute_data_4  )

imp_4 <- mice( impute_data_4, m = 50, seed = 123, print = FALSE )
summary( imp_4 )
plot( imp_4 )

long_data_4 <- complete( imp_4, action = "long", include = TRUE )
summary( long_data_4 )

fit_model_4 <- with( as.mids( long_data_4 ),
                     glm( identity_change ~ sexual_identity_2010, family = binomial( link = "log" ) ) )

pooled_summary_4 <- summary( pool( fit_model_4 ), conf.int = TRUE )
pooled_summary_4

model_4 <- tibble(
  "Estimate (log RR)" = round( pooled_summary_4[[ 2, "estimate" ]], 3 ), 
  "SE" = round( pooled_summary_4[[ 2, "std.error" ]], 3 ),
  "Lower CI" = round( pooled_summary_4[[ 2, "conf.low" ]], 3 ),
  "Upper CI" = round( pooled_summary_4[[ 2, "conf.high" ]], 3 ),
  
  "RR [95% CI]" = paste0(
    round( exp( pooled_summary_4[[ 2, "estimate" ]] ), 2 ), " [",
    round( exp( pooled_summary_4[[ 2, "conf.low" ]] ), 2 ), ", ",
    round( exp( pooled_summary_4[[ 2, "conf.high" ]] ), 2 ), "]" )
)
model_4[["Imputation Model"]] <- "Model 4: sexual identity, age, sex, country of birth, education, occupation"
model_4