plan <- drake::drake_plan(  
  # Compile expert database
  mortality_data = 
    compile_data(file = drake::file_in("raw_data/podolepis_data.csv"),
                 type = "survival"),
  
  growth_data = 
    compile_data(file = drake::file_in("raw_data/podolepis_data.csv"),
                 type = "growth"),
  
  climate_data =
    summarise_climate(file = drake::file_in("raw_data/nichemapper_output.csv"),
                      census_dates = unique(c(mortality_data$start_date, 
                                              mortality_data$next_date)), 
                      wilting_pt = 0.15),
  
  
  ################# Survival modelling #################
  
  mortality_model_data = dplyr::left_join(mortality_data, climate_data,
                                          by = c("site_id", "census")) %>%
    dplyr::mutate(
      leaf_length_cs = (leaf_length - 6)/(2 * sd(leaf_length)),
      D0cm_cs = (D0cm - (10*24*365.25))/(2 * sd(D0cm))),
  
  # Cubic M-splines hazard model (Flexible parametric model)
  
  ms_hazard_model = rstanarm::stan_surv(formula = 
                                          Surv(julian_start, 
                                               julian_next, 
                                               death_next) ~ 
                                          treatment_id + 
                                          leaf_length_cs + 
                                          D0cm_cs + 
                                          (1|site_id) + 
                                          (1|ind_id),
                                        data = mortality_model_data, 
                                        basehaz = "ms"),
  
  ms_hazard_model_kfold = rstanarm::kfold(ms_hazard_model, 
                                          folds = 
                                            loo::kfold_split_grouped(
                                              K=10, 
                                              x= mortality_model_data$ind_id),
                                          cores = parallel::detectCores()),
  
  # Exponential hazard model
  
  exp_hazard_model = rstanarm::stan_surv(formula = 
                                           Surv(julian_start, 
                                                julian_next, 
                                                death_next) ~ 
                                           treatment_id + 
                                           leaf_length_cs + 
                                           D0cm_cs + 
                                           (1|site_id) + 
                                           (1|ind_id),
                                         data = mortality_model_data, 
                                         basehaz = "exp"),
  
  exp_hazard_model_kfold = rstanarm::kfold(exp_hazard_model, 
                                           folds = 
                                             loo::kfold_split_grouped(
                                               K=10, 
                                               x= mortality_model_data$ind_id),
                                           cores = parallel::detectCores()),
  
  # Weibull hazard model
  
  weibull_hazard_model = rstanarm::stan_surv(formula = 
                                               Surv(julian_start, 
                                                    julian_next, 
                                                    death_next) ~ 
                                               treatment_id + 
                                               leaf_length_cs + 
                                               D0cm_cs + 
                                               (1|site_id) + 
                                               (1|ind_id),
                                             data = mortality_model_data, 
                                             basehaz = "weibull"),
  
  weibull_hazard_model_kfold = rstanarm::kfold(weibull_hazard_model, 
                                               folds = 
                                                 loo::kfold_split_grouped(
                                                   K=10, 
                                                   x= mortality_model_data$ind_id),
                                               cores = parallel::detectCores()),
  
  # Gompertz hazard model
  
  gompertz_hazard_model = rstanarm::stan_surv(formula = 
                                                Surv(julian_start, 
                                                     julian_next, 
                                                     death_next) ~ 
                                                treatment_id + 
                                                leaf_length_cs + 
                                                D0cm_cs + 
                                                (1|site_id) + 
                                                (1|ind_id),
                                              data = mortality_model_data, 
                                              basehaz = "gompertz"),
  
  gompertz_hazard_model_kfold = rstanarm::kfold(
    gompertz_hazard_model, 
    folds = 
      loo::kfold_split_grouped(
        K=10, 
        x= mortality_model_data$ind_id),
    cores = parallel::detectCores()),
  
  # Compare hazard models
  
  hazard_model_decision = collate_cv_stats(x = list(ms_hazard_model_kfold, 
                                                    exp_hazard_model_kfold, 
                                                    weibull_hazard_model_kfold,
                                                    gompertz_hazard_model_kfold)),
  
  plot_hazard_decision =
    plot_cv(hazard_model_decision,
            model_labs = c("ms_hazard_model" = "Cubic M-splines hazard model",
                           "exp_hazard_model" = "Exponential hazard model",
                           "weibull_hazard_model" = "Weibull hazard model",
                           "gompertz_hazard_model" = "Gompertz hazard model"),
            outfile = file_out("manuscript/figures/hazard_CV.pdf"),
            height = 6,
            width = 6
    ),
  
  survival_plot = 
    # Coefficient plot
    ggplot2::ggsave(
      cowplot::plot_grid(
        plot_effects(model = gompertz_hazard_model, 
                     pars = c("(Intercept)",
                              "treatment_id", 
                              "leaf_length_cs",
                              "D0cm_cs"),
                     par_labels = c("(Intercept)" = "Intercept", 
                                    "treatment_id" = "Gap treatment", 
                                    "leaf_length_cs" = "Leaf length",
                                    "D0cm_cs" = "CDCH (surface)"),
                     xlabel = "Standardized effects"),
        # Treatment
        plot_survcurve(model = gompertz_hazard_model,
                       new_data = data.frame(
                         key = c("Control", "Gap"),
                         leaf_length_cs = c(0,0),
                         treatment_id = c(0,1),
                         D0cm_cs = c(0,0),
                         ind_id = c(0,0),
                         site_id = c(0,0)),
                       legend_label = "Treatment",
                       xlabel = "Year") +
          ggplot2::theme(legend.position = "top"),
        
        # Leaf length
        plot_survcurve(model = gompertz_hazard_model,
                       new_data = data.frame(
                         key = c("2 cm", "10 cm"),
                         leaf_length_cs = 
                           # Convert key values to standardized values
                           c((2 - 6)/(2 * sd(mortality_model_data$leaf_length)),
                             (10 - 6)/(2 * sd(mortality_model_data$leaf_length))),
                         treatment_id = c(0,0),
                         D0cm_cs = c(0,0),
                         ind_id = c(0,0),
                         site_id = c(0,0)),
                       legend_label = "Leaf length",
                       xlabel = "Year") +
          ggplot2::theme(legend.position = "top"),
        
        # Cumulative degree hours
        plot_survcurve(model = gompertz_hazard_model,
                       new_data = data.frame(
                         key = c("5 °C", "15 °C"),
                         leaf_length_cs = c(0,0),
                         treatment_id = c(0,0),
                         D0cm_cs = 
                           c(((5*24*365.25) - (10*24*365.25))/(2 * sd(mortality_model_data$D0cm)),
                             ((15*24*365.25 - (10*24*365.25))/(2 * sd(mortality_model_data$D0cm)))),
                         ind_id = c(0,0),
                         site_id = c(0,0)),
                       legend_label = "Average Surface temperature",
                       xlabel = "Year") +
          ggplot2::theme(legend.position = "top"),
        ncol = 1, labels = LETTERS),
      filename = "manuscript/figures/hazard_figure.pdf",
      width = 5,
      height = 8),
  
  plot_site_hazard_effects =
    plot_ranefs(model = gompertz_hazard_model,
                type = "site_id",
                ylabel = "Site effects",
                outfile = "manuscript/figures/hazard_site_effect.pdf",
                width = 5,
                height = 7),
  
  plot_ind_hazard_effects =
    plot_ranefs(model = gompertz_hazard_model,
                type = "ind_id",
                ylabel = "Individual effects",
                outfile = "manuscript/figures/hazard_ind_effect.pdf",
                width = 5,
                height = 7),
  
  ################# Growth modelling #################
  
  growth_model_data = dplyr::left_join(growth_data, climate_data,
                                       by = c("site_id", "census")) %>%
    dplyr::mutate(D0cm_cs = (D0cm - (10*24*365.25))/(2 * sd(D0cm))),
  
  # Logistic model
  logistic_model =
    brms::brm(
      bf(
        leaf_length | trunc(lb=0) ~ Asym/(1 + (Asym/init_leaf_length -1) * exp(-exp(lograte) * julian_date)), 
        lograte ~ treatment_id + D0cm_cs + (1|site_id) + (1|ind_id),
        Asym ~ 1, nl = TRUE),
      data = growth_model_data, 
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      cores = 4, 
      prior = 
        prior(normal(0, 5),class = "b", nlpar ="lograte") + 
        prior(uniform(5,30), lb=5, ub=30, nlpar="Asym")
    ),
  
  logistic_growth_kfold = brms::kfold(logistic_model,
                                      K=10,
                                      folds = "grouped",
                                      group = "ind_id"),
  
  # Exponential model
  
  neg_exp_growth_model =
    brms::brm(
      bf(
        leaf_length | trunc(lb=0) ~ Asym * (1 -(1 - (init_leaf_length/Asym)) * exp(-exp(lograte) * julian_date)), 
        lograte ~ treatment_id + D0cm_cs + (1|site_id) + (1|ind_id),
        Asym ~ 1, nl = TRUE),
      data = growth_model_data, 
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      cores = 4, 
      prior = 
        prior(normal(0,5),class = "b", nlpar ="lograte") + 
        prior(uniform(5,30), lb=5, ub=30, nlpar="Asym")
    ),
  
  neg_exp_growth_kfold = brms::kfold(neg_exp_growth_model, 
                                     K=10,
                                     folds = "grouped",
                                     group = "ind_id"),
  
  # von Bertalanffy model
  vb_growth_model =
    brms::brm(
      bf(
        leaf_length | trunc(lb=0) ~ Asym * (1 + ((init_leaf_length/Asym)^(0.33)-1) * exp(-exp(lograte) * julian_date))^3, 
        lograte ~ treatment_id + D0cm_cs + (1|site_id) + (1|ind_id),
        Asym ~ 1, nl = TRUE),
      data = growth_model_data, 
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      cores = 4, 
      prior = 
        prior(normal(0,5),class = "b", nlpar ="lograte") + 
        prior(uniform(5,30), lb=5, ub=30, nlpar="Asym")
    ),
  
  vb_growth_kfold = brms::kfold(vb_growth_model, 
                                K=10,
                                folds = "grouped",
                                group = "ind_id"),
  
  # Compare growth models
  
  growth_model_decision = collate_cv_stats(x = list(logistic_growth_kfold, 
                                                    neg_exp_growth_kfold, 
                                                    vb_growth_kfold)),
  
  plot_growth_decision =
    plot_cv(growth_model_decision,
            model_labs = c("logistic_model" = "Logistic model",
                           "exp_growth_model" = "Negative exponential model",
                           "vb_growth_model" = "von Bertalanffy model"),
            outfile = file_out("manuscript/figures/growth_CV.pdf"),
            height = 4,
            width = 6
    ),
  
  growth_plot = 
    ggplot2::ggsave(
      cowplot::plot_grid(
        # Coefficient plot
        plot_effects(logistic_model, 
                     pars = c("b_lograte_Intercept",
                              "b_lograte_treatment_id", 
                              "b_lograte_D0cm_cs"),
                     par_labels = c("b_lograte_Intercept" = "Intercept", 
                                    "b_lograte_treatment_id" = "Gap treatment", 
                                    "b_lograte_D0cm_cs" = "CDCH (surface)"),
                     xlabel = "Standardized effects"),
        
        # Treatment response
        plot_growthcurve(model = logistic_model, 
                         new_data = 
                           data.frame(key = rep(c("Control", "Gap"), each =5),
                                      julian_date = rep(0:4, times = 2),
                                      init_leaf_length = rep(6,10),
                                      treatment_id = rep(c(0,1), each =5),
                                      D0cm_cs = rep(0,10)),
                         xlabel = "Year",
                         legend_label = "Treatment") +
          
          ggplot2::theme(legend.position = "top"),
        
        # Cumulative degree hour response
        plot_growthcurve(model = logistic_model, 
                         new_data = 
                           data.frame(key = rep(c("5 °C", "15 °C"), each =5),
                                      julian_date = rep(0:4, times = 2),
                                      init_leaf_length = rep(6,10),
                                      treatment_id = rep(c(0,0), each =5),
                                      D0cm_cs =
                                        rep(c(((5*24*365.25) - (10*24*365.25))/(2 * sd(growth_model_data$D0cm)),
                                              ((15*24*365.25 - (10*24*365.25))/(2 * sd(growth_model_data$D0cm)))), each =5)),
                         xlabel = "Year",
                         legend_label = "Average Surface temperature") +
          ggplot2::theme(legend.position = "top"),
        ncol=1, labels = LETTERS),
      filename = "manuscript/figures/growth_figure.pdf",
      width = 5,
      height = 7),
  
  plot_site_growth_effects =
    plot_ranefs(model = logistic_model,
                type = "site_id",
                ylabel = "Site effects",
                outfile = "manuscript/figures/growth_site_effect.pdf",
                width = 5,
                height = 7),
  
  plot_ind_growth_effects =
    plot_ranefs(model = logistic_model,
                type = "ind_id",
                ylabel = "Individual effects",
                outfile = "manuscript/figures/growth_ind_effect.pdf",
                width = 5,
                height = 7)
)







