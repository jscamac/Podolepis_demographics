plan <- drake::drake_plan(  
  # Compile expert database
  mortality_data = 
    compile_data(file = drake::file_in("raw_data/podolepis_data.csv"),
                 type = "survival"),
  
  growth_data = 
    compile_data(file = drake::file_in("raw_data/podolepis_data.csv"),
                 type = "growth"),
  
  
  ################# Survival modeling #################
  
  mortality_model_data = 
    mortality_data %>%
    dplyr::mutate(
      elevation_cs = (elevation - 1740)/(2 * sd(elevation)),
      leaf_length_cs = (leaf_length - 6)/(2 * sd(leaf_length))),
  
  # Cubic M-splines hazard model (Flexible parametric model)
  ms_hazard_model = rstanarm::stan_surv(formula = 
                                          Surv(julian_start, 
                                               julian_next, 
                                               death_next) ~ 
                                          leaf_length_cs + 
                                          elevation_cs * treatment_id +
                                          aspect_id * treatment_id +
                                          elevation_cs * aspect_id +
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
                                           leaf_length_cs + 
                                           elevation_cs * treatment_id +
                                           aspect_id * treatment_id +
                                           elevation_cs * aspect_id +
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
                                               elevation_cs * treatment_id +
                                               aspect_id * treatment_id +
                                               elevation_cs * aspect_id +
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
                                                leaf_length_cs + 
                                                elevation_cs * treatment_id +
                                                aspect_id * treatment_id +
                                                elevation_cs * aspect_id +
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
        plot_effects(model = ms_hazard_model,
                     pars = c("(Intercept)",
                              "treatment_id",
                              "leaf_length_cs",
                              "aspect_id",
                              "elevation_cs",
                              "treatment_id:aspect_id",
                              "elevation_cs:treatment_id",
                              "elevation_cs:aspect_id"),
                     par_labels = c("(Intercept)" = "Intercept",
                                    "treatment_id" = "Gap treatment",
                                    "leaf_length_cs" = "Leaf length",
                                    "aspect_id" = "SE Aspect",
                                    "elevation_cs" = "Elevation",
                                    "treatment_id:aspect_id" = "SE Aspect x Gap",
                                    "elevation_cs:treatment_id" = "Elevation x Gap",
                                    "elevation_cs:aspect_id" = "Elevation x SE Aspect"
                     ),
                     xlabel = "Standardized effects"),
        # Treatment
        plot_survcurve(model = ms_hazard_model,
                       new_data = data.frame(
                         key = c("Control", "Gap"),
                         leaf_length_cs = c(0,0),
                         treatment_id = c(0,1),
                         aspect_id = c(0,0),
                         elevation_cs = c(0,0),
                         ind_id = c(0,0),
                         site_id = c(0,0)),
                       legend_label = "Treatment",
                       xlabel = "Year") +
          ggplot2::theme(legend.position = "top") +
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,3)),

        # Leaf length
        plot_survcurve(model = ms_hazard_model,
                       new_data = data.frame(
                         key = c("1 cm", "18 cm"),
                         leaf_length_cs =
                           # Convert key values to standardized values
                           c((1 - 6)/(2 * sd(mortality_model_data$leaf_length)),
                             (18 - 6)/(2 * sd(mortality_model_data$leaf_length))),
                         treatment_id = c(0,0),
                         aspect_id = c(0,0),
                         elevation_cs = c(0,0),
                         ind_id = c(0,0),
                         site_id = c(0,0)),
                       legend_label = "Leaf length",
                       xlabel = "Year") +
          ggplot2::theme(legend.position = "top") +
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,3)),

        # Aspect
        plot_survcurve(model = ms_hazard_model,
                       new_data = data.frame(
                         key = c("NW", "SE"),
                         leaf_length_cs = c(0,0),
                         treatment_id = c(0,0),
                         aspect_id = c(0,1),
                         elevation_cs = c(0,0),
                         ind_id = c(0,0),
                         site_id = c(0,0)),
                       legend_label = "Aspect",
                       xlabel = "Year") +
          ggplot2::theme(legend.position = "top") +
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,3)),
        # Elevation
        plot_survcurve(model = ms_hazard_model,
                       new_data = data.frame(
                         key = c("1620", "1860"),
                         leaf_length_cs = c(0,0),
                         treatment_id = c(0,0),
                         aspect_id = c(0,0),
                         elevation_cs = # Convert key values to standardized values
                           c((1620 - 1740)/(2 * sd(mortality_model_data$elevation)),
                             (1860 - 1740)/(2 * sd(mortality_model_data$elevation))),
                         ind_id = c(0,0),
                         site_id = c(0,0)),
                       legend_label = "Elevation",
                       xlabel = "Year") +
          ggplot2::theme(legend.position = "top") +
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,3)),
        ncol = 1, labels = LETTERS),
      filename = "manuscript/figures/hazard_figure.pdf",
      width = 5,
      height = 8),

  plot_site_hazard_effects =
    plot_ranefs(model = ms_hazard_model,
                type = "site_id",
                ylabel = "Site effects",
                outfile = "manuscript/figures/hazard_site_effect.pdf",
                width = 5,
                height = 7),

  plot_ind_hazard_effects =
    plot_ranefs(model = ms_hazard_model,
                type = "ind_id",
                ylabel = "Individual effects",
                outfile = "manuscript/figures/hazard_ind_effect.pdf",
                width = 5,
                height = 7),
  
  ################# Growth modelling #################
  
  growth_model_data = 
    growth_data %>%
    dplyr::mutate(elevation_cs = (elevation - 1740)/(2 * sd(elevation))),
  
  # Logistic model
  logistic_model =
    brms::brm(
      bf(
        leaf_length | trunc(lb=0) ~ Asym/(1 + (Asym/init_leaf_length -1) * exp(-exp(lograte) * julian_date)), 
        lograte ~ treatment_id * elevation_cs + treatment_id * aspect_id + elevation_cs * aspect_id + 
          (1|site_id) + (1|ind_id),
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
        lograte ~ treatment_id * elevation_cs + treatment_id * aspect_id + elevation_cs * aspect_id + 
          (1|site_id) + (1|ind_id),
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
        lograte ~ treatment_id * elevation_cs + treatment_id * aspect_id + elevation_cs * aspect_id + 
          (1|site_id) + (1|ind_id),
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
        plot_effects(vb_growth_model,
                     pars = c("b_lograte_Intercept",
                              "b_lograte_treatment_id",
                              "b_lograte_aspect_id",
                              "b_lograte_elevation_cs",
                              "b_lograte_treatment_id:aspect_id",
                              "b_lograte_treatment_id:elevation_cs",
                              "b_lograte_elevation_cs:aspect_id"),
                     par_labels = c("b_lograte_Intercept" = "Intercept",
                                    "b_lograte_treatment_id" = "Gap treatment",
                                    "b_lograte_aspect_id" = "SE Aspect",
                                    "b_lograte_elevation_cs" = "Elevation",
                                    "b_lograte_treatment_id:aspect_id" = "SE Aspect x Gap",
                                    "b_lograte_treatment_id:elevation_cs" = "Elevation x Gap",
                                    "b_lograte_elevation_cs:aspect_id" = "Elevation x SE Aspect"),
                     xlabel = "Standardized effects"),
        # Treatment response
        plot_growthcurve(model = vb_growth_model,
                         new_data =
                           data.frame(key = rep(c("Control", "Gap"), each =100),
                                      julian_date = rep(seq(0.001, 4, length.out = 100), times = 2),
                                      init_leaf_length = 1,
                                      treatment_id = rep(c(0,1), each =100),
                                      aspect_id = 0,
                                      elevation_cs = 0),
                         xlabel = "Year",
                         legend_label = "Treatment") +

          ggplot2::theme(legend.position = "top"),

        # Aspect
        plot_growthcurve(model = vb_growth_model,
                         new_data =
                           data.frame(key = rep(c("NW", "SE"), each =100),
                                      julian_date = rep(seq(0.001, 4, length.out = 100), times = 2),
                                      init_leaf_length = 1,
                                      treatment_id = 0,
                                      aspect_id = rep(c(0,1), each =100),
                                      elevation_cs = 0),
                         xlabel = "Year",
                         legend_label = "Aspect") +
          ggplot2::theme(legend.position = "top"),

        # Elevation
        plot_growthcurve(model = vb_growth_model,
                         new_data =
                           data.frame(key = rep(c("1620", "1860"), each =100),
                                      julian_date = rep(seq(0.001, 4, length.out = 100), times = 2),
                                      init_leaf_length = 1,
                                      treatment_id = 0,
                                      aspect_id = 0,
                                      elevation_cs = # Convert key values to standardized values
                                        rep(c((1620 - 1740)/(2 * sd(growth_model_data$elevation)),
                                              (1860 - 1740)/(2 * sd(growth_model_data$elevation))), each=100)),
                         xlabel = "Year",
                         legend_label = "Elevation") +
          ggplot2::theme(legend.position = "top"),
        ncol=1, labels = LETTERS),
      filename = "manuscript/figures/growth_figure.pdf",
      width = 5,
      height = 7),

  plot_site_growth_effects =
    plot_ranefs(model = vb_growth_model,
                type = "site_id",
                ylabel = "Site effects",
                outfile = "manuscript/figures/growth_site_effect.pdf",
                width = 5,
                height = 7),

  plot_ind_growth_effects =
    plot_ranefs(model = vb_growth_model,
                type = "ind_id",
                ylabel = "Individual effects",
                outfile = "manuscript/figures/growth_ind_effect.pdf",
                width = 5,
                height = 7)
)







