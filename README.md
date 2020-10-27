## Reproducing analyses

This analysis requires R 4.0 or higher to be installed as well as `dplyr`, `rstanarm`, `brms`, `drake`,`loo`,`future`,`cowplot` & `ggplot2`

In order to maximise the reproducibility of our analyses, we've implemented the code in a make-like workflow using the R package drake.
To reproduce the analysis simply download this repository and open R in the downloaded directory. Then simply run:

```
source("run_drake.R")
```

This should clean, summarise and plot the data.

The workflow instructions can be found in the folder `drake_plan`.

To extract processed data simply find the relevant target in `drake_plan/plan.R` and run the following:

```
drake::readd(target_name)
```

For example, say we want to look at the species summary statistics would could obtain these by running:

```
drake::readd(mortality_model_data)
```