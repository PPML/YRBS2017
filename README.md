# YRBS 2017 Predictions

Details and replication code for the manuscript "State-level population estimates of sexual minority adolescents in the United States: a predictive modeling study".

#### R code

| File (Markdown with output)                                        | File (R code)                                                        | Purpose                                                                                                                               |
|--------------------------------------------------------------------|----------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------|
| [train_and_predict.md](train_and_predict.md)                       | [train_and_predict.Rmd](train_and_predict.Rmd)                       | Generates LOO (leave-one-out) predictions for each state for which we have observed responses to one or both of the focal questions.  |
| [evaluate_loo_predictions.md](evaluate_loo_predictions.md)         | [evaluate_loo_predictions.Rmd](evaluate_loo_predictions.Rmd)         | Evaluates the LOO predictions against the true proportions we observe in YRBS data.                                                   |
| [make_final_state_predictions.md](make_final_state_predictions.md) | [make_final_state_predictions.Rmd](make_final_state_predictions.Rmd) | Combines the LOO predictions with true proportions and makes predictions for all the states without responses to the focal questions. |
| [stats_figures_for_paper.md](stats_figures_for_paper.md) | [stats_figures_for_paper.Rmd](stats_figures_for_paper.Rmd) | Creates the figures included in the paper. |

#### Other contents
* `data/` contains input and output CSVs used in the R code. Note that the raw YRBS survey data is not public data and is not included.

#### Requirements
* Git large file storage https://git-lfs.github.com/
* R, RStudio, and the R packages loaded in the R notebooks
