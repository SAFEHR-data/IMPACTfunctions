

clean_pca_data <- function(PCA_dataframe = PCAs,
                           phases_dataframe = Phases,
                           near_full_threshold = 95,    # reservoir volume (mL) treated as "near full"
                           min_jump_ml = 20,    # minimum positive jump in reservoir volume to call "new bag"
                           priming_allowance_ml = 5,     # allow up to 5mL of post-bag "priming" to be removed once
                           zero_dose_eps_mg = 0.2,   # dose treated as ~0 for mg drugs (morphine/oxycodone)
                           zero_dose_eps_mcg  = 4,     # dose treated as ~0 for fentanyl (mcg)
                           bg_ml_grid = c(0, 0.5, 1.0, 1.5, 2.0),  # candidate background rates (mL/hr) for grid search
                           tz = "Europe/London"

){


}
