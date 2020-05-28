###############################################################################
# Entry point for application
#
# Author: Vivek Katial
# Created 2020-05-26 01:07:24
###############################################################################


# Dependencies ------------------------------------------------------------

library(shiny)
library(tidyverse)
library(here)

source(here("utils/mlflow-utils.R"))

# Read Data ---------------------------------------------------------------

d_runs <- get_mlflow_data(here("data/d_runs.csv"))
