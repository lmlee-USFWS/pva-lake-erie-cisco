# pva-lake-erie-cisco

## Overview

This repository contains a R Shiny application that allows users to run a population viability analysis for Cisco in Lake Erie. The analysis is based on a stochastic Leslie matrix projection model (female-only). The model computes population persistence for user-specified scenarios.

Additional details describing the model and preliminary results can be found in this [white paper](https://github.com/lmlee-USFWS/pva-lake-erie-cisco/blob/main/wp_PVALakeErieCisco_FollowUp_final.pdf).

More information on restoring coregonines to the Great Lakes can be found [here](https://www.greatlakesciscoes.org/).

## Installation

Start an R session and run these lines:

`list.of.packages <- c("compositions","dplyr","EnvStats","ggplot2","mpmsim","popbio","readxl",
"reshape2","rhandsontable","rsconnect","shiny","shinyalert","shinycssloaders","shinydashboard","shinyjs",
"utils","xlsx")`

`new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]`

`if(length(new.packages)) install.packages(new.packages)`

`shiny::runGitHub("pva-lake-erie-cisco","lmlee-USFWS")`


## Getting help

Contact the [project maintainer](mailto:Laura_Lee@fws.gov) for help with this repository.

## Contribute
Contact the project maintainer for information about contributing to this repository. Submit a [GitHub Issue](https://github.com/lmlee-USFWS/pva-lake-erie-cisco/issues) to report a bug or request a feature or enhancement.

## USFWS Disclaimer
The United States Fish and Wildlife Service (FWS) GitHub project code is provided on an “as is” basis and the user assumes responsibility for its use. FWS has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by FWS. The FWS seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by FWS or the United States Government.

-----

![](https://i.creativecommons.org/l/by/4.0/88x31.png) This work is
licensed under a [Creative Commons Attribution 1.0 International
License](https://creativecommons.org/licenses/by/1.0/).
