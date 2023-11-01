******************** ECON807 Policy Project ********************************
* Daniel Sanchez
* SFU Spring 2023

* This script prepares data for analysis

// -------------- Preliminaries ----------------------------------------- //

* Clear my workspace

clear

* No need to set a working directory as a Stata project is being used

* Ensures compatibility with other Stata versions

version 15 

* Turn off the more button to see more results in the screen

set more off

* Turns off any log that is open 

capture log close 

* Start a new log

log using log_policy_project, replace

// --------------------- Load data ------------------------------ //

import excel using "data/directorio_companias.xlsx"

// -------------- End ----------------------------------------- //

* Turn off the log

log close

* Exit

exit