SOEL
====

SOEL (**S**imulation of **O**ak **E**arly **L**ife history) is an individual-based model of the oak lifecycle. SOEL simulates individual acorns, seedlings, saplings, and mature trees and their interactions with animals (including weevil infestation, acorn predation and dispersal by small mammals, and herbivory).

The model (`SOEL.nlogo`) is implemented in the [NetLogo](http://ccl.northwestern.edu/netlogo/) language and can be called from R using the `run_SOEL.R` script. Data to parameterize the model were primarily collected as part of the [Hardwood Ecosystem Experiment](http://www.heeforeststudy.org), a long-term study of forest ecosystem responses to management.

A description of the model along with several case studies are published in the following paper:

Kellner, Kenneth F.; Swihart, Robert K. 2017. Simulation of oak early life history and interactions with disturbance via an individual-based model, SOEL. PLOS ONE. [[link]]()

A more detailed description of SOEL and how it was developed can be found in the supplementary information (S1 Appendix).

Metadata
--------

`appendices`: LaTeX code for S1 Appendix and R code for individual parameter regression models (provided in S3 Code).

`data`: Raw datasets from the HEE used to parameterize the model (see S3 Code).

`casestudies`: R code to set up and run the case studies in the manuscript and the validation experiments found in S1 Appendix.

`figures`: Code to generate figures 2-6 in the manuscript and figures 1-6 in S1 Appendix.

`JABOWA.nlogo`: Source code for JABOWA implementation in NetLogo, used in the validation experiments in S1 Appendix. Can be called in R using `run_SOEL.R` script. Part of S2 Code.

`run_SOEL.R`: R script that can call SOEL and the JABOWA implementation from within R using the RNetLogo package. The script can run multiple simulations in parallel.

`SOEL.nlogo`: Source code for SOEL in the NetLogo language. Part of S2 Code.

`utility_functions.R`: Functions for formatting and analyzing SOEL output.
