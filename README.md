# Description

This ensemble of scripts aims to assess the current and projected P
cycle in EU agricultural soils using the calibrated DayCent model at
European level and data-derived soil properties, advanced input data
sets, and representative management practices. The description of the
model and assumptions can be found either in the article or in the
supplementary material. In case you want to know individual model
parameters not described in the manuscript or the referred literature,
contact the author of the study.

Data analysis was conducted in r, using the following packages:

**LIST OF PACKAGES AND VERSION**

Packages without specified installation are available on CRAN
repository.

> library(raster) v3.5-29

> library(rasterVis) v0.51.2

> library(ggplot2) v3.3.6

> library(sf) v1.0-7

> library(tidyverse) v1.3.1

> library(readxl) v1.4.1

> library(rgeos) v0.5-9

> library (rgdal) v1.5-32

> library (dplyr) v1.0.10

> library (sp) v1.4-7

> library(\"xlsx\") v0.6.5

> library(terra)v. 1.6-17

The folder includes the following scripts:

**1\_current.R**

**2\_projection.R**

**3\_P\_EU.qgz**

#1\_current.R

This script allows to calculate the average annual P flows used to
determine the P budget for the period 2010--2019. In detail the script
loads the rasterstack with the current P balance flows (Current\_EU.tif)
and the current P balance (Pbal\_EU.tif). It then calculates the current
average yearly values for:

a.  P input (mineral + organic)
b.  mineral P input
c.  organic P input
d.  P input through weathering of parent rock
e.  Net P erosion
f.  P export via grain harvest and residue removal
g.  organic P leaching
h.  Available P pool (shown as resin extractable (Model output) and
    transformed to Olsen extractable using Steinfurth et al. 2021)
i.  Total P pool
j.  P balance

These average values were used in the manuscript text, and the
individual raster files of the individual flows incorporated in Figures
2, 3, 4, 5, S10, and Table S3.

#2\_projection.R

This script enables the assessment of projected phosphorus (P) flows
depending on the agricultural management practice scenarios. The raster
stacks encompass the P flows of the agricultural management practice
scenarios applied at 100% of the agricultural areas for the periods
2020--2029 and 2040--2049. The scenarios include the business as usual
(BAU**), increased use of cover crops (CC), increased use of cover crops
that fix nitrogen (CC\_nfix), reduced mineral P input (lowPmin), and
reduced organic P input (lowPorg).

Additionally, raster files depicting the available P (in Olsen
extractable P) for the period 2010--2019 and the bulk density were
loaded to calculate the three different target areas (Pav\_suff, leach,
Peros\_max) where the scenarios would be applied at (lines 134-152).
These areas were used for cropping, masking, and merging the scenario
raster files with the BAU raster file (lines 130-834). The resulting
merged raster files were then used to calculate the average annual flows
used in the manuscript text, values presented in Figures 6, S11, S12,
S13, and Tables S4 and S5.

Then we analyzed how the target areas evolve over time (from 2010-2019
to 2040-2049) depending on the agricultural management (lines 838-904)
and identified which agricultural management practice scenario resulted
in the lowest erosion, the lowest P balance, and the highest P export
(lines 907-937). This analysis resulted in Figure 7, and Table S6.

**References**

Steinfurth, K., Hirte, J., Morel, C., Buczko, U., 2021. Conversion
equations between Olsen-P and other methods used to assess plant
available soil phosphorus in Europe -- a review. Geoderma 401 (December
2020), 115339. https://doi.org/10.1016/j. geoderma.2021.115339.
