# Lake Trout Movement and Spawning Locations within the Tangle Lakes System

This study used telemetric procedures to describe movement and locate spawning areas of lake trout Salvelinus namaycush within the 4 interconnected Tangle Lakes (Upper, Round, Shallow and Lower). A total of 100 radio tags were deployed among the 4 lakes: 13 tags in Upper Tangle, 25 tags in Round Tangle, 22 tags in Shallow Tangle and 40 tags in Lower Tangle. Fish were tracked with airplane, boat and 3 fixed-tracking stations between the lakes. 

In addition, boat tracking occurred at night with spotlights to document spawning aggregations of lake trout in all 4 lakes.

## Operational Plan

The operational plan can be found here:

https://www.adfg.alaska.gov/FedAidPDFs/ROP.SF.3F.2022.03.pdf

## Repository Contents:

### /R

*TangleLakes_supplemental.R* contains some supplemental analysis, since much of the analysis has already been performed elsewhere.  This script has three distinct sections:

- Visualizing movement between discrete lakes as Sankey plots (in which movement is visualized as flow) and discrete time-series plots (in which movement is visualized as points and connecting lines).
- Tabulating movement and creating some additional summaries, also producing basic plots of length composition
- A Bayesian survival model summarizing whether individual-level variables had any overall effect on survival over the course of a project, after accounting for differences in survival rates between sequential surveys.  


### /flat_data

*Tangle_movement.csv* contains the data used as input for the .R file above.  This
sheet was a subset of cells copied from the master .xlsx dataset.

### /figures

All figures created by the .R file.

- *Sankey_full.jpg* and *Sankey_collapse.jpg* give both Sankey (flow) plots, with all possible states and the collapsed states, respectively.
- *DiscreteTS_full.jpg* and *DiscreteTS_tangle.jpg* give both discrete time-series plots, with all possible states and the set of four Tangle lakes, respectively.
- *length_box.jpg* and *length_hist.jpg* give simple length compositions
- *survmodelXX.jpg* give output plots from the set of models for NATURAL mortality
- *survmodelXX_fishingmort.jpg* give output plots from the set of models for FISHING mortality.

### /tables

Tables created by the .R file.  These are created as individual .csv files without any formatting.

- *tableto_XXX.csv* and *tablefrom_XXX.csv* give the counts and proportions of tagged fish migrating TO and FROM each lake/state, relative to each lake/state.  Effectively, these are tabular representations of the flows depicted in the Sankey plots.
- *sum_mvt_betweenlakes.csv* tabulates the numbers of times fish moved between each pair of lakes.  It should be noted that the data source for this was the locations as determined by flights & boat surveys, rather than the fixed stations.
- *scenario_tbl.csv* tabulates the number of fish that stay located in each lake or each pair of lakes.