**Project Name**
In-person presidential campaign rallies and trends in country-level COVID-19 cases and deaths

**Project Members**
Daniel Arias, Kritika Anand, and Tianxiao Zhao

**Background and Motivation**

In-person rallies are large events and may contribute to the spread of COVID-19; in many jurisdictions in the United States, large gatherings have been prohibited or are regulated to promote social distancing and reduce the transmission of COVID-19, and the federal government has issued guidance underscoring the risk large events pose when social distancing may not be readily enforceable. In the lead-up to the November 3 2020 Presidential election, President Trump conducted numerous in-person rallies, both before and after the White House COVID-19 outbreak of September-October 2020. In the 30 days prior to the election, President Trump held 45 in-person rallies across multiple states (Vice President Biden has held numerous drive-in rallies during the same period). Given social distancing guidelines and numerous COVID-19 diagnoses among the President’s re-election campaign (including the President himself), numerous public health researchers, professionals, and observers have raised concerns that the President’s in-person rallies may be contributing to the spread of COVID-19.

Conflicting investigations and working papers have suggested that rallies are and are not associated with local spikes in COVID-19 transmission (Dave et al 2020; Bernheim et al 2020). Motivated by the urgency of the COVID-19 pandemic and the saliency of whether in-person rallies may be associated with excess cases and mortality, we are interested in using time-series data on county-level COVID-19 cases and deaths to visualize trends in transmission in counties in which in-person campaign rallies were hosted and to compare these counties to others in which in-person events were not held. We hope that this analysis will provide additional information on the potential impacts and risks of in-person rallies, encouraging safer measures and accountability for activities that pose risk to the public.

**Initial Questions**

* Is there a geographic overlap in locations of counties and locations of increased COVID-19 cases, in and around counties that held rallies? 
* Did COVID-19 cases increase in counties that held rallies over the subsequent 3 weeks?
* Were counties that held rallies experiencing different levels of COVID-19 cases and deaths (prior to rallies) than comparable counties that did not hold rallies?
* Was the presence of a Trump rally in a county associated with a significant change in the trend of COVID-19 cases and deaths in the ensuing period, compared to similar counties that did not host rallies?

**Data**

Our data were collected from the following sources:

* COVID-19 case and death data at the county-level were collected from the Center for Systems Science and Engineering, Johns Hopkins University: https://github.com/CSSEGISandData/COVID-19

* Exposure data (location and date of rallies) were compiled manually from https://en.wikipedia.org/wiki/List_of_post-election_Donald_Trump_rallies, and were verified against the campaign's posted schedule (https://www.donaldjtrump.com/events/) and composites of the President's official schedule (https://factba.se/topic/calendar). 

* Median income of counties in 2018 data were obtained from the Economic Research Service of the U.S. Department of Agriculture: https://www.ers.usda.gov/data-products/county-level-data-sets/ 

* County land area size data (used to calculate population density) were obtained from the U.S. Census Bureau: https://www.census.gov/library/publications/2011/compendia/usa-counties-2011.html
 
**Design Overview**

* Data visualization and visual analysis using heat maps, choropleths, line graphs
* Propensity score matching to allow comparison in counties with and without rallies 
* Shiny app to allow for visualization of trends in specific counties and comparison across different measures of disease burden

**Screencast Link**

**Guide to Files**
Our Rmarkdown code and knitted HTML file can be found under the folder "code." All the necessary data to run the analysis can be found in the folder "data". We have also added a guideline for our Shiny app, which can be found in the main folder.
 
