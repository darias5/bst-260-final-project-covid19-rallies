**Background and Motivation**

In-person rallies are large events and may contribute to the spread of COVID-19; in many jurisdictions in the United States, large gatherings have been prohibited or are regulated to promote social distancing and reduce the transmission of COVID-19, and the federal government has issued guidance underscoring the risk large events pose when social distancing may not be readily enforceable. In the lead-up to the November 3 2020 Presidential election, President Trump has conducted numerous in-person rallies, both before and after the White House COVID-19 outbreak of September-October 2020. In the 30 days prior to the election, President Trump held 45 in-person rallies across multiple states (Vice President Biden has held numerous drive-in rallies during the same period). Given social distancing guidelines and numerous COVID-19 diagnoses among the President’s re-election campaign (including the President himself), numerous public health researchers, professionals, and observers have raised concerns that the President’s in-person rallies may be contributing to the spread of COVID-19. Conflicting investigations and working papers have suggested that rallies are and are not associated with local spikes in COVID-19 transmission (Dave et al 2020; Bernheim et al 2020). Motivated by the urgency of the COVID-19 pandemic and the saliency of whether in-person rallies may be associated with excess cases and mortality, we are interested in using time-series data on county-level COVID-19 cases and deaths to visualize trends in transmission in counties in which in-person campaign rallies were hosted and to compare these counties to others in which in-person events were not held. 

**Project Objectives**

*Research goals:*
To examine whether the presence of a Trump rally in a county was associated with a significant change in the trend of COVID-19 cases and deaths in the ensuing period, compared to similar regions that did not host rallies

*Academic goals:*
Learn more about how COVID data is collected, organized, and reported
Learn about the selection of controls in observational studies and the implications for drawing causal inferences
Learn about different strategies for visualizing time-series data and testing for discontinuities

*Implications:*
Increases in COVID-19 cases and deaths that are potentially attributable to political rallies would underscore the importance of social distance guidelines

*Optional features:*
Choice of control -- propensity score matching, synthetic controls

**Data?**

Our data will primarily be collected from these two sources:
 
COVID-19 data at county-level:  https://github.com/nytimes/covid-19-data
 
Exposure data (location and timing of rallies): https://en.wikipedia.org/wiki/List_of_post-election_Donald_Trump_rallies, verified against the campaign's posted schedule (https://www.donaldjtrump.com/events/) and composites of the President's official schedule (https://factba.se/topic/calendar)
 
 
**Design Overview**
* Difference-in-difference comparing counties before and after rallies to counties that didn’t host rallies in the same pre-post period
* Interrupted time series / testing for structural breaks in COVID case and deaths in counties with rallies
* Synthetic controls (optional, if we have time) - examining different control techniques for identifying counterfactual counties without rallies or composite counties to serve as synthetic controls
* Data visualization and visual analysis using heat maps, choropleths, line graphs
 
