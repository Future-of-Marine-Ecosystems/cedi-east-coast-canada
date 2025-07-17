# **Future climate-driven ecological disruption in a network of Marine Protected Areas on Canada’s east coast**

[Amy L. Irvine](https://oceansnorth.org/our-team/), [Gabriel Reygondeau](https://idsc.miami.edu/gabriel-reygondeau/), [Ryan R.E. Stanley](https://www.researchgate.net/profile/Ryan-Stanley), [Derek P. Tittensor](https://www.fomelab.org/our-people-1/derek-tittensor)

**Code and data citation**

Irvine, A.L., Reygondeau, G., Stanley, R.R.E., and D.P. Tittensor. Future climate-driven ecological disruption in a network of Marine Protected Areas on Canada’s east coast - code & data. [![DOI](https://zenodo.org/badge/920247465.svg)](https://doi.org/10.5281/zenodo.14767204)

Code and data to calculate the CEDI index from Irvine et al. Given a set of spatial areas and species/trait lists for different time periods / scenarios in each of these spatial areas, the code here calculates the CEDI index (and other FD metrics) for those areas. Here it is applied to Canada's east coast protected area network, with the time periods and scenarios being present day, mid-century under _RCP2.6_, mid-century under _RCP8.5_, end-century under _RCP2.6_, and end-century under _RCP8.5_. The species and traits lists are provided as R objects to demonstrate the form of the data, and are originally derived from species distribution models and [Fishbase](https://fishbase.se/)/[Sealifebase](https://www.sealifebase.se/) supplemented by literature review, as described in the publication.

**Abstract**

Climate-induced species range shifts alter ecological assemblages, yet little is known of the consequences for ecosystem functioning. We combined species distribution model (SDM) projections with species traits to develop a spatially-explicit risk index for assessing climate change impacts on ecosystem functioning. The ‘Climate Ecological Disruption Index’ (_CEDI_) is an easy-to-interpret metric that builds on existing approaches to quantifying functional diversity, providing a novel foundation for evaluating functional consequences of climate-induced species range shifts and identifying areas at risk. We applied CEDI to a marine protected area network on Canada’s east coast, where it indicated high potential for ecological disruption, with a maximum value of 0.35 (> 1/3 turnover in functional groups). Our approach is generalizable, aiding spatial conservation planning by translating projected species range shifts from SDMs into potential ecological disruption, thereby supporting the integration of climate resilience into management strategies and informing conservation planning efforts in a warming world.

![ ](/output/Figure1.pdf)

__Figure 1__ Conceptual approach for the Climate Ecological Disruption Index (CEDI), connecting projections of climate-driven species redistributions to ecosystem functioning consequences. The CEDI evaluates the gain and loss of novel ecological roles (represented by functional groups or FGs) through time. In this example, three species are emigrating and one immigrating, but only one FG is lost, and one gained. 

![](/output/Figure2.pdf)

__Figure 2__ The Fisheries and Oceans Canada Scotian Shelf-Bay of Fundy bioregion (“Maritimes Region”; black outline) with a superimposed map of Canada showing existing national conservation sites in blue. The Maritime Conservation Network within the Maritimes Region is composed of Marine Protected Areas (MPAs), Other Effective area-based Conservation Measures (OECMs), and proposed Areas of Interest (AOIs) that are likely to become MPAs (blue), and draft sites (orange). The Jordan Basin Marine Refuge (JBMR), Corsair and Georges Canyons Conservation Area (CGCCA), Eastern Canyons Marine Refuge (ECMR), and Cold Seeps (CS) sites are labelled.

![](/output/Figure3-detailed.png)

__Figure 3__ Mean relative projected ecological disruption (Climate Ecosystem Disruption Index or CEDI values) for the Maritime Conservation Network within the Fisheries and Oceans Canada Maritimes Region (black outline), from present-day to mid-century (left column) and end-century (right column) under SSP1-2.6 (top row) and SSP5-8.5 (bottom row). ¬The Jordan Basin Marine Refuge (JBMR) and Cold Seeps (CS) proposed site are highlighted for having high CEDI (potential for ecological disruption). The Corsair and Georges Canyons Conservation Area (CCGCCA) and Eastern Canyons Marine Refuge (ECMR) are highlighted for having low CEDI (potential for ecological disruption).
