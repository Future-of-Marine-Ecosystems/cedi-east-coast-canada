# **Future climate-driven ecological disruption in a network of Marine Protected Areas on Canada’s east coast**

[Amy L. Irvine](https://oceansnorth.org/our-team/), [Gabriel Reygondeau](https://idsc.miami.edu/gabriel-reygondeau/), [Ryan R.E. Stanley](https://www.researchgate.net/profile/Ryan-Stanley), [Derek P. Tittensor](https://www.fomelab.org/our-people-1/derek-tittensor)

**Code and data citation**

Irvine, A.L., Reygondeau, G., Stanley, R.R.E., and D.P. Tittensor. Future climate-driven ecological disruption in a network of Marine Protected Areas on Canada’s east coast - code & data. [![DOI](https://zenodo.org/badge/920247465.svg)](https://doi.org/10.5281/zenodo.14767204)

Code and data to calculate the CEDI index from Irvine et al. Given a set of spatial areas and species/trait lists for different time periods / scenarios in each of these spatial areas, the code here calculates the CEDI index (and other FD metrics) for those areas. Here it is applied to Canada's east coast protected area network, with the time periods and scenarios being present day, mid-century under _RCP2.6_, mid-century under _RCP8.5_, end-century under _RCP2.6_, and end-century under _RCP8.5_. The species and traits lists are provided as R objects to demonstrate the form of the data, and are originally derived from species distribution models and [Fishbase](https://fishbase.se/)/[Sealifebase](https://www.sealifebase.se/) supplemented by literature review, as described in the publication.

**Abstract**

Climate-induced species range shifts alter ecological assemblages, yet little is known of the consequences for ecosystem functioning. We combined species distribution model (SDM) projections with species traits to develop an index for assessing the risks of climate change for ecosystem functioning, then applied it to a marine protected area network on Canada’s east coast. The ‘Climate Ecosystem Disruption Index’ (_CEDI_) is easy-to-interpret, builds on existing approaches to quantifying functional diversity, and provides a novel foundation for evaluating the functional consequences of climate-induced species range shifts to identify areas at risk. The CEDI indicated high potential for ecological disruption in the conservation network, with a maximum value of 0.35 (> 1/3 turnover in functional groups) in Jordan Basin Marine Refuge. Our approach aids spatial conservation planning by translating projected shifts from SDMs into potential ecological disruption, to help build climate-resilience into management and inform conservation planning efforts in a warming world.

![ ](/output/Figure1.jpg)

__Figure 1__ Fisheries and Oceans Canada Maritimes Region imposed on a map of Canada (a), which contains the Maritime Conservation Network composed of Marine Protected Areas (MPAs), Other Effective area-based Conservation Measures (OECMs), and proposed Areas of Interest (AOIs). Labeled sites are used as example locations for enhanced CEDI analysis. 

![](/output/Fig2.png)

__Figure 2__ Conceptual approach for the Climate Ecological Disruption Index (CEDI), connecting projections of climate-driven species redistributions to ecosystem functioning consequences. The TEDI evaluates the gain and loss of novel ecological roles (represented by functional entities, FE; Mouillot et al., 2014) through time. In this example, three species are emigrating and one immigrating, but only one FE is lost and one gained.

![](/output/Fig3.png)

__Figure 3__ Relative projected ecological disruption (Climate Ecosystem Disruption Index values) for the Maritime Conservation Network within the Fisheries and Oceans Canada Maritimes Region (black outline), from present-day to mid-century (left column) and end-century (right column) under SSP1-2.6 (top row) and SSP5-8.5 (bottom row). ¬The Jordan Basin Marine Refuge (left red box) and Cold Seeps proposed site (right red box) are highlighted for generally having the highest CEDI - the highest potential for ecological disruption. The Corsair and Georges Canyons Marine Refuge (left green box) and Eastern Canyons Marine Refuge (right green box) is highlighted for the lowest CEDI and potential for ecological disruption.