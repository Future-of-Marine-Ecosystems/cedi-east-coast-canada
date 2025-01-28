# Calculate the CEDI index (and other functional metrics) for Canada's Maritimes
# conservaiton network.
#
# From Irvine et al. (2025)

# Load relevant packages
library(mFD)


# Load functions to calculate the CEDI and create community trait matrices 
source("R/calculate_cedi.R")
source("R/create_community_matrices.R")

# Load the trait data. This is a list where each element is an individual 
# site in the conservation network. Each site is a list where each element is a 
# time period and scenario (current, mid and end century for 
# SSP1-2.5 and SSP5-8.5. Finally, each time period and scenario is a data frame
# with  species code (from fishbase), species id (for this analysis), 
# scientific name, fishbase name, and  the traits used in the analysis (vertical 
# position, diet, habitat zone, taxon, body length, trophic level). There is 
# also a column to uniquely identify each species by combining the spid and 
# taxon.
load('data/trait data/traits_per_mpa_all_times.Rdata')

# Categorical bins for body size and trophic level
ordered_levels = list(BodyLength_bin = c(0.05, 0.5, 1, 5, 10, 30, 50, 100, 300, 600, 900, 1200, ">1200"),
                      TrophicLevel_bin = c(1, 2, 3, 4))

# Indicate whether traits are nominal or ordinal and weight traits equally
trait_type <- data.frame(trait_names = c("VertPos", "Diet", "HabitatZone", "Taxon", "BodyLength_bin", "TrophicLevel_bin"),
                         trait_type = c("N", "N", "N", "N", "O", "O"),
                         trait_weight = 1)

# Extract MPA (actually MPAs + OECMs, existing and proposed) names
mpa_names = names(traits_per_mpa_all_times)

# Create a list object to hold the community trait matrices for each MPA
maritimes_traits_per_mpa = list()

# Create the community trait matrices for each MPA. The community trait matrices
# for each MPA are a two-element list where the first element is a 5 x n matrix 
# wheren is the total number of species found in at least one time period in a
# spatial location. Each row represents a time period, and each column a species
# Each element of the matrix is TRUE if that species is found in that site at 
# that time period, and otherwise FALSE. The second element of the list is an 
# n x 6 data frame where each column is a trait and each row is a species. The 
# elements of the matrix give the trait values of each species. Note that 
# these species are in the same order as in the first element of the list
for (ii in 1:length(mpa_names))
{
  traits_for_mpa = traits_per_mpa_all_times[[ii]]
  maritimes_traits_per_mpa[[ii]] = create_community_trait_matrices(traits_for_mpa, trait_type, ordered_levels)
}

# OK, the data are now in a format appropriate for calculating the CEDI (and 
# other functional metrics)
dissimilarity_by_mpa = list()

# Calculate the CEDI for each site and time period
for (ii in 1:length(mpa_names))
{
  dissimilarity_by_mpa[[ii]] = calculate_CEDI(maritimes_traits_per_mpa[[ii]], traits_per_mpa_all_times[[ii]], trait_type)
}

# Add MPA names to the outputs
names(dissimilarity_by_mpa) = mpa_names

# Save the results
save(dissimilarity_by_mpa, file = "output/dissimilarity_by_mpa.Rdata")

# Note that there are many FE outputs as well as the CEDI. Here we 
# concentrate on just the CEDI values. Let's just print the CEDI values for
# RCP-SSP5-8.5 end century
print("Climate ecological disruption index (CEDI) values for each conservation
      site under RCP-SSP5-8.5 at end century: ")
for (ii in 1:length(dissimilarity_by_mpa))
{
  print(paste0(mpa_names[ii], ": ", round(dissimilarity_by_mpa[[ii]]$CEDI[10], 2)))
}

