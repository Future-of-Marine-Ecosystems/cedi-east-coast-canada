#-------------------------------------------------------------------------------
# Function to assemble the community matrices needed for the FE analysis
#-------------------------------------------------------------------------------

# Parameters:
# trait_list_for_all_times - a list object where each element is a data frame
# for each each time period with columns for species code (from fishbase), 
# species id (for this analysis), scientific name, fishbase name, and then the 
# traits used in the analysis (vertical position, diet, habitat zone, taxon, 
# body length, trophic level). There is also a column to uniquely identify each 
# species by combining the spid and taxon.
#
# trait_type - A p x 3 matrix, where p is the number of traits. The first column
# is the name of each trait. The second column is the type of each trait
# (N = nominal, O = ordinal). The third column is the weight of each trait,
# where a value of 1 represents equally weighted traits.
# 
# ordered_levels - a list object where each element is a vector of ordered 
# levels for each ordinal trait
#
# Return value:
# mFD_data - a two-element list where the first element is a 5 x n matrix with
# n is the total number of species found in at least one time period in a
# spatial location. Each row represents a time period, and each column a species
# Each element of the matrix is TRUE if that species is found in that site at 
# that time period, and otherwise FALSE. The second element of the list is an 
# n x 6 data frame where each column is a trait and each row is a species. The 
# elements of the matrix give the trait values of each species. Note that 
# these species are in the same order as in the first element of the list
#
# Create the community and trait matrices / data frames for conducting the 
# analysis
create_community_trait_matrices <- function(trait_list_for_all_times, trait_type, ordered_levels = NA) {
  
  # Names of community time period scenarios (CTPS - a single community at different points in time)
  community_time_point_names = names(trait_list_for_all_times)
  
  # A vector to hold all unique species across all community CTPS
  comm_spid_all = vector()
  
  # Loop through to get unique species IDs across all time points / scenarios
  for (ii in 1:length(trait_list_for_all_times))
  {
    eval(parse(text = paste0("comm_spid_all = unique(c(comm_spid_all, trait_list_for_all_times$", community_time_point_names[ii], "$spid_taxon))")))
  }
  
  # Set up a matrix with one row per community CTPS, and one colunn per unique species across all CTPS
  comm = matrix(nrow = length(trait_list_for_all_times), ncol = length(comm_spid_all), dimnames = list(CTPS = names(trait_list_for_all_times), unique_species_id = paste0(comm_spid_all)))
  colnames(comm) = paste0(comm_spid_all)
  rownames(comm) = names(trait_list_for_all_times)
  
  # Fill in the matrix so that if a species is present in a community CTPS then it is TRUE, otherwise FALSE
  for (kk in 1:dim(comm)[1])
  {
    eval(parse(text = paste0("comm[kk,] = colnames(comm) %in% trait_list_for_all_times$", community_time_point_names[kk], "$spid_taxon")))
  }
  
  # Now create a trait matrix to hold traits for all unique species across all CTPS
  traits = matrix(nrow = length(comm_spid_all), ncol = length(trait_type$trait_names), dimnames = list(unique_species_id = comm_spid_all, trait = trait_type$trait_names))
  
  # Being extra careful here - although the unique spid should be in the same place in both matrices.
  for (kk in 1:dim(traits)[1]) {
    
    # Find which CTPS a specific species is found in
    sp_of_interest = rownames(traits)[kk]
    CTPS = rownames(comm)[match(TRUE, comm[, which(sp_of_interest == colnames(comm))])]
    
    # Extract those traits for the traits lists
    traits_sp_of_interest = eval(parse(text = paste0("trait_list_for_all_times$", CTPS, "[which(trait_list_for_all_times$", CTPS, "$spid_taxon %in% sp_of_interest),]")))
    
    # Extract the traits to the traits matrix. Again being careful with trait names
    for (rr in 1:length(trait_type$trait_names))
    {
      traits[kk,trait_type$trait_names[rr]] = eval(parse(text = paste0("as.character(traits_sp_of_interest$", trait_type$trait_names[rr], ")")))
    }
  }
  
  # Turn into a data frame
  traits_df = as.data.frame(traits)
  
  # Convert nominal traits to factors ; ordinal traits to ordered factors ; quantitative traits to numeric
  for (ii in 1:length(trait_type$trait_names))
  {
    if (trait_type$trait_type[ii] =="N")
      eval(parse(text = paste0("traits_df$", trait_type$trait_names[which(trait_type$trait_type =="N")], "= factor(traits_df$", trait_type$trait_names[which(trait_type$trait_type =="N")], ")")))
    
    if (trait_type$trait_type[ii] =="O")
    {
      ordered_traits = which(trait_type$trait_type == "O")
      for (jj in 1:length(ordered_traits))
      {
        eval(parse(text = paste0("traits_df$", trait_type$trait_names[ordered_traits[jj]], "= ordered(traits_df$", trait_type$trait_names[ordered_traits[jj]], ",
                               levels = ordered_levels$", trait_type$trait_names[ordered_traits[jj]], ")")))
      }
      
    }
    if (trait_type$trait_type[ii] =="Q")
      eval(parse(text = paste0("traits_df$", trait_type$trait_names[which(trait_type$trait_type =="Q")], "= as.numeric(traits_df$", trait_type$trait_names[which(trait_type$trait_type =="Q")], ")")))
    
  }
  
  return(mFD_data = list(community_species = comm, traits = traits_df))
}
