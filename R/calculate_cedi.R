#-------------------------------------------------------------------------------
# Function to calculate the climate ecological disruption index (CEDI). Also
# returns other FD metrics.
#-------------------------------------------------------------------------------

# Parameters:
# mFD_data - a two-element list where the first element is a 5 x n matrix with
# n is the total number of species found in at least one time period in a
# spatial location. Each row represents a time period, and each column a species
# Each element of the matrix is TRUE if that species is found in that site at 
# that time period, and otherwise FALSE. The second element of the list is an 
# n x 6 data frame where each column is a trait and each row is a species. The 
# elements of the matrix give the trait values of each species. Note that 
# these species are in the same order as in the first element of the list
#
# spid_taxon_all_times - A list of length 5, where each element of the list
# is a data frame containing the species codes, name, and traits of all species
# in the spatial location in a particular time period. 
#
# trait_type - A p x 3 matrix, where p is the number of traits. The first column
# is the name of each trait. The second column is the type of each trait
# (N = nominal, O = ordinal). The third column is the weight of each trait,
# where a value of 1 represents equally weighted traits.
#
# return_details_for_individual_FES - if TRUE returns details for individual
# functional entities; if FALSE, just returns community level metrics.
#
# Return value: 
# A list with the following elements
# CEDI = The climate ecological disruption index for each combination of time
# periods
# FE_number = number of FEs for each time period
# FRed = functional redundancy for each time period
# FOrd = functional over-redundancy for each time period
# FVul = functional vulnerability for each time period
# FE_immigrating = number of FEs immigrating for each combination of time
#   periods
# FE_emigrating = number of FES emigrating for each combination of time periods
# FE_same = number of FEs remaining the same for each combination of time 
#   periods
# FE_turnover = FE turnover
# FE_nestedness = FE nestedness
# FE_details = traits for each FE and how they change over time in terms of the 
# number of species
# total_FE_space = total number of possible FEs that could exist based on trait 
# combinations passed in
calculate_CEDI = function(mFD_data, spid_taxon_all_times, trait_type, return_details_for_individual_FES = FALSE)
{

  # Convert species to functional entities
  fe_sp <- sp.to.fe(sp_tr = mFD_data$traits,
                    tr_cat = trait_type)
  
  # Calculate functional redundancy metrics
  func_redundancy <- alpha.fd.fe(mFD_data$comm, fe_sp)
  
  # Extract the number of FEs for each community time period scenario (CTPS)
  # (strictly speaking, the number with one or more species within)
  FE_number = matrix(t(func_redundancy$"asb_fdfe"[, "nb_fe"]), nrow = 1, ncol = length(names(spid_taxon_all_times)), dimnames = list(NULL, paste0(names(spid_taxon_all_times), "_nmbr_FEs")))
  
  # Extract the functional redundancy for each community time period scenario (CTPS)
  # (strictly speaking, the number with one or more species within)
  FRed = matrix(t(func_redundancy$"asb_fdfe"[, "fred"]), nrow = 1, ncol = length(names(spid_taxon_all_times)), dimnames = list(NULL, paste0(names(spid_taxon_all_times), "_FRed")))
  
  # Extract the functional over-redundancy for each community time period scenario (CTPS)
  # (strictly speaking, the number with one or more species within)
  FOrd = matrix(t(func_redundancy$"asb_fdfe"[, "fored"]), nrow = 1, ncol = length(names(spid_taxon_all_times)), dimnames = list(NULL, paste0(names(spid_taxon_all_times), "_FOrd")))
  
  # Extract the functional vulnerability for each community time period scenario (CTPS)
  # (strictly speaking, the number with one or more species within)
  FVul = matrix(t(func_redundancy$"asb_fdfe"[, "fvuln"]), nrow = 1, ncol = length(names(spid_taxon_all_times)), dimnames = list(NULL, paste0(names(spid_taxon_all_times), "_FVul")))
  
  # Create matrices to hold results
  pairwise_CTPS_combinations = combn(names(spid_taxon_all_times), 2)
  combination_names = apply(pairwise_CTPS_combinations, 2, function(x) paste(x, collapse = "_"))
  FE_immigrating = matrix(data = NA, nrow = 1, ncol = length(pairwise_CTPS_combinations[1,]), dimnames = list(NULL, paste0(combination_names, "_imm")))
  FE_emigrating = matrix(data = NA, nrow = 1, ncol = length(pairwise_CTPS_combinations[1,]), dimnames = list(NULL, paste0(combination_names, "_em")))
  FE_same = matrix(data = NA, nrow = 1, ncol = length(pairwise_CTPS_combinations[1,]), dimnames = list(NULL, paste0(combination_names, "_same")))
  CEDI = matrix(data = NA, nrow = 1, ncol = length(pairwise_CTPS_combinations[1,]), dimnames = list(NULL, paste0(combination_names, "_CEDI")))
  FE_turnover = matrix(data = NA, nrow = 1, ncol = length(pairwise_CTPS_combinations[1,]), dimnames = list(NULL, paste0(combination_names, "_turn")))
  FE_nestedness = matrix(data = NA, nrow = 1, ncol = length(pairwise_CTPS_combinations[1,]), dimnames = list(NULL, paste0(combination_names, "_nest")))
  
  # Extract number of FEs in each CTPS
  nmbr_FEs_per_time = as.data.frame(func_redundancy[["details_fdfe"]][["asb_fe_nbsp"]])
  for (ii in 1:length(spid_taxon_all_times)) #for each time period
  {
    eval(parse(text = paste0(names(spid_taxon_all_times)[ii], "_FEs = nmbr_FEs_per_time[\"", names(spid_taxon_all_times)[ii], "\",]")))
  }
  
  # Functional traits for each FE
  fe_tr <- fe_sp$"fe_tr"
  
  # Combine the above into a single data frame
  fe_tr = as.data.frame(fe_tr)
  fe_tr = merge(fe_tr, t(nmbr_FEs_per_time), by = 0)
  
  # Number of species in each FE summed across time periods. Commented out as
  # not currently used; retained just for illustrative purposes
  #fe_nb_sp <- fe_sp$"fe_nb_sp"
  
  # Add columns for number of FEs immigrating, emigrating, and staying
  for (ii in 1:length(combination_names))
  {
    eval(parse(text = paste0("fe_tr$", combination_names[ii], "_imm = NA")))
    eval(parse(text = paste0("fe_tr$", combination_names[ii], "_em = NA")))
    eval(parse(text = paste0("fe_tr$", combination_names[ii], "_change_in_same = NA")))
  }
  
  # Rename rows for the below code to work
  rownames(fe_tr) = fe_tr$Row.names
  fe_tr = fe_tr[,-1]
  
  # Calculate metrics of change
  # Loop through each combination of comparisons. Compare the categories to all 
  # others except same (hence -1)
  for (kk in 1:(length(names(spid_taxon_all_times)) - 1)) 
  {
    # Compare category kk to all other categories in increasing order - 
    # do not need to compare to previous categories
    for (jj in (kk + 1):length(spid_taxon_all_times))    
    {
      time1 <- names(spid_taxon_all_times)[kk]
      time2 <- names(spid_taxon_all_times)[jj]
      
      # Immigrating = FEs have 0 species in time 1 and >0 species in time 2
      eval(parse(text = paste0("FEs_imm <- colnames(", time1, "_FEs)[which((", time1, "_FEs==0) & !(", time2, "_FEs ==0))]"))) #these files have a number of species in each FE
      FE_immigrating[1,paste0(time1, "_", time2, "_imm")] = length(FEs_imm)
      
      # Emigrating / extirpation = FEs have 0 species in time 2 and >0 species 
      # in time 1
      eval(parse(text = paste0("FEs_em <-  colnames(", time2, "_FEs)[which((", time2, "_FEs==0) & !(", time1, "_FEs ==0))]")))
      FE_emigrating[1,paste0(time1, "_", time2, "_em")] = length(FEs_em)
      
      # Same = FEs with >0 species in both time 1 and time 2
      eval(parse(text = paste0("FEs_same <-  colnames(", time1, "_FEs)[which(!(", time1, "_FEs==0) & !(", time2, "_FEs ==0) &
                             colnames(", time1, "_FEs) %in% colnames(", time2, "_FEs))]")))
      FE_same[1,paste0(time1, "_", time2, "_same")] = length(FEs_same)
      
      # CEDI for FEs = (number immigrating + number emigrating) / (2 * number same + number immigrating + number emigrating)
      CEDI[1,paste0(time1, "_", time2, "_CEDI")] = (length(FEs_imm) + length(FEs_em))/(2*(length(FEs_same)) + length(FEs_imm) + length(FEs_em))
      
      # Turnover = min(immigrating, emigrating) / same + min(immigrating, emigrating)
      FE_turnover[1,paste0(time1, "_", time2, "_turn")] = (min(length(FEs_imm), length(FEs_em))) / (length(FEs_same) + min(length(FEs_imm), length(FEs_em)))
      
      # Nestedness = (max(imm, em) - min(imm, em) / 2* number same + number im + number em) * (number same / number same + min(imm, em))
      FE_nestedness[1,paste0(time1, "_", time2, "_nest")] = ((max(length(FEs_imm), length(FEs_em)) - min(length(FEs_imm), length(FEs_em)))/
                                                               (2*(length(FEs_same)) + length(FEs_imm) + length(FEs_em)))*
        (length(FEs_same)/  (length(FEs_same) + min(length(FEs_imm), length(FEs_em))))
      
      
      # Extract details for specific functional entities in terms of changes in their composition
      for (ii in 1:length(FEs_imm))
      {
        eval(parse(text = paste0("col_with_imm_fe = which(colnames(", time2, "_FEs) %in% FEs_imm[ii])")))
        eval(parse(text = paste0("number_of_species_immigrating = ", time2, "_FEs[,col_with_imm_fe]")))
        row_imm_FE_traits = which(rownames(fe_tr) %in% FEs_imm[ii])
        fe_tr[row_imm_FE_traits,paste0(time1, "_", time2, "_imm")] = number_of_species_immigrating
      }
      for (ii in 1:length(FEs_em))
      {
        eval(parse(text = paste0("col_with_em_fe = which(colnames(", time1, "_FEs) %in% FEs_em[ii])")))
        eval(parse(text = paste0("number_of_species_emigrating = ", time1, "_FEs[,col_with_em_fe]")))
        row_em_FE_traits = which(rownames(fe_tr) %in% FEs_em[ii])
        fe_tr[row_em_FE_traits,paste0(time1, "_", time2, "_em")] = number_of_species_emigrating
      }
      for (ii in 1:length(FEs_same))
      {
        eval(parse(text = paste0("col_with_same_fe = which(colnames(", time2, "_FEs) %in% FEs_same[ii])")))
        eval(parse(text = paste0("number_of_spp_changed_in_same_fe = (", time2, "_FEs[,col_with_same_fe]) - (", time1, "_FEs[,col_with_same_fe])")))
        row_same_FE_traits = which(rownames(fe_tr) %in% FEs_same[ii])
        fe_tr[row_same_FE_traits,paste0(time1, "_", time2, "_change_in_same")] = number_of_spp_changed_in_same_fe
      }
    }
  }
  
  # Return results
  if (return_details_for_individual_FES == TRUE) {
    dissimilarity_results = list(CEDI = CEDI,
                                 FE_number = FE_number,
                                 FRed = FRed,
                                 FOrd = FOrd,
                                 FVul = FVul,
                                 FE_immigrating = FE_immigrating,
                                 FE_emigrating = FE_emigrating,
                                 FE_same = FE_same,
                                 FE_turnover = FE_turnover,
                                 FE_nestedness = FE_nestedness,
                                 FE_details = fe_tr,
                                 total_FE_space = prod(apply(mFD_data$traits,2, tempfun <- function(x) {length(unique(x))})))
  } else
  {
    dissimilarity_results = list(CEDI = CEDI,
                                 FE_number = FE_number,
                                 FRed = FRed,
                                 FOrd = FOrd,
                                 FVul = FVul,
                                 FE_immigrating = FE_immigrating,
                                 FE_emigrating = FE_emigrating,
                                 FE_same = FE_same,
                                 FE_turnover = FE_turnover,
                                 FE_nestedness = FE_nestedness,
                                 total_FE_space = prod(apply(mFD_data$traits,2, tempfun <- function(x) {length(unique(x))})))
  }
  
  return(dissimilarity_results)
}