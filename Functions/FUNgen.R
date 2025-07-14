calculate_kinship <- function(pop_df, pars) {
  
  require(kinship2)
  
  # Specify required parameters
  needed_pars <- c("founder_ids", "founder_kinship")
  
  # Prepare pedigree dataframe by selecting relevant columns and removing duplicates
  ped_df <- pop_df %>%
    dplyr::filter(!duplicated(id)) %>%
    dplyr::select(id, mother_id, father_id, sex)
  
  # Create pedigree object using the 'kinship2' package's pedigree function
  ped <- with(ped_df, pedigree(id = id, momid = mother_id, dadid = father_id, sex = ifelse(sex == "M", 1, 2)))
  
  # Calculate kinship matrix from the pedigree
  kin <- kinship(ped)
  
  # Override kinship values for founders with user-provided values
  kin[pars$founder_ids, pars$founder_ids] <- pars$founder_kinship
  
  # The following commented section creates all possible dam × sire combinations
  # and calculates their kinship, accounting for founder kinship.
  # It has been commented out but may be useful for specific mating evaluations.
  
  return(kin)
}

calculate_kinship_genlib <- function(pop_df, pars) {
  # Prepare pedigree dataframe in GENLIB format
  ped_df <- pop_df %>%
    dplyr::filter(!duplicated(id)) %>%
    dplyr::select(ind = id, father = father_id, mother = mother_id,sex)
  
  id_levels<-c(ped_df$ind,ped_df$father,ped_df$mother)%>%unique
  
  
  # Save the original ID mapping for later
  id_map <- data.frame(
    id_chr = id_levels,
    id_num = as.numeric(factor(id_levels, levels = id_levels)),
    stringsAsFactors = FALSE
  )
  
  ped_df<-ped_df%>%
    dplyr::mutate(ind=as.numeric(factor(ind,levels=id_levels)),
                  father=as.numeric(factor(father,levels=id_levels)),
                  mother=as.numeric(factor(mother,levels=id_levels)))%>%
    dplyr::mutate(father=case_when(
      is.na(father)~0,
      TRUE~father
    ),mother=case_when(
      is.na(mother)~0,
      TRUE~mother
    ),
    sex=case_when(
      sex=="M"~1,
      sex=="F"~2
    ))
  
  # Create a GENLIB genealogy object
  gen <- GENLIB::gen.genealogy(
    ped = ped_df)
  
  k<-gen.phi(gen = gen)
  
  
  # Ensure founder kinship override
  kin[pars$founder_ids, pars$founder_ids] <- pars$founder_kinship
  
  return(as.matrix(kin))  # Return as a base R matrix
}


calculate_inbreeding <- function(pop_df, pars) {
  
  # Specify required parameters
  needed_pars <- c("founder_ids", "Fp")
  
  # Prepare the main population dataframe
  temp <- pop_df %>%
    dplyr::arrange(is.na(Fi))%>%
    dplyr::filter(!duplicated(id)) %>%
    dplyr::mutate(
      # Assign founder inbreeding if missing and listed as a founder
      Fi = case_when(
        is.na(Fi) & id %in% pars$founder_ids ~ pars$Fp,
        # Assign Fi = 0 for individuals with no known parents and not in founder_ids
        is.na(Fi) & is.na(mother_id) & is.na(father_id) ~ 0,
        # Otherwise keep existing Fi
        TRUE ~ Fi
      ),
      priority = 1,  # Used later to track original vs. imputed entries
      t = NULL       # Remove any existing 't' variable
    )
  
  # Extract individuals still missing Fi
  missing_Fi <- temp %>%
    dplyr::filter(is.na(Fi)) %>%
    dplyr::mutate(priority = 2)  # Mark as needing estimation
  
  
  id_filter<-missing_Fi%>%
    dplyr::filter(!duplicated(data.frame(mother_id,father_id)))%>%
    dplyr::select(mother_id,father_id)
  
  # Get kinship matrix using the earlier function
  kin <- calculate_kinship(pop_df = pop_df, pars = pars,id_filter=id_filter)
  
  # Lookup kinship between each individual's parents
  parent_kin <- kin[missing_Fi$mother_id, missing_Fi$father_id] %>% diag()
  
  # Extract Fi for fathers of missing individuals
  Fdad <- temp %>%
    dplyr::filter(!is.na(Fi), sex == "M") %>%
    dplyr::pull(Fi)
  names(Fdad) <- temp %>%
    dplyr::filter(!is.na(Fi), sex == "M") %>%
    dplyr::pull(id)
  Fdad <- Fdad[missing_Fi$father_id]
  
  # Extract Fi for mothers of missing individuals
  Fmom <- temp %>%
    dplyr::filter(!is.na(Fi), sex == "F") %>%
    dplyr::pull(Fi)
  names(Fmom) <- temp %>%
    dplyr::filter(!is.na(Fi), sex == "F") %>%
    dplyr::pull(id)
  Fmom <- Fmom[missing_Fi$mother_id]
  
  # Estimate inbreeding coefficients using standard formula:
  # Fi = (Fi_mother + Fi_father)/2 + kin(mother, father) * (1 - (Fi_mother + Fi_father)/2)
  Fi <- ((Fdad + Fmom) / 2) + (parent_kin * (1 - ((Fdad + Fmom) / 2)))
  
  # Assign calculated Fi back to missing individuals
  missing_Fi$Fi <- Fi
  
  # Combine estimated and original individuals; mark all with t = -1
  new <- plyr::rbind.fill(temp, missing_Fi) %>%
    mutate(t = -1)
  
  # Clean and return output using tidy_pop_df
  resu <- tidy_pop_df(new) %>%
    dplyr::select(id,Fi)
  
  if(any(is.na(resu$Fi))){stop("Some Fi's not calculated")}
  
  return(resu)
}


