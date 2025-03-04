#' Clean STI antibiotic drugs
#'
#' Clean the names of antibiotic treatment regimens in the chlamydia, gonorrhea, and syphilis data. Some prior data cleaning may be required, such as making sure multiple drugs in the same column are separated by commas, and commas are only used for this purpose. The function can be updated as more drugs are added.
#' @param data a dataframe
#' @param input the name of the input variable
#' @param output the name assigned to the cleaned, output variable
#' @export

clean_antibiotics <- function(data, input = "antibiotic_treatment", output = "antibiotic_treatment"){
  
  cleaned_data <- data |>
    dplyr::mutate(max_drugs = str_count(!!sym(input),",") + 1,
                  max_drugs = max(max_drugs, na.rm = TRUE)) |>
    #separate_wider_delim(!!sym(input), names = paste0("xab", 1:max(.$max_drugs)), delim = ",", too_few = "align_start", too_many = "error", cols_remove = FALSE) |>
    #First, remove dosage and other information from each drug in the field to leave only the base name. May need to sometimes update this with new conversions.
    separate_wider_delim(!!sym(input), names = c("xab1", "xab2", "xab3"), delim = ",", too_few = "align_start", too_many = "error", cols_remove = FALSE) |>
    dplyr::mutate(dplyr::across(tidyr::starts_with("xab"), stringr::str_trim),
                  dplyr::across(tidyr::starts_with("xab"), ~dplyr::case_when(
                    is.na(.) | . == "" ~ "Unknown",
                    stringr::str_length(.) == 1 ~ "Unknown",
                    !is.na(as.numeric(.)) ~ "Unknown",
                    stringr::str_detect(stringr::str_to_lower(.), "^azithromycin|^axithromycin") ~ "Azithromycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^doxycycline|^vibramycin") ~ "Doxycycline",
                    stringr::str_detect(stringr::str_to_lower(.), "^cefixime") ~ "Cefixime",
                    stringr::str_detect(stringr::str_to_lower(.), "^ceftriaxone|^rocephin") ~ "Ceftriaxone",
                    stringr::str_detect(stringr::str_to_lower(.), "^gentamicin|^gentamycin") ~ "Gentamicin",
                    stringr::str_detect(stringr::str_to_lower(.), "^cefadroxil") ~ "Cefadroxil",
                    stringr::str_detect(stringr::str_to_lower(.), "^clindamycin") ~ "Clindamycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^ciprofloxacin") ~ "Ciprofloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "^macrobid") ~ "Nitrofurantoin",
                    stringr::str_detect(stringr::str_to_lower(.), "^tenofovir|^emtricitabine|^bictegravir") ~ "Unknown", #these are antivirals, remove
                    stringr::str_detect(stringr::str_to_lower(.), "^cefoxitin|^mefoxin") ~ "Other",
                    stringr::str_detect(stringr::str_to_lower(.), "^vibramycin") ~ "Vibramycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^amoxicillin") ~ "Amoxicillin",
                    stringr::str_detect(stringr::str_to_lower(.), "^levofloxacin|^levaquin") ~ "Levofloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "^metronidazole|^flagyl") ~ "Metronidazole",
                    stringr::str_detect(stringr::str_to_lower(.), "^erythromycin") ~ "Erythromycin",
                    stringr::str_detect(stringr::str_to_lower(.), "^benzathine penicillin g") & stringr::str_detect(., "2\\.4|2 4") ~ "Benzathine penicillin G 2.4",
                    stringr::str_detect(stringr::str_to_lower(.), "^benzathine penicillin g") ~ "Benzathine penicillin G",
                    stringr::str_detect(stringr::str_to_lower(.), "^penicillin g benzathine") ~ "Benzathine penicillin G",
                    stringr::str_detect(stringr::str_to_lower(.), "^aqueous crystalline penicillin") ~ "Aqueous crystalline penicillin G",
                    stringr::str_detect(stringr::str_to_lower(.), "^tetracycline") ~ "Tetracycline",
                    stringr::str_detect(stringr::str_to_lower(.), "^probenecid") ~ "Probenecid",
                    stringr::str_detect(stringr::str_to_lower(.), "^penicillin g procaine") ~ "Penicillin G Procaine",
                    stringr::str_detect(stringr::str_to_lower(.), "^gemifloxacin") ~ "Gemifloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "^keflex") ~ "Ciprofloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "^cefdinir") ~ "Cefdinir",
                    stringr::str_detect(stringr::str_to_lower(.), "^ondansetron|^zofran") ~ "Unknown", #not AB
                    stringr::str_detect(stringr::str_to_lower(.), "^diflucan") ~ "Fluconazole",
                    stringr::str_detect(stringr::str_to_lower(.), "^bactrim ds") ~ "Trimethoprim/Sulfamethoxazole",
                    stringr::str_detect(stringr::str_to_lower(.), "^cefpodoxime") ~ "Cefpodoxime",
                    
                    
                    stringr::str_detect(stringr::str_to_lower(.), "azithromycin|axithromycin") ~ "Azithromycin",
                    stringr::str_detect(stringr::str_to_lower(.), "levaquin") ~ "Levaquin",
                    stringr::str_detect(stringr::str_to_lower(.), "ceftriaxone|rocephin") ~ "Ceftriaxone",
                    stringr::str_detect(stringr::str_to_lower(.), "doxycycline|vibramycin") ~ "Doxycycline",
                    stringr::str_detect(stringr::str_to_lower(.), "cefixime") ~ "Cefixime",
                    stringr::str_detect(stringr::str_to_lower(.), "ceftriaxone|rocephin") ~ "Ceftriaxone",
                    stringr::str_detect(stringr::str_to_lower(.), "gentamicin|gentamycin") ~ "Gentamicin",
                    stringr::str_detect(stringr::str_to_lower(.), "cefadroxil") ~ "Cefadroxil",
                    stringr::str_detect(stringr::str_to_lower(.), "clindamycin") ~ "Clindamycin",
                    stringr::str_detect(stringr::str_to_lower(.), "ciprofloxacin") ~ "Ciprofloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "macrobid") ~ "Nitrofurantoin",
                    stringr::str_detect(stringr::str_to_lower(.), "cefoxitin|mefoxin") ~ "Other",
                    stringr::str_detect(stringr::str_to_lower(.), "vibramycin") ~ "Vibramycin",
                    stringr::str_detect(stringr::str_to_lower(.), "amoxicillin") ~ "Amoxicillin",
                    stringr::str_detect(stringr::str_to_lower(.), "levofloxacin|levaquin") ~ "Levofloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "metronidazole|flagyl") ~ "Metronidazole",
                    stringr::str_detect(stringr::str_to_lower(.), "erythromycin") ~ "Erythromycin",
                    stringr::str_detect(stringr::str_to_lower(.), "benzathine penicillin g") & stringr::str_detect(., "2\\.4|2 4") ~ "Benzathine penicillin G 2.4",
                    stringr::str_detect(stringr::str_to_lower(.), "benzathine penicillin g") ~ "Benzathine penicillin G",
                    stringr::str_detect(stringr::str_to_lower(.), "penicillin g benzathine") ~ "Benzathine penicillin G",
                    stringr::str_detect(stringr::str_to_lower(.), "aqueous crystalline penicillin") ~ "Aqueous crystalline penicillin G",
                    stringr::str_detect(stringr::str_to_lower(.), "tetracycline") ~ "Tetracycline",
                    stringr::str_detect(stringr::str_to_lower(.), "probenecid") ~ "Probenecid",
                    stringr::str_detect(stringr::str_to_lower(.), "penicillin g procaine") ~ "Penicillin G Procaine",
                    stringr::str_detect(stringr::str_to_lower(.), "gemifloxacin") ~ "Gemifloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "keflex") ~ "Ciprofloxacin",
                    stringr::str_detect(stringr::str_to_lower(.), "cefdinir") ~ "Cefdinir",
                    stringr::str_detect(stringr::str_to_lower(.), "tenofovir|emtricitabine|bictegravir") ~ "Unknown", #these are antivirals, remove
                    stringr::str_detect(stringr::str_to_lower(.), "ondansetron|zofran") ~ "Unknown", #not AB
                    stringr::str_detect(stringr::str_to_lower(.), "diflucan") ~ "Fluconazole",
                    stringr::str_detect(stringr::str_to_lower(.), "bactrim ds") ~ "Trimethoprim/Sulfamethoxazole",
                    stringr::str_detect(stringr::str_to_lower(.), "cefpodoxime") ~ "Cefpodoxime",
                    .default = .)),
                  
                  xab2 = dplyr::if_else(xab1 == xab2, "Unknown", xab2),
                  xab3 = dplyr::if_else(xab1 == xab3 | xab2 == xab3, "Unknown", xab3),
                  interm = dplyr::case_when(
                    xab2 == "Unknown" & xab3 == "Unknown" ~ xab1,
                    xab3 == "Unknown" ~ paste(xab1, xab2, sep = ","),
                    xab2 == "Unknown" ~ paste(xab1, xab3, sep = ","),
                    .default = paste(xab1, xab2, xab3, sep = ","))) |>
    #Second, arrange the drug names in alphetical order so there aren't dupes of the same drugs in a different order
    tidyr::separate_wider_delim(interm, names = c("xab12", "xab22", "xab32"), delim = ",", too_few = "align_start", too_many = "error") |>
    dplyr::mutate(
      !!sym(output) := dplyr::case_when(
        is.na(xab22) & is.na(xab32) ~ xab12,
        is.na(xab32) ~ purrr::pmap_chr(list(xab12, xab22), ~paste(sort(c(...)), collapse = ",")),
        is.na(xab22) ~ purrr::pmap_chr(list(xab12, xab32), ~paste(sort(c(...)), collapse = ",")),
        .default = purrr::pmap_chr(list(xab12, xab22, xab32), ~paste(sort(c(...)), collapse = ","))))
  
  #Currently this function is only set up to handle a max of 3 drugs in one column. Could expand if necessary.
  #In this case, combine_regimens would also need to be updated
  if (max(cleaned_data$max_drugs) > 3) {
    stop("More than three drugs supplied in one column. Edit function or underlying data to proceed.")
  }
  
  return(cleaned_data |> select(-starts_with("xab")))
  
}
