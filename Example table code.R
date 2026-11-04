

# 1. Prepare data and transform ga_strata into a descriptive factor
analysis_data <- data_off %>%
  filter(rand_sib == 1) %>% 
  dplyr::select(
    "sex.factor", "multi.factor", "ga_calc", "PNA_rand", "bw", "Birth_weight_Z", "b_hc", 
    "Birth_HC_Z", "as1.new", "as5.new", "cord_ph", "rand_gp", "rand_sib", "ga_strata", "IVH_prerand.f"
  ) %>%
  mutate(ga_strata = factor(ga_strata, 
                            levels = c(1, 2),
                            labels = c("<28 Weeks PMA", "28-32 Weeks PMA"))) 

# 2. Reusable baseline table layout function
generate_base_table <- function(data_input, by_var) {
  data_input %>%
    tbl_summary(
      by = !!sym(by_var),
      percent = "column",
      missing = "ifany", 
      missing_text = "(Missing)",
      digits = all_categorical() ~ c(0, 0),
      type = list(
        sex.factor ~ "categorical",  
        multi.factor ~ "categorical",
        ga_calc ~ "continuous2", 
        PNA_rand ~ "continuous2",
        bw ~ "continuous2",
        Birth_weight_Z ~ "continuous2",
        b_hc ~ "continuous2",
        Birth_HC_Z ~ "continuous2",
        as1.new ~ "continuous2",
        as5.new ~ "continuous2",
        cord_ph ~ "continuous2",
        IVH_prerand.f ~ "categorical"
      ),
      statistic = list(
        all_categorical() ~ "{n} / {N} ({p}%)",
        all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"), 
        c(as1.new, as5.new) ~ c("{median} ({p25}, {p75})")
      ),
      label = list(
        sex.factor ~ 'Sex', multi.factor ~ "Multiple birth", ga_calc ~ "Gestational age at birth (weeks PMA)",
        PNA_rand ~ 'PNA at randomisation (hours)',
        bw ~ 'Birth weight (g)', Birth_weight_Z ~ 'Birth weight Z-score',
        b_hc ~ 'Birth head circumference (cm)',         
        Birth_HC_Z ~ 'Birth head circumference Z-score', 
        as1.new ~ 'APGAR score at 1 minute', 
        as5.new ~ 'APGAR score at 5 minutes', 
        cord_ph ~ 'Cord pH',
        IVH_prerand.f ~ 'IVH Grade prior to enrolment'
      )
    )
}

# 3. Stratify the table and inject dynamic sample size {n} into headers
table_stratified <- analysis_data %>%
  tbl_strata(
    strata = ga_strata, 
    .tbl_fun = ~ generate_base_table(.x, by_var = "rand_gp"),
    .header = "**{strata} (N={n})**" 
  )

# 4. Create the final total columns with a matching N header
total_n <- nrow(analysis_data) 

table_total <- generate_base_table(analysis_data, by_var = "rand_gp") %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_spanning_header(all_stat_cols() ~ paste0("**All Strata Combined (N=", total_n, ")**")) 

# 5. Merge everything side-by-side
final_table <- tbl_merge(
  tbls = list(table_stratified, table_total),
  tab_spanner = FALSE 
)

# Output final table
final_table

###################Maternal factors

# 1. Prepare data and transform ga_strata into a descriptive factor
analysis_data_mat <- data %>%
  filter(rand_sib == 1) %>% 
  dplyr::select("m_ethnic", "type1.factor", "type2.factor", "type3.factor", "type4.factor", "type5.factor","type6.factor", 
                "type7.factor", "chorio.f.new", "ans.f.new", "rand_gp", "romdurn_hr_new", "ga_strata", "prom.factor", "delmode") %>%
  mutate(ga_strata = factor(ga_strata, 
                            levels = c(1, 2),
                            labels = c("<28 Weeks PMA", "28-32 Weeks PMA"))) 

# 2. Reusable baseline table layout function
generate_base_table <- function(data_input, by_var) {
  data_input %>%
    tbl_summary(
      by = !!sym(by_var),
      percent = "column",
      missing = "ifany", 
      missing_text = "(Missing)",
      digits = all_categorical() ~ c(0, 0),
      type = list(
        m_ethnic ~ "categorical",  # Set PMH as categorical
        type1.factor  ~ "categorical",
        type2.factor  ~ "categorical",
        type3.factor  ~ "categorical",
        type4.factor  ~ "categorical",
        type5.factor  ~ "categorical",
        type6.factor  ~ "categorical",
        type7.factor  ~ "categorical",
        chorio.f.new ~ "categorical",
        ans.f.new ~ "categorical",
        prom.factor ~ "categorical",
        romdurn_hr_new ~ "continuous2",
        delmode ~ "categorical"
      ),
      statistic = list(
        all_categorical() ~ "{n} / {N} ({p}%)",
        all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})")
      ),
      label = list(m_ethnic ~ 'Ethnicity', chorio.f.new ~ "Chorioamnionitis", 
                   type1.factor  ~ "Hypertension",
                   type2.factor  ~ "Asthma",
                   type3.factor  ~ "Diabetes",
                   type4.factor  ~ "Hyperthyroidism",
                   type5.factor  ~ "Hypothyroidism",
                   type6.factor  ~ "Epilepsy",
                   type7.factor  ~ "Other",
                   chorio.f.new ~ "Chorioamnionitis",
                   ans.f.new ~ "Use of antenatal steroids",
                   prom.factor ~ 'Premature rupture of membranes',
                   romdurn_hr_new ~ 'Time between PROM and birth (h)',
                   delmode ~ 'Mode of delivery'
      )
    )
}

# 3. Stratify the table and inject dynamic sample size {n} into headers
table_stratified_mat <- analysis_data_mat %>%
  tbl_strata(
    strata = ga_strata, 
    .tbl_fun = ~ generate_base_table(.x, by_var = "rand_gp"),
    .header = "**{strata} (N={n})**" 
  )

# 4. Create the final total columns with a matching N header
total_n <- nrow(analysis_data_mat) 

table_total_mat <- generate_base_table(analysis_data_mat, by_var = "rand_gp") %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_spanning_header(all_stat_cols() ~ paste0("**All Strata Combined (N=", total_n, ")**")) 

# 5. Merge everything side-by-side
final_table_mat <- tbl_merge(
  tbls = list(table_stratified_mat, table_total_mat),
  tab_spanner = FALSE 
)

# Output final table
final_table_mat