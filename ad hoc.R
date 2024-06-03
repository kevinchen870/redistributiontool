lga = read_absmap('lga2022')
saveRDS(lga,'LGA.rds')

SA1_selected = SA1 %>%
  filter(Proposed.Division == 'Wills')

DT::datatable(SA1 %>% 
    filter(sa1_code_2021 %in% SA1_selected$sa1_code_2021) %>%
    mutate(Division = str_to_title(Current.Division)) %>%
    st_drop_geometry(data_all) %>%
    summarise(tot_project = sum(tot_project)) %>%
    mutate(deviation = paste0(round((tot_project-127238)/127238 * 100,2),
                              '%')) %>%
  rename('Total selected projected population' =tot_project),
  style="bootstrap",
  selection = "multiple",
  rownames = F,
  extensions = 'Buttons',
  escape=FALSE,
  options = list(lengthChange = F,
                 scrollX = "auto",
                 dom = 'Bfrtip',
                 buttons = c('copy')))

popdata.May.totals = read.xlsx("files/Vic May 2024-proposed-electoral-divisions-SA1-and-SA2.xlsx") %>%
  filter(!is.na(`SA1.Code.(2021.SA1s)`)) %>%
  mutate(sa1_code_2021 = 
           as.character(substr(`SA1.Code.(2021.SA1s)`,1,7))) %>%
  rename(tot_project = `Projected.Enrolment.17/04/28`) %>%
  group_by(sa1_code_2021) %>%
  summarise(tot_project = sum(tot_project))

popdata.May = read.xlsx("files/Vic May 2024-proposed-electoral-divisions-SA1-and-SA2.xlsx") %>%
  filter(!is.na(`SA1.Code.(2021.SA1s)`)) %>%
  mutate(sa1_code_2021 = 
           as.character(substr(`SA1.Code.(2021.SA1s)`,1,7))) %>%
  arrange(sa1_code_2021,desc(`Projected.Enrolment.17/04/28`)) %>%
  distinct(sa1_code_2021, .keep_all = TRUE) %>%
    left_join(popdata.May.totals, by = 'sa1_code_2021')

saveRDS(popdata.May, 'files/PopdataMay.RDS')           
            

