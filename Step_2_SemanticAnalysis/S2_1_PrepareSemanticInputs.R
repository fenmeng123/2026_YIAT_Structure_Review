# Author: Kunru Song
# Prepare three wording versions and the domain-neutral benchmark definitions.
library(dplyr)
library(readxl)
input_dir <- 'Res_1_Data_for_analyses'
raw_items <- read_excel(file.path(input_dir,'YIAT_20_Item_Wording_Four_Versions_v01.xlsx'),sheet='Wordings_Long') %>%
  mutate(source_excel_row=row_number()+1L)
raw_domains <- read_excel(file.path(input_dir,'YIAT_Domain_Neutral_Benchmark_Dictionary_v02.xlsx'),sheet='Benchmark_Dictionary')
versions <- list()
for (version in c('MAK_YOUNG_2020','CHINESE','ORIGINAL')) {
  versions[[version]] <- raw_items %>% filter(Version_ID==version) %>% arrange(Item_Number) %>%
    transmute(item_id=Item_ID,item_number=as.integer(Item_Number),item_text=Item_Wording,
      display_wording=if_else(Language=='en',sub('^How often (?:do you|does|do)\\s+','',Item_Wording,perl=TRUE),Item_Wording),
      item_version=Version_ID,language=Language,source_wording=Source_Wording,shared_stem=Shared_Stem,
      upstream_source_cell=Source_Cell,source_excel_row,preparation_note=Preparation_Note) %>% as.data.frame()
}
domains <- raw_domains %>% transmute(domain_id=Criterion_ID,framework=Framework,
  domain_label=Standardized_Label,Standardized_Label,definition_text=Domain_Neutral_Definition,
  criterion_type=Criterion_Type,language='en',construct=Construct,source_reference=Source_Reference,
  source_url=Source_URL,source_paragraphs=Source_Paragraphs,source_excel_row=row_number()+1L) %>% as.data.frame()
inputs <- list(author='Kunru Song',versions=versions,domains=domains,
  text_policy='Encode full item wording and definition only; shortened labels and display stems do not enter the encoder.')
saveRDS(inputs,file.path(input_dir,'Semantic_Inputs.rds'))
jsonlite::write_json(inputs,file.path(input_dir,'Semantic_Inputs.json'),pretty=TRUE,auto_unbox=TRUE,dataframe='rows')
