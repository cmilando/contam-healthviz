
out_dir <- "D:\\BU_backup\\contam_test\\ASTHMA_database\\0_final"
rj <- "r1x1_run.JSON"

files_to_run <- read_json(file.path(out_dir, "01_torun", rj))
for(i in seq(files_to_run)) {
  files_to_run[[i]]$f_name = names(files_to_run)[[i]]
}
names(files_to_run) <- NA

data.frame(files_to_run) %>%
  select(contains("opts.")) %>%
  pivot_longer(cols = contains("opts.")) %>%
  print(n = 100)

# so here's the idea, use DT: https://rstudio.github.io/DT/shiny.html

# 1. load rj
# 2. iterate through, and make the table, include a unique identifier, 
#    or just hide the column that is the super long row name
#
#    This probably involves an Rbind of a `long` format of the `opts` object, 
#    followed by a pivot_wider to make the table, and fill with "NA" or similar
#    so that different levels of `opts` are allowed
#
# 3. When user selects the row, that is that pulls up the graphs and tables. 
#    If they select multiple rows, those are made different fill or other colors
#    As long as the naming convention is similar, this **SHOULD** work out