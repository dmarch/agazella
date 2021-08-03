

library(flextable)

## Load dependencies
source("setup.R")
sp_code <- "GAZ"  # species code
mod_code <- "brt"

# import optim parameters
indir <- paste(output_data, "habitat-model-v2", sp_code, mod_code, sep="/")
mod_out <- read.csv(paste0(indir, "/", sp_code, "_", mod_code, "_optim_params.csv"))

# output dir
outdir <- paste(output_data, "fig", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# prepare table
tbl <- mod_out %>%
  dplyr::arrange(desc(cv.AUC)) %>%
  #dplyr::slice(1:20) %>%
  mutate(AUC = round(AUC, 2),
          cv.AUC = round(cv.AUC, 2),
         PER = round(PER, 1),
         cv.PER = round(cv.PER, 1)) %>%
  dplyr::select(lr, tc, bf, n.trees, AUC, cv.AUC, PER, cv.PER)

# create tablr
ft <- flextable(tbl) 

ft <- set_header_labels(ft, lr = "Learning rate", 
                        tc = "Tree complexity", 
                        bf = "Bag fraction", 
                        n.trees = "Num. of trees",
                        AUC = "AUC",
                        cv.AUC = "CV AUC",
                        PER = "Deviance explained (%)",
                        cv.PER = "CV Deviance explained (%)")

# font size
ft <- fontsize(ft, part = "all", size = 14)

#format data
ft <- colformat_num(ft, na_str = "n.a.")

# set properties
ft <- set_table_properties(ft, width = 1, layout = "autofit")  # autofit to width

# highlight select combination in bold
ft <- bold(ft, i = 1, bold = TRUE, part = "body")

# Export plot
p_png <- paste0(outdir, "/table_optim.png")
save_as_image(ft, p_png)

p_doc <- paste0(outdir, "/table_optim.docx")
save_as_docx(ft, path = p_doc)
