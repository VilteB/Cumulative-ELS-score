require(openxlsx)

summdf <- function(object) {
  # take summary object, clean the strings and note them as row.names, return a data.frame
  m <- apply(object, 2, function(y) as.numeric(sub(".*:", "", y))) 
  m <- as.data.frame(m, row.names = c('Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max.', 'NAs'))
  m[8,] <- apply(sample, 2, sd, na.rm = T)
  row.names(m)[8] <- 'SD'
  return(m[, -1])
}

flowchart <- function(df, return_selected_sample = F) {
  
  fc <- list(initial_sample = nrow(df))
  # enough prenatal variables
  step1 <- df[df$pre_percent_missing < 50.0,]
  loss <- nrow(step1) - as.numeric(fc[length(fc)])
  fc <- c(fc, no_pren = loss, after_pren_selection = nrow(step1))
  # enough postnatal variables
  step2 <- step1[step1$post_percent_missing < 50.0,]
  loss <- nrow(step2) - as.numeric(fc[length(fc)])
  fc <- c(fc, no_post = loss, after_post_selection = nrow(step2))
  # internalizing score
  step3 <- step2[!is.na(step2$intern_score_z),] 
  loss <- nrow(step3) - as.numeric(fc[length(fc)])
  fc <- c(fc, no_intern = loss, after_inte_selection = nrow(step3))
  # fat mass 
  step4 <- step3[!is.na(step3$FAT_MASS_Z),] # fmi_z
  loss <- nrow(step4) - as.numeric(fc[length(fc)])
  fc <- c(fc, no_fatmass = loss, after_fatm_selection = nrow(step4))
  # exclude twins
  step5 <- step4[step4$twin == 0,] 
  loss <- nrow(step5) - as.numeric(fc[length(fc)])
  fc <- c(fc, no_twins = loss, after_twin_selection = nrow(step5))
  #  exclude siblings
  finalsample <- step5[step5$sibling == 0,]
  loss <- nrow(finalsample) - as.numeric(fc[length(fc)])
  fc <- c(fc, no_siblings = loss, final_sample = nrow(finalsample))
  
  print(fc)
  
  if (return_selected_sample == T) { return(finalsample) }
  
}

# Load datasets
if (exists("imp") == F) { 
  imp_path <- file.choose() # choose the 'imputation_list_full.rds' file
  imp <- readRDS(imp_path)
}
# original dataset (with missing data)
data <- mice::complete(imp, action = 0)

# add correct fat mass 
out  <- readRDS(file.choose()) # choose "PCMout_cov_aux.rds"
data$FAT_MASS_Z <- out$fat_mass_z
data$RISK_GROUPS <- out$risk_groups

################################################################################

sample <- flowchart(data, return_selected_sample = T)
# ============================================================================ #

# Flowchart
fc <- capture.output(flowchart(ELSPCM_essentials))[1:(which(fc=='$final_sample')+2)]
fcm <- as.data.frame(t(matrix(unlist(fc), ncol = 13)[1:2, ])) 
fcm <- data.frame(fcm[,-1], row.names = fcm[,1])
names(fcm) = 'N'
fcm$N <- as.numeric(sub("\\[1]", "", fcm$N))

# Sample summary
s <- summdf(summary(sample))

# Group specific summary
bys <- by(sample, sample$RISK_GROUPS, summary)

ht <- summdf(bys[["healthy"]])
it <- summdf(bys[["internalizing_only"]])
ft <- summdf(bys[["cardiometabolic_only"]])
mm <- summdf(bys[["multimorbid"]])

# sex and ethnicity
bysex <- by(sample, sample$sex, summary)

boys <- summdf(bysex[[1]])
girl <- summdf(bysex[[2]])

# Correlation matrix
cors <- as.data.frame(cor(sample[, !names(sample) %in% c('IDC', 'twin', 'mother', 
                                                         'sex', 'ethnicity', 'RISK_GROUPS',
                                                         'risk_groups', 'risk_groups_rec')], 
            use = 'pairwise.complete.obs'))

################################################################################

# Export the outputs of summary statistics into an xlsx file with one model per sheet

stats <- list("flowchart" = fcm, "summ_full" = s, "corr_mat" = cors,
              "summ_health" = ht, "summ_intern" = it, "summ_fatmas" = ft, "summ_multim" = mm, 
              "summ_boys" = boys, "summ_girls" = girl)

openxlsx::write.xlsx(stats, file = paste0(pathtodata, "Descriptives.xlsx"), 
                     row.names = T, overwrite = T)
