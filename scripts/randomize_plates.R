####### INPUTS ########
# Design file (json) that specifies the plates and the designs to use (character)
design_file <- "./design_file.json"
# random seed (int)
date <- Sys.Date()
seed <- as.numeric(date)
# output folder (dir created in from initialize_experiment) (character)
out_dir <- "./"
# number of pipette channels on NIMBUS
n_channels <- 4
# fill
fill <- "media"
# colour
colour <- "substrate"
# shape
shape <- "strain"
#col_palette
col_palette <- c(RColorBrewer::brewer.pal(8, 'Dark2'), RColorBrewer::brewer.pal(3, 'Set1')[1],'black')
#shape_palette
shape_palette <- c(1:10)
#fill_palette
fill_palette <- c('red', 'blue', 'green', 'yellow')
##### END INPUTS ######

#### LIBRARIES ####
library(ggplot2)
library(ggthemes)
library(dplyr)
library(rjson)
library(tibble)
library(wesanderson)
#### END LIBRARIES ####

####### FUNCTIONS #############
get_plate_dims <- function(format){
    if(format == 6){
        c(2,3)
    }else if(format == 12){
        c(3,4)
    }else if(format == 24){
        c(4,6)
    }else if(format == 48){
        c(6,8)
    }else if(format == 96){
        c(8,12)
    }else if(format == 384){
        c(16,24)
    }else if(format == 1536){
        c(32,48)
    }else if(format == 3456){
        c(48,72)
    }else{
        stop(paste0("Specified format (", format, ") is not supported."))
    }
}

get_wells <- function(format){
    dims <- get_plate_dims(format)
    unlist(lapply(LETTERS[1:dims[1]], function(x){paste0(x, 1:dims[2])}))
}
####### END FUNCTIONS #############

# set seed
set.seed(seed) # prior to 2022-11-14, the seed was set to the value of 4
write.csv(data.frame(Date = date, Seed = seed), "seed_used.csv")

# load design file
design_json <- fromJSON(file = design_file)
# expand n replicates to a 1:n vector
for(i in 1:length(design_json$designs)){
    design_json$designs[[i]]$replicates <- 1:design_json$designs[[i]]$replicates
}

#### TO DO ######
# function to validate design file
#    if identical == TRUE, all plate formats must be the same
#    if v1 is null, make sure c1 and c2 are not null
## check that specified design(s) to use for the plate are given as designs, if not: error
## check that replicates are specified, if not: error
#### END TO DO ######

# get names of medias, strains and substrates. Names must be unique.
medias <- names(design_json$medias)
if(any(duplicated(medias))){
    stop("Media names must be ubique.")
}
strains <- names(design_json$strains)
if(any(duplicated(strains))){
    stop("Strain names must be ubique.")
}
substrates <- names(design_json$substrates)
if(any(duplicated(substrates))){
    stop("Substrate names must be ubique.")
}

# get the number of plates in the design file and the format of each plate.
n_plates <- length(design_json$plates)
plate_format <- if(design_json$identical_plates == TRUE){
    design_json$plates[[1]]$format # if plates are meant to be idential, plate format should be the same for all plates
}else{
    unlist(lapply(design_json$plates, function(x) x$format))
}

# get the well formats for each plate
wells <- lapply(design_json$plates, function(x) get_wells(x$format))

# create a data frame from all combinations of the supplied vectors or factors from the specified designs
designs_df <- do.call(rbind, lapply(design_json$designs, expand.grid))
# if plates are meant to be identical, the total number of designs can't exceed the number of wells for one plate
if(design_json$identical_plates == TRUE && nrow(designs_df)>plate_format){
    stop(paste0("Plate format (", plate_format, " wells) can't support the specified design (", nrow(designs_df), " wells)."))
}
# if plates are not meant to be identical, the total number of designs can't exceed the number of wells for all plates
if(nrow(designs_df) > sum(unlist(lapply(design_json$plates, function(x) x$format)))){
    stop(paste0("Only ", sum(unlist(lapply(design_json$plates, function(x) x$format))), " wells available, but ", sum(unlist(lapply(designs_df, nrow))), " required. Decrease number of designs or add more wells."))
}

# list of each design, ignoring replicates
condition_cols <- designs_df %>%
    group_by_at(colnames(designs_df)[-which(colnames(designs_df) == "replicates")]) %>%
    group_split() %>%
    as.list()

blank <- design_json$blank[[1]]
names(blank) <- names(design_json$blank)[1]

sample_cntr <- 1
blank_idx <- NULL
blank_cntr <- 1
for(i in 1:length(condition_cols)){
    if(all(condition_cols[[i]][[names(blank)]] == blank)){
        condition_cols[[i]]$SPLC <- paste0("SPLC", blank_cntr)
        condition_cols[[i]]$SPL <- ""
        blank_idx <- c(blank_idx, i)
        blank_cntr <- blank_cntr + 1
    }else{
        condition_cols[[i]]$SPLC <- ""
        condition_cols[[i]]$SPL <- paste0("SPL", sample_cntr)
        sample_cntr <- sample_cntr + 1
    }
}

for(i in 1:length(condition_cols)){
    if(i %in% blank_idx){
        next
    }else{
        spl <- unlist(as.list(condition_cols[[i]] %>% select('strain', 'substrate', 'media') %>% slice(1)))
        for(j in blank_idx){
            splc <- unlist(as.list(condition_cols[[j]] %>% select('strain', 'substrate', 'media') %>% slice(1)))
            if(all(spl == splc)){
                condition_cols[[i]]$SPLC <- condition_cols[[j]]$SPLC
            }
        }
    }
}

# randomize the order of designs
rand_design_order <- sample(1:length(condition_cols), length(condition_cols), replace = FALSE)

# create the sample plate layouts
sample_plates <- list()
# for each plate...
for(i in 1:n_plates){
    # get the plate dimensions (rows x cols)
    dims <- get_plate_dims(design_json$plates[[i]]$format)
    # all replicates for the same design are run on the same plate and it is assumed that there are 4 reps per design
    designs <- rand_design_order[1:(prod(dims)/4)]
    # remove used designs from randomized pool
    rand_design_order <- rand_design_order[-c(1:(prod(dims)/4))]
    # convert to a data frame
    sample_plate <- do.call(rbind, condition_cols[designs])
    # add column, well, row and plate info
    sample_plate$column <- unlist(lapply(1:dims[2], rep, dims[1]))
    sample_plate$well <- ""
    idx <- seq(1,prod(dims),dims[1])
    for(j in 1:length(idx)){
        sample_plate$well[idx[j]:(idx[j]+dims[1]-1)] <- c(wells[[i]][seq(j, prod(dims), dims[2]*2)], wells[[i]][seq(j+dims[2], prod(dims), dims[2]*2)])
    }
    sample_plate$row <- substr(sample_plate$well,1,1)
    sample_plate$row_n <- match(sample_plate$row,LETTERS)
    sample_plate$plate <- paste0("Plate ", i)
    # add media, strain and substrate info
    sample_plate$media <- factor(names(design_json$medias)[sample_plate$media], levels = names(design_json$medias))
    sample_plate$strain <- factor(names(design_json$strains)[sample_plate$strain], levels = names(design_json$strains))
    sample_plate$substrate <- factor(names(design_json$substrates)[sample_plate$substrate], levels = names(design_json$substrates))
    sample_plates[[i]] <- as.data.frame(sample_plate)
}

# combine into one data frame
sample_plate_df <- do.call(rbind, sample_plates)
dims <- get_plate_dims(96)

# create visual of sample plate
pdf(paste0(out_dir, 'img/setup/sampleplate_layout2.pdf'), height = (4*n_plates), width = 15)
ggplot(data = sample_plate_df, aes(x = column, y = row_n)) +
    geom_point(aes_string(fill = fill), shape = 21, size = 10, alpha = 0.1) +
    geom_point(aes_string(shape = shape, color = colour), stroke = 1) +
    geom_text(aes(label = SPL), size = 5, alpha = 0.2) +
    geom_text(aes(label = SPLC), size = 5, alpha = 0.2) +
    scale_color_manual(values = col_palette) +
    scale_shape_manual(values = shape_palette) +
    scale_fill_manual(values = fill_palette) +
    scale_y_reverse(breaks = 1:dims[1], labels = LETTERS[1:dims[1]]) +
    scale_x_continuous(breaks = 1:dims[2], position = 'top') +
    xlab('') +
    ylab('') +
    theme_base() +
    theme(axis.ticks = element_blank(), plot.background = element_blank(), legend.box = "horizontal") +
    guides(fill = guide_legend(order = 1), shape = guide_legend(order = 2), colour = guide_legend(order = 3)) +
    facet_grid(rows = "plate")
dev.off()

for(i in 1:length(sample_plates)){
    tmp <- sample_plates[[i]] %>% select("SPL", "SPLC", "strain", "substrate", "media", "replicates", "plate", "well", "column", "row", "row_n")
    tmp <- tmp %>% group_by(substrate, media) %>% group_split() %>% as.list()
    for(j in 1:length(tmp)){
        x <- unique(tmp[[j]]$SPLC)
        tmp[[j]]$SPLC <- x[grep("SPLC", x)]
    }
    tmp <- do.call(rbind, tmp)
    write.csv(tmp, paste0(out_dir, "sample_plates/Sample_", gsub(" ", "_", unique(tmp$plate)), ".csv"))
}

# create culture plate (assuming it is a 96WP)
culture_plate <- data.frame(well = rep("", 96))
cp_wells <- get_wells(96)

# add wells in NIBMUS dispense order (column-wise, 4 wells at a time, one well spacing)
idx <- seq(1,96,8)
for(i in 1:length(idx)){
    culture_plate$well[idx[i]:(idx[i]+7)] <- c(cp_wells[seq(i, 96, 24)], cp_wells[seq(i+12, 96, 24)])
}

# add substrate and strain info
culture_plate$type <- "Substrate"
culture_plate$content <- "Empty"

culture_plate_strains <- strains[unlist(lapply(1:length(strains), rep, 8))]
culture_plate_substrates <- substrates[unlist(lapply(1:length(substrates), rep, 4))]

culture_plate$type[1:length(culture_plate_strains)] <- "Strain"
culture_plate$content[1:length(culture_plate_strains)] <- culture_plate_strains

culture_plate$content[(length(culture_plate_strains) + 1):(length(culture_plate_strains) + length(culture_plate_substrates))] <- "Substrate"
culture_plate$content[(length(culture_plate_strains) + 1):(length(culture_plate_strains) + length(culture_plate_substrates))] <- culture_plate_substrates

# count the number of times liquid has been transferred out of this well. Will be used to ensure that wells with the most liquid remaining will be used.
culture_plate$transfers <- 0

# add column and row info
culture_plate$column <- as.numeric(substr(x = culture_plate$well, start = 2, stop = 100000000L))
culture_plate$row <- substr(x = culture_plate$well, start = 1, stop = 1)
culture_plate$row_n <- match(culture_plate$row,LETTERS)

# create a visual of the culture plate
pdf(paste0(out_dir, 'img/setup/cultureplate_layout.pdf'), height = 5, width = 12)
ggplot(data = culture_plate, aes(x = column, y = row_n)) +
    geom_point(shape = 21, size = 7, alpha = 0.3) +
    geom_point(aes(color = type), size = 0) +
    geom_text(aes(label = content, color = type), size = 3, show.legend = FALSE) +
    scale_y_reverse(breaks = 1:8, labels = LETTERS[1:8]) +
    scale_x_continuous(breaks = 1:12, position = 'top') +
    xlab('') +
    ylab('') +
    theme_base() +
    theme(axis.ticks = element_blank(), plot.background = element_blank()) +
    guides(colour = guide_legend(override.aes = list(size=3)))
dev.off()

# get the working volumes for each plate
v_total <- unlist(lapply(design_json$plates, function(x) x$volume))

# create worklist for Hamilton method
worklists <- list()
for(i in 1:length(sample_plates)){
    # use the sample plate df for each plate as a template
    worklist <- sample_plates[[i]]
    # add a sample_id column
    worklist <- worklist %>% mutate(sample_id = paste0(sub(" ", "_", plate),"_","Sample_",1:n()))
    # init well, vol and liq class columns for substrate and strain
    worklist$substrate_well <- ""
    worklist$substrate_vol <- 0
    worklist$substrate_liq_class <- ""
    worklist$strain_well <- ""
    worklist$strain_vol <- 0
    worklist$strain_liq_class <- ""
    # for each column in a 96WP...
    for(j in seq(1,nrow(worklist),4)){
        for(k in c('substrate', 'strain')){
            # get the culture plate wells containing the j:j+3 substrates/strains
            culture_plate_idx <- which(culture_plate$content == worklist[[k]][j:(j+3)])
            # if the culture plate has more than 4 wells containing the substrate/strain, 
            # figure out which set of wells has the most volume remaining and use those.
            if(length(culture_plate_idx) > 4){
                set_idx <- 1
                min_transfers <- sum(culture_plate$transfers[culture_plate_idx[1:4]])
                for(l in seq(5, length(culture_plate_idx), 4)){
                    if(sum(culture_plate$transfers[culture_plate_idx[l:(l+3)]]) < min_transfers){
                        set_idx <- l
                        min_transfers <- sum(culture_plate$transfers[culture_plate_idx[l:(l+3)]])
                    }
                }
                culture_plate_idx <- culture_plate_idx[set_idx:(set_idx+3)]
                culture_plate$transfers[culture_plate_idx] <- culture_plate$transfers[culture_plate_idx] + 1
            }
            # add location on culture plate
            worklist[[paste0(k, '_well')]][j:(j+3)] <- culture_plate$well[culture_plate_idx]
            # add volume to transfer to sample plate and liquid class to use
            vols <- NULL
            for(l in culture_plate_idx){
                v_c <- design_json[[paste0(k,'s')]][[culture_plate$content[l]]]
                if(!is.null(v_c$v1)){
                    vols <- c(vols, v_c$v1)
                }else{
                    vols <- c(vols, v_c$c2 * v_total[i] / v_c$c1)
                }
            }
            worklist[[paste0(k, '_vol')]][j:(j+3)] <- vols
            worklist[[paste0(k, '_liq_class')]][j:(j+3)] <- unlist(lapply(culture_plate$content[culture_plate_idx], function(x) design_json[[paste0(k,'s')]][[which(x == names(design_json[[paste0(k,'s')]]))]]$liq_class))
        }
    }
    # add volume and liquid class for media. Location already specified from sample_plates[[i]]
    worklist$media_vol <- v_total[i] - worklist$substrate_vol - worklist$strain_vol
    worklist$media_liq_class <- unlist(lapply(worklist$media, function(x) design_json$medias[[x]]$liq_class))
    worklists[[i]] <- worklist
}

# combine
worklist <- do.call(rbind, worklists)
worklist$plate <- as.numeric(sub("Plate ", "", worklist$plate))
worklist$media <- match(worklist$media, medias)
# reformat to be compatible with Hamilton method
worklist_xls <- worklist %>% select(plate, sample_id, well, media, media_vol, media_liq_class, substrate_well, substrate_vol, substrate_liq_class, strain_well, strain_vol, strain_liq_class)
colnames(worklist_xls) <- c("Plate Number", "Sample ID", "Target Well", "Media", "Media Volume", "Media Liquid Class", "Substrate Well", "Substrate Volume", "Substrate Liquid Class", "Inoculum Well", "Inoculum Volume", "Inoculum Liquid Class")

# write to xls
xlsx::write.xlsx(as.data.frame(worklist_xls), paste0(out_dir, 'worklist.xls'),col.names = T, row.names = F)