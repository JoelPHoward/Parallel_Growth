library(ggplot2)
library(ggthemes)
library(lubridate)
library(scales)
library(reshape2)
library(dplyr)
library(stringr)
library(wesanderson)

arg_lst <- list(raw_data = list.files("./data/raw_data", full.names = TRUE),
                sample_plate = list.files("./sample_plates", full.names = TRUE))

RAW_DATA <- lapply(arg_lst[['raw_data']], readLines)
RUN_DATE <- str_match(string = arg_lst[['raw_data']][1], pattern = "\\./data/raw_data/(.*)_runID")[,2]
EXPERIMENT_ID <- str_match(string = arg_lst[['raw_data']][1], pattern = "runID_(.*)_plateID_")[,2]
PLATE_ID <- lapply(arg_lst[['raw_data']], function(x){str_match(string = x, pattern = "plateID_(.*)\\.txt")[,2]})
SAMPLE_PLATE <- lapply(arg_lst[['sample_plate']], read.csv, row.names = 1)

get_growth_curves <- function(raw_data, experiment_id, plate_id, sample_plate){
    data <- list()
    data_flag <- 0 # set to 1 when a "" line is encountered (precedes that to add to table)
    data_name <- NULL
    data_row <- 1
    for(j in 1:length(raw_data)){
        if(raw_data[j] == "" & data_flag == 0){# next line will be data
            data_flag <- 1
        }else if(raw_data[j] == "" & data_flag == 1){# data ends
            data_flag <- 0
            data_row <- 1
        }else if(raw_data[j] != "" & data_flag == 0){# this is the next data_name
            data_name <- raw_data[j]
        }else{# this is another line of data
            data[[data_name]][data_row] <- raw_data[j]
            data_row <- data_row + 1
        }
    }
    tbls <- lapply(data, function(x){
        read.table(text = paste(x, collapse = '\n'), sep = '\t', header = T)
    })
    growth <- melt(tbls[['600']], id.vars = colnames(tbls[['600']])[1:2])
    growth$Time <- hms(growth$Time)
    growth <- growth %>% left_join(sample_plate, by = c('variable' = 'well'))
    growth <- growth %>% group_by(strain, substrate, media, replicates) %>% arrange(Time) %>% ungroup() #%>% mutate(time = 0:(n()-1))
    growth <- growth %>% rename(time = Time)
    return(growth)
}

growth_data <- list()
for(i in 1:length(RAW_DATA)){
    growth_data[[i]] <- get_growth_curves(raw_data = RAW_DATA[[i]], experiment_id = EXPERIMENT_ID, plate_id = PLATE_ID[[i]], sample_plate = SAMPLE_PLATE[[i]])
    growth_data[[i]]$Date <- as.Date(RUN_DATE, format = "%y%m%d")
    growth_data[[i]]$Experiment_ID <- EXPERIMENT_ID
    colnames(growth_data[[i]]) <- c("Time", "Temp", "Well", "OD", "SPL", "SPLC", "Strain", "Substrate", "Media", "Replicate", "Plate", "Column", "Row", "Row_n", "Date", "Experiment_ID")
    growth_data[[i]] <- growth_data[[i]] %>% select("Experiment_ID", "Date", "Time", "Temp", "SPL", "SPLC", "Strain", "Substrate", "Media", "Replicate", "Plate", "Well", "OD")
    write.csv(growth_data[[i]], paste0("./data/processed_data/unfiltered/", RUN_DATE, "_runID_", EXPERIMENT_ID, "_plateID_", PLATE_ID[[i]], "_unfiltered_.csv"))
}

blanks <- do.call(rbind, lapply(growth_data, function(x){
    x %>% filter(SPL == "") %>% group_by(SPLC, Time) %>% summarise(Blank_Mean = mean(OD))
})) %>% ungroup()

for(i in 1:length(growth_data)){
    growth_data[[i]] <- growth_data[[i]] %>% left_join(blanks, by = c("Time", "SPLC"))
    if(class(growth_data[[i]]$OD) != "numeric"){
        if(any(growth_data[[i]])){
            growth_data[[i]] <- growth_data[[i]][-which(growth_data[[i]]$OD == "?????"),]
        }
        growth_data[[i]]$OD <- as.numeric(growth_data[[i]]$OD)
    }
    growth_data[[i]] <- growth_data[[i]] %>%
        filter(SPL != "") %>%
        mutate(OD_Blanked = OD - Blank_Mean) %>%
        group_by(Experiment_ID, Date, Time, SPL, Strain, Substrate, Media) %>%
        summarise(Mean_Temp = mean(Temp), SD_Temp = sd(Temp), Mean_OD_Blanked = mean(OD_Blanked), SD_OD_Blanked = sd(OD_Blanked)) %>%
        ungroup()
}
growth_data <- do.call(rbind, growth_data)
growth_data$Time <- as.numeric(growth_data$Time, "hours")
growth_data <- growth_data %>% select("Experiment_ID", "Date", "Time", "SPL", "Strain", "Substrate", "Media", "Mean_Temp", "SD_Temp", "Mean_OD_Blanked", "SD_OD_Blanked")

write.csv(growth_data, paste0('./data/processed_data/filtered/', RUN_DATE, "_runID_", EXPERIMENT_ID,'_filtered.csv'))

cols <- c("black", wes_palettes[["Darjeeling1"]])
pdf(paste0("./img/growth_curves/", RUN_DATE, "_", EXPERIMENT_ID, "_growth_results_default.pdf"), width = 12, height = 6)
ggplot(growth_data, aes(x = Time, y = Mean_OD_Blanked)) + 
    geom_point(aes(color = Substrate, shape = Media)) + 
    scale_color_manual(values = cols) +
    #scale_x_time(labels = label_time(format = '%H')) + 
    facet_wrap(~Strain) +
    ylab("OD600") +
    xlab("Time (h)") +
    theme_base() +
    theme(strip.text = element_text(face = "italic"))
dev.off()

pdf(paste0("./img/growth_curves/", RUN_DATE, "_", EXPERIMENT_ID, "_growth_results_freeY.pdf"), width = 12, height = 6)
ggplot(growth_data, aes(x = Time, y = Mean_OD_Blanked)) + 
    geom_point(aes(color = Substrate, shape = Media)) + 
    scale_color_manual(values = cols) +
    #scale_x_time(labels = label_time(format = '%H')) + 
    facet_wrap(~Strain, scales = "free_y") +
    ylab("OD600") +
    xlab("Time (h)") +
    theme_base() +
    theme(strip.text = element_text(face = "italic"))
dev.off()