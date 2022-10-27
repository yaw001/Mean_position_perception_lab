library(rjson)
library(tidyverse)
library(lsr)
library(sp)
setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_lab_data/Precision_data")

#data transformation
all.data_precision = list()
subject = 1
for(file.name in list.files(pattern = '*.json')) {
  json_file = fromJSON(file = file.name)
  json_file[['subject']] = subject
  all.data_precision[[subject]] = json_file
  subject = subject + 1
}

setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_lab/Analysis")
source("helper.R")

trial_response_precision = tibble()
num_subj = length(all.data_precision)
num_subj
for(i in 1:num_subj) {
  for(j in 1:660) {
    trial_response_precision = bind_rows(trial_response_precision,
                                    tibble(subject = all.data_precision[[i]]$subject,
                                           sid = all.data_precision[[i]]$client$sid,
                                           trial_number = all.data_precision[[i]]$trials[[j]]$trialNumber,
                                           mean_index = all.data_precision[[i]]$trials[[j]]$mean_index,
                                           group_1_size = all.data_precision[[i]]$trials[[j]]$group_1_size,
                                           group_2_size = all.data_precision[[i]]$trials[[j]]$group_2_size,
                                           group_1_sd = all.data_precision[[i]]$trials[[j]]$group_1_sd,
                                           group_2_sd = all.data_precision[[i]]$trials[[j]]$group_2_sd,
                                           inner_width = all.data_precision[[i]]$trials[[j]]$inner_width,
                                           inner_height = all.data_precision[[i]]$trials[[j]]$inner_height,
                                           width_height = min(inner_width,inner_height)*0.98,
                                           aspect_ratio = inner_width/inner_height,
                                           # normalized coordinate
                                           group_1_coord = list(coord_fcn(all.data_precision[[i]]$trials[[j]]$group_1_coord,width_height)),
                                           group_2_coord = list(coord_fcn(all.data_precision[[i]]$trials[[j]]$group_2_coord,width_height)),
                                           
                                           all_coordinates = list(coord_all_fcn(all.data_precision[[i]]$trials[[j]]$all_coordinates,width_height)),
                                           response_coordinate = list(c(all.data_precision[[i]]$trials[[j]]$response_coord$x/width_height,
                                                                        all.data_precision[[i]]$trials[[j]]$response_coord$y/width_height)
                                           ),
                                           mean_group_1 = list(c(all.data_precision[[i]]$trials[[j]]$mean_group_1$x/width_height,
                                                                 all.data_precision[[i]]$trials[[j]]$mean_group_1$y/width_height)
                                           ),
                                           mean_group_2 = list(c(all.data_precision[[i]]$trials[[j]]$mean_group_2$x/width_height,
                                                                 all.data_precision[[i]]$trials[[j]]$mean_group_2$y/width_height)
                                           ),
                                           mean_all = list(c(all.data_precision[[i]]$trials[[j]]$mean_all$x/width_height,
                                                             all.data_precision[[i]]$trials[[j]]$mean_all$y/width_height)
                                           )
                                    )
    )
  }
}

#Error analysis
tb.errors_precision= trial_response_precision %>% 
  rowwise() %>% 
  mutate(size_ratio = group_1_size/group_2_size,
         precision_ratio = 1/(group_1_sd/group_2_sd),
         mean_index = mean_index,
         mean_all_recenter = list(c(mean_all[1]-0.5,mean_all[2]-0.5)),
         all_coordinates_recenter = list(recenter(all_coordinates,mean_all)),
         group_1_coord_recenter = list(recenter(group_1_coord,mean_all)),
         group_2_coord_recenter = list(recenter(group_2_coord,mean_all)),
         response_recenter = list(response_coordinate - mean_all),
         
         convex_hull_coord = list(compute_ch(all_coordinates_recenter)),
         mean_ch = list(colMeans(convex_hull_coord)),
         
         covariance = list(cov(all_coordinates_recenter)),
         eigenval = list(eigen(covariance)),
         eigenvec = list(eigen(covariance)$vector),
         group_1_mean_x= mean(group_1_coord_recenter$x),
         group_1_mean_y= mean(group_1_coord_recenter$y),
         group_2_mean_x= mean(group_2_coord_recenter$x),
         group_2_mean_y= mean(group_2_coord_recenter$y),
         
         proj.x_all = list(as.matrix(all_coordinates_recenter)%*%eigenvec[,1]),
         proj.y_all = list(as.matrix(all_coordinates_recenter)%*%eigenvec[,2]),
         proj.x_all_mean = mean(proj.x_all),
         proj.y_all_mean = mean(proj.y_all),
         
         proj.x_all_ch = list(as.matrix(convex_hull_coord)%*%eigenvec[,1]),
         proj.y_all_ch = list(as.matrix(convex_hull_coord)%*%eigenvec[,2]),
         proj.x_all_mean_ch = mean(proj.x_all_ch),
         proj.y_all_mean_ch = mean(proj.y_all_ch),
         
         proj.x_group_1 = list(as.matrix(group_1_coord_recenter)%*%eigenvec[,1]),
         proj.y_group_1 = list(as.matrix(group_1_coord_recenter)%*%eigenvec[,2]),
         proj.x_group_2 = list(as.matrix(group_2_coord_recenter)%*%eigenvec[,1]),
         proj.y_group_2 = list(as.matrix(group_2_coord_recenter)%*%eigenvec[,2]),
         
         proj.x_group_1_mean = mean(proj.x_group_1),
         proj.y_group_1_mean = mean(proj.y_group_1),
         proj.x_group_2_mean = mean(proj.x_group_2),
         proj.y_group_2_mean = mean(proj.y_group_2),
         proj.x_response = response_recenter%*%eigenvec[,1],
         proj.y_response = response_recenter%*%eigenvec[,2],
         min_x_proj = min(proj.x_all),
         max_x_proj = max(proj.x_all),
         min_y_proj = min(proj.y_all),
         max_y_proj = max(proj.y_all),
         # Check responses
         # responses located within the boundary of objects (main_axis)?
         wihtin_boundary_x = ifelse(proj.x_response>min_x_proj && proj.x_response<max_x_proj, "T", "F"),
         
         # responses located between the group means?
         between_means_x = ifelse(proj.x_response>min(proj.x_group_1_mean,proj.x_group_2_mean) &&
                                    proj.x_response<max(proj.x_group_1_mean,proj.x_group_2_mean),
                                  "T","F"),
         # Absolute error
         abs_error_to_all = abs_err_dist(response_recenter,c(0,0)),
         abs_error_to_all_proj_x = abs_err_dist(proj.x_response,c(0,0)),
         abs_error_to_ch = abs_err_dist(response_recenter,mean_ch),
         
         group_1_weight_raw = compute_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_response)$weight_group_1,
         group_1_weight = ifelse(group_1_weight_raw<=0, 0.001, group_1_weight_raw),
         true_group_1_weight = group_1_size/(group_1_size + group_2_size),
         group_2_weight= 1-group_1_weight,
         true_group_2_weight = group_2_size/(group_1_size + group_2_size)
  )

# Data cleaning and sanity checks (exclusion)
# Exclusion criterion

# Attention check trials
tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",][which(tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",]$mean_index==(-1)),] %>% 
  pull(subject) %>% 
  unique()

#Check the out-of-boundary response trials
coord_out = tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",] %>% 
  pull(all_coordinates)
# Add index column for each dataframes in a list
coord_out = coord_out %>% Map(cbind, ., trial_num = seq_along(.),type = "stimuli") %>% do.call(rbind,.)

coord_response_out = tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",] %>% pull(response_coordinate) %>% do.call(rbind,.) %>% 
  as.data.frame() %>% mutate(trial_num = 1:nrow(.),type = "response")
colnames(coord_response_out)[1:2] = c("x", "y")
all_coord_out = rbind(coord_out, coord_response_out)
all_coord_out %>% ggplot(aes(x=x,y=y,color=type)) +geom_point() + facet_wrap(trial_num~.)

# within_boundary_x 
# Likely due to button misclick
tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="F",] %>% print(n=100)
tb.errors_precision = tb.errors_precision[tb.errors_precision$wihtin_boundary_x=="T",] 

#Exclude practice and attention check trials
tb.errors_precision_dat = tb.errors_precision[-which(tb.errors_precision$mean_index %in% (-(1:7))),]

# weird trials
tb.errors_precision_dat[tb.errors_precision_dat$group_1_weight>=1,]

#Mean absolute error for individual subject
mean_abs_error_precision = tb.errors_precision_dat %>% filter(size_ratio == 1,precision_ratio==1) %>% group_by(subject) %>% 
  summarise(mean_abs_error = mean(abs_error_to_all)) %>% 
  mutate(z_score = (mean_abs_error - mean(mean_abs_error))/sd(mean_abs_error)) 

mean_abs_error_precision %>%  pull(mean_abs_error) %>% hist()
mean_abs_error_precision %>%filter(z_score>2)

# Mean estimate analysis

# Cardinality ratio = 1 / Precision ratio = 0.5, 2, 1
# Cardinality ratio = 0.5 (baseline = 4, 8, 16) / Precision ratio = 0.5, 2, 1

# Group_1 weight vs.Group_2 cardinality
# raw data (subject)
tb.errors_precision_dat$group_1_size = as.factor(tb.errors_precision_dat$group_1_size)
tb.errors_precision_dat$group_2_size = as.factor(tb.errors_precision_dat$group_2_size)
tb.errors_precision_dat$size_ratio = as.factor(tb.errors_precision_dat$size_ratio)
tb.errors_precision_dat$precision_ratio = as.factor(tb.errors_precision_dat$precision_ratio)

tb.errors_precision_dat  %>% filter(size_ratio==0.5, subject==3) %>% 
  ggplot(aes(x=precision_ratio, y = group_1_weight)) + 
  # geom_boxplot()+
  geom_point(shape=16,size=3)+
  geom_jitter(position=position_jitter(0.1),shape=16,size=2.5)+
  geom_point(aes(x=precision_ratio, y = true_group_1_weight), shape=17, size=3, color = "red") +
  facet_wrap(.~subject)

# Average
tb.errors_precision_dat %>% group_by(subject, group_1_size, size_ratio, precision_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(precision_ratio,size_ratio,group_1_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  ggplot(aes(x=precision_ratio,y=mean_group_1_weight,color=group_1_size,group=group_1_size)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=precision_ratio,y=mean_group_1_weight))+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.15,size=1.2)+
  geom_point(aes(x=precision_ratio, y = true_group_1_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=precision_ratio, y = true_group_1_weight,group=2),color="red")+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_wrap(.~size_ratio)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank())




