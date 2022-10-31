library(rjson)
library(tidyverse)
library(lsr)
library(sp)
setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_lab_data/Cardinality_data")

#data transformation
all.data_size = list()
subject = 1
for(file.name in list.files(pattern = '*.json')) {
  json_file = fromJSON(file = file.name)
  json_file[['subject']] = subject
  all.data_size[[subject]] = json_file
  subject = subject + 1
}

setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_lab/Analysis")
source("helper.R")

trial_response_size = tibble()
num_subj = length(all.data_size)
num_subj
for(i in 1:num_subj) {
  for(j in 1:660) {
    trial_response_size = bind_rows(trial_response_size,
                                    tibble(subject = all.data_size[[i]]$subject,
                                           sid = all.data_size[[i]]$client$sid,
                                           trial_number = all.data_size[[i]]$trials[[j]]$trialNumber,
                                           mean_index = all.data_size[[i]]$trials[[j]]$mean_index,
                                           group_1_size = all.data_size[[i]]$trials[[j]]$group_1_size,
                                           group_2_size = all.data_size[[i]]$trials[[j]]$group_2_size,
                                           inner_width = all.data_size[[i]]$trials[[j]]$inner_width,
                                           inner_height = all.data_size[[i]]$trials[[j]]$inner_height,
                                           width_height = min(inner_width,inner_height)*0.98,
                                           aspect_ratio = inner_width/inner_height,
                                           # normalized coordinate
                                           group_1_coord = list(coord_fcn(all.data_size[[i]]$trials[[j]]$group_1_coord,width_height)),
                                           group_2_coord = list(coord_fcn(all.data_size[[i]]$trials[[j]]$group_2_coord,width_height)),
                                           
                                           all_coordinates = list(coord_all_fcn(all.data_size[[i]]$trials[[j]]$all_coordinates,width_height)),
                                           response_coordinate = list(c(all.data_size[[i]]$trials[[j]]$response_coord$x/width_height,
                                                                        all.data_size[[i]]$trials[[j]]$response_coord$y/width_height)
                                           ),
                                           mean_group_1 = list(c(all.data_size[[i]]$trials[[j]]$mean_group_1$x/width_height,
                                                              all.data_size[[i]]$trials[[j]]$mean_group_1$y/width_height)
                                           ),
                                           mean_group_2 = list(c(all.data_size[[i]]$trials[[j]]$mean_group_2$x/width_height,
                                                                 all.data_size[[i]]$trials[[j]]$mean_group_2$y/width_height)
                                           ),
                                           mean_all = list(c(all.data_size[[i]]$trials[[j]]$mean_all$x/width_height,
                                                             all.data_size[[i]]$trials[[j]]$mean_all$y/width_height)
                                           )
                                    )
    )
  }
}

#Error analysis
tb.errors_size= trial_response_size %>% 
  rowwise() %>% 
  mutate(size_ratio = group_1_size/group_2_size,
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
         true_group_2_weight = group_2_size/(group_1_size + group_2_size),
         # group_1_weight_edge = compute_edge_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_all, proj.x_response)$weight_group_1,
         # group_2_weight_edge = 1-group_1_weight_edge
         
  )

# Data cleaning and sanity checks (exclusion)
# Exclusion criterion

# Attention check trials
tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",][which(tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",]$mean_index==(-1)),] %>% 
  pull(subject) %>% 
  unique()

#Check the out-of-boundary response trials
coord_out = tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",] %>% 
  pull(all_coordinates)
# Add index column for each dataframes in a list
coord_out = coord_out %>% Map(cbind, ., trial_num = seq_along(.),type = "stimuli") %>% do.call(rbind,.)

coord_response_out = tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",] %>% pull(response_coordinate) %>% do.call(rbind,.) %>% 
  as.data.frame() %>% mutate(trial_num = 1:nrow(.),type = "response")
colnames(coord_response_out)[1:2] = c("x", "y")
all_coord_out = rbind(coord_out, coord_response_out)
all_coord_out %>% ggplot(aes(x=x,y=y,color=type)) +geom_point() + facet_wrap(trial_num~.)

# Subject exclusion
# all.data_size[[19]]$client$sid

# within_boundary_x 
# Likely due to button misclick
tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",] %>% print(n=100)
tb.errors_size = tb.errors_size[tb.errors_size$wihtin_boundary_x=="T",] 

#Exclude practice and attention check trials
tb.errors_size_dat = tb.errors_size[-which(tb.errors_size$mean_index %in% (-(1:7))),]

# weird trials
tb.errors_size_dat[tb.errors_size_dat$group_1_weight>=1,]
tb.errors_size_dat = tb.errors_size_dat[(tb.errors_size_dat$group_1_weight<=1),]

#Mean absolute error for individual subject
mean_abs_error_size = tb.errors_size_dat %>% filter(size_ratio == 1) %>% group_by(subject) %>% 
  summarise(mean_abs_error = mean(abs_error_to_all)) %>% 
  mutate(z_score = (mean_abs_error - mean(mean_abs_error))/sd(mean_abs_error)) 

mean_abs_error_size %>%  pull(mean_abs_error) %>% hist()
mean_abs_error_size %>% filter(z_score>2)
tb.errors_size_dat %>% filter(subject==8)

# Group_1 weight vs.Group_2 cardinality
# raw data (subject)
tb.errors_size_dat$group_2_size = as.factor(tb.errors_size_dat$group_2_size)
tb.errors_size_dat %>% filter(subject==8) %>% 
  ggplot(aes(x=group_2_size, y = group_1_weight)) + 
  geom_boxplot()+
  # geom_point(shape=16,size=3) + 
  geom_jitter(position=position_jitter(0.1),shape=16,size=2.5)+
  geom_point(aes(x=group_2_size, y = true_group_1_weight), shape=17, size=3, color = "red")+
  facet_wrap(group_1_size~subject)

all.data_size[[8]]$client$sid

# average
# raw data (subject)
tb.errors_size_dat %>% filter(group_1_size == 4) %>% 
  group_by(subject,group_1_size,group_2_size) %>% 
  summarise(n = n(), mean_group_1_weight = mean(group_1_weight),median_group_1_weight = median(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  ggplot(aes(x=group_2_size, y = mean_group_1_weight)) + 
  geom_point(shape=16,size=3) + 
  # geom_point(aes(x=group_2_size, y = median_group_2_weight),shape=16,color="blue",size=3)+
  geom_point(aes(x=group_2_size, y = true_group_1_weight),shape=17,size=3,color="red")+
  facet_wrap(group_1_size~subject)

#Log weight ratio vs. log size_ratio
tb.errors_size_dat=tb.errors_size_dat %>% rowwise() %>% mutate(log_weight_ratio = log(group_1_weight/group_2_weight),
                                                               true_log_weight_ratio = log(size_ratio))
# log weight ratio
tb.errors_size_dat %>% filter(group_1_size == 4) %>% 
  group_by(subject,group_1_size,true_log_weight_ratio) %>% 
  summarise(n = n(), mean_log_weight_ratio = mean(log_weight_ratio), true_log_weight_ratio=mean(true_log_weight_ratio)) %>% 
  ggplot(aes(x=true_log_weight_ratio, y = mean_log_weight_ratio)) + 
  geom_point(shape=16,size=3) + 
  geom_point(aes(x=true_log_weight_ratio, y = true_log_weight_ratio),shape=17,size=3)+
  facet_wrap(group_1_size~subject)

# Mean performance
# group_1_weight
tb.errors_size_dat %>% group_by(subject, group_1_size, group_2_size, size_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_1_weight),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  group_by(group_1_size,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_1_weight = mean(group_1_weight),
            se_group_1_weight = sd(group_1_weight)/sqrt(n),
            true_group_1_weight=mean(true_group_1_weight)) %>% 
  ggplot(aes(x=group_2_size,y=mean_group_1_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=group_2_size,y=mean_group_1_weight,group = 2))+
  geom_errorbar(aes(ymin=mean_group_1_weight-se_group_1_weight,ymax=mean_group_1_weight+se_group_1_weight),width=0.15,size=1.2)+
  geom_point(aes(x=group_2_size, y = true_group_1_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=group_2_size, y = true_group_1_weight,group=2),color="red")+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_wrap(.~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')

#group_2_weight
tb.errors_size_dat %>% group_by(subject, group_1_size, group_2_size, size_ratio) %>% 
  summarise(n = n(), 
            group_2_weight = mean(group_2_weight),
            true_group_2_weight=mean(true_group_2_weight)) %>% 
  group_by(group_1_size,group_2_size) %>% 
  summarise(n=n(), 
            mean_group_2_weight = mean(group_2_weight),
            se_group_2_weight = sd(group_2_weight)/sqrt(n),
            true_group_2_weight=mean(true_group_2_weight)) %>% 
  ggplot(aes(x=group_2_size,y=mean_group_2_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=group_2_size,y=mean_group_2_weight,group = 2)) +
  geom_errorbar(aes(ymin=mean_group_2_weight-se_group_2_weight,ymax=mean_group_2_weight+se_group_2_weight),width=0.15,size=1.2)+
  geom_point(aes(x=group_2_size, y = true_group_2_weight),shape=17,size=3,color="red")+
  geom_line(aes(x=group_2_size, y = true_group_2_weight,group=2),color="red")+
  # geom_point(aes(x=group_2_size, y = mean_group_2_edge),shape=17,size=3,color="blue")+
  # geom_line(aes(x=group_2_size, y = mean_group_2_edge,group=2),color="blue")+
  facet_wrap(.~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')

# Distributions
tb.errors_size_dat %>% group_by(subject, group_1_size, group_2_size, size_ratio) %>% 
  summarise(n = n(), 
            group_1_weight = mean(group_2_weight),
            true_group_1_weight=mean(true_group_2_weight)) %>%
  ggplot(aes(x=group_1_weight))+geom_histogram() + facet_wrap(group_1_size~size_ratio)


# log weight ratio
tb.errors_size_dat %>% group_by(subject, group_1_size, group_2_size, size_ratio) %>% 
  summarise(n = n(), log_weight_ratio = mean(log_weight_ratio),true_log_weight_ratio=mean(true_log_weight_ratio)) %>% 
  group_by(group_1_size, size_ratio) %>% 
  summarise(n=n(), 
            mean_log_weight_ratio = mean(log_weight_ratio), 
            se_log_weight_ratio = sd(log_weight_ratio)/sqrt(n),
            true_log_weight_ratio = mean(true_log_weight_ratio)) %>% 
  ggplot(aes(x=true_log_weight_ratio,y=mean_log_weight_ratio)) + 
  geom_point(shape=16,size=3) + 
  geom_line(aes(x=true_log_weight_ratio,y=mean_log_weight_ratio,group = 2))+
  geom_errorbar(aes(ymin=mean_log_weight_ratio-se_log_weight_ratio,ymax=mean_log_weight_ratio+se_log_weight_ratio),width=0.15,size=1.2)+
  geom_point(aes(x=true_log_weight_ratio, y = true_log_weight_ratio),shape=17,size=3,color="red")+
  geom_line(aes(x=true_log_weight_ratio, y = true_log_weight_ratio,group=2),color="red")+
  facet_wrap(.~group_1_size)+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')


# Models
# Cardinality-Weighting model: Global mean

# Edge effect: Weighting of the two groups is not based on the group means but rather the boundary
tb.errors_size_dat = tb.errors_size_dat %>% mutate(
  x_proj_inner_lower = compute_inner_boundary(proj.x_group_1,proj.x_group_2,proj.x_group_1_mean,proj.x_group_2_mean)$lower_inner_bound,
  x_proj_inner_upper = compute_inner_boundary(proj.x_group_1,proj.x_group_2,proj.x_group_1_mean,proj.x_group_2_mean)$upper_inner_bound,
  
  inner_group = ((proj.x_group_2_mean < proj.x_group_1_mean) && (proj.x_all_mean < x_proj_inner_lower))||
  ((proj.x_group_2_mean > proj.x_group_1_mean) && (proj.x_all_mean > x_proj_inner_upper)),
  
  inner_group_response = ((proj.x_group_2_mean < proj.x_group_1_mean) && (proj.x_response < x_proj_inner_lower))||
    ((proj.x_group_2_mean > proj.x_group_1_mean) && (proj.x_response > x_proj_inner_upper)),
  
  consistent_side = inner_group == inner_group_response
)

edge_proportion = tb.errors_size_dat %>% filter(size_ratio !=1) %>% 
  group_by(group_1_size,size_ratio) %>% 
  summarise(total = n(),
            inconsistent=sum(consistent_side==F),
            outside = sum(inner_group == T & inner_group_response == F),
            proportion_inconsitent = inconsistent/total,
            proportion_outside = outside/inconsistent) %>% 
  mutate(category = paste0("(",group_1_size,",",size_ratio,")"))

edge_proportion %>%  ggplot(aes(x=as.factor(size_ratio), y=proportion_inconsitent, fill = proportion_outside)) +
  geom_col()+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
  scale_x_discrete(limits = rev)+
  facet_wrap(.~group_1_size)




