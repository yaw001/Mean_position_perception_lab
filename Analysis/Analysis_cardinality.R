library(rjson)
library(tidyverse)
library(lsr)
library(sp)
setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_lab_data")

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
         abs_error_to_ch = abs_err_dist(response_recenter,mean_ch),
         
         group_1_weight_raw = compute_weight(proj.x_group_1_mean, proj.x_group_2_mean, proj.x_response)$weight_group_1,
         group_1_weight = ifelse(group_1_weight_raw<=0, 0.001, group_1_weight),
         true_group_1_weight = group_1_size/(group_1_size + group_2_size),
         group_2_weight= 1-group_1_weight,
         true_group_2_weight = group_2_size/(group_1_size + group_2_size)
  )

# Data cleaning and sanity checks (exclusion)
# Exclusion criterion
# within_boundary_x 
tb.errors_size[tb.errors_size$wihtin_boundary_x=="F",] %>% print(n=100)
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
# all.data_size[[1]]$client$sid

# weird trials
tb.errors_size[tb.errors_size$group_1_weight>=1,]

#Exclude practice and attention check trials
tb.errors_size_dat = tb.errors_size[-which(tb.errors_size$mean_index %in% (-(1:7))),]

#Errors grouped by size_ratio (within-subjects)
tb.errors_size_dat$abs_error_to_all %>% hist()

# Group_1 weight vs.Group_2 cardinality
# raw data
tb.errors_size_dat$group_2_size = as.factor(tb.errors_size_dat$group_2_size)
tb.errors_size_dat %>% 
  ggplot(aes(x=group_2_size, y = group_2_weight)) + 
  geom_boxplot()+
  # geom_point(shape=16,size=3) + 
  geom_jitter(position=position_jitter(0.1),shape=16,size=2.5)+
  geom_point(aes(x=group_2_size, y = true_group_2_weight), shape=17, size=3, color = "red")+
  facet_wrap(group_1_size~subject)

# average
tb.errors_size_dat %>% 
  group_by(subject,group_1_size,group_2_size) %>% 
  summarise(n = n(), mean_group_2_weight = mean(group_2_weight),true_group_2_weight=mean(true_group_2_weight)) %>% 
  ggplot(aes(x=group_2_size, y = mean_group_2_weight)) + 
  geom_point(shape=16,size=3) + 
  geom_point(aes(x=group_2_size, y = true_group_2_weight),shape=17,size=3)+
  facet_wrap(group_1_size~subject)

#Log weight ratio vs. log size_ratio
tb.errors_size_dat=tb.errors_size_dat %>% rowwise() %>% mutate(log_weight_ratio = log(group_1_weight/group_2_weight),
                                                       true_log_weight_ratio = log(size_ratio))
tb.errors_size_dat[955,]$group_2_weight
#Individual best-fitted slope
tb.errors_size %>% 
  subset(as.numeric(subject)>10,as.numeric(subject)<=20) %>%
  ggplot(aes(x=true_log_weight_ratio,y =log_weight_ratio))+
  geom_point()+
  geom_point(aes(x=true_log_weight_ratio,y = true_log_weight_ratio),color="red",size=1.5)+
  geom_line(aes(x=true_log_weight_ratio,y = true_log_weight_ratio),color="red",size=1.1,linetype="dashed")+
  geom_line(aes(x=true_log_weight_ratio,y = 0),color="red",size=1.1,linetype="dashed")+
  labs(x="Log cardinality ratio (Group 1/Group 2)", y="Log group weight ratio (Group 1/Group 2)")+
  geom_smooth(method="lm", se = T)+
  facet_wrap(subject~.,ncol = 5)

# tb.errors_size = tb.errors_size %>% subset(group_1_size==12)
# tb.errors_size = tb.errors_size %>% subset(group_1_size>3)

slope_dist = tb.errors_size %>% 
  # filter(size_ratio_r>=1/8) %>% 
  mutate(log_cardinality_ratio = log(size_ratio_r),
         log_weight_ratio = log(Group_1_weight/Group_2_weight)) %>% group_by(subject) %>% 
  summarise(slope=summary(lm(log_weight_ratio~log_cardinality_ratio))$coefficients[2,1])

slope_dist %>% print(n=100)
mean(slope_dist$slope)

slope_dist %>% ggplot(aes(subject,slope))+geom_point()+
  scale_y_continuous(breaks=seq(-0.1,2,by=0.2))+
  theme_bw()+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16))

slope_dist %>% ggplot(aes(slope))+
  geom_histogram(binwidth = 0.2,fill="white",color="black",position="identity",boundary=-0.2)+
  scale_x_continuous(breaks=seq(-0.2,1.3,by=0.2))+
  scale_y_continuous(breaks=seq(0,20,by=2))+
  theme_bw()+
  labs(x="Best-fitted slopes", y="Number of subjects")+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')

# #Equal weighting (slope around 0)
# cardinality_3_equal = slope_dist$subject[slope_dist$slope<0]
# slope_dist[slope_dist$slope<0.05,]
# cardinality_3_equal
# #Exclude equal weighting
# tb.errors_size = tb.errors_size[!tb.errors_size$subject%in%cardinality_3_equal,]
tb.errors_size = tb.errors_size %>% 
  # filter(group_1_size==6) %>% 
  # filter(size_ratio_r>=1/8) %>% 
  mutate(log_cardinality_ratio = log(size_ratio_r),
         log_weight_ratio = log(Group_1_weight/Group_2_weight))

summary(lm(log_weight_ratio~log_cardinality_ratio, data = tb.errors_size))

lm_dat = tb.errors_size %>%  group_by(subject,size_ratio_r) %>% 
  summarise(n=n(),
            log_weight_ratio = mean(log(Group_1_weight/Group_2_weight))) %>% 
  mutate(log_cardinality_ratio = log(size_ratio_r))

summary(lm(log_weight_ratio~log_cardinality_ratio, data = lm_dat))$coefficients[2,1]

library(lme4)
tb.errors_size$subject = tb.errors_size$subject %>% as.factor()
tb.errors_size$log_cardinality_ratio = tb.errors_size$log_cardinality_ratio %>% as.numeric()
lmer(log_weight_ratio ~ log_cardinality_ratio + (0+log_cardinality_ratio|subject), data = tb.errors_size) %>% summary()
# best-fitted slope
# 500ms = 0.504 (remove equal weighting)
# unlimited = 0.53 (remove equal weighting) 0.51(all)
# fine-grained = 0.44

#Examine the cardinality weighting 

#group_1 weight only
group_1_weight_12 = tb.errors_size %>% 
  # filter(size_ratio_r == 1/24||
  #                                                            size_ratio_r == 1/12||
  #                                                            size_ratio_r == 1/8||
  #                                                            size_ratio_r == 1/4||
  #                                                            size_ratio_r == 1/2||
  #                                                            size_ratio_r == 1) %>%
  group_by(subject,size_ratio_r) %>%
  summarise(n=n(),
            weight_subject = mean(Group_1_weight_ori)) %>%
  group_by(size_ratio_r) %>%
  summarise(n=n(),
            mean_weight = mean(weight_subject),
            se_weight = sd(weight_subject)/sqrt(n)) %>%  
  mutate(group_1_size = 12) %>% 
  mutate(true_weight = c(1/3,1/2)) %>% 
  mutate(group_2_size = as.factor(c(24,12)))
  # mutate(equal_weight=rep(1/2,4))

# setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_data/Analysis")
# save(group_1_weight_12, file="group_1_weight_12_raw.Rdata")


group_1_weight_12 %>% ggplot(aes(x=group_2_size,y=mean_weight)) +
  geom_point()+
  geom_line(size=1.2,group=6)+
  geom_errorbar(aes(ymin=mean_weight-se_weight,ymax=mean_weight+se_weight),width=0.15,size=1.2)+
  geom_point(aes(x=group_2_size,y = true_weight),color="red",size=1.5)+
  geom_line(aes(x=group_2_size,y = true_weight,group=6),color="red", linetype = "dashed",size=1.1)+
  geom_point(aes(x=group_2_size,y = 0.5),color="red",size=1.5)+
  geom_line(aes(x=group_2_size,y = 0.5,group=6),color="red", linetype = "dashed",size=1.1)+
  theme_bw()+
  scale_y_continuous(limits = c(0,1))+
  labs(x="Cardinality ratio (Group 1/Group 2)", y="Mean group 1 weight")+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')

#Group weight ratio against cardinality ratio
#between-subject
group_weight_ratio_with_size_ratio = tb.errors_size %>% 
  filter(size_ratio_r == 1/8||
           size_ratio_r == 1/4||
           size_ratio_r == 1/2||
           size_ratio_r == 1) %>%
  group_by(subject,size_ratio_r) %>%
  summarise(n=n(),
            weight_ratio_subject = mean(log(Group_1_weight/Group_2_weight))) %>%
  group_by(size_ratio_r) %>%
  summarise(n=n(),
            mean_weight = mean(weight_ratio_subject),
            se_weight = sd(weight_ratio_subject)/sqrt(n)) %>%
  mutate(true_weight = log(size_ratio_r)) %>%
  mutate(equal_weight=rep(0,length(size_ratio_r)))

group_weight_ratio_with_size_ratio %>% ggplot(aes(x=log(as.numeric(size_ratio_r)),y=mean_weight)) + 
  geom_point()+
  geom_line(size=1.2)+
  geom_errorbar(aes(ymin=mean_weight-se_weight,ymax=mean_weight+se_weight),width=0.15,size=1.2)+
  geom_point(aes(x=log(as.numeric(size_ratio_r)),y = true_weight),color="red",size=1.5)+
  geom_line(aes(x=log(as.numeric(size_ratio_r)),y = true_weight),color="red",size=1.1, linetype ="dashed")+
  geom_point(aes(x=log(as.numeric(size_ratio_r)),y = equal_weight),color="red",size=1.5)+
  geom_line(aes(x=log(as.numeric(size_ratio_r)),y = equal_weight),color="red",size=1.1,linetype ="dashed")+
  # geom_point(aes(x=log(as.numeric(size_ratio_r)),y = density_weight),color="blue",size=1.5)+
  # geom_line(aes(x=log(as.numeric(size_ratio_r)),y = density_weight),color="blue",size=1.1,linetype ="dashed")+
  # geom_errorbar(aes(ymin=density_weight-se_density_weight,ymax=density_weight+se_density_weight),width=0.15,size=1.2,color="blue")+
  # geom_smooth(method="lm", se = T)+
  # geom_hline(yintercept = 0,size=1.2,color="red")+
  theme_bw()+
  # scale_y_continuous(limits = c(0,0.13))+
  labs(x="Log cardinality ratio (Group 1/Group 2)", y="Log group weight ratio (Group 1/Group 2)")+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')

#collapse across subject
group_weight_ratio_with_size_ratio_subject = tb.errors_size %>% group_by(size_ratio_r) %>%
  summarise(n=n(),
            mean_weight = mean(log(Group_1_weight/Group_2_weight)),
            se_weight = sd(log(Group_1_weight/Group_2_weight))/sqrt(n)) %>%
  mutate(true_weight = log(size_ratio_r)) %>%
  mutate(equal_weight=rep(0,length(size_ratio_r)))

group_weight_ratio_with_size_ratio_subject %>% ggplot(aes(x=log(as.numeric(size_ratio_r)),y=mean_weight)) + 
  geom_point()+
  geom_line(size=1.2)+
  geom_errorbar(aes(ymin=mean_weight-se_weight,ymax=mean_weight+se_weight),width=0.15,size=1.2)+
  geom_point(aes(x=log(as.numeric(size_ratio_r)),y = true_weight),color="red",size=1.5)+
  geom_line(aes(x=log(as.numeric(size_ratio_r)),y = true_weight),color="red",size=1.1, linetype ="dashed")+
  geom_point(aes(x=log(as.numeric(size_ratio_r)),y = equal_weight),color="red",size=1.5)+
  geom_line(aes(x=log(as.numeric(size_ratio_r)),y = equal_weight),color="red",size=1.1,linetype ="dashed")+
  # geom_point(aes(x=log(as.numeric(size_ratio_r)),y = density_weight),color="blue",size=1.5)+
  # geom_line(aes(x=log(as.numeric(size_ratio_r)),y = density_weight),color="blue",size=1.1,linetype ="dashed")+
  # geom_errorbar(aes(ymin=density_weight-se_density_weight,ymax=density_weight+se_density_weight),width=0.15,size=1.2,color="blue")+
  # geom_smooth(method="lm", se = T)+
  # geom_hline(yintercept = 0,size=1.2,color="red")+
  theme_bw()+
  # scale_y_continuous(limits = c(0,0.13))+
  labs(x="Log cardinality ratio (Group 1/Group 2)", y="Log group weight ratio (Group 1/Group 2)")+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')

#For combining
group_weight_ratio_with_size_ratio_3_with_equal = tb.errors_size %>% subset(group_1_size == 3) %>%  group_by(subject,size_ratio_r) %>%
  summarise(n=n(),
            weight_ratio_subject = mean(log(Group_1_weight/Group_2_weight))) %>%
  group_by(size_ratio_r) %>%
  summarise(n=n(),
            mean_weight = mean(weight_ratio_subject),
            se_weight = sd(weight_ratio_subject)/sqrt(n)) %>%
  mutate(true_weight = log(size_ratio_r)) %>%
  mutate(group_1_size=3)

group_weight_ratio_with_size_ratio_6_with_equal = tb.errors_size %>% subset(group_1_size == 6) %>%  group_by(subject,size_ratio_r) %>%
  summarise(n=n(),
            weight_ratio_subject = mean(log(Group_1_weight/Group_2_weight))) %>%
  group_by(size_ratio_r) %>%
  summarise(n=n(),
            mean_weight = mean(weight_ratio_subject),
            se_weight = sd(weight_ratio_subject)/sqrt(n)) %>%
  mutate(true_weight = log(size_ratio_r)) %>%
  mutate(group_1_size=6)

group_weight_ratio_with_size_ratio_12_with_equal = tb.errors_size %>% subset(group_1_size == 12) %>%  group_by(subject,size_ratio_r) %>%
  summarise(n=n(),
            weight_ratio_subject = mean(log(Group_1_weight/Group_2_weight))) %>%
  group_by(size_ratio_r) %>%
  summarise(n=n(),
            mean_weight = mean(weight_ratio_subject),
            se_weight = sd(weight_ratio_subject)/sqrt(n)) %>%
  mutate(true_weight = log(size_ratio_r)) %>%
  mutate(group_1_size=12)

# setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_data/Analysis")
# save(group_weight_ratio_with_size_ratio_3_with_equal, file="group_weight_ratio_with_size_ratio_3_with_equal.Rdata")
# 
# setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_data/Analysis")
# save(group_weight_ratio_with_size_ratio_6_with_equal, file="group_weight_ratio_with_size_ratio_6_with_equal.Rdata")
# 
# setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_data/Analysis")
# save(group_weight_ratio_with_size_ratio_12_with_equal, file="group_weight_ratio_with_size_ratio_12_with_equal.Rdata")

# setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_data/Analysis")
# save(group_weight_ratio_with_size_ratio_3_unlimited, file="group_weight_ratio_with_size_ratio_3_unlimited.Rdata")

# setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_data/Analysis")
# save(group_weight_ratio_with_size_ratio_3_fined, file="group_weight_ratio_with_size_ratio_3_fined.Rdata")

#mean group weight
# Group_3_weight = c(tb.errors_size %>% subset(group_1_size == 3) %>% pull(Group_1_weight),
#                    tb.errors_size %>% subset(group_2_size == 3) %>% pull(Group_2_weight))
# mean(Group_3_weight)
# 
# Group_6_weight = c(tb.errors_size %>% subset(group_2_size == 6) %>% pull(Group_2_weight))
# Group_6_weight %>% hist()
# mean(Group_6_weight)
# 
# Group_12_weight = c(tb.errors_size %>% subset(group_2_size == 12) %>% pull(Group_2_weight))
# Group_12_weight %>% hist()
# mean(Group_12_weight)
# 
# Group_24_weight = c(tb.errors_size %>% subset(group_2_size == 24) %>% pull(Group_2_weight))
# Group_24_weight %>% hist()
# mean(Group_24_weight)
# 
# mean_Group_weight_all_3 = data.frame(x= c(3,6,12,24),
#                                        y = c(mean(Group_3_weight),
#                                              mean(Group_6_weight),
#                                              mean(Group_12_weight),
#                                              mean(Group_24_weight)))

# setwd("/Users/young/Desktop/UCSD/Research/Mean_position_perception_data/Analysis")
# save(mean_Group_weight_all_3, file="mean_Group_weight_all_3.Rdata")


#All conditions
# between subject
group_weight_ratio_with_size_ratio_s = tb.errors_size %>% group_by(subject,size_ratio_s) %>%
  summarise(n=n(),
            weight_ratio_subject = mean(log(Group_1_weight/Group_2_weight))) %>%
  group_by(size_ratio_s) %>%
  summarise(n=n(),
            mean_weight = mean(weight_ratio_subject),
            se_weight = sd(weight_ratio_subject)/sqrt(n)) %>% 
  mutate(log_size_ratio = log(c(1,1/2,1,1/4,1/8,1,1/2,1/2,1/4,1)),
         true_weight = log(c(1,1/2,1,1/4,1/8,1,1/2,1/2,1/4,1)),
         group_1_size = as.factor(c(12,12,24,3,3,3,3,6,6,6)),
         equal_weight=rep(0,10))

#Collapse across subject
group_weight_ratio_with_size_ratio_s = tb.errors_size %>% group_by(size_ratio_s) %>%
  summarise(n=n(),
            mean_weight = mean(log(Group_1_weight/Group_2_weight)),
            se_weight = sd(log(Group_1_weight/Group_2_weight))/sqrt(n)) %>%
  mutate(log_size_ratio = log(c(1,1/2,1,1/4,1/8,1,1/2,1/2,1/4,1)),
         true_weight = log(c(1,1/2,1,1/4,1/8,1,1/2,1/2,1/4,1)),
         group_1_size = as.factor(c(12,12,24,3,3,3,3,6,6,6)),
         equal_weight=rep(0,10))

group_weight_ratio_with_size_ratio_s$size_ratio_s = factor(group_weight_ratio_with_size_ratio_s$size_ratio_s,
                                                        levels = c("3/3",
                                                                   "6/6",
                                                                   "12/12",
                                                                   "24/24",
                                                                   "3/6",
                                                                   "6/12",
                                                                   "12/24",
                                                                   "3/12",
                                                                   "6/24",
                                                                   "3/24"))


group_weight_ratio_with_size_ratio_s %>% ggplot(aes(x=log_size_ratio,y=mean_weight, color = group_1_size)) + 
  geom_point()+
  geom_line(size=1.2)+
  geom_errorbar(aes(ymin=mean_weight-se_weight,ymax=mean_weight+se_weight),width=0.15,size=1.2)+
  geom_point(aes(x=log_size_ratio,y = true_weight),color="darkgrey",size=1.5)+
  geom_line(aes(x=log_size_ratio,y = true_weight),color="darkgrey",size=1.1,linetype="dashed")+
  geom_point(aes(x=log_size_ratio,y = equal_weight),color="darkgrey",size=1.5)+
  geom_line(aes(x=log_size_ratio,y = equal_weight),color="darkgrey",size=1.1,linetype="dashed")+
  scale_color_manual(values=c("red","green","blue","yellow"))+
  # geom_point(aes(x=log(as.numeric(size_ratio_r)),y = sqrt_weight),color="red",size=1.5)+
  # geom_line(aes(x=log(as.numeric(size_ratio_r)),y = sqrt_weight),color="red",size=1.1)+
  # geom_smooth(method="lm", se = T)+
  # geom_hline(yintercept = 0,size=1.2,color="red")+
  theme_bw()+
  # scale_y_continuous(limits = c(0,0.13))+
  labs(x="Log cardinality ratio (Group 1/Group 2)", y="Log group weight ratio (Group 1/Group 2)")+
  theme(axis.text = element_text(size= 16),
        text = element_text(size= 16),
        panel.grid = element_blank(),
        legend.position = 'none')

# #nearest neighbor
# library(spatstat)
# mean(nndist(tb.errors_size$group_1_coord[[1]]))
# mean(nndist(tb.errors_size$group_1_coord[[3]]))
# mean_nnd <- function(dat){
#   return(mean(nndist(dat)))
# }
# tb.errors_size=tb.errors_size %>% rowwise() %>% mutate(mean_nnd_1 = mean_nnd(group_1_coord),
#                                         mean_nnd_2 = mean_nnd(group_2_coord),
#                                         log_nnd_ratio = log(mean_nnd_1/mean_nnd_2),
#                                         log_density_ratio = -log(mean_nnd_1/mean_nnd_2),
#                                         log_weight_ratio = log(Group_1_weight/Group_2_weight),
#                                         true_log_weight_ratio = log(size_ratio_r))
# 
# # tb.errors_size %>% filter(size_ratio_r==0.125) %>% pull(log_nnd_ratio) %>% hist()
# # tb.errors_size %>% filter(size_ratio_r==0.25) %>% pull(log_nnd_ratio) %>% hist()
# # tb.errors_size %>% filter(size_ratio_r==0.5) %>% pull(log_nnd_ratio) %>% hist()
# # tb.errors_size %>% filter(size_ratio_r==1) %>% pull(log_nnd_ratio) %>% hist()
# 
# tb.errors_size$log_weight_ratio
# tb.errors_size$true_log_weight_ratio
# tb.errors_size$log_nnd_ratio
# 
# nnd_weight_ratio = data.frame(log_nnd_ratio = tb.errors_size$log_nnd_ratio,
#                               log_density_ratio =tb.errors_size$log_density_ratio,
#                               log_weight_ratio = tb.errors_size$log_weight_ratio,
#                               true_log_weight_ratio = tb.errors_size$true_log_weight_ratio,
#                               size_ratio_s = tb.errors_size$size_ratio_s,
#                               size_ratio_r = tb.errors_size$size_ratio_r)
# 
# mean_log_nnd_ratio_s = nnd_weight_ratio %>% group_by(size_ratio_s) %>% summarise(mean_log_nnd_ratio = mean(log_nnd_ratio))
# 
# mean_log_nnd_ratio_r = nnd_weight_ratio %>% group_by(size_ratio_r) %>% summarise(mean_log_nnd_ratio = mean(log_nnd_ratio))
# 
# mean_log_density_ratio_r = nnd_weight_ratio %>% group_by(size_ratio_r) %>% summarise(n=n(),
#                                                                                      mean_log_density_ratio = mean(log_density_ratio),
#                                                                                      se_log_density_ratio = sd(log_density_ratio)/sqrt(n))
# 
# mean_log_nnd_ratio_r =  mean_log_nnd_ratio_r %>% mutate(log_size_ratio_r = log(size_ratio_r))
# 
# mean_log_nnd_ratio_r %>% ggplot(aes(x=log_size_ratio_r,y=mean_log_nnd_ratio)) + geom_point()
# 
# mean_log_density_ratio_r =  mean_log_density_ratio_r %>% mutate(log_size_ratio_r = log(size_ratio_r))
# 
# mean_log_density_ratio_r %>% ggplot(aes(x=log_size_ratio_r,y=mean_log_density_ratio)) + geom_point()
# 
# # nnd_weight_ratio %>% ggplot(aes(x=log_nnd_ratio, y=log_weight_ratio))+geom_point() +
# #   geom_point(aes(x=log_nnd_ratio, y=true_log_weight_ratio),color="red")+
# #   geom_smooth(method = "lm")+
# #   facet_wrap(.~size_ratio_s)
# # 
# # nnd_weight_ratio %>% ggplot(aes(x=true_log_weight_ratio, y=log_nnd_ratio))+
# #   geom_point()+ 
# #   facet_wrap(.~size_ratio_s)
# 
# # group_weight_ratio_nnd = cbind(group_weight_ratio_with_size_ratio_s,mean_log_nnd_ratio=mean_log_nnd_ratio$mean_log_nnd_ratio)
# # 
# # group_weight_ratio_nnd %>% ggplot(aes(x=mean_log_nnd_ratio,y=mean_weight, color = group_1_size)) + 
# #   geom_point()+
# #   geom_line(size=1.2)+
# #   geom_errorbar(aes(ymin=mean_weight-se_weight,ymax=mean_weight+se_weight),width=0.15,size=1.2)+
# #   geom_point(aes(x=mean_log_nnd_ratio,y = true_weight),color="red",size=1.5)+
# #   geom_line(aes(x=mean_log_nnd_ratio,y = true_weight),color="red",size=1.1,linetype="dashed")+
# #   geom_point(aes(x=mean_log_nnd_ratio,y = equal_weight),color="red",size=1.5)+
# #   geom_line(aes(x=mean_log_nnd_ratio,y = equal_weight),color="red",size=1.1,linetype="dashed")+
# #   # geom_point(aes(x=log(as.numeric(size_ratio_r)),y = sqrt_weight),color="red",size=1.5)+
# #   # geom_line(aes(x=log(as.numeric(size_ratio_r)),y = sqrt_weight),color="red",size=1.1)+
# #   # geom_smooth(method="lm", se = T)+
# #   # geom_hline(yintercept = 0,size=1.2,color="red")+
# #   theme_bw()+
# #   # scale_y_continuous(limits = c(0,0.13))+
# #   labs(x="Log cardinality ratio (Group 1/Group 2)", y="Log group weight ratio (Group 1/Group 2)")+
# #   theme(axis.text = element_text(size= 16),
# #         text = element_text(size= 16),
# #         panel.grid = element_blank(),
# #         legend.position = 'none')
# 
# #Individual subject fit
# # tb.errors_size %>% filter(group_1_size == 3,subject<19) %>% ggplot(aes(x=true_log_weight_ratio,y =log_weight_ratio))+
# #   geom_point()+
# #   geom_point(aes(x=true_log_weight_ratio,y = true_log_weight_ratio),color="red",size=1.5)+
# #   geom_line(aes(x=true_log_weight_ratio,y = true_log_weight_ratio),color="red",size=1.1,linetype="dashed")+
# #   geom_smooth(method="lm", se = T)+
# #   facet_wrap(.~subject)
# #     
# # 
# # #Collapse across all subject
# # tb.errors_size %>% subset(log_weight_ratio> (-4)&group_1_size==3) %>% ggplot(aes(x=true_log_weight_ratio,y =log_weight_ratio))+
# #   geom_point()+
# #   geom_point(aes(x=true_log_weight_ratio,y = true_log_weight_ratio),color="red",size=1.5)+
# #   geom_line(aes(x=true_log_weight_ratio,y = true_log_weight_ratio),color="red",size=1.1,linetype="dashed")+
# #   geom_smooth(method="lm", se = T)
# # 
# # tb.errors_size %>% subset(log_weight_ratio> (-4)&group_1_size==3)%>% ggplot(aes(x=log_weight_ratio))+
# #   geom_histogram()+
# #   facet_wrap(.~as.factor(size_ratio_r))
# # 
# 
# 
