########################### MODEL PURRR HELPER FUNCTIONS ################
quantile_error <- function(pred,obs,quant){
  preds <- data.frame(pred = pred, obs = obs) %>%
    filter(quantile(seq(0,max(obs)), quant)>obs)
  return(preds)
}
r_squared <- function(pred,obs){
  rsq <- summary(lm(obs ~ pred))$r.squared 
  if(is.na(rsq) | is.nan(rsq) | is.infinite(rsq)){
    rsq <- 0
  } 
  return(rsq)
}
mae <- function(pred,obs){
  mean(abs(pred-obs))
}
normalized<-function(y) {
  x<-y[!is.na(y)]
  x<-(x - min(x)) / (max(x) - min(x))
  y[!is.na(y)]<-x
  return(y)
}
glm_fit <- function(dat, formula, family){
  glm_model <- glm(formula, data = dat, family = family)
  return(glm_model)
}
sar_pred <- function(model, newdata = NA, type = "response"){
  pred <- as.data.frame(predict(model, type = "response")^2)$fit
  ifelse(pred > 100, 100, pred) ########$$$$$$$$$%%%%%%% REMOVE PRED OUTLIERS !!!!!!!!!
}
lm_predict <- function(model, newdata, type = "response", 
                       sqrt = FALSE, y_var = NA, 
                       offset_var = NA, offset_amnt = NA){
  if(is(model,"ranger")){
    pred <- predict(model, data = newdata, type = type)
    pred <- pred$predictions
  } else if(is(model, "cv.glmnet")){
    newx <- newdata[, setdiff(colnames(newdata),y_var)]
    newx <- data.matrix(newx)
    opt_lambda <- model$lambda.min
    glmnet_fit <- model$glmnet.fit
    if(is.na(offset_var)){
      pred <- predict(glmnet_fit, s = opt_lambda, newx = newx, type = type)
      pred <- as.numeric(pred)
    } else {
      offset <- newdata[,offset_var]
      pred <- predict(glmnet_fit, s = opt_lambda, newx = newx, type = type,
                      newoffset = log(offset+offset_amnt))
      pred <- as.numeric(pred)
    }
  } else {
    pred <- predict(model, newdata = newdata, type = type)
  }
  if(isTRUE(sqrt)) pred <- pred^2
  return(pred)
}
rmse <- function(pred,obs){
  rmse <- sqrt(mean((pred-obs)^2))
  return(rmse)
}
maape <- function(pred,obs){
  # https://www.sciencedirect.com/science/article/pii/S0169207016000121
  mape <- mean(atan(abs((obs-pred)/obs)),na.rm=TRUE)
  return(mape)
}
rf_fit <- function(dat, formula, mtry_add = 0, importance = "none"){
  mtry <- floor(sqrt(ncol(dat)-1))+mtry_add
  rf_model <- ranger(formula, data = dat, 
                     mtry = mtry,
                     splitrule = "variance",
                     importance = importance,
                     num.trees = 500,
                     min.node.size = 10)
  return(rf_model)
}
mape <- function(pred, obs){
  x <- mean(abs((obs-pred)/obs) * 100, na.rm=TRUE)
  return(x)
}
log_dev <- function(pred, obs){
  log_dev <- -log(dpois(obs, lambda = pred))
  return(log_dev)
}
logdev_p <- function(pred, obs){
  x <- -log(dpois(obs, lambda = pred))
  x <- round(exp(-x),3)
  x <- mean(x, na.rm = TRUE)
  return(x)
}
score_model <- function(dat){
  dat <- dat %>%
    mutate(R2     = map2_dbl(pred, test_y, r_squared),
           MAE    = map2_dbl(pred, test_y, mae),
           MAAPE  = map2_dbl(pred, test_y, maape),
           RMSE   = map2_dbl(pred, test_y, rmse),
           logdev = map2_dbl(pred, test_y, logdev_p))
  return(dat)
}
########################### END PURRR MODEL HELPER FUNCS #######################

########################### MODEL PLOTTING FUNCTIONS ###########################
plot_fold_pred <- function(preds, obs, type = "fit", title){
  if(is(preds,"list")){
    fold_reps <- map_int(preds, length)
    k = length(preds)
    pred  = as.numeric(unlist(preds))
    obs   = as.numeric(unlist(obs))
  } else {
    fold_reps <- 1
    k <- 1
    pred  = preds
    obs   = obs
  }
  preds <- data.frame(pred  = pred,
                      obs   = obs,
                      fold  = rep(1:k, times = fold_reps),
                      id    = seq(1:length(pred[[1]])))
  preds$resid <- preds$pred - preds$obs
  preds$std_resid <- scale(preds$resid, center = TRUE)
  
  if(type == "fit"){
    ggplot(preds, aes(x = obs, y = pred)) +
      geom_abline(intercept = 0, slope = 1) +
      geom_smooth(method = "lm", se = FALSE) +
      # geom_point(aes(color = as.factor(fold))) +
      geom_point(color = "gray10") +
      scale_y_continuous(limits = c(min(preds$pred),max(preds$obs))) +
      coord_equal() +
      labs(title = title) +
      plotTheme()+
      theme(
        legend.position = "none"
      )
  } else if(type == "residuals"){
    ggplot(preds, aes(x = pred, y = std_resid)) +
      geom_hline(yintercept = 0) +
      # geom_point(aes(color = as.factor(fold))) +
      geom_point(color = "gray10") +
      scale_y_continuous(limits = c(-5,5)) +
      labs(title = title) +
      plotTheme() +
      theme(
        legend.position = "none"
      )
  }
}
# 
# pred_dat <- data.frame(pred = protective_rf_pred_dat$pred,
#                        test_y  = protective_rf_pred_dat$test_y,
#                        net_id = protective_rf_pred_dat$test_net_id)
# model_name = "TEMP"
# MAE_geoplot <- net_Richmond %>%
#   left_join(., pred_dat, by = "net_id") %>%
#   mutate(feature_name = paste0(model_name," ", "MAE")) 
# MAE_geoplot <- score_model(MAE_geoplot) %>%
#   make_cuts(., "logdev")

model_pred_geoplot <- function(pred, test_y, test_net_id, study_poly, 
                               base_map, model_name){
  pred_dat <- data.frame(pred = pred,
                         obs  = test_y,
                         net_id = test_net_id)
  MAE_geoplot <- study_poly %>%
    left_join(., pred_dat, by = "net_id") %>% 
    mutate(MAE = round(abs(pred - obs),2),
           cuts = case_when(MAE == 0 ~ "0", 
                            MAE > 0 & MAE < 0.5 ~ "0.1 to 0.49",
                            MAE >= 0.5 & MAE < 1 ~ "0.5 to 0.99",
                            MAE >= 1 & MAE < 3 ~ "1 to 2.99",
                            MAE >= 3 ~ "3+"))
  
  MAE_plot <- make_fishnet_dist_plot(MAE_geoplot, base_map,
                                     var_name = "MAE")
  
  pred_geoplot <- study_poly %>%
    left_join(., pred_dat, by = "net_id") %>% 
    mutate(prediction = round(pred,2),
           cuts = case_when(prediction == 0 ~ "0", 
                            prediction > 0 & prediction < 0.5 ~ "0.1 to 0.49",
                            prediction >= 0.5 & prediction < 1 ~ "0.5 to 0.99",
                            prediction >= 1 & prediction < 3 ~ "1 to 2.99",
                            prediction >= 3 ~ "3+"))

    pred_plot <- make_fishnet_dist_plot(pred_geoplot, base_map, 
                                        var_name = "Count")

  return(list(MAE_geoplot = MAE_plot, pred_geoplot = pred_plot))
}
###################################################################
g <- function(x) dplyr::glimpse(x)
plot_pred <- function(model, obs, type = "response"){
  if(class(model)[1] %in% c("sarlm")){
    pred <- as.data.frame(predict(model, type = type))$signal
  } else {
    pred <- predict(model, type = type)
  }
  preds <- data.frame(pred = pred,
                      obs  = obs)
  ggplot(preds, aes(x = obs, y = pred)) +
    geom_point() +
    coord_equal() +
    theme_bw()
}
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
feature_corrplot <- function(data, title){
  cps_cor <- cor(data)
  #p.mat <- cor.mtest(data)$p # MDH removed p-value
  # CANGE COLOR RAMP HERE!
  #col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  #col_ramp = col(200)
  col_ramp <- viridisLite::viridis(200)
  p <- corrplot(cps_cor, method = "color", col = col_ramp,
                type = "upper", number.cex = .7,
                addCoef.col = "black", tl.col = "black", tl.srt = 90, 
                sig.level = 0.01, insig = "blank", diag = FALSE, title = title, mar=c(0,0,1,0))
  return(p)
}
NN_point_features <- function(var_list, fishnet, k){
  NN_results <- foreach(i = seq_along(var_list),
                        .export=c('nn_function'),
                        .packages=c('raster', 'sf', 'dplyr', "FNN", "tibble", "tidyr")) %dopar% { 
                          feature <- names(var_list)[i]
                          fishnet_centroid_XY <- st_coordinates(st_centroid(fishnet))
                          dat <- var_list[[i]] 
                          if(nrow(dat) >= k){
                            net_NN <- nn_function(fishnet_centroid_XY,
                                                  st_coordinates(dat)[,1:2], k) %>%
                              mutate(feature_name = paste0("NN_",feature),
                                     net_id = fishnet$net_id) %>%
                              left_join(., fishnet, by = "net_id") %>%
                              rename("value" = value.x) %>%
                              dplyr::select(-value.y) %>%
                              st_as_sf()
                          } else {
                            net_NN <- data.frame(value = rep(NA, nrow(fishnet))) %>%
                              mutate(feature_name =  paste0("NN_",feature),
                                     net_id = fishnet$net_id) %>%
                              left_join(., fishnet, by = "net_id") %>%
                              rename("value" = value.x) %>%
                              dplyr::select(-value.y) %>%
                              st_as_sf()                       
                          }
                        }
  names(NN_results) <- paste0("NN_",names(var_list))
  return(NN_results)
}
Euclidean_point_features <- function(var_list, dist_raster, raster_mask, fishnet){
  ED_results <- foreach::foreach(i = seq_along(var_list), 
                                 .combine='comb', .multicombine=TRUE,
                                 .init=list(list(), list()),
                                 .export=c('distanceFromPoints', 'raster_to_fishnet'),
                                 .packages=c('raster', 'sf', 'dplyr')) %dopar% { 
                                   feature <- names(var_list)[i]
                                   bs_dist <- distanceFromPoints(dist_raster, sf::st_coordinates(var_list[[feature]]))
                                   bs_clip <- raster::mask(bs_dist, mask = as(raster_mask, "Spatial"))
                                   fea_mean_dist <- raster_to_fishnet(bs_clip,fishnet,paste0("ed_",feature))
                                   list(fea_mean_dist, bs_clip)
                                 }
  dist_results <- ED_results[[1]]
  dist_rasters <- ED_results[[2]]
  names(dist_results) <- paste0("ED_",names(var_list))
  names(dist_rasters) <- paste0("ED_",names(var_list))
  return(list(dist_results, dist_raster))
}
Aggregate_points_Features <- function(var_list, fishnet){
  agg_results <- foreach(i = seq_along(var_list),
                         .packages=c('raster', 'sf', 'dplyr')) %dopar% { 
                           feature <- names(var_list)[i]
                           dat <- var_list[[i]] %>%
                             mutate(value = 1) %>%
                             dplyr::select(value)
                           net_agg <- aggregate(dat, fishnet, sum) %>%
                             mutate(feature_name = paste0("agg_",feature),
                                    net_id = fishnet$net_id)
                         }
  names(agg_results) <- paste0("agg_",names(var_list))
  return(agg_results)
}

get_individual_features <- function(data, field, prefix, count_threshold){
  feat_list <- list()
  feat_subset <- data %>%
    group_by(!!as.name(field)) %>%
    mutate(cnt = n()) %>%
    filter(cnt >= count_threshold) %>%
    ungroup()
  Fea_codes <- unique(feat_subset[[field]])
  for(i in seq_along(Fea_codes)){
    feat_codes_i <- feat_subset %>%
      filter(!!as.name(field) == Fea_codes[i])
    feat_list[[paste0(prefix,"_",Fea_codes[i])]] <- feat_codes_i
  }
  return(feat_list)
}
nn_function <- function(measureFrom,measureTo,k) {
  
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    dplyr::summarize(value = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr ::select(-thisPoint)
  
  return(output)  
}
make_na_binary <- function(x){
  x <- ifelse(is.na(x),0,1)
}
divide_by <- function(dividend,divisor){
  result <- integer(length(dividend))
  for(i in seq_along(dividend)){
    if(divisor[i] == 0){
      result_i <- 0
    } else if(divisor[i] > 0){
      result_i <- (dividend[i]/divisor[i])
    }
    result[i] <- result_i
  }
  return(result)
}
comb <- function(x, ...) {
# https://stackoverflow.com/questions/19791609/saving-multiple-outputs-of-foreach-dopar-loop/19801108
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

raster_to_fishnet <- function(rast_dat,fishnet,feature_name){
  fishnet <- fishnet %>%
    mutate(net_id = as.numeric(net_id))
  buff_cnt_extract <- raster::extract(x = rast_dat, y = st_centroid(fishnet),
                                      buffer = sqrt(as.numeric(st_area(fishnet[1,])))/2)
  names(buff_cnt_extract) <- fishnet$net_id
  buff_cnt_extract_unlist <- data.table::rbindlist(lapply(buff_cnt_extract,data.frame),idcol = TRUE) %>%
    rename(net_id = `.id`,
           value = `X..i..`)
  buff_pnt_dist <- buff_cnt_extract_unlist %>%
    group_by(net_id) %>%
    summarise(feature_name = feature_name,
              cell_count = n(),
              mean_dist = mean(value, na.rm=T), 
              max_dist  = suppressWarnings(max(value, na.rm = T)),
              min_dist  = suppressWarnings(min(value, na.rm = T)),
              rng_dist  = length(unique(value))) %>%
    ungroup() %>%
    mutate(net_id = as.numeric(net_id)) %>%
    left_join(.,fishnet, by = "net_id") %>%
    st_as_sf()
}
# make_cuts <- function(dat,field_name = "mean_dist", p = seq(0,1,0.1)){
#   dat_vec <- pull(dat, !!as.name(field_name))
#   dat <- dat %>%
#     mutate(cut_val = as.character(Hmisc::cut2(dat_vec,
#                                               cuts = unique(as.numeric(quantile(dat_vec, na.rm=T,
#                                                                                 p = p))))))
# }

make_cuts <- function(dat,field_name = "mean_dist", p = seq(0,1,0.1), 
                      cuts = c("quantiles", "breaks"), n_breaks = 5){
  cuts <- match.arg(cuts)
  dat_vec <- pull(dat, !!as.name(field_name))
  if(cuts == "quantiles"){
    dat <- dat %>%
      mutate(cut_val = as.character(Hmisc::cut2(dat_vec,
                                                cuts = unique(as.numeric(quantile(dat_vec, na.rm=T,
                                                                                  p = p))),
                                                digits = 3)))
  } else if(cuts == "breaks"){
    dat <- dat %>%
      mutate(cut_val = Hmisc::cut2(dat_vec, g = n_breaks, digits = 3))
  }
}

make_fishnet_dist_plot <- function(dist_dat, base_map, alpha = 0.8, 
                                   direction = 1, var_name){
  #layer_name <- unique(dist_dat$feature_name)
  p <- ggmap(base_map) +
    geom_sf(data = ll(dist_dat), aes(fill = cuts), 
            color = NA, alpha = alpha, inherit.aes = F) +
    scale_fill_viridis_d(option = "D", 
                         direction = direction, 
                         name = var_name,
                         na.translate = F) +
    mapTheme()
  return(p)
}
st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}
find_dupes <- function(dat){
  unq <- nrow(unique(st_drop_geometry(dat)))
  tot <- nrow(dat)
  if(unq == tot){
    return("no dupes here")
  } else {
    return("DUPLICATED ROWS!")
  }
}
ll <- function(dat, proj4 = 4326){
  st_transform(dat, proj4)
}
make_stamen <- function(dat_lst, save_path = NULL,
                        color = "red", buffer = 5000, w = 8, h = 8, save_plot = FALSE){ 
  layer_name <- names(dat_lst)
  map_data   <- dat_lst[[1]]
  base_map   <- get_map(location = unname(st_bbox(ll(st_buffer(map_data,buffer)))),
                        source = "stamen",
                        maptype = "toner")
  gg <- ggmap(base_map) +
    geom_sf(data = ll(map_data), inherit.aes = FALSE, color = I(color))
  if(isTRUE(save_plot)){
    ggsave(file.path(save_path, "stamen_maps",paste0(layer_name,".png")),
           plot = gg, width = w, height = h)
  } else {
    return(gg)
  }
}
q_labels <- function(values, round = 1, width = NULL){
  if(is.null(width)){
    max_l <- nchar(floor(max(values)))
    width = max_l + round + 1 # 1 added for decimal char
  }
  qq <- quantile(values, seq(0,0.9,0.1),na.rm=T)
  qq <- as.character(round(qq,round))
  qq <- str_pad(qq,width = width, side = "right", pad = 0)
}  
gplot_data <- function(x, maxpixels = 50000)  {
  # mimics function of spatstat::gplot() so that geom_tile can be used instead
  # https://stackoverflow.com/questions/48955504/how-to-overlay-a-transparent-raster-on-ggmap
  # https://github.com/statnmap/SDMSelect/
  # Sébastien Rochette
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}
get_window <- function(dat, buff_dist = 5000){
  bb_coords <- unname(st_bbox(ll(st_buffer(dat,buff_dist))))
  w1 <- c(bb_coords[c(1,3)])
  w2 <- c(bb_coords[c(2,4)])
  window <- owin(w1,w2)
}
mapTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    panel.border = element_blank()
  )
}
plotTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0), 
    axis.title.x = element_text(size = 10, family = "sans", face = "plain", hjust = 1, vjust = -0.5),
    axis.title.y = element_text(size = 10, family = "sans", face = "plain", hjust = 1, vjust = 1),
    axis.text = element_text(size = 9, family = "sans", face = "plain"),
    panel.background = element_blank(),
    panel.grid.minor = element_line(colour = "gray"),
    panel.grid.major = element_line(colour = "gray"),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    axis.line = element_blank()
  )
}
bin_class <- function(dat, bin_col = "pred", 
                      quantile_labels = 100, break_vec = c(-1, 30, 50, 70, 90, 100)){
  if(is(dat, "sf")){
    dat <- st_drop_geometry(dat)
  }
  pred_bin <- as.numeric(.bincode(dat[,bin_col]+1e-8, # wiggle factor to get above zero
                                  breaks = quantile(dat[,bin_col],
                                                    seq(0,1, by=(1/quantile_labels)), 
                                                    na.rm = TRUE,
                                                    labels = seq(1,quantile_labels,1))))
  pred_bin_class <- as.numeric(cut(pred_bin, 
                                   breaks = break_vec, 
                                   na.rm  = TRUE,
                                   labels = seq(1,length(break_vec)-1,1)))
  pred_bin_class <- ifelse(is.na(pred_bin_class), length(break_vec)-1, pred_bin_class)
}

