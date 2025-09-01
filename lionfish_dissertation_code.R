library(lionfish)
library(data.table)
data(ChemicalManufacturingProcess)
data <- data.table(ChemicalManufacturingProcess)
col_names <- names(data)
mp_cols <- grep("^ManufacturingProcess", col_names, value = TRUE)
bm_cols <- grep("^BiologicalMaterial", col_names, value = TRUE)
new_mp_names <- sub("^ManufacturingProcess", "ManPr", mp_cols)
new_bm_names <- sub("^BiologicalMaterial", "BioMat", bm_cols)
setnames(data, old = mp_cols, new = new_mp_names)
setnames(data, old = bm_cols, new = new_bm_names)

data <- data[rowSums(is.na(data))<3]
data <- data[, lapply(.SD, function(x) if (sum(is.na(x)) <= 3) x else NULL)]
data <- data[-c(99,153,154,158,159,160)]
data <- data[, lapply(.SD, function(x) if (sum(is.na(x)) <= 0) x else NULL)]

min_yield <- min(data$Yield, na.rm = TRUE)
max_yield <- max(data$Yield, na.rm = TRUE)
intervals <- seq(min_yield, max_yield, length.out = 11)
yield_values <- findInterval(data$Yield, intervals, rightmost.closed = TRUE)
yield_labels <- paste0("Yield ", head(intervals, -1), "-", tail(intervals, -1))

library(ggplot2)
library(viridis)
yield_values_factor <- factor(yield_values, labels = yield_labels)

ggplot(data, aes(x = Yield, fill = yield_values_factor)) +
  geom_histogram(binwidth = (max_yield - min_yield) / 30, color = "black") +
  scale_fill_viridis(option = "plasma", discrete = TRUE) + theme_minimal() +
  labs(title = "Histogram of the process yield", x = "Yield", y = "Count", fill = "Yield Intervals")

data <- data[, lapply(.SD, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))]
data_wo_y <- copy(data)
data_wo_y[, Yield := NULL]

library(tourr)
display_size <- 5 # Adjust to fit your display
if (check_venv()){
  init_env(env_name = "r-lionfish", virtual_env = "virtual_env")
  } else if (check_conda_env()){
  init_env(env_name = "r-lionfish", virtual_env = "anaconda")
  }

set.seed(42)
lda_tour_history_1d <- save_history(data_wo_y, tour_path = guided_tour(lda_pp(yield_values),d=1))
lda_tour_history_2d <- save_history(data_wo_y, tour_path = guided_tour(lda_pp(yield_values),d=2))

half_range <- max(sqrt(rowSums(data_wo_y^2)))
feature_names <- colnames(data_wo_y)
obj1 <- list(type="1d_tour", obj=lda_tour_history_1d)
obj2 <- list(type="2d_tour", obj=lda_tour_history_2d)

interactive_tour(data=data_wo_y, plot_objects=list(obj1, obj2),
                 feature_names=feature_names,
                 half_range=half_range/2,
                 n_plot_cols=2,
                 preselection=yield_values,
                 preselection_names=yield_labels,
                 display_size= display_size,
                 color_scale="plasma")

interactive_tour(data=data_wo_y, plot_objects=list(obj2),
                 feature_names=feature_names,
                 half_range=half_range/2,
                 n_plot_cols=2,
                 preselection=yield_values,
                 preselection_names=yield_labels,
                 display_size= display_size,
                 color_scale="plasma")

if (requireNamespace("randomForest")){
  set.seed(42)
  library(randomForest)
  rf <- randomForest(Yield~.,data)
  importance_df <- data.frame(rf$importance)
  importance_df <- as.data.table(rf$importance, keep.rownames = "Feature")
  sorted_importance <- importance_df[order(-IncNodePurity)]
  top_10_features <- sorted_importance[1:10, Feature]
  print(top_10_features)
}


data_rf_sel <- data[, ..top_10_features]
grand_tour_history_1d <- save_history(data_rf_sel, tour_path = guided_tour(lda_pp(yield_values),d=1))
lda_tour_history_2d <- save_history(data_rf_sel, tour_path = guided_tour(lda_pp(yield_values),d=2))
half_range <- max(sqrt(rowSums(data_rf_sel^2)))
feature_names <- colnames(data_rf_sel)
obj1 <- list(type="1d_tour", obj=grand_tour_history_1d)
obj2 <- list(type="2d_tour", obj=lda_tour_history_2d)

interactive_tour(data=data_rf_sel,
                 plot_objects=list(obj1, obj2),
                 feature_names=feature_names,
                 half_range=half_range/2, n_plot_cols=2,
                 preselection=yield_values,
                 preselection_names=yield_labels,
                 n_subsets=10,
                 display_size= display_size,
                 color_scale = "plasma")
