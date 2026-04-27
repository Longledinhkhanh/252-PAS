# Tải các thư viện bổ sung cho vẽ biểu đồ suy diễn
library(ggplot2)
library(dplyr)
library(broom)      # Để lấy Tỷ số chênh (Odds Ratio) từ mô hình
library(ggridges)   # Để vẽ Ridgeline Plot

# =========================================================================
# BIỂU ĐỒ 1: FOREST PLOT - SỨC MẠNH CỦA CÁC YẾU TỐ (ODDS RATIO)
# =========================================================================
cat("\nĐang xử lý dữ liệu và vẽ Biểu đồ Forest Plot...\n")

# Lấy các hệ số từ mô hình, tính Odds Ratio (exponentiate = TRUE) và Khoảng tin cậy
tidy_model <- tidy(model_ordinal, conf.int = TRUE, exponentiate = TRUE) %>%
  # Loại bỏ các mốc ranh giới (cutpoints/intercepts có chứa dấu "|")
  filter(!grepl("\\|", term))

# Vẽ Forest Plot
plot_forest <- ggplot(tidy_model, aes(x = estimate, y = reorder(term, estimate))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#0072B2", size = 0.8, linewidth = 1.2) +
  # Đường vô giá trị (Null line) tại x = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  # Chuyển trục X sang thang đo logarit để dễ nhìn nếu OR quá lớn
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 15)) + 
  labs(title = "Biểu đồ 1: Forest Plot - Mức độ tác động của thói quen lên béo phì",
       subtitle = "Odds Ratio (OR) và Khoảng tin cậy 95% (Thang đo Log)",
       x = "Tỷ số chênh (OR > 1: Tăng nguy cơ béo phì | OR < 1: Giảm nguy cơ)",
       y = "Các biến số độc lập") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
    dev.new()
print(plot_forest)


# =========================================================================
# BIỂU ĐỒ 2: HEATMAP MA TRẬN NHẦM LẪN (CONFUSION MATRIX)
# =========================================================================

cat("\nĐang vẽ Biểu đồ Heatmap Ma trận nhầm lẫn...\n")

# Chuyển bảng Confusion Matrix thành Data Frame
cm_data <- as.data.frame(conf_matrix$table)

# Vẽ Heatmap
plot_cm <- ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  # Tạo dải màu từ trắng đến đỏ đậm
  scale_fill_gradient(low = "white", high = "#D55E00") +
  # In số liệu lên từng ô
  geom_text(aes(label = Freq), color = "black", size = 4, fontface = "bold") +
  labs(title = "Biểu đồ 2: Heatmap Ma trận nhầm lẫn (Confusion Matrix)",
       subtitle = "Trực quan hóa sự mờ nhạt giữa Thừa cân và Béo phì",
       x = "Thực tế (Reference)",
       y = "Mô hình Dự đoán (Prediction)",
       fill = "Số lượng") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14))
    dev.new()

print(plot_cm)

# =========================================================================
# BIỂU ĐỒ 3: RIDGELINE PLOT - ĐỘ TUỔI VÀ BÉO PHÌ
# =========================================================================
cat("\nĐang vẽ Biểu đồ Ridgeline Plot cho Độ tuổi...\n")

plot_ridge <- ggplot(data_model, aes(x = Age, y = Obesity_Level, fill = Obesity_Level)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5, color = "white") +
  # Lựa chọn dải màu đẹp
  scale_fill_viridis_d(option = "plasma") + 
  labs(title = "Biểu đồ 3: Ridgeline Plot - Sự dịch chuyển độ tuổi qua các mức béo phì",
       subtitle = "Mức độ béo phì tăng dần tỷ lệ thuận với sự trưởng thành",
       x = "Độ tuổi (Age)",
       y = "Mức độ Béo phì") +
  theme_ridges() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(hjust = 0.5))
    dev.new()
print(plot_ridge)