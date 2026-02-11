# ================================
# Phylogenetic Tree Branch/Node Analysis
# ================================

# Load required packages
library(ape)
library(ggplot2)
library(dplyr)
library(tibble)

# ========== CONFIGURATION ==========
tree_file <- "G:/1_Projects2023/1YChromosome/8_M7scSEAFarmermigrations/M7_fina.nex"  # Input NEXUS tree file
output_csv <- "M7_branch_node_summary.csv"  # Output summary table
bin_resolution <- 29  # Binning resolution in years
time_range <- c(20000, 0)  # X-axis limits: from ancient to recent
# ===================================

# Step 1: Read and validate tree
if (!file.exists(tree_file)) stop("❌ Tree file not found. Please check the path.")
tree <- read.nexus(tree_file)

# Step 2: Compute divergence times for each node (from root to tips)
node_heights <- node.depth.edgelength(tree)
max_height <- max(node_heights)
divergence_times <- max_height - node_heights  # Ancient to present

# Step 3: Extract internal node divergence times (i.e., branching points)
n_tips <- length(tree$tip.label)
internal_nodes <- (n_tips + 1):(n_tips + tree$Nnode)
branch_times <- divergence_times[internal_nodes]

# Step 4: Bin divergence times
branch_df <- tibble(
  Divergence_Time = round(branch_times / bin_resolution) * bin_resolution
)

# Step 5: Count branches/nodes per divergence bin and compute cumulative values
summary_df <- branch_df %>%
  count(Divergence_Time, name = "New_Branches") %>%
  arrange(desc(Divergence_Time)) %>%  # From ancient to recent
  mutate(
    Cumulative_Branches = cumsum(New_Branches),
    New_Nodes = New_Branches,
    Cumulative_Nodes = cumsum(New_Nodes)
  ) %>%
  arrange(Divergence_Time)  # Optional: re-sort chronologically (recent to ancient)

# Step 6: Export results
write.csv(summary_df, output_csv, row.names = FALSE)
cat("✅ Summary written to:", output_csv, "\n")

# Step 7: General plotting function (no log transform)
plot_metric <- function(data, x, y, title, ylab, color) {
  ggplot(data, aes_string(x = x, y = y)) +
    geom_line(color = color, size = 1.2) +
    scale_x_reverse(limits = time_range, expand = c(0, 0)) +
    labs(title = title,
         x = "Divergence Time (years before present)",
         y = ylab) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12)
    )
}

# Step 8: Generate plots
p1 <- plot_metric(summary_df, "Divergence_Time", "New_Branches",
                  "New Branches by Divergence Time", "New Branches", "forestgreen")

p2 <- plot_metric(summary_df, "Divergence_Time", "Cumulative_Branches",
                  "Cumulative Branches by Divergence Time", "Cumulative Branches", "darkgreen")

p3 <- plot_metric(summary_df, "Divergence_Time", "New_Nodes",
                  "New Nodes by Divergence Time", "New Nodes", "royalblue")

p4 <- plot_metric(summary_df, "Divergence_Time", "Cumulative_Nodes",
                  "Cumulative Nodes by Divergence Time", "Cumulative Nodes", "navy")

# Step 9: Show all plots
print(p1)
print(p2)
print(p3)
print(p4)

########新节点或分支柱状图
# Step 7: General plotting function with option for histogram
plot_metric <- function(data, x, y, title, ylab, color, geom_type = "line") {
  p <- ggplot(data, aes_string(x = x, y = y))
  
  if (geom_type == "hist") {
    p <- p + geom_col(fill = color, width = bin_resolution * 0.9)
  } else {
    p <- p + geom_line(color = color, size = 1.2)
  }
  
  p +
    scale_x_reverse(limits = time_range, expand = c(0, 0)) +
    labs(title = title,
         x = "Divergence Time (years before present)",
         y = ylab) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12)
    )
}

# Step 8: Generate plots
p1 <- plot_metric(summary_df, "Divergence_Time", "New_Branches",
                  "New Branches by Divergence Time", "New Branches", "forestgreen", geom_type = "hist")

p2 <- plot_metric(summary_df, "Divergence_Time", "Cumulative_Branches",
                  "Cumulative Branches by Divergence Time", "Cumulative Branches", "darkgreen")

p3 <- plot_metric(summary_df, "Divergence_Time", "New_Nodes",
                  "New Nodes by Divergence Time", "New Nodes", "royalblue", geom_type = "hist")

p4 <- plot_metric(summary_df, "Divergence_Time", "Cumulative_Nodes",
                  "Cumulative Nodes by Divergence Time", "Cumulative Nodes", "navy")

# Step 9: Show all plots
print(p1)
print(p2)
print(p3)
print(p4)

#新节点和累计节点同时画图
# Normalize function
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

# Merge the new and cumulative metrics into one data frame with normalized values
summary_df <- summary_df %>%
  mutate(
    norm_New_Branches = normalize(New_Branches),
    norm_Cumulative_Branches = normalize(Cumulative_Branches),
    norm_New_Nodes = normalize(New_Nodes),
    norm_Cumulative_Nodes = normalize(Cumulative_Nodes)
  )

# Plot 1: New Branches (histogram) + Cumulative Branches (line)
p_combined_branch <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = norm_New_Branches), fill = "forestgreen", width = bin_resolution * 0.9) +
  geom_line(aes(y = norm_Cumulative_Branches), color = "darkgreen", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "New Branches (normalized)",
    sec.axis = sec_axis(~., name = "Cumulative Branches (normalized)")
  ) +
  labs(title = "New vs. Cumulative Branches by Divergence Time",
       x = "Divergence Time (years before present)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "forestgreen"),
    axis.title.y.right = element_text(color = "darkgreen")
  )

# Plot 2: New Nodes (histogram) + Cumulative Nodes (line)
p_combined_node <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = norm_New_Nodes), fill = "royalblue", width = bin_resolution * 0.9) +
  geom_line(aes(y = norm_Cumulative_Nodes), color = "navy", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "New Nodes (normalized)",
    sec.axis = sec_axis(~., name = "Cumulative Nodes (normalized)")
  ) +
  labs(title = "New vs. Cumulative Nodes by Divergence Time",
       x = "Divergence Time (years before present)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "royalblue"),
    axis.title.y.right = element_text(color = "navy")
  )

# Print combined plots
print(p_combined_branch)
print(p_combined_node)

#不归一化直接使用不同尺度的Y轴
# Compute scaling factor between primary and secondary axis
branch_scale_factor <- max(summary_df$Cumulative_Branches, na.rm = TRUE) / max(summary_df$New_Branches, na.rm = TRUE)
node_scale_factor <- max(summary_df$Cumulative_Nodes, na.rm = TRUE) / max(summary_df$New_Nodes, na.rm = TRUE)

# Plot 1: New Branches (histogram) + Cumulative Branches (line)
p_combined_branch <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = New_Branches), fill = "forestgreen", width = bin_resolution * 0.9) +
  geom_line(aes(y = Cumulative_Branches / branch_scale_factor), color = "darkgreen", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "New Branches",
    sec.axis = sec_axis(~ . * branch_scale_factor, name = "Cumulative Branches")
  ) +
  labs(title = "New vs. Cumulative Branches by Divergence Time",
       x = "Divergence Time (years before present)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "forestgreen"),
    axis.title.y.right = element_text(color = "darkgreen")
  )

# Plot 2: New Nodes (histogram) + Cumulative Nodes (line)
p_combined_node <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = New_Nodes), fill = "royalblue", width = bin_resolution * 0.9) +
  geom_line(aes(y = Cumulative_Nodes / node_scale_factor), color = "navy", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "New Nodes",
    sec.axis = sec_axis(~ . * node_scale_factor, name = "Cumulative Nodes")
  ) +
  labs(title = "New vs. Cumulative Nodes by Divergence Time",
       x = "Divergence Time (years before present)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "royalblue"),
    axis.title.y.right = element_text(color = "navy")
  )

# Print combined figures
print(p_combined_branch)
print(p_combined_node)












