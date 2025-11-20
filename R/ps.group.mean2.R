
ps.group.mean2 <- function(ps, group = "Group", scale = TRUE) {
  # ---- 1. 提取 OTU 表 ----
  otu_table <- as.data.frame(t(vegan_otu(ps)))  # 行=样本, 列=OTU

  # ---- 2. 提取分组信息 ----
  design <- as.data.frame(sample_data(ps))
  if (!group %in% colnames(design)) {
    stop(paste("分组列", group, "不存在于 sample_data(ps) 中！"))
  }
  group_factor <- as.factor(design[[group]])

  # ---- 3. 归一化（相对丰度可选）----
  OTU <- as.matrix(otu_table)
  if (scale) {
    norm <- t(t(OTU) / colSums(OTU, na.rm = TRUE))  # 相对丰度
  } else {
    norm <- OTU
  }

  # ---- 4. 按分组计算均值 ----
  # 先绑定分组信息
  df <- as.data.frame(t(norm))
  df$Group <- group_factor

  # 用 dplyr 直接 group_by + summarise
  mean_df <- df %>%
    group_by(Group) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

  # 转置回来: 行=OTU, 列=分组
  norm2 <- as.data.frame(t(mean_df[,-1]))
  colnames(norm2) <- mean_df$Group
  norm2$mean <- rowMeans(norm2, na.rm = TRUE)
  norm2$ID <- rownames(norm2)

  return(norm2)
}

