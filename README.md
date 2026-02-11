# 1-Tree-time-node

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

本项目是一个专门用于**系统发育树（Phylogenetic Tree）处理与分析**的工具箱。内容涵盖了从基础的树文件转换、时间轴缩放、节点数据统计到高级的 BSP 群体动态分析及可视化注释。

## 📁 目录结构说明

为了方便管理和调用，脚本按照分析流程分为以下模块：

- **`0-树文件处理/`**: 包含各类树文件格式（Newick, Nexus, Phylip等）的读取、清洗及批量转换脚本。
- **`1-treeio时间缩放/`**: 基于 R 语言 `treeio` 及相关包，实现进化树的分支长度向时间尺度（Time-scaling）的转换。
- **`2-节点统计/`**: 用于提取进化树节点信息、计算 Bootstrap 支持度分布、统计分叉点时间等。
- **`3-BSP/`**: 群体有效大小动态分析（Bayesian Skyline Plot）的相关脚本与输入文件准备。
- **`4-itol注释/`**: 自动生成 [iTOL](https://itol.embl.de/) 可视化所需的注释文件（Colors, Labels, Heatmaps等）。
- **`5-Beast-BSP/`**: 针对 BEAST 软件输出的 trees 文件进行后处理及 BSP 曲线绘制。
- **`6-iqtree-for-phynder/`**: 适配 IQ-TREE 结果以供 phynder 等下游软件进行支系归类的专用工具。

## 🛠️ 核心工具链
使用本项目可能需要以下环境支持：
- **R**: `ggtree`, `treeio`, `ape`
- **Python**: `Biopython`, `ete3`
- **生物信息工具**: IQ-TREE, BEAST2, FigTree
