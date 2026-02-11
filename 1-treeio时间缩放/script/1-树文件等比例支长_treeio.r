library(ape)
library(treeio)

tree_file <- "/mnt/c/Users/Administrator/Desktop/cqHan_combine_20k.final.trees"
output_file <- "/mnt/c/Users/Administrator/Desktop/cqHan_combine_20k.final.93K.trees"
target_root_age <- 93000
time_field_pattern <- "height|length|age|time|tmrca|HPD"

should_scale <- function(name) {
	if (is.null(name) || !nzchar(name)) {
		return(FALSE)
	}
	grepl(time_field_pattern, name, ignore.case = TRUE)
}

scale_numeric_container <- function(value, scaling_factor, force = FALSE) {
	if (is.numeric(value)) {
		return(value * scaling_factor)
	}
	if (is.list(value)) {
		return(lapply(value, function(x) scale_numeric_container(x, scaling_factor, TRUE)))
	}
	if (force && is.vector(value) && is.numeric(as.numeric(value))) {
		num <- suppressWarnings(as.numeric(value))
		if (!any(is.na(num))) {
			return(num * scaling_factor)
		}
	}
	value
}

scale_phylogeny <- function(phy) {
	if (!is.ultrametric(phy, tol = 1e-6)) {
		stop("输入树不是超度量树，请先在 BEAST 中生成或检查树的分支长度")
	}
	current_height <- max(node.depth.edgelength(phy))
	scaling_factor <- target_root_age / current_height
	phy$edge.length <- phy$edge.length * scaling_factor
	list(phy = phy, factor = scaling_factor)
}

scale_annotations <- function(df, scaling_factor) {
	if (is.null(df) || !ncol(df)) {
		return(df)
	}
	for (nm in names(df)) {
		if (!should_scale(nm)) {
			next
		}
		col <- df[[nm]]
		if (is.numeric(col)) {
			df[[nm]] <- col * scaling_factor
		} else if (is.list(col)) {
			df[[nm]] <- lapply(col, function(x) scale_numeric_container(x, scaling_factor, TRUE))
		}
	}
	df
}

scale_extra_info <- function(info, scaling_factor) {
	if (is.null(info) || !length(info)) {
		return(info)
	}
	if (is.list(info)) {
		for (nm in names(info)) {
			if (should_scale(nm)) {
				info[[nm]] <- scale_numeric_container(info[[nm]], scaling_factor, TRUE)
			} else if (is.list(info[[nm]])) {
				info[[nm]] <- scale_extra_info(info[[nm]], scaling_factor)
			}
		}
	}
	info
}

beast_tree <- tryCatch(read.beast(tree_file), error = function(e) NULL)

if (!is.null(beast_tree)) {
	res <- scale_phylogeny(as.phylo(beast_tree))
	beast_tree@phylo <- res$phy
	beast_tree@data <- scale_annotations(beast_tree@data, res$factor)
	beast_tree@extraInfo <- scale_extra_info(beast_tree@extraInfo, res$factor)
	write.beast(beast_tree, file = output_file)
	tree_count <- 1L
} else {
	trees <- tryCatch(read.nexus(tree_file), error = function(e) read.tree(tree_file))
	if (inherits(trees, "multiPhylo")) {
		scaled_trees <- lapply(trees, function(tr) scale_phylogeny(tr)$phy)
		if (!is.null(names(trees))) {
			names(scaled_trees) <- names(trees)
		}
		class(scaled_trees) <- "multiPhylo"
		write.nexus(scaled_trees, file = output_file)
		tree_count <- length(trees)
	} else {
		res <- scale_phylogeny(trees)
		write.tree(res$phy, file = output_file)
		tree_count <- 1L
	}
}

cat("原始树数：", tree_count, "\n")
cat("已按目标根年龄", target_root_age, "缩放并输出到：", output_file, "\n")
