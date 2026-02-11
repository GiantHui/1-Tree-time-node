#!/usr/bin/env python3
"""Convert IQ-TREE style Newick trees for pathPhynder.

IQ-TREE writes bootstrap supports as internal node labels (e.g. ")87:"),
which pathPhynder treats as duplicated node names. This script simply removes
those labels so that the resulting tree matches previously working files (such
as ``3617_9th.tree``).
"""
from __future__ import annotations

import argparse
import re
from pathlib import Path

# Match ")<support>:<branch_length>" where support is numeric and may include
# optional decimal points. Branch lengths can be in plain or scientific
# notation and we keep the original formatting by echoing the matched string.
SUPPORT_PATTERN = re.compile(r"\)(\d+(?:\.\d+)?):([+-]?(?:\d+\.\d+|\d+)(?:[eE][+-]?\d+)?)")


def convert_newick(tree_text: str) -> str:
    """Remove internal node labels so the format matches known-good trees."""

    def _replace(match: re.Match[str]) -> str:
        branch_length = match.group(2)
        return f"):{branch_length}"

    return SUPPORT_PATTERN.sub(_replace, tree_text)


def main() -> None:
    parser = argparse.ArgumentParser(description="Format an IQ-TREE Newick file for pathPhynder.")
    parser.add_argument("input", type=Path, help="Path to the input .treefile produced by IQ-TREE")
    parser.add_argument("-o", "--output", type=Path, help="Output path (defaults to <input>.phynder.tree)")

    args = parser.parse_args()
    input_path: Path = args.input
    output_path: Path

    if args.output:
        output_path = args.output
    else:
        output_path = input_path.with_suffix("")
        output_path = output_path.with_name(output_path.name + ".phynder.tree")

    if not input_path.is_file():
        raise FileNotFoundError(f"Cannot read input tree: {input_path}")

    tree_text = input_path.read_text(encoding="utf-8")
    converted = convert_newick(tree_text)
    output_path.write_text(converted, encoding="utf-8")

    print(f"Wrote converted tree to {output_path}")


if __name__ == "__main__":
    main()
