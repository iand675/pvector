#!/usr/bin/env python3
"""Parse Criterion CSV results and generate SVG bar charts for the README."""

import csv
import math
import os
import re
import sys

COLORS = {
    "[]": "#6c757d",
    "[] (++ [x])": "#6c757d",
    "[] (!!)" : "#6c757d",
    "[] (:)": "#6c757d",
    "Vector": "#4c72b0",
    "Vector snoc": "#4c72b0",
    "Vector cons": "#4c72b0",
    "PVector": "#dd8452",
    "PVector snoc": "#dd8452",
    "PVector cons": "#dd8452",
    "PVector foldChunks": "#dd8452",
    "Seq": "#55a868",
    "Seq |>": "#55a868",
    "Seq <|": "#55a868",
}

IMPL_ORDER = ["[]", "Vector", "PVector", "Seq"]
MATCH_ORDER = ["[]", "PVector", "Vector", "Seq"]  # PVector before Vector to avoid substring match

def color_for(impl):
    if impl in COLORS:
        return COLORS[impl]
    for k, v in COLORS.items():
        if k in impl:
            return v
    return "#999999"

def canonical_impl(name):
    for k in MATCH_ORDER:
        if k in name:
            return k
    return name

def fmt_time(seconds):
    if seconds < 1e-6:
        return f"{seconds*1e9:.1f} ns"
    elif seconds < 1e-3:
        return f"{seconds*1e6:.1f} \u00b5s"
    elif seconds < 1:
        return f"{seconds*1e3:.1f} ms"
    else:
        return f"{seconds:.2f} s"

def parse_csv(path):
    groups = {}
    with open(path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row["Name"]
            if name == "Name":
                continue
            mean = float(row["Mean"])
            # Split from the right: impl is the last segment, size is second-to-last
            parts = name.rsplit("/", 2)
            if len(parts) == 3:
                op, size, impl = parts
            elif len(parts) == 2:
                op, impl = parts[0], parts[1]
                size = "N/A"
            else:
                continue
            size = size.strip()
            op = op.strip()
            impl = impl.strip()
            key = (op, size)
            if key not in groups:
                groups[key] = {}
            groups[key][impl] = mean
    return groups

def generate_svg(title, impls_data, width=520, bar_h=28, gap=6):
    """Generate an SVG grouped bar chart. impls_data = [(impl_name, seconds), ...]"""
    if not impls_data:
        return ""

    impls_data = sorted(impls_data, key=lambda x: IMPL_ORDER.index(canonical_impl(x[0])) if canonical_impl(x[0]) in IMPL_ORDER else 99)

    max_val = max(v for _, v in impls_data)
    if max_val == 0:
        return ""

    label_w = 80
    chart_w = width - label_w - 90
    top_margin = 32
    n = len(impls_data)
    chart_h = n * (bar_h + gap) + gap
    total_h = chart_h + top_margin + 8

    lines = []
    lines.append(f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{total_h}" viewBox="0 0 {width} {total_h}">')
    lines.append(f'<style>')
    lines.append(f'  text {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif; }}')
    lines.append(f'  .title {{ font-size: 13px; font-weight: 600; fill: #24292f; }}')
    lines.append(f'  .label {{ font-size: 11px; fill: #57606a; }}')
    lines.append(f'  .value {{ font-size: 11px; fill: #24292f; font-weight: 500; }}')
    lines.append(f'</style>')
    lines.append(f'<rect width="{width}" height="{total_h}" fill="#ffffff" rx="6"/>')
    lines.append(f'<text x="{width//2}" y="20" text-anchor="middle" class="title">{title}</text>')

    for i, (impl, val) in enumerate(impls_data):
        y = top_margin + gap + i * (bar_h + gap)
        bw = max(2, (val / max_val) * chart_w)
        c = color_for(impl)

        lines.append(f'<text x="{label_w - 6}" y="{y + bar_h//2 + 4}" text-anchor="end" class="label">{impl}</text>')
        lines.append(f'<rect x="{label_w}" y="{y}" width="{bw:.1f}" height="{bar_h}" fill="{c}" rx="3"/>')
        lines.append(f'<text x="{label_w + bw + 6}" y="{y + bar_h//2 + 4}" class="value">{fmt_time(val)}</text>')

    lines.append('</svg>')
    return "\n".join(lines)


def main():
    csv_path = os.path.join(os.path.dirname(__file__), "results.csv")
    chart_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), "doc", "bench")
    os.makedirs(chart_dir, exist_ok=True)

    groups = parse_csv(csv_path)

    target_size = "10000"
    ops_to_chart = [
        "snoc (build n)",
        "fromList",
        "index (middle)",
        "head",
        "last",
        "update (single, middle, nf)",
        "foldl' (+)",
        "foldr (:) []",
        "map (+1)",
        "filter even",
        "filter (> 0) (keeps all)",
        "reverse",
        "take (n/2)",
        "append",
        "replicate",
        "cons (build n)",
        "foldl' . map (fused)",
        "foldl' . filter (fused)",
        "foldl' . map . filter (fused)",
        "foldl' . take (fused)",
    ]

    generated = []
    for op in ops_to_chart:
        key = (op, target_size)
        if key not in groups:
            continue
        data = groups[key]
        impls = list(data.items())
        if not impls:
            continue
        safe_name = re.sub(r'[^a-zA-Z0-9]+', '-', op).strip('-').lower()
        svg = generate_svg(f"{op}  (n = {target_size})", impls)
        if svg:
            fname = f"{safe_name}.svg"
            fpath = os.path.join(chart_dir, fname)
            with open(fpath, "w") as f:
                f.write(svg)
            generated.append((op, fname, data))
            print(f"  generated {fname}")

    print(f"\n{len(generated)} charts written to {chart_dir}/")

    readme_table_lines = []
    readme_table_lines.append("")
    readme_table_lines.append("## Benchmark Results (n = 10,000)")
    readme_table_lines.append("")

    core_ops = [
        "snoc (build n)",
        "fromList",
        "index (middle)",
        "head",
        "last",
        "update (single, middle, nf)",
        "foldl' (+)",
        "foldr (:) []",
        "map (+1)",
        "filter even",
        "reverse",
        "take (n/2)",
        "append",
    ]

    readme_table_lines.append("### Core operations")
    readme_table_lines.append("")
    readme_table_lines.append("| Operation | List | Vector | PVector | Seq |")
    readme_table_lines.append("|-----------|------|--------|---------|-----|")
    for op in core_ops:
        key = (op, target_size)
        if key not in groups:
            continue
        d = groups[key]
        cols = []
        for impl in IMPL_ORDER:
            found = False
            for k, v in d.items():
                if canonical_impl(k) == impl:
                    cols.append(fmt_time(v))
                    found = True
                    break
            if not found:
                cols.append("--")
        readme_table_lines.append(f"| {op} | {' | '.join(cols)} |")

    readme_table_lines.append("")
    readme_table_lines.append("### Stream fusion")
    readme_table_lines.append("")
    readme_table_lines.append("| Pipeline | Vector | PVector |")
    readme_table_lines.append("|----------|--------|---------|")
    fused_ops = [
        "foldl' . map (fused)",
        "foldl' . filter (fused)",
        "foldl' . map . filter (fused)",
        "foldl' . take (fused)",
    ]
    for op in fused_ops:
        key = (op, target_size)
        if key not in groups:
            continue
        d = groups[key]
        vec_val = "--"
        pv_val = "--"
        for k, v in d.items():
            if "Vector" in k and "PVector" not in k:
                vec_val = fmt_time(v)
            elif "PVector" in k:
                pv_val = fmt_time(v)
        readme_table_lines.append(f"| {op} | {vec_val} | {pv_val} |")

    readme_table_lines.append("")
    readme_table_lines.append("### Charts")
    readme_table_lines.append("")
    for op, fname, _ in generated:
        readme_table_lines.append(f"#### {op}")
        readme_table_lines.append(f"![{op}](doc/bench/{fname})")
        readme_table_lines.append("")

    output = "\n".join(readme_table_lines)
    outpath = os.path.join(os.path.dirname(os.path.dirname(__file__)), "BENCHMARK_SECTION.md")
    with open(outpath, "w") as f:
        f.write(output)
    print(f"\nREADME section written to {outpath}")

if __name__ == "__main__":
    main()
