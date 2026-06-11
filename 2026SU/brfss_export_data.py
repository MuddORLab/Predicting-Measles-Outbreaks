#!/usr/bin/env python3
"""
Export question-level data from the Texas DSHS BRFSS Tableau dashboard.

Run with the Jupyter kernel/env where tableauscraper is installed:

    /Users/sean/opt/anaconda3/envs/py396_env/bin/python brfss_export_questions.py

Or paste the main body into a notebook cell.
"""

from __future__ import annotations

import argparse
import re
import sys
import time
from pathlib import Path

import pandas as pd
from tableauscraper import TableauScraper as TS


TABLEAU_URL = "https://tabexternal.dshs.texas.gov/t/THD/views/BRFSSRedesignDraft/BRFSS"

DEFAULT_DASHBOARDS = [
    "Data Table Builder 2011+",
    "Data Table Builder 2002-2010",
]


def safe_name(value: object, max_len: int = 120) -> str:
    name = re.sub(r"[^A-Za-z0-9_.-]+", "_", str(value)).strip("_")
    return name[:max_len] or "blank"


def unique_values(values: list[object]) -> list[object]:
    seen = set()
    output = []
    for value in values or []:
        key = str(value)
        if key and key not in seen:
            seen.add(key)
            output.append(value)
    return output


def iter_filters(workbook):
    for worksheet in workbook.worksheets:
        try:
            filters = worksheet.getFilters()
        except Exception:
            continue

        for filter_info in filters or []:
            yield worksheet, filter_info


def find_filter(workbook, column_contains: str):
    matches = []
    needle = column_contains.lower()

    for worksheet, filter_info in iter_filters(workbook):
        column = str(filter_info.get("column", ""))
        values = unique_values(filter_info.get("values", []))

        if needle in column.lower() and values:
            matches.append((len(values), worksheet, filter_info, column, values))

    if not matches:
        available = sorted(
            {
                str(filter_info.get("column", ""))
                for _, filter_info in iter_filters(workbook)
                if filter_info.get("column")
            }
        )
        raise RuntimeError(
            f"No filter containing {column_contains!r} was found. "
            f"Available filters: {available}"
        )

    matches.sort(key=lambda item: item[0], reverse=True)
    _, worksheet, filter_info, column, values = matches[0]
    return worksheet, filter_info, column, values


def export_workbook_data(
    workbook,
    dashboard_name: str,
    question: object,
    output_dir: Path,
    separate_files: bool,
) -> list[pd.DataFrame]:
    frames = []

    for worksheet in workbook.worksheets:
        df = worksheet.data.copy()
        if df.empty:
            continue

        df.insert(0, "dashboard", dashboard_name)
        df.insert(1, "question", question)
        df.insert(2, "worksheet", worksheet.name)
        frames.append(df)

        if separate_files:
            filename = (
                f"{safe_name(dashboard_name)}__"
                f"{safe_name(question, 80)}__"
                f"{safe_name(worksheet.name)}.csv"
            )
            df.to_csv(output_dir / filename, index=False)

    return frames


def load_tableau_workbook(url: str):
    scraper = TS()

    try:
        scraper.loads(url)
    except AttributeError as exc:
        if "tsConfigContainer" in str(exc):
            raise RuntimeError(
                "Tableau did not return its workbook config. This usually means "
                "the server blocked direct Python/tableauscraper requests. "
                "Open the dashboard in a browser to confirm access, or use a "
                "browser automation fallback such as Playwright."
            ) from exc
        raise

    return scraper.getWorkbook()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", default=TABLEAU_URL)
    parser.add_argument("--output-dir", default="brfss_question_exports")
    parser.add_argument(
        "--dashboard",
        action="append",
        dest="dashboards",
        help="Dashboard sheet to scrape. Can be provided more than once.",
    )
    parser.add_argument(
        "--question-filter-contains",
        default="Question",
        help="Text used to find the Tableau question dropdown/filter.",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Only scrape the first N questions. Useful for testing.",
    )
    parser.add_argument(
        "--list-only",
        action="store_true",
        help="List detected questions and save brfss_question_list.csv only.",
    )
    parser.add_argument(
        "--separate-files",
        action="store_true",
        help="Also save one CSV per question/worksheet.",
    )
    parser.add_argument("--sleep", type=float, default=0.25)
    # Jupyter injects its own kernel args, e.g. --f=/path/to/kernel.json.
    # parse_known_args keeps the script usable both from a terminal and a notebook.
    args, _unknown = parser.parse_known_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    dashboards = args.dashboards or DEFAULT_DASHBOARDS
    root_workbook = load_tableau_workbook(args.url)

    all_frames = []
    question_rows = []

    for dashboard_name in dashboards:
        print(f"\nDashboard: {dashboard_name}")
        workbook = root_workbook.goToSheet(dashboard_name)

        _, _, question_column, questions = find_filter(
            workbook, args.question_filter_contains
        )

        if args.limit is not None:
            questions = questions[: args.limit]

        print(f"Question filter: {question_column}")
        print(f"Questions found: {len(questions)}")

        for question in questions:
            question_rows.append(
                {
                    "dashboard": dashboard_name,
                    "question_filter": question_column,
                    "question": question,
                }
            )

        if args.list_only:
            continue

        current_workbook = workbook

        for idx, question in enumerate(questions, start=1):
            print(f"  [{idx}/{len(questions)}] {question}")

            try:
                question_worksheet, _, current_question_column, _ = find_filter(
                    current_workbook, args.question_filter_contains
                )
                current_workbook = question_worksheet.setFilter(
                    current_question_column,
                    question,
                    dashboardFilter=True,
                )
            except Exception as exc:
                print(f"    skipped: {exc}", file=sys.stderr)
                continue

            frames = export_workbook_data(
                current_workbook,
                dashboard_name,
                question,
                output_dir,
                args.separate_files,
            )
            all_frames.extend(frames)
            time.sleep(args.sleep)

    question_list = pd.DataFrame(question_rows)
    question_list_path = output_dir / "brfss_question_list.csv"
    question_list.to_csv(question_list_path, index=False)
    print(f"\nQuestion list saved: {question_list_path}")

    if not args.list_only and all_frames:
        combined = pd.concat(all_frames, ignore_index=True)
        combined_path = output_dir / "brfss_questions_combined.csv"
        combined.to_csv(combined_path, index=False)
        print(f"Combined data saved: {combined_path}")

    if not args.list_only and not all_frames:
        print("No worksheet data was exported.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
