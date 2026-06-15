#!/usr/bin/env python3
from pathlib import Path
from difflib import SequenceMatcher
import json
import re

from openpyxl import load_workbook
import pandas as pd
from playwright.sync_api import sync_playwright, TimeoutError

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
URL = "https://tabexternal.dshs.texas.gov/t/THD/views/BRFSSRedesignDraft/BRFSS"

DASHBOARD       = "Data Table Builder 2011+"
TABLE_WORKSHEET = "Data Table 2011+"

YEAR                = "2014"
GEOGRAPHIC_CATEGORY = "Public Health Region"
AREAS = [
    "Public Health Region 1",
    "Public Health Region 8",
    "Public Health Region 11",
]

FEATURE_FILE = Path("/Users/sean/Downloads/2014_PHR2_BRFSS_Summary_Tables.xlsx")

FEATURE_NAME_FILE = Path(
    "/Users/sean/Summer 2026 Research/Measles-Outbreak-and-Public-Policy-Reluctance/"
    "2026SU/data/raw/brfss/2024_PHR8_BRFSS_Summary_Tables.xlsx"
)

ONLY_USE_TARGET_FEATURE_NAMES = True

MAX_TOPICS              = None
MAX_QUESTIONS_PER_TOPIC = None

OUTPUT_CSV = Path("/Users/sean/Documents/Codex/2026-06-11/i-need-to-scrape-all-the/brfss_2014_phr_1_8_11_target_variables.csv")
# ─────────────────────────────────────────────────────────────────────────────

MATCH_STOP_WORDS = {
    "a", "an", "and", "any", "are", "by", "do", "for", "had", "has", "have",
    "in", "is", "of", "on", "or", "the", "to", "with", "you", "your",
    "about", "adult", "adults", "age", "all", "asked", "been", "calculated",
    "current", "ever", "had", "last", "month", "months", "number", "past",
    "percent", "percentage", "questionnaire", "routine", "screening", "shot",
    "shots", "status", "test", "tests", "told", "variable", "year", "years",
    "yr", "yrs",
}


def clean_feature_text(value):
    value = "" if value is None or pd.isna(value) else str(value)
    value = re.sub(r"^(questionnaire|calculated variable):\s*", "", value, flags=re.I)
    return value.strip()


def normalize_for_match(value):
    value = clean_feature_text(value).lower()
    value = value.replace("&", " and ")
    value = value.replace("+", " plus ")
    value = re.sub(r"\byrs?\b", " year ", value)
    value = re.sub(r"[^a-z0-9]+", " ", value)
    value = re.sub(r"\bin the\b", " ", value)
    return " ".join(value.split())


def match_score(left, right):
    left = normalize_for_match(left)
    right = normalize_for_match(right)

    if not left or not right:
        return 0

    left_words = set(left.split())
    right_words = set(right.split())
    overlap = len(left_words & right_words) / max(min(len(left_words), len(right_words)), 1)
    sequence = SequenceMatcher(None, left, right).ratio()
    return (0.7 * overlap) + (0.3 * sequence)


def important_words(value):
    return {
        word
        for word in normalize_for_match(value).split()
        if len(word) >= 3 and word not in MATCH_STOP_WORDS
    }


def has_shared_important_word(left_texts, right_texts):
    left_words = set()
    right_words = set()

    for text in left_texts:
        left_words.update(important_words(text))

    for text in right_texts:
        right_words.update(important_words(text))

    return bool(left_words & right_words)


def load_features_from_workbook(workbook_path):
    workbook = load_workbook(workbook_path, read_only=True, data_only=True)
    features = []

    for sheet_name in workbook.sheetnames:
        if sheet_name.lower() in ["contents", "index"]:
            continue

        sheet = workbook[sheet_name]
        features.append({
            "feature": sheet_name,
            "search_texts": [
                sheet_name,
                clean_feature_text(sheet["A3"].value),
                clean_feature_text(sheet["A5"].value),
            ],
        })

    workbook.close()
    return features


def find_best_feature_match(search_texts, features, minimum_score):
    best_feature = None
    best_score = 0

    for feature in features:
        if not has_shared_important_word(search_texts, feature["search_texts"]):
            continue

        score = max(
            match_score(left, right)
            for left in search_texts
            for right in feature["search_texts"]
        )

        if score > best_score:
            best_feature = feature
            best_score = score

    if best_score < minimum_score:
        return None

    return best_feature


def load_feature_list():
    source_features = load_features_from_workbook(FEATURE_FILE)

    if FEATURE_NAME_FILE.exists():
        target_features = load_features_from_workbook(FEATURE_NAME_FILE)
    else:
        target_features = source_features

    final_features = []

    for source_feature in source_features:
        target_feature = find_best_feature_match(
            source_feature["search_texts"],
            target_features,
            minimum_score=0.62,
        )

        if target_feature is None and ONLY_USE_TARGET_FEATURE_NAMES:
            continue

        source_feature["output_feature"] = (
            target_feature["feature"] if target_feature else source_feature["feature"]
        )
        final_features.append(source_feature)

    return final_features


def find_matching_feature(topic, question, features):
    dashboard_text = f"{topic} {question}"
    best_feature = None
    best_score = 0

    for feature in features:
        if not has_shared_important_word([dashboard_text, question], feature["search_texts"]):
            continue

        score = max(match_score(dashboard_text, text) for text in feature["search_texts"])

        if score > best_score:
            best_feature = feature["output_feature"]
            best_score = score

    if best_score < 0.62:
        return None

    return best_feature


def clean_column_name(value):
    value = str(value).lower()
    value = "".join(char if char.isalnum() else "_" for char in value)
    return "_".join(part for part in value.split("_") if part)


def close_open_dropdowns(page):
    for _ in range(3):
        try:
            if page.get_by_role("option").first.is_visible():
                page.keyboard.press("Escape")
                page.wait_for_timeout(500)
            else:
                break
        except Exception:
            break


def wait_for_tableau_ready(page):
    close_open_dropdowns(page)

    # Wait until Tableau's interaction/update overlay is gone.
    try:
        page.locator(".tab-glass").first.wait_for(state="hidden", timeout=15000)
    except Exception:
        try:
            page.locator(".tab-glass").first.wait_for(state="detached", timeout=5000)
        except Exception:
            pass

    page.wait_for_timeout(800)


def open_dropdown(page, button_name):
    wait_for_tableau_ready(page)
    
    # Target by a more resilient relative text locator combined with role
    dropdown_button = page.get_by_role("button", name=button_name).first
    dropdown_button.scroll_into_view_if_needed()
    dropdown_button.click(force=True)
    
    page.get_by_role("option").first.wait_for(state="visible", timeout=7000)
    values = page.get_by_role("option").all_inner_texts()
    
    page.keyboard.press("Escape")
    page.get_by_role("option").first.wait_for(state="hidden", timeout=5000)
    return [value.strip() for value in values if value.strip()]


def choose_dropdown_value(page, button_name, value):
    wait_for_tableau_ready(page)

    dropdown_button = page.get_by_role("button", name=button_name).first
    dropdown_button.wait_for(state="visible", timeout=15000)
    dropdown_button.scroll_into_view_if_needed()

    expanded = dropdown_button.get_attribute("aria-expanded")
    if expanded != "true":
        dropdown_button.click(timeout=15000)

    page.get_by_role("option").first.wait_for(state="visible", timeout=10000)

    option_locator = page.get_by_role("option", name=value, exact=True)
    option_locator.wait_for(state="visible", timeout=10000)

    response_text = None
    try:
        with page.expect_response(
            lambda response: "/commands/tabdoc/" in response.url and response.request.method == "POST",
            timeout=15000
        ) as response_info:
            option_locator.click()
        response_text = response_info.value.text()
    except TimeoutError:
        pass

    close_open_dropdowns(page)
    wait_for_tableau_ready(page)
    return response_text


def get_tableau_string_values(command_response):
    data_segments = (
        command_response["vqlCmdResponse"]["layoutStatus"]["applicationPresModel"]
        .get("dataDictionary", {})
        .get("dataSegments", {})
    )
    for segment in data_segments.values():
        if not segment: continue
        for column in segment.get("dataColumns", []):
            if column.get("dataType") == "cstring":
                return column.get("dataValues", [])
    return []


def get_table_zone(command_response):
    zones = (
        command_response["vqlCmdResponse"]["layoutStatus"]["applicationPresModel"]
        .get("workbookPresModel", {})
        .get("dashboardPresModel", {})
        .get("zones", {})
    )
    for zone in zones.values():
        if not zone: continue
        if zone.get("worksheet") != TABLE_WORKSHEET: continue
        if "vizData" in zone.get("presModelHolder", {}).get("visual", {}):
            return zone
    return None


def decode_tableau_value(index, string_values):
    if index is None: return None
    position = abs(index) - 1 if index < 0 else index
    return string_values[position] if 0 <= position < len(string_values) else None


def command_response_to_table(command_text):
    command_response = json.loads(command_text)
    string_values    = get_tableau_string_values(command_response)
    zone             = get_table_zone(command_response)

    if not string_values or not zone:
        return pd.DataFrame()

    viz_data  = zone["presModelHolder"]["visual"]["vizData"]
    pane_data = viz_data["paneColumnsData"]
    columns   = pane_data["vizDataColumns"]
    panes     = pane_data["paneColumnsList"]

    table_columns = {}
    row_count     = 0

    for column in columns:
        column_name    = column.get("userFriendlyFieldCaption") or column.get("fieldCaption")
        pane_indices   = column.get("paneIndices")   or []
        column_indices = column.get("columnIndices") or []

        if not column_name or not pane_indices or not column_indices: continue

        pane          = panes[pane_indices[0]]
        pane_column   = pane["vizPaneColumns"][column_indices[0]]
        value_indices = pane_column.get("valueIndices") or pane_column.get("aliasIndices") or []
        values        = [decode_tableau_value(i, string_values) for i in value_indices]

        if values:
            table_columns[column_name] = values
            row_count = max(row_count, len(values))

    for column_name, values in list(table_columns.items()):
        if len(values) < row_count:
            table_columns[column_name] = values + [None] * (row_count - len(values))

    return pd.DataFrame(table_columns)


def get_total_percent_values(command_text):
    table = command_response_to_table(command_text)

    if table.empty:
        return {}

    total_rows = table[
        (table["Demographic Category"] == "Total") &
        (table["Demographic Group"].isin(["total", "Total"]))
    ]

    values = {}

    for _, row in total_rows.iterrows():
        response    = str(row.get("Response Description1", "")).strip()
        measure     = str(row.get("Measure Names", "")).lower()
        percent     = row.get("Measure Values")
        sample_size = row.get("Min. Sample Size (copy)") or row.get("Sample Size (copy)")

        if sample_size is not None and "sample_size" not in values:
            values["sample_size"] = sample_size

        if not response or percent is None: continue

        if "percent" in measure and "lower" not in measure and "upper" not in measure:
            values[f"total_{clean_column_name(response)}_percent"] = percent

    return values
    


# ── MAIN ─────────────────────────────────────────────────────────────────────

rows = []
features = load_feature_list()

print(f"Loaded {len(features)} features from {FEATURE_FILE}", flush=True)

with sync_playwright() as playwright:
    browser = playwright.chromium.launch(
        headless=True,
        args=["--disable-blink-features=AutomationControlled"],
    )
    context = browser.new_context(
        viewport={"width": 1280, "height": 1400},
        user_agent=(
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/148.0.0.0 Safari/537.36"
        ),
        locale="en-US",
    )
    page = context.new_page()

    print("Opening dashboard...", flush=True)
    page.goto(URL, wait_until="domcontentloaded", timeout=60000)

    nav_button = page.get_by_role("button", name=f"Navigate to '{DASHBOARD}'")
    nav_button.wait_for(state="visible", timeout=30000)

    print("Opening data table...", flush=True)
    try:
        with page.expect_response(lambda r: "/commands/tabdoc/" in r.url, timeout=15000):
            nav_button.click()
    except TimeoutError:
        pass

    wait_for_tableau_ready(page)

    print("Getting topics...", flush=True)
    topics = open_dropdown(page, re.compile(r"Select Health Topic", re.I))

    if MAX_TOPICS is not None:
        topics = topics[:MAX_TOPICS]

    for area in AREAS:
        area_number = re.sub(r"[^\d]", "", area)

        print(f"\nArea: {area}", flush=True)

        # Area depends on Geographic Category, so keep these adjacent.
        choose_dropdown_value(
            page,
            re.compile(r"Select Geographic Category", re.I),
            GEOGRAPHIC_CATEGORY,
        )

        choose_dropdown_value(
            page,
            re.compile(r"Select Area", re.I),
            area,
        )

        # Year can stay fixed while looping through this PHR.
        choose_dropdown_value(page, re.compile(r"Select Year", re.I), YEAR)

        for topic in topics:
            print(f"  Topic: {topic}", flush=True)

            choose_dropdown_value(
                page,
                re.compile(r"Select Health Topic", re.I),
                topic,
            )

            questions = open_dropdown(
                page,
                re.compile(r"(Question Asked|Description1)", re.I),
            )

            if MAX_QUESTIONS_PER_TOPIC is not None:
                questions = questions[:MAX_QUESTIONS_PER_TOPIC]

            for question in questions:
                feature = find_matching_feature(topic, question, features)

                if feature is None:
                    print(f"    Skipping, not in feature file: {question}", flush=True)
                    continue

                print(f"    Feature: {feature}", flush=True)
                print(f"      Question: {question}", flush=True)

                response_text = choose_dropdown_value(
                    page,
                    re.compile(r"(Question Asked|Description1)", re.I),
                    question,
                )

                if not response_text or "vqlCmdResponse" not in response_text:
                    print("      No Tableau response; skipping row", flush=True)
                    continue

                total_values = get_total_percent_values(response_text)

                row = {
                    "feature": feature,
                    "area": area_number,
                    "question": question,
                }
                row.update(total_values)
                rows.append(row)

    browser.close()

df = pd.DataFrame(rows).drop_duplicates()
df.to_csv(OUTPUT_CSV, index=False)
print(f"\nSaved {len(df)} rows to {OUTPUT_CSV}", flush=True)