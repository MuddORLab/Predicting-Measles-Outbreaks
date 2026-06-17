#!/usr/bin/env python3
from pathlib import Path
from difflib import SequenceMatcher
import json
import re

from openpyxl import load_workbook
import pandas as pd
from playwright.sync_api import sync_playwright, TimeoutError

# CONFIGURATION 
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

# DSHS 2014 PHR BRFSS summary workbook; any PHR copy works — sheet names are the feature labels.
# Expected location: ~/Downloads/ (temporary; move to data/raw/brfss/ for reproducibility)
FEATURE_FILE = Path("/Users/sean/Downloads/2014_PHR2_BRFSS_Summary_Tables.xlsx")

# DSHS 2024 PHR BRFSS summary workbook; used only to normalize feature names to current terminology.
# Expected location: data/raw/brfss/
FEATURE_NAME_FILE = Path(
    "/Users/sean/Summer 2026 Research/Measles-Outbreak-and-Public-Policy-Reluctance/"
    "2026SU/data/raw/brfss/2024_PHR8_BRFSS_Summary_Tables.xlsx"
)

# When True, rows whose topic/question can't be matched to the 2024 naming workbook are dropped.
ONLY_USE_TARGET_FEATURE_NAMES = True

# The "Select Area" dropdown only lists regions that have data for the CURRENTLY
# selected Health Topic. So before switching the Area we first select an "anchor"
# topic that is known to have data for every target PHR — otherwise a leftover
# restrictive topic from the previous region (e.g. "Walking For Transportation",
# which has no PHR-level data) leaves the Area list empty and the area selection
# crashes. "Adult Immunizations" has data for PHR 1, 8, and 11 in 2014. If you
# change YEAR or AREAS and the area selection fails, pick a different anchor.
ANCHOR_TOPIC = "Adult Immunizations"

MAX_TOPICS              = None
MAX_QUESTIONS_PER_TOPIC = None

# Scraped output CSV; one row per feature/area/question combination.
# Expected location: data/brfss_export_data/
OUTPUT_CSV = Path("/Users/sean/Documents/Codex/2026-06-11/i-need-to-scrape-all-the/brfss_2014_phr_1_8_11_target_variables.csv")


# Words that appear in almost every health topic name and carry no discriminating signal.
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
    # Excel cells sometimes start with "Questionnaire:" or "Calculated Variable:"
    # as a label prefix; strip it so only the meaningful description remains.
    value = "" if value is None or pd.isna(value) else str(value)
    value = re.sub(r"^(questionnaire|calculated variable):\s*", "", value, flags=re.I)
    return value.strip()


def normalize_for_match(value):
    # Flatten surface-level variation before comparing: & and + are written both
    # ways across the dashboard and workbook; "yrs" and "year" refer to the same
    # thing; punctuation adds noise without meaning.
    value = clean_feature_text(value).lower()
    value = value.replace("&", " and ")
    value = value.replace("+", " plus ")
    value = re.sub(r"\byrs?\b", " year ", value)
    value = re.sub(r"[^a-z0-9]+", " ", value)
    value = re.sub(r"\bin the\b", " ", value)
    return " ".join(value.split())


def match_score(left, right):
    # Blended score: word-overlap ratio weighted heavier (0.7) than character
    # sequence similarity (0.3) because health topic names share key nouns
    # regardless of word order or surrounding filler.
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
    # Length >= 3 drops single-letter tokens and two-letter abbreviations that
    # slip past the stop-word list but still carry no discriminating signal.
    return {
        word
        for word in normalize_for_match(value).split()
        if len(word) >= 3 and word not in MATCH_STOP_WORDS
    }


def has_shared_important_word(left_texts, right_texts):
    # Cheap pre-filter: if two descriptions share no important word at all,
    # skip the more expensive match_score computation entirely.
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
        # Skip navigation-only sheets that don't correspond to a health feature.
        if sheet_name.lower() in ["contents", "index"]:
            continue

        sheet = workbook[sheet_name]
        # A3 and A5 typically hold the long question description and variable
        # label in DSHS summary workbooks — more text gives the matcher more signal.
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

    # Reject weak matches — a score below the threshold means the best candidate
    # still isn't similar enough to trust as the same feature.
    if best_score < minimum_score:
        return None

    return best_feature


def load_feature_list():
    # Source workbook: 2014 data whose sheet names are the canonical feature labels.
    source_features = load_features_from_workbook(FEATURE_FILE)

    # Target workbook: 2024 naming conventions. When present, output uses its
    # sheet names so the final CSV aligns with current DSHS terminology.
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
    # Concatenate topic + question so both the broad category and the specific
    # wording contribute to the match — either alone can be ambiguous.
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
    # Convert Tableau's display-friendly response labels (e.g. "Yes, During Pregnancy")
    # into safe, lowercase, underscore-separated identifiers for the CSV header.
    value = str(value).lower()
    value = "".join(char if char.isalnum() else "_" for char in value)
    return "_".join(part for part in value.split("_") if part)


def close_open_dropdowns(page):
    # Tableau sometimes leaves a dropdown visually open after an interaction;
    # pressing Escape closes it so subsequent clicks land on the right target.
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

    # Robust open with retries (same reasoning as choose_dropdown_value: don't
    # trust a single force-click — Tableau intermittently fails to render the
    # option list, especially after many prior interactions).
    values = []
    for _ in range(4):
        try:
            page.keyboard.press("Escape")
        except Exception:
            pass
        page.wait_for_timeout(400)
        try:
            dropdown_button.click(force=True, timeout=15000)
        except Exception:
            pass
        try:
            page.get_by_role("option").first.wait_for(state="visible", timeout=6000)
            values = page.get_by_role("option").all_inner_texts()
            break
        except TimeoutError:
            continue

    try:
        page.keyboard.press("Escape")
        page.get_by_role("option").first.wait_for(state="hidden", timeout=5000)
    except Exception:
        pass
    return [value.strip() for value in values if value.strip()]


def choose_dropdown_value(page, button_name, value, skip_if_selected=True):
    wait_for_tableau_ready(page)

    dropdown_button = page.get_by_role("button", name=button_name).first
    dropdown_button.wait_for(state="visible", timeout=15000)
    dropdown_button.scroll_into_view_if_needed()

    if skip_if_selected and (dropdown_button.inner_text() or "").strip() == str(value).strip():
        return None

    option_locator = page.get_by_role("option", name=value, exact=True)

    opened = False
    for _ in range(4):
        try:
            page.keyboard.press("Escape")
        except Exception:
            pass
        page.wait_for_timeout(400)
        try:
            dropdown_button.click(force=True, timeout=15000)
        except Exception:
            pass
        try:
            option_locator.wait_for(state="visible", timeout=6000)
            opened = True
            break
        except TimeoutError:
            continue
    if not opened:
        raise RuntimeError(f"Could not open dropdown {button_name!r} to select {value!r}")

    response_text = None
    try:
        # Intercept the POST that Tableau fires when a filter value changes —
        # that response contains the updated viz data we parse into a table.
        with page.expect_response(
            lambda response: "/commands/tabdoc/" in response.url and response.request.method == "POST",
            timeout=15000
        ) as response_info:
            option_locator.scroll_into_view_if_needed()
            option_locator.click()
        response_text = response_info.value.text()
    except TimeoutError:
        pass

    close_open_dropdowns(page)
    wait_for_tableau_ready(page)
    return response_text


def get_tableau_string_values(command_response):
    # Tableau sends all string values in a shared dictionary to avoid repeating
    # identical strings across columns. Rows reference values by index into this list.
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
    # A Tableau dashboard response contains multiple zones (filters, titles, charts).
    # We specifically want the zone that renders the data table worksheet.
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
    # Negative indices are Tableau's "alias" references — the absolute position
    # is the same, the sign just signals the value came from the alias dictionary.
    if index is None: return None
    position = abs(index) - 1 if index < 0 else index
    return string_values[position] if 0 <= position < len(string_values) else None


def command_response_to_table(command_text):
    # Reconstruct the visible crosstab from Tableau's wire format: columns list
    # their pane/column index into a nested structure that holds the actual
    # value-index arrays, which we decode using the shared string dictionary.
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

    # Pad shorter columns so every column has the same length before creating the DataFrame.
    for column_name, values in list(table_columns.items()):
        if len(values) < row_count:
            table_columns[column_name] = values + [None] * (row_count - len(values))

    return pd.DataFrame(table_columns)


def get_total_percent_values(command_text):
    table = command_response_to_table(command_text)

    if table.empty:
        return {}

    # Filter to the aggregate "Total" row — all other demographic breakdowns
    # (by age, sex, race, etc.) are not needed for this analysis.
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

        # Capture sample size once; it's the same for every response option in a question.
        if sample_size is not None and "sample_size" not in values:
            values["sample_size"] = sample_size

        if not response or percent is None: continue

        # Skip confidence-interval columns — we only want the point estimate.
        if "percent" in measure and "lower" not in measure and "upper" not in measure:
            values[f"total_{clean_column_name(response)}_percent"] = percent

    return values



# MAIN

rows = []
features = load_feature_list()

print(f"Loaded {len(features)} features from {FEATURE_FILE}", flush=True)

with sync_playwright() as playwright:
    # IMPORTANT: run headed. In headless Chromium, Tableau's quick-filter
    # dropdowns (especially the cascading "Select Area" list) frequently fail to
    # populate, which makes the PHR-8 area selection time out. A real viewport
    # renders them reliably. Verified by hand that the headed flow selects every
    # PHR correctly.
    browser = playwright.chromium.launch(
        headless=False,
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
    def open_data_table_builder():
        """Open a FRESH page on the Data Table Builder dashboard.

        Each PHR gets its own clean page so it never inherits the previous
        region's tangled filter state. That leftover state -- a "dead" topic with
        no PHR data, a parenthesized/invalid area -- is what made the 2nd and 3rd
        regions fail while the 1st (which always started clean) worked. Starting
        fresh makes every region behave like the first one.
        """
        new_page = context.new_page()
        print("Opening dashboard...", flush=True)
        new_page.goto(URL, wait_until="domcontentloaded", timeout=60000)
        nav_button = new_page.get_by_role("button", name=f"Navigate to '{DASHBOARD}'")
        nav_button.wait_for(state="visible", timeout=30000)
        print("Opening data table...", flush=True)
        try:
            with new_page.expect_response(lambda r: "/commands/tabdoc/" in r.url, timeout=15000):
                nav_button.click()
        except TimeoutError:
            pass
        wait_for_tableau_ready(new_page)
        return new_page

    for area in AREAS:
        area_number = re.sub(r"[^\d]", "", area)

        print(f"\nArea: {area}", flush=True)

        # Fresh page per region: start from the dashboard's clean default state
        # instead of inheriting the previous PHR's filter mess.
        page = open_data_table_builder()

        # ── Control ordering (this is what fixes the PHR-switch crash) ───────
        # The dashboard's controls are interdependent:
        #   * The "Select Area" list only contains regions that have data for
        #     the CURRENTLY selected Health Topic. The previous PHR's loop ends
        #     on whatever topic came last (e.g. "Walking For Transportation",
        #     which has no PHR-level data), so the Area list is empty and the
        #     next region can't be selected.
        #   * Changing the Health Topic RESETS Geographic Category and Year to
        #     their defaults whenever the current topic has no data for the
        #     current geography. (A valid->valid topic change keeps them.)
        #
        # So we must establish the topic FIRST, using an anchor topic that has
        # data for every target PHR, and only THEN layer geography -> area ->
        # year on top of it. Selecting geography/area/year does NOT reset the
        # topic, so the full context sticks and the Area list always contains
        # the target PHR.
        # The Area filter's option list repopulates ASYNCHRONOUSLY after the
        # topic/geography change — opening it too soon catches an empty list,
        # which is what made the PHR-8 selection fail. So we (re)establish the
        # PHR-exposing context, wait for the cascade to settle, then select the
        # area, retrying the whole thing if the option still isn't there yet.
        def establish_phr_context():
            choose_dropdown_value(page, re.compile(r"Select Health Topic", re.I), ANCHOR_TOPIC)
            choose_dropdown_value(page, re.compile(r"Select Geographic Category", re.I), GEOGRAPHIC_CATEGORY)
            wait_for_tableau_ready(page)
            page.wait_for_timeout(3500)  # let the cascading Area domain repopulate

        establish_phr_context()
        for attempt in range(3):
            try:
                choose_dropdown_value(page, re.compile(r"Select Area", re.I), area)
                break
            except RuntimeError:
                if attempt == 2:
                    raise
                print(f"    Area not ready yet; re-establishing context (retry {attempt + 1})", flush=True)
                page.wait_for_timeout(2500)
                establish_phr_context()

        choose_dropdown_value(page, re.compile(r"Select Year", re.I), YEAR)

        # ── Context-dependent topic-list fix ─────────────────────────────────
        # Re-read the Health Topic options AFTER Area + Year are set. This
        # dropdown is context-dependent: its available options change with the
        # selected Area and Year. Reading it once up front (under the default
        # Metro / 2023 context) and then trying to pick a topic that has no data
        # for this PHR + 2014 makes the option wait_for() below time out and
        # crash (e.g. "Actions to Control High Blood Pressure"). Reading it here
        # lists only the topics that actually exist for this area + year.
        print("  Getting topics for this area...", flush=True)
        topics = open_dropdown(page, re.compile(r"Select Health Topic", re.I))
        if MAX_TOPICS is not None:
            topics = topics[:MAX_TOPICS]


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

                # Always re-fire the query for the question (skip_if_selected=False)
                # so we capture its Tableau data response even if this question
                # happens to already be the displayed one.
                response_text = choose_dropdown_value(
                    page,
                    re.compile(r"(Question Asked|Description1)", re.I),
                    question,
                    skip_if_selected=False,
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

        # Done with this region — close its tab before starting the next one.
        page.close()

    browser.close()

# Deduplicate in case the same feature/area/question was captured more than once
# (can happen if a question appears under multiple topics).
df = pd.DataFrame(rows).drop_duplicates()
df.to_csv(OUTPUT_CSV, index=False)
print(f"\nSaved {len(df)} rows to {OUTPUT_CSV}", flush=True)