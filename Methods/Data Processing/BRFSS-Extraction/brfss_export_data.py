"""
Scrape ALL Texas DSHS BRFSS health-survey percentages from a Tableau dashboard.

This program automates the DSHS "Data Table Builder" Tableau dashboard to pull
2014 BRFSS estimates for three Public Health Regions (PHR 1, 8, and 11). For
each region it walks every available Health Topic and every Question with a
Playwright browser, and intercepts Tableau's internal JSON responses to
reconstruct the data table (rather than scraping rendered HTML).

For each question it keeps the aggregate "Total" row (the point-estimate
percentages plus sample size). No feature-name matching is performed -- every
question and answer the dashboard exposes is written out, one CSV row per
area/topic/question, with one column per answer option. Any normalization or
mapping to canonical BRFSS variable names is left to downstream analysis.

CUSTOMIZATION (Signified with @TODO)
  * YEAR / AREAS: which survey year and which regions to scrape.
  * ANCHOR_TOPIC: if you change YEAR or AREAS and area selection
    fails, pick a topic that has data for all your target areas 
    (see the comment on ANCHOR_TOPIC).
  * MAX_TOPICS / MAX_QUESTIONS_PER_TOPIC : set to small integers (e.g. 2)
    for a quick test run before a full scrape.
  * OUTPUT_CSV : where the CSV is written; you MUST set this to a
    path on your own computer (see SETUP step 5).
"""
from pathlib import Path
import json
import re
import time

import pandas as pd
from playwright.sync_api import sync_playwright, TimeoutError

# CONFIGURATION @TODO
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

# @TODO
# The "Select Area" dropdown only lists regions that have data for the CURRENTLY
# selected Health Topic. So before switching the Area we first select an "anchor"
# topic that is known to have data for every target PHR. If you
# change YEAR or AREAS and the area selection fails, pick a different anchor.
ANCHOR_TOPIC = "Adult Immunizations"

# @TODO
MAX_TOPICS              = None
MAX_QUESTIONS_PER_TOPIC = None

# Output CSV: one row per area/topic/question.
# Saved next to this script by default (no editing needed). To put it elsewhere,
# change BASE_DIR or set OUTPUT_CSV to an absolute path.
BASE_DIR = Path(__file__).resolve().parent
OUTPUT_CSV = BASE_DIR / "brfss_2014_phr_1_8_11_all_questions.csv"
OUTPUT_CSV.parent.mkdir(parents=True, exist_ok=True)


def clean_column_name(value):
    # Convert Tableau's display-friendly response labels 
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


def choose_dropdown_value(page, button_name, value, skip_if_selected=True, require_table=False):
    wait_for_tableau_ready(page)

    dropdown_button = page.get_by_role("button", name=button_name).first
    dropdown_button.wait_for(state="visible", timeout=15000)
    dropdown_button.scroll_into_view_if_needed()

    if skip_if_selected and (dropdown_button.inner_text() or "").strip() == str(value).strip():
        return None

    option_locator = page.get_by_role("option", name=value, exact=True)

    def open_to_option():
        # Robust open with retries: don't trust a single force-click — Tableau
        # intermittently fails to render the option list, especially after many
        # prior interactions.
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
                return True
            except TimeoutError:
                continue
        return False

    # require_table=True (used for the Question filter) means we must capture the
    # Data Table response this selection triggers. The critical reliability point:
    # we WAIT for that response to actually arrive instead of
    # capturing only what shows up during a fixed wait. After many interactions
    # the dashboard gets sluggish and the response lands a few seconds late. We poll
    # the captured responses until one carries the worksheet zone, then retry the
    # whole selection (re-opening the dropdown) if it never came.
    response_text = None
    attempts = 2 if require_table else 1

    for attempt in range(attempts):
        if not open_to_option():
            raise RuntimeError(f"Could not open dropdown {button_name!r} to select {value!r}")

        captured = []
        def collect_tabdoc(response):
            try:
                if "/commands/tabdoc/" in response.url and response.request.method == "POST":
                    captured.append(response)
            except Exception:
                pass

        page.on("response", collect_tabdoc)
        try:
            option_locator.scroll_into_view_if_needed()
            option_locator.click()

            if require_table:
                deadline = time.time() + 30
                checked = 0
                while time.time() < deadline:
                    # Parse only responses we haven't looked at yet.
                    while checked < len(captured):
                        response = captured[checked]
                        checked += 1
                        try:
                            text = response.text()
                        except Exception:
                            continue
                        if "vqlCmdResponse" not in text:
                            continue
                        try:
                            if get_table_zone(json.loads(text)) is not None:
                                response_text = text
                        except Exception:
                            continue
                    if response_text:
                        break
                    page.wait_for_timeout(400)

            close_open_dropdowns(page)
            wait_for_tableau_ready(page)
        finally:
            page.remove_listener("response", collect_tabdoc)

        # Navigation selects (topic/geography/area/year) don't need the response body
        if not require_table or response_text:
            break

        # No Data Table response this round. Log what we did get
        commands = []
        for response in captured:
            try:
                commands.append(response.url.split("/commands/tabdoc/")[-1].split("?")[0])
            except Exception:
                pass
        print(f"      [retry {attempt + 1}] no Data Table zone in {len(captured)} response(s): {commands}", flush=True)
        page.wait_for_timeout(2500)

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


class TableauStringDict:
    """The cumulative string dictionary for a Tableau session.

    Tableau does NOT resend the full string dictionary on every filter change.
    It ships the dictionary in numbered segments: segment 0 is a full base, and
    higher-numbered segments are deltas appended after the current base. A single
    filter-change response usually carries only a delta (e.g. just segment "3"),
    whose viz value-indices are GLOBAL positions into base+deltas concatenated in
    order — so that response cannot be decoded on its own. This was the bug behind
    the blank rows: get_tableau_string_values read one response's lone delta
    segment and every index missed. We instead fold every response's segments into
    one growing dictionary and decode against the whole thing.
    """

    def __init__(self):
        self.segments = {}

    def update(self, command_response):
        application = (
            command_response.get("vqlCmdResponse", {})
            .get("layoutStatus", {})
            .get("applicationPresModel")
        )
        if not application:
            return

        data_segments = application.get("dataDictionary", {}).get("dataSegments", {})
        incoming = {}
        for key, segment in data_segments.items():
            try:
                index = int(key)
            except (TypeError, ValueError):
                continue
            values = []
            if segment:
                for column in segment.get("dataColumns", []):
                    if column.get("dataType") == "cstring":
                        values = column.get("dataValues", [])
                        break
            incoming[index] = values

        if not incoming:
            return

        if incoming.get(0):
            # A non-empty segment 0 is a fresh full base — it resets the whole
            # dictionary (later responses' deltas are relative to this new base).
            self.segments = {index: values for index, values in incoming.items() if values}
        else:
            # Delta: extend the current base. An explicitly empty segment clears it.
            for index, values in incoming.items():
                if values:
                    self.segments[index] = values
                elif index in self.segments:
                    self.segments[index] = []

    def values(self):
        flat = []
        for index in sorted(self.segments):
            flat.extend(self.segments[index])
        return flat


class SessionStringDict:
    """Folds every `/commands/tabdoc/` response in a page session into one
    cumulative TableauStringDict, in network arrival order.

    A persistent response listener (attached in open_data_table_builder) appends
    every tabdoc POST here; values() lazily merges any not-yet-folded responses
    and returns the full string list to decode the current table against.
    """

    def __init__(self):
        self._responses = []
        self._folded = 0
        self._dict = TableauStringDict()

    def add_response(self, response):
        try:
            if "/commands/tabdoc/" in response.url and response.request.method == "POST":
                self._responses.append(response)
        except Exception:
            pass

    def values(self):
        while self._folded < len(self._responses):
            response = self._responses[self._folded]
            self._responses[self._folded] = None  # release the buffered body
            self._folded += 1
            try:
                text = response.text()
            except Exception:
                continue
            if "vqlCmdResponse" not in text:
                continue
            try:
                self._dict.update(json.loads(text))
            except Exception:
                continue
        return self._dict.values()


# Reset to a fresh instance for each region (each region uses its own page).
SESSION = SessionStringDict()


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


def command_response_to_table(command_text, string_values=None):
    # Reconstruct the visible crosstab from Tableau's wire format: columns list
    # their pane/column index into a nested structure that holds the actual
    # value-index arrays, which we decode using the shared string dictionary.
    # string_values should be the SESSION-accumulated dictionary. We fall back to
    # this single response's dictionary only when no accumulated one is supplied.
    command_response = json.loads(command_text)
    if string_values is None:
        string_values = get_tableau_string_values(command_response)
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
    # Decode this response's table against the SESSION-accumulated string
    # dictionary so delta-only responses resolve correctly.
    table = command_response_to_table(command_text, string_values=SESSION.values())

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

# ── Incremental saving ──────────────────────────────────────────────────────
# We save after each region so a crash (or a flaky dashboard) doesn't throw away
# the regions already scraped, and a re-run can resume where it left off.

def save_rows(rows_to_save, output_path):
    """Merge a region's rows into OUTPUT_CSV, keeping one consistent column set.

    Each question exposes a DIFFERENT set of answer-option columns, so every
    region's DataFrame has a different column set/order. A plain append
    (mode="a", header written once) appends by POSITION, not by name, so the
    second region's values would be written under the first region's header and
    silently misaligned. Instead we read whatever is already on disk and
    pd.concat it with the new rows: concat aligns by column NAME and fills absent
    cells with NaN, so every row stays under the correct header. 

    Returns the number of new (deduplicated) rows contributed by this call.
    """
    if not rows_to_save:
        return 0

    new_df = pd.DataFrame(rows_to_save).drop_duplicates()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    if output_path.exists():
        # dtype=str: don't let pandas reinterpret values (e.g. area "1" -> 1) on
        # the round-trip; everything is written back out as text anyway.
        existing = pd.read_csv(output_path, dtype=str)
        combined = pd.concat([existing, new_df], ignore_index=True).drop_duplicates()
    else:
        combined = new_df

    # Write to a temp file and atomically swap it in. If the process dies
    # mid-write, the original file (with the earlier regions) is left intact
    # instead of being truncated — exactly what incremental saving is meant to
    # protect against.
    tmp_path = output_path.with_suffix(output_path.suffix + ".tmp")
    combined.to_csv(tmp_path, index=False)
    tmp_path.replace(output_path)
    return len(new_df)


def region_already_saved(output_path, area_number):
    """True if this region's rows are already in OUTPUT_CSV (resume support).

    A region is written in a single save_rows call AFTER all of its topics, so a
    region is either fully present or fully absent — there is no half-saved
    region to detect. Reads only the `area` column to stay fast.
    """
    if not output_path.exists():
        return False
    existing = pd.read_csv(output_path, usecols=["area"], dtype=str)
    return area_number in set(existing["area"])

# MAIN

rows = []

print(f"Output CSV: {OUTPUT_CSV}", flush=True)

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
        # the platform it claims doesn't need to match the machine you run on
        # it just presents the dashboard with non-automated-looking browser signature.
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
        region's tangled filter state. 
        """
        new_page = context.new_page()
        # Fold every tabdoc response (navigation + filter changes) into the
        # session string dictionary so delta-only responses can be decoded.
        new_page.on("response", SESSION.add_response)
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

        #double checks if region data is already in the CSV
        if region_already_saved(OUTPUT_CSV, area_number):
            print(f"\nArea: {area} — already in {OUTPUT_CSV.name}, skipping", flush=True)
            continue

        print(f"\nArea: {area}", flush=True)

        # Fresh page per region: start from the dashboard's clean default state
        # The string dictionary is per-page, so reset it before opening the new page.
        SESSION = SessionStringDict()
        page = open_data_table_builder()

        # ── Control ordering
        # The dashboard's controls are interdependent
        # so we must establish the topic FIRST, using an anchor topic that has
        # data for every target PHR, and only THEN layer geography -> area ->
        # year on top of it. Selecting geography/area/year does NOT reset the
        # topic, so the full context sticks and the Area list always contains
        # the target PHR.
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

        # Re-read the Health Topic options AFTER Area + Year are set. This
        # dropdown is context-dependent: its available options change with the
        # selected Area and Year. 
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
                page.wait_for_timeout(500)

                # Always re-fire the query for the question (skip_if_selected=False)
                # so we capture its Tableau data response even if this question
                # happens to already be the displayed one. require_table=True makes
                # the selection retry until the Data Table response is captured,
                # instead of accepting whichever tabdoc POST happened to land first.
                response_text = choose_dropdown_value(
                    page,
                    re.compile(r"(Question Asked|Description1)", re.I),
                    question,
                    skip_if_selected=False,
                    require_table=True,
                )

                if not response_text or "vqlCmdResponse" not in response_text:
                    print(f"      No Data Table response after retries; skipping: {question}", flush=True)
                    continue

                total_values = get_total_percent_values(response_text)

                row = {
                    "area": area_number,
                    "topic": topic,
                    "question": question,
                }
                row.update(total_values)
                rows.append(row)

        # Done with this region — close its tab before starting the next one.
        saved_count = save_rows(rows, OUTPUT_CSV)
        print(f" Saved {saved_count} rows for {area} to {OUTPUT_CSV}", flush=True)
        rows = []
        page.close()

    # Final safety flush — should be empty if every region saved successfully above.
    save_rows(rows, OUTPUT_CSV)
    print(f"\nDone. Output written incrementally to {OUTPUT_CSV}", flush=True)
    browser.close()
