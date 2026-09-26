#!/usr/bin/env python3
"""Run the suite and prepend the result to the readme's Test status table.

Run this AFTER committing the work, so HEAD is the commit being reported on, then
commit the readme as its own `docs:` commit. Never fold it in with `git commit
--amend`: amending rehashes the commit, which orphans the SHA the row just
recorded and makes the row unreproducible.

Refuses to record anything when the working tree is dirty. A row names a commit,
and on a dirty tree that commit describes something other than what actually ran,
which makes the row a false claim rather than a weak one.

Usage: test-record.py [--dry-run]
"""

from __future__ import annotations

import re
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

ROOT = Path(__file__).resolve().parent.parent
README = ROOT / "readme.org"
MARKER = "|---+---+---+---|"
TZ = ZoneInfo("Europe/Athens")
SUITE = ("emacs", "--batch", "-Q", "-l", "ert", "-l", "test/run-tests.el",
         "-f", "ert-run-tests-batch-and-exit")


def git(*args: str) -> str:
    return subprocess.run(
        ("git", *args), cwd=ROOT, capture_output=True, text=True, check=True
    ).stdout.strip()


def working_tree_is_clean() -> bool:
    return git("status", "--porcelain") == ""


def run_suite() -> tuple[int, int, float]:
    """Returns (passed, failed, seconds). Raises if ERT emits no summary."""
    start = time.monotonic()
    proc = subprocess.run(SUITE, cwd=ROOT, capture_output=True, text=True)
    seconds = time.monotonic() - start
    tail = (proc.stdout + proc.stderr).strip().splitlines()

    # Parse the summary rather than trusting the exit code: a run that selects
    # no tests also exits 0, and recording "0 failed" for it would be a lie.
    for line in reversed(tail):
        summary = re.match(r"Ran (\d+) tests?, (\d+) results? as expected, (\d+) unexpected", line)
        if summary:
            ran, expected, unexpected = map(int, summary.groups())
            if ran == 0:
                raise SystemExit("the suite ran no tests, refusing to record")
            return expected, unexpected, seconds

    raise SystemExit(f"no ERT summary line found; last output:\n{chr(10).join(tail[-15:])}")


def humanise(seconds: float) -> str:
    minutes, secs = divmod(round(seconds), 60)
    return f"{minutes}m {secs:02d}s" if minutes else f"{secs}s"


def main() -> None:
    dry_run = "--dry-run" in sys.argv

    if not working_tree_is_clean():
        raise SystemExit(
            "working tree is dirty, refusing to record.\n"
            "A row names a commit. On a dirty tree that commit is not what ran.\n"
            "Commit or stash first."
        )

    sha = git("rev-parse", "--short", "HEAD")
    passed, failed, seconds = run_suite()
    if failed:
        raise SystemExit(f"{failed} tests failed, refusing to record a red run")
    stamp = datetime.now(TZ).strftime("%Y-%m-%d %H:%M")
    row = f"| {stamp} | ~{sha}~ | {passed} passed, {failed} failed | {humanise(seconds)} |"

    print(row)
    if dry_run:
        return

    text = README.read_text()
    if MARKER not in text:
        raise SystemExit(f"could not find the Test status table header in {README}")

    README.write_text(text.replace(MARKER, f"{MARKER}\n{row}", 1))
    print(f"recorded in {README.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
