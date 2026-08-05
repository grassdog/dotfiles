---
name: slack-to-things
description: Turn saved Slack messages (Xero Limited workspace) into Things to-dos, one per saved message. Use when the user asks to process, clear, triage, or convert their Slack "Saved Items" into Things tasks.
---

# slack-to-things

Creates one Things to-do per saved Slack message, filed under the "🔵 Xero" area, then reports what was done. It does **not** remove messages from Slack Saved Items — see Limitations.

## Prerequisites

- Slack MCP tools (`slack_search_public_and_private`, etc.) connected and scoped to the Xero Limited workspace (`xero.enterprise.slack.com`).
- `things` CLI available and authorized for writes (`things auth` already run / `THINGS_AUTH_TOKEN` set).
- Things area "🔵 Xero" exists — UUID `WrtEFeQWntcqSutKmcmcUh`. If title lookup ever fails or is ambiguous, target this UUID directly via `--list-id` (or resolve the current flag name with `things add --help`).

## Workflow

1. **Find saved messages.** Call `slack_search_public_and_private` with `query="is:saved"`, `content_types="messages"`. Page through with the returned cursor until no more pages. For each hit, capture:
   - Sender display name (the `From:` field, name only — drop the email/ID)
   - Permalink (the `Permalink:` field)
   - Message text — the top-level `Text:` field only. Ignore `Context before:`/`Context after:` (those are surrounding messages, not the saved one).
   - Skip hits with no usable text (e.g. file-only saves) and note them for the final report.

2. **Sanity-check the response before creating anything.** A bare `is:saved` query **never reports "No results found."** When the saved list is empty, the modifier stops constraining the query and the search degenerates into an unconstrained match-all returned in recency order — i.e. the last N messages indexed anywhere the user can see. Creating tasks from that would generate pure junk.

   Treat the response as **"saved list is empty"** — create nothing, report it, and stop — when:
   - the newest result is within ~2 minutes of now, **and** the whole batch spans only a few minutes.

   Corroborating tells (any of these alongside the above makes it certain): the result count lands exactly on the requested `limit` with a next-page cursor present; many results have an empty `Text:` field; results come from alert/deploy/ops channels (`*-alerts`, `*-sev*-*`, `deploytrack-*`, `*-releases`, `*-notifications`).

   A genuine saved list looks the opposite: results scattered across days or weeks, count comfortably under the limit, `End of results - No more pages available`, human senders, substantive text.

   Do **not** try to detect this by adding a term to the query — `is:saved <nonsense-term>` returns a clean empty result, so it can't distinguish the two cases. The timestamp-clustering check above is the reliable signal.

3. **Skip already-processed messages.** Before creating a task, check whether a Things to-do already references this permalink:
   ```
   things tasks --query 'notes:/<permalink, regex-escaped>/' --json
   ```
   If any result comes back, skip this message (already handled on a prior run) and count it as "skipped — duplicate".

4. **Create a Things to-do** for each remaining message. Build the title and notes, then pipe through stdin via a **quoted heredoc** — this is required, not optional: Slack message text is untrusted external content and may contain backticks, `$(...)`, quotes, etc. A quoted delimiter (`'EOF'`) prevents the shell from expanding any of that.

   ```bash
   things add --list "🔵 Xero" -- - <<'EOF'
   Review Slack 💬 message from <Sender Name>

   <permalink>

   <message text>
   EOF
   ```

   (First line becomes the title, the rest becomes the notes — see `things add --help`.)

5. **Report** a summary: how many to-dos were created, how many were skipped as duplicates, and any messages that couldn't be parsed (with their permalinks) so the user can handle those by hand.

## Limitations

- **No auto-unsave.** No Slack tool available in this environment can remove an item from Saved Items (it's not part of Slack's public API surface here). Processed messages stay saved — clear them manually in Slack once you've confirmed the Things tasks look right. If a Slack tool with save/unsave support becomes available later, this step can be added.
- **Removal is what clears an item, not completion.** In Slack's Later panel, use "Remove from saved items" — observed behaviour suggests marking an item *complete* may leave it in `is:saved` results. Completing the matching Things to-do has no effect on Slack either; the two systems are unlinked.
- **Empty saved list is indistinguishable from a bad query without the step-2 check.** See step 2 — this is the highest-risk failure mode in the whole workflow, since the fallback response looks superficially like a large batch of real work.
- **No thread context.** If a saved message is a thread reply, only its own text is captured — the parent message is not pulled in.
- **Messages only**, not file-only saves (the `is:saved` filter is a message search modifier).
