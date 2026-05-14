# 🐻 Bear Notes Skill

> A Claude Code skill that gives Claude full access to your [Bear](https://bear.app) notes through Bear's native CLI.

Transform your personal knowledge base into an AI-powered assistant. Search, read, create, and update Bear notes seamlessly within Claude Code.

---

## ✨ Features

- 🔍 **Search notes** using Bear's full search syntax (`#tag`, `@today`, `@todo`, exact phrases, negation, etc.)
- 📅 **Date filters** via inline operators: `@last7days`, `@date(>2024-01-01)`, `@cdate(...)`
- 📖 **Read full content** with raw markdown preserved
- ✍️ **Create notes** with tags, titles, and `--if-not-exists` idempotency
- 🔄 **Edit notes** with find/replace, insert-before/after, append, prepend, or full overwrite
- 🏷️ **Manage tags** — list, add, remove, rename, delete
- 📌 **Pin / archive / trash / restore / open** notes
- 📎 **Attachments** — list, add, save, delete

## 📦 Requirements

- macOS with [Bear](https://bear.app) installed
- Bear's native CLI at `/Applications/Bear.app/Contents/MacOS/bearcli` (bundled with recent Bear builds)
- Claude Code

The skill performs **no network calls**. All operations run locally through `bearcli`, which talks to Bear's local database and the Bear app directly.

## 🚀 Usage

Place this directory at `~/.claude/skills/bear-notes/` (or wherever your Claude Code skills live). Then invoke it in Claude Code:

```
/skill bear-notes
```

Claude will use `bearcli` directly to:

1. ✅ Answer questions grounded in your notes
2. 🔎 Search with full Bear search syntax
3. 📝 Create new notes from conversation
4. ✏️ Edit existing notes (find/replace, append, overwrite)
5. 🏷️ Suggest tags based on your existing taxonomy

## 💡 Examples

**Search your notes:**

> "What notes do I have about Python?"

**Filter by date:**

> "Show me my work notes from the last 7 days"
> "Find notes I created today about meetings"
> "What did I write about Docker in October 2024?"

**Summarize:**

> "Summarize my notes about dotfiles"

**Edit:**

> "Append today's standup to my Daily Notes note"
> "Replace every 'WIP' with 'In Progress' in the Q1 planning note"

**Create:**

> "Save this code snippet to Bear with tags programming and python"

## 🛠️ Direct CLI Usage

`bearcli` is self-documenting:

```bash
alias bearcli=/Applications/Bear.app/Contents/MacOS/bearcli

bearcli help                 # overview + subcommand list
bearcli help <subcommand>    # detailed help for one command
bearcli help all             # full reference (long)
```

A few common invocations:

```bash
# Search — Bear's inline operators, not flags
bearcli search "Python" --format json
bearcli search "@last7days #work" --format json
bearcli search "@cdate(>2025-01-01) @cdate(<2025-02-01)" --format json
bearcli search "@today @todo" --format json

# Read
bearcli cat NOTE_ID
bearcli cat --title "Meeting Notes"
bearcli show NOTE_ID --format json --fields all

# Create
bearcli create "New Note" --content "Body" --tags "work,draft" --format json
echo "Body" | bearcli create "New Note" --format json

# Edit
bearcli append NOTE_ID --content "Extra paragraph"
bearcli edit NOTE_ID --find "TODO" --replace "DONE" --all
bearcli overwrite NOTE_ID --content "# Title\nNew body"

# Tags
bearcli tags list --format json
bearcli tags add NOTE_ID work projects
bearcli tags rename old-name new-name

# Lifecycle
bearcli pin add NOTE_ID global
bearcli archive NOTE_ID
bearcli trash NOTE_ID
bearcli restore NOTE_ID
bearcli open NOTE_ID --edit
```

### Output formats

- `--format tsv` (default) — tab-separated, no header
- `--format csv` — RFC 4180 with header
- `--format json` — structured JSON; errors also go to stdout as JSON

### Bear search syntax cheat sheet

| Operator                                          | Meaning                                    |
| ------------------------------------------------- | ------------------------------------------ |
| `keyword` / `"exact phrase"`                      | Text search                                |
| `-term`                                           | Exclude                                    |
| `#tag`, `!#tag`, `#*/tag`                         | Tag (with children / exact / subtags only) |
| `@today`, `@yesterday`, `@last7days`              | Modification date                          |
| `@date(YYYY-MM-DD)`, `@date(>...)`, `@date(<...)` | Modification date range                    |
| `@ctoday`, `@createdNdays`, `@cdate(...)`         | Creation date                              |
| `@todo`, `@done`, `@task`                         | Task state                                 |
| `@tagged`, `@untagged`, `@pinned`                 | State                                      |
| `@images`, `@files`, `@code`, `@attachments`      | Content type                               |
| `@title`                                          | Restrict text terms to titles              |
| `@wikilinks`, `@backlinks`                        | Link relationships                         |

Full reference: <https://bear.app/faq/how-to-search-notes-in-bear/>

## 🔒 Privacy

- 100% local — no external API calls
- `bearcli` reads/writes Bear's local database and uses Bear directly
- No API keys, no telemetry

## 📁 Files

```
bear-notes/
├── SKILL.md            # Skill definition that Claude Code loads
├── README.md           # This file
```

## 🐛 Troubleshooting

**`bearcli` not found at `/Applications/Bear.app/Contents/MacOS/bearcli`:** Ensure Bear is installed and recent enough to ship the CLI. Open Bear once to initialize its database.

**Search returns nothing:** Verify notes aren't archived or trashed (or pass `--location all`). Try broader terms. Confirm tag names with `bearcli tags list`.

**Write commands silently succeed:** That's expected. Mutating commands (`edit`, `append`, `overwrite`, `archive`, `trash`, etc.) print nothing on success — the exit code is the signal.

**Encrypted notes:** Content of encrypted notes is not accessible through the CLI by design.

## 📚 Related

- [Bear](https://bear.app) — the notes app
- [Claude Code](https://docs.claude.com/claude-code)

## 📄 License

MIT
