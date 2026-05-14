#!/usr/bin/env bun

/**
 * NotePlan CLI
 *
 * A command-line interface for interacting with NotePlan notes.
 */

import { searchCommand } from "./commands/search";
import { listNotesCommand } from "./commands/list-notes";
import { getNoteCommand } from "./commands/get-note";
import { getNotesCommand } from "./commands/get-notes";
import { createNoteCommand } from "./commands/create-note";
import { appendToNoteCommand } from "./commands/append-to-note";

interface CliArgs {
  command?: string;
  args: string[];
  options: Record<string, string | boolean | undefined>;
}

function parseCliArgs(): CliArgs {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    showHelp();
    process.exit(0);
  }

  const command = args[0];
  const remainingArgs = args.slice(1);

  // Simple option parsing
  const options: Record<string, string | boolean | undefined> = {};
  const positionalArgs: string[] = [];

  for (let i = 0; i < remainingArgs.length; i++) {
    const arg = remainingArgs[i];

    if (arg.startsWith("--")) {
      const key = arg.slice(2);
      const nextArg = remainingArgs[i + 1];

      if (nextArg && !nextArg.startsWith("--")) {
        options[key] = nextArg;
        i++; // Skip next arg
      } else {
        options[key] = true;
      }
    } else {
      positionalArgs.push(arg);
    }
  }

  return {
    command,
    args: positionalArgs,
    options,
  };
}

function showHelp() {
  console.log(`
NotePlan CLI - Interact with NotePlan notes from the command line

Usage:
  noteplan <command> [options]

Commands:
  search <query>              Search for notes by title or content
    --start-date YYYY-MM-DD   Filter notes created on or after date
    --include-archive         Include @Archive folder in search (default: false)

  list-notes                  List all non-calendar notes (titles only)

  get-note <title>            Get a specific note by title

  get-notes                   Get calendar notes within a date range
    --type daily|weekly       Type of calendar notes
    --start-date YYYY-MM-DD   Start date (required)
    --end-date YYYY-MM-DD     End date (optional)

  create-note <title>         Create a new note
    --text <content>          Note content (required)
    --folder <folder>         Folder path relative to Notes/ (optional)

  append-to-note <title>      Append content to an existing note
    --text <content>          Content to append (required)

  help                        Show this help message

Examples:
  noteplan search "project ideas" --start-date 2024-01-01
  noteplan search "old notes" --include-archive
  noteplan get-note "Meeting Notes"
  noteplan get-notes --type daily --start-date 2024-12-01 --end-date 2024-12-31
  noteplan create-note "New Idea" --text "Content here" --folder "Projects"
  noteplan append-to-note "Todo List" --text "- New task"
`);
}

async function main() {
  try {
    const { command, args, options } = parseCliArgs();

    switch (command) {
      case "search": {
        const query = args[0];
        if (!query) {
          throw new Error("Search query is required");
        }
        await searchCommand({
          query,
          startDate: options["start-date"] as string | undefined,
          endDate: options["end-date"] as string | undefined,
          includeArchive: options["include-archive"] === true,
        });
        break;
      }

      case "list-notes":
        await listNotesCommand();
        break;

      case "get-note": {
        const title = args[0];
        if (!title) {
          throw new Error("Note title is required");
        }
        await getNoteCommand({ title });
        break;
      }

      case "get-notes": {
        const type = options["type"] as string;
        const startDate = options["start-date"] as string;

        if (!type) {
          throw new Error(
            "Note type is required (use --type daily|weekly|quarterly|yearly)",
          );
        }
        if (!startDate) {
          throw new Error(
            "Start date is required (use --start-date YYYY-MM-DD)",
          );
        }
        if (!["daily", "weekly", "quarterly", "yearly"].includes(type)) {
          throw new Error(
            "Invalid type. Must be: daily, weekly, quarterly, or yearly",
          );
        }

        await getNotesCommand({
          type: type as "daily" | "weekly" | "quarterly" | "yearly",
          startDate,
          endDate: options["end-date"] as string | undefined,
        });
        break;
      }

      case "create-note": {
        const title = args[0];
        const text = options["text"] as string;

        if (!title) {
          throw new Error("Note title is required");
        }
        if (!text) {
          throw new Error('Note text is required (use --text "content")');
        }

        await createNoteCommand({
          title,
          text,
          folder: options["folder"] as string | undefined,
        });
        break;
      }

      case "append-to-note": {
        const title = args[0];
        const text = options["text"] as string;

        if (!title) {
          throw new Error("Note title is required");
        }
        if (!text) {
          throw new Error('Text to append is required (use --text "content")');
        }

        await appendToNoteCommand({
          title,
          text,
        });
        break;
      }

      case "help":
      case "--help":
      case "-h":
        showHelp();
        break;

      default:
        console.error(`Unknown command: ${command}`);
        console.error('Run "noteplan help" for usage information');
        process.exit(1);
    }
  } catch (error) {
    console.error(
      JSON.stringify(
        {
          error: error instanceof Error ? error.message : "Unknown error",
        },
        null,
        2,
      ),
    );
    process.exit(1);
  }
}

main();
