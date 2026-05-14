/**
 * NotePlan file system operations
 * Handles reading, writing, and scanning NotePlan notes
 */

import { join, basename, dirname, relative } from "path";
import { readdir, readFile, writeFile, mkdir, stat } from "fs/promises";
import { existsSync } from "fs";
import { homedir } from "os";
import type { Note, NotePlanPaths, NoteType } from "../types";
import { toISO8601String } from "./dates";

/**
 * Detect NotePlan documents directory
 * Tries common locations for NotePlan 3
 */
export function getNotePlanPaths(): NotePlanPaths {
  const home = homedir();

  // NotePlan 3 default location
  const possiblePaths = [
    join(
      home,
      "Library/Containers/co.noteplan.NotePlan3/Data/Library/Application Support/co.noteplan.NotePlan3",
    ),
  ];

  const envPath = process.env.NOTEPLAN_PATH;
  if (envPath) {
    possiblePaths.unshift(envPath);
  }

  for (const path of possiblePaths) {
    if (existsSync(path)) {
      return {
        root: path,
        notes: join(path, "Notes"),
        calendar: join(path, "Calendar"),
      };
    }
  }

  throw new Error(
    "Could not find NotePlan documents directory. " +
      "Set NOTEPLAN_PATH environment variable to specify location.",
  );
}

/**
 * Determine note type from filename and folder
 */
export function getNoteType(filename: string, folder: string): NoteType {
  // Calendar notes are in the Calendar folder
  if (folder.includes("Calendar")) {
    // Daily: YYYYMMDD.md
    if (/^\d{8}\.md$/.test(filename)) {
      return "daily";
    }

    // Weekly: YYYY-Wnn.md
    if (/^\d{4}-W\d{2}\.md$/.test(filename)) {
      return "weekly";
    }

    // Quarterly: YYYY-Qn.md
    if (/^\d{4}-Q[1-4]\.md$/.test(filename)) {
      return "quarterly";
    }

    // Yearly: YYYY.md
    if (/^\d{4}\.md$/.test(filename)) {
      return "yearly";
    }
  }

  return "note";
}

/**
 * Extract title from note content
 * First line of the note is typically the title (with # prefix for markdown)
 */
export function extractTitle(content: string, filename: string): string {
  const lines = content.split("\n");

  // Try to get first non-empty line
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.length > 0) {
      // Remove markdown heading prefix if present
      return trimmed.replace(/^#+\s*/, "");
    }
  }

  // Fallback to filename without extension
  return basename(filename, ".md");
}

/**
 * Read a note file and return Note object
 */
export async function readNote(
  filePath: string,
  paths: NotePlanPaths,
  includeContent: boolean = true,
): Promise<Note> {
  const content = includeContent ? await readFile(filePath, "utf-8") : "";
  const stats = await stat(filePath);

  const filename = basename(filePath);
  const folder = relative(paths.root, dirname(filePath));
  const noteType = getNoteType(filename, folder);

  // For calendar notes, use filename as title; for regular notes, extract from content
  let title: string;
  if (noteType !== "note") {
    title = basename(filename, ".md");
  } else {
    title = includeContent
      ? extractTitle(content, filename)
      : basename(filename, ".md");
  }

  const note: Note = {
    id: relative(paths.root, filePath),
    title,
    folder,
    created: toISO8601String(stats.birthtime),
    modified: toISO8601String(stats.mtime),
    type: noteType,
  };

  if (includeContent) {
    note.content = content;
  }

  return note;
}

/**
 * Recursively scan a directory for note files
 */
export async function scanDirectory(
  dirPath: string,
  paths: NotePlanPaths,
  includeContent: boolean = false,
  includeArchive: boolean = false,
): Promise<Note[]> {
  const notes: Note[] = [];

  try {
    const entries = await readdir(dirPath, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dirPath, entry.name);

      if (entry.isDirectory()) {
        // Skip hidden directories
        if (entry.name.startsWith(".")) continue;

        // Skip @Archive folder unless explicitly included
        if (entry.name === "@Archive" && !includeArchive) continue;

        // Always skip @Templates and @Trash folders
        if (entry.name === "@Templates" || entry.name === "@Trash") continue;

        // Recursively scan subdirectories
        const subNotes = await scanDirectory(
          fullPath,
          paths,
          includeContent,
          includeArchive,
        );
        notes.push(...subNotes);
      } else if (entry.isFile() && entry.name.endsWith(".md")) {
        // Skip hidden files
        if (entry.name.startsWith(".")) continue;

        try {
          const note = await readNote(fullPath, paths, includeContent);
          notes.push(note);
        } catch (error) {
          // Skip files that can't be read
          console.error(`Warning: Could not read ${fullPath}:`, error);
        }
      }
    }
  } catch (error) {
    console.error(`Warning: Could not scan directory ${dirPath}:`, error);
  }

  return notes;
}

/**
 * Get all regular notes (non-calendar notes)
 */
export async function getAllNotes(
  includeContent: boolean = false,
  includeArchive: boolean = false,
): Promise<Note[]> {
  const paths = getNotePlanPaths();

  if (!existsSync(paths.notes)) {
    return [];
  }

  return scanDirectory(paths.notes, paths, includeContent, includeArchive);
}

/**
 * Get all calendar notes (daily, weekly, quarterly, yearly)
 */
export async function getAllCalendarNotes(
  includeContent: boolean = false,
): Promise<Note[]> {
  const paths = getNotePlanPaths();

  if (!existsSync(paths.calendar)) {
    return [];
  }

  return scanDirectory(paths.calendar, paths, includeContent, false);
}

/**
 * Get calendar notes by filenames
 */
export async function getCalendarNotes(filenames: string[]): Promise<Note[]> {
  const paths = getNotePlanPaths();
  const notes: Note[] = [];

  for (const filename of filenames) {
    const filePath = join(paths.calendar, filename);

    if (existsSync(filePath)) {
      try {
        const note = await readNote(filePath, paths, true);
        notes.push(note);
      } catch (error) {
        console.error(`Warning: Could not read ${filePath}:`, error);
      }
    }
  }

  return notes;
}

/**
 * Find a note by exact title match
 */
export async function findNoteByTitle(title: string): Promise<Note | null> {
  const notes = await getAllNotes(false);

  // Try exact match first
  const exactMatch = notes.find(
    (note) => note.title.toLowerCase() === title.toLowerCase(),
  );

  if (exactMatch) {
    // Re-read with content
    const paths = getNotePlanPaths();
    const fullPath = join(paths.root, exactMatch.id);
    return readNote(fullPath, paths, true);
  }

  return null;
}

/**
 * Create a new note
 */
export async function createNote(
  title: string,
  content: string,
  folder?: string,
): Promise<Note> {
  const paths = getNotePlanPaths();

  // Construct full path
  let targetDir = paths.notes;
  if (folder && folder.trim().length > 0) {
    targetDir = join(paths.notes, folder);
  }

  // Create directory if it doesn't exist
  if (!existsSync(targetDir)) {
    await mkdir(targetDir, { recursive: true });
  }

  // Create filename from title
  const filename = `${title}.md`;
  const filePath = join(targetDir, filename);

  // Check if file already exists
  if (existsSync(filePath)) {
    throw new Error(`Note with title "${title}" already exists`);
  }

  // Write the file
  await writeFile(filePath, content, "utf-8");

  // Return the created note
  return readNote(filePath, paths, true);
}

/**
 * Append content to an existing note
 */
export async function appendToNote(
  title: string,
  content: string,
): Promise<Note> {
  const note = await findNoteByTitle(title);

  if (!note) {
    throw new Error(`Note with title "${title}" not found`);
  }

  const paths = getNotePlanPaths();
  const filePath = join(paths.root, note.id);

  // Read existing content
  const existingContent = await readFile(filePath, "utf-8");

  // Append new content with proper spacing
  const newContent = existingContent.trimEnd() + "\n" + content;

  // Write back
  await writeFile(filePath, newContent, "utf-8");

  // Return updated note
  return readNote(filePath, paths, true);
}
