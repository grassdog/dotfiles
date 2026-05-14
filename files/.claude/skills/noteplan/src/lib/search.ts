/**
 * Search utilities for NotePlan CLI
 * Uses MiniSearch for full-text fuzzy search
 */

import MiniSearch from "minisearch";
import type { Note, SearchOptions, SearchResult } from "../types";
import { getAllNotes, getAllCalendarNotes } from "./noteplan";

interface SearchableNote extends Note {
  id: string;
}

/**
 * Create and configure a MiniSearch index for notes
 */
function createSearchIndex(): MiniSearch<SearchableNote> {
  return new MiniSearch<SearchableNote>({
    fields: ["title", "content", "folder"], // Fields to index for search
    storeFields: ["id", "title", "folder", "created", "modified", "type"], // Fields to return in results
    searchOptions: {
      boost: {
        title: 3, // Title matches are 3x more important
        folder: 1, // Folder matches have normal weight
        content: 1, // Content matches have normal weight
      },
      fuzzy: 0.2, // Allow fuzzy matching with 20% tolerance
      prefix: true, // Enable prefix matching (e.g., "proj" matches "project")
    },
  });
}

/**
 * Search notes with MiniSearch
 */
export async function searchNotes(
  options: SearchOptions,
): Promise<SearchResult[]> {
  const {
    query,
    startDate,
    endDate,
    includeContent = false,
    includeArchive = false,
  } = options;

  if (!query || query.trim().length === 0) {
    throw new Error("Search query cannot be empty");
  }

  // Get all notes with content for indexing (both regular and calendar notes)
  const regularNotes = await getAllNotes(true, includeArchive);
  const calendarNotes = await getAllCalendarNotes(true);
  const notes = [...regularNotes, ...calendarNotes];

  // Create and populate search index
  const searchIndex = createSearchIndex();
  searchIndex.addAll(notes as SearchableNote[]);

  // Perform search
  const rawResults = searchIndex.search(query, {
    boost: { title: 3, folder: 1, content: 1 },
    fuzzy: 0.2,
    prefix: true,
  });

  // Convert to SearchResult format
  let results: SearchResult[] = rawResults.map((result) => {
    const note = notes.find((n) => n.id === result.id);

    if (!note) {
      throw new Error(`Note ${result.id} not found in search results`);
    }

    const searchResult: SearchResult = {
      ...note,
      score: result.score,
    };

    // Optionally remove content from results
    if (!includeContent) {
      delete searchResult.content;
    }

    return searchResult;
  });

  // Filter by date range if specified
  if (startDate) {
    results = results.filter((note) => {
      const noteDate = new Date(note.created);
      return noteDate >= startDate;
    });
  }

  if (endDate) {
    results = results.filter((note) => {
      const noteDate = new Date(note.created);
      return noteDate <= endDate;
    });
  }

  // Sort by score (highest first)
  results.sort((a, b) => b.score - a.score);

  return results;
}

/**
 * Simple search in notes by title only (faster, no indexing needed)
 */
export async function searchNotesByTitle(query: string): Promise<Note[]> {
  const notes = await getAllNotes(false);
  const lowerQuery = query.toLowerCase();

  return notes
    .filter((note) => note.title.toLowerCase().includes(lowerQuery))
    .sort((a, b) => {
      // Sort by how early the match appears in the title
      const aIndex = a.title.toLowerCase().indexOf(lowerQuery);
      const bIndex = b.title.toLowerCase().indexOf(lowerQuery);
      return aIndex - bIndex;
    });
}
