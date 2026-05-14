/**
 * Type definitions for NotePlan CLI
 */

export type NoteType = "note" | "daily" | "weekly" | "quarterly" | "yearly";

export interface Note {
  id: string; // Unique identifier (file path relative to NotePlan root)
  title: string; // Note title
  folder: string; // Parent folder relative to root (e.g., "Notes/Projects")
  content?: string; // Note content (optional for list operations)
  created: string; // ISO 8601 date string
  modified: string; // ISO 8601 date string
  type: NoteType; // Type of note
}

export interface SearchOptions {
  query: string;
  startDate?: Date;
  endDate?: Date;
  includeContent?: boolean;
  includeArchive?: boolean;
}

export interface SearchResult extends Note {
  score: number; // Relevance score from search
}

export interface GetNotesOptions {
  type: "daily" | "weekly" | "quarterly" | "yearly";
  startDate: Date;
  endDate?: Date;
}

export interface CreateNoteOptions {
  title: string;
  text: string;
  folder?: string;
}

export interface AppendToNoteOptions {
  title: string;
  text: string;
}

export interface NotePlanPaths {
  root: string; // NotePlan root directory
  notes: string; // Regular notes folder
  calendar: string; // Calendar notes folder
}
