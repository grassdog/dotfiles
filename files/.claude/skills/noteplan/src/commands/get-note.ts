/**
 * Get note command implementation
 */

import { findNoteByTitle } from '../lib/noteplan';

export interface GetNoteCommandOptions {
  title: string;
}

export async function getNoteCommand(options: GetNoteCommandOptions) {
  try {
    const { title } = options;

    if (!title || title.trim().length === 0) {
      throw new Error('Note title is required');
    }

    // Find note by title
    const note = await findNoteByTitle(title);

    if (!note) {
      throw new Error(`Note with title "${title}" not found`);
    }

    // Output result as JSON
    console.log(JSON.stringify(note, null, 2));
  } catch (error) {
    console.error(JSON.stringify({
      error: error instanceof Error ? error.message : 'Unknown error',
    }, null, 2));
    process.exit(1);
  }
}
