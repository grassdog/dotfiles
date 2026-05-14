/**
 * List notes command implementation
 */

import { getAllNotes } from '../lib/noteplan';

export async function listNotesCommand() {
  try {
    // Get all notes without content (faster)
    const notes = await getAllNotes(false);

    // Output results as JSON
    console.log(JSON.stringify(notes, null, 2));
  } catch (error) {
    console.error(JSON.stringify({
      error: error instanceof Error ? error.message : 'Unknown error',
    }, null, 2));
    process.exit(1);
  }
}
