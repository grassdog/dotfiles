/**
 * Create note command implementation
 */

import { createNote } from '../lib/noteplan';
import { sanitizeTitle, sanitizeFolderPath } from '../lib/sanitize';

export interface CreateNoteCommandOptions {
  title: string;
  text: string;
  folder?: string;
}

export async function createNoteCommand(options: CreateNoteCommandOptions) {
  try {
    const { title, text, folder } = options;

    // Validate required parameters
    if (!title || title.trim().length === 0) {
      throw new Error('Note title is required');
    }

    if (!text) {
      throw new Error('Note text is required (use --text)');
    }

    // Sanitize title and folder
    const sanitizedTitle = sanitizeTitle(title);
    const sanitizedFolder = folder ? sanitizeFolderPath(folder) : undefined;

    // Create the note
    const note = await createNote(sanitizedTitle, text, sanitizedFolder);

    // Output result as JSON
    console.log(JSON.stringify(note, null, 2));
  } catch (error) {
    console.error(JSON.stringify({
      error: error instanceof Error ? error.message : 'Unknown error',
    }, null, 2));
    process.exit(1);
  }
}
