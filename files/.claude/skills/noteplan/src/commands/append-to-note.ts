/**
 * Append to note command implementation
 */

import { appendToNote } from '../lib/noteplan';

export interface AppendToNoteCommandOptions {
  title: string;
  text: string;
}

export async function appendToNoteCommand(options: AppendToNoteCommandOptions) {
  try {
    const { title, text } = options;

    // Validate required parameters
    if (!title || title.trim().length === 0) {
      throw new Error('Note title is required');
    }

    if (!text) {
      throw new Error('Text to append is required (use --text)');
    }

    // Append to the note
    const note = await appendToNote(title, text);

    // Output result as JSON
    console.log(JSON.stringify(note, null, 2));
  } catch (error) {
    console.error(JSON.stringify({
      error: error instanceof Error ? error.message : 'Unknown error',
    }, null, 2));
    process.exit(1);
  }
}
