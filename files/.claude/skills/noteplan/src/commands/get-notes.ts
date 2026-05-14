/**
 * Get notes command implementation (for calendar notes)
 */

import { getCalendarNotes } from "../lib/noteplan";
import { parseDate, getCalendarFilenames } from "../lib/dates";

export interface GetNotesCommandOptions {
  type: "daily" | "weekly" | "quarterly" | "yearly";
  startDate: string;
  endDate?: string;
}

export async function getNotesCommand(options: GetNotesCommandOptions) {
  try {
    const { type, startDate, endDate } = options;

    // Validate type
    if (!["daily", "weekly", "quarterly", "yearly"].includes(type)) {
      throw new Error(
        `Invalid note type: ${type}. Must be one of: daily, weekly, quarterly, yearly`,
      );
    }

    // Parse start date
    if (!startDate) {
      throw new Error("Start date is required");
    }

    const startDateParsed = parseDate(startDate);
    if (!startDateParsed) {
      throw new Error(`Invalid start date: ${startDate}`);
    }

    // Parse end date (optional, defaults to today)
    let endDateParsed: Date;
    if (endDate) {
      const parsed = parseDate(endDate);
      if (!parsed) {
        throw new Error(`Invalid end date: ${endDate}`);
      }
      endDateParsed = parsed;
    } else {
      // Default to today if no end date specified
      endDateParsed = new Date();
    }

    // Generate filenames for the date range
    const filenames = getCalendarFilenames(
      type,
      startDateParsed,
      endDateParsed,
    );

    // Get the calendar notes
    const notes = await getCalendarNotes(filenames);

    // Output results as JSON
    console.log(JSON.stringify(notes, null, 2));
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
