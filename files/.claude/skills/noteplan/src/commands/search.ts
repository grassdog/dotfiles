/**
 * Search command implementation
 */

import { searchNotes } from "../lib/search";
import { parseDate } from "../lib/dates";

export interface SearchCommandOptions {
  query: string;
  startDate?: string;
  endDate?: string;
  includeArchive?: boolean;
}

export async function searchCommand(options: SearchCommandOptions) {
  try {
    const { query, startDate, endDate, includeArchive = false } = options;

    if (!query || query.trim().length === 0) {
      throw new Error("Search query is required");
    }

    // Parse dates if provided
    let startDateParsed: Date | undefined;
    let endDateParsed: Date | undefined;

    if (startDate) {
      const parsed = parseDate(startDate);
      if (!parsed) {
        throw new Error(`Invalid start date: ${startDate}`);
      }
      startDateParsed = parsed;
    }

    if (endDate) {
      const parsed = parseDate(endDate);
      if (!parsed) {
        throw new Error(`Invalid end date: ${endDate}`);
      }
      endDateParsed = parsed;
    }

    // Perform search
    const results = await searchNotes({
      query,
      startDate: startDateParsed,
      endDate: endDateParsed,
      includeContent: true,
      includeArchive,
    });

    // Output results as JSON
    console.log(JSON.stringify(results, null, 2));
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
