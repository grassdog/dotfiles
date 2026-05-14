/**
 * Date utilities for NotePlan CLI
 * Handles date parsing and calendar note filename generation
 */

import {
  format,
  startOfQuarter,
  startOfYear,
  subQuarters,
  subYears,
  parseISO,
  isValid,
  eachDayOfInterval,
  eachWeekOfInterval,
  getQuarter,
} from "date-fns";

/**
 * Generate NotePlan calendar note filenames
 */

export function toDailyNoteFilename(date: Date): string {
  return format(date, "yyyyMMdd") + ".md";
}

export function toWeeklyNoteFilename(date: Date): string {
  // NotePlan uses ISO week format: YYYY-Wnn
  const isoWeek = format(date, "II"); // ISO week number with leading zero
  const year = format(date, "RRRR"); // ISO week-numbering year
  return `${year}-W${isoWeek}.md`;
}

export function toQuarterlyNoteFilename(date: Date): string {
  const quarter = getQuarter(date);
  const year = format(date, "yyyy");
  return `${year}-Q${quarter}.md`;
}

export function toYearlyNoteFilename(date: Date): string {
  return format(date, "yyyy") + ".md";
}

/**
 * Generate all filenames for a date range based on note type
 */
export function getCalendarFilenames(
  type: "daily" | "weekly" | "quarterly" | "yearly",
  startDate: Date,
  endDate: Date,
): string[] {
  const filenames: string[] = [];

  switch (type) {
    case "daily": {
      const days = eachDayOfInterval({ start: startDate, end: endDate });
      return days.map(toDailyNoteFilename);
    }

    case "weekly": {
      const weeks = eachWeekOfInterval(
        { start: startDate, end: endDate },
        { weekStartsOn: 1 }, // Monday
      );
      return weeks.map(toWeeklyNoteFilename);
    }

    case "quarterly": {
      // Generate quarters manually since date-fns doesn't have eachQuarterOfInterval
      let current = startOfQuarter(startDate);
      const end = startOfQuarter(endDate);

      while (current <= end) {
        filenames.push(toQuarterlyNoteFilename(current));
        current = startOfQuarter(subQuarters(current, -1)); // Add 1 quarter
      }
      return filenames;
    }

    case "yearly": {
      // Generate years manually
      let current = startOfYear(startDate);
      const end = startOfYear(endDate);

      while (current <= end) {
        filenames.push(toYearlyNoteFilename(current));
        current = startOfYear(subYears(current, -1)); // Add 1 year
      }
      return filenames;
    }

    default:
      return [];
  }
}

/**
 * Parse a date string (ISO format: YYYY-MM-DD)
 */
export function parseDate(input: string): Date | null {
  const parsed = parseISO(input);
  return isValid(parsed) ? parsed : null;
}

/**
 * Format date as ISO string (YYYY-MM-DD)
 */
export function toISODateString(date: Date): string {
  return format(date, "yyyy-MM-dd");
}

/**
 * Format date as ISO 8601 string with time
 */
export function toISO8601String(date: Date): string {
  return date.toISOString();
}
