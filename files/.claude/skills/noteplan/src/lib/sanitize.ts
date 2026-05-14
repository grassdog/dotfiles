/**
 * Filename sanitization utilities for NotePlan CLI
 * Ensures titles are safe for filesystem usage
 */

/**
 * Characters that are not allowed in filenames on most filesystems
 */
const ILLEGAL_CHARS = /[<>:"/\\|?*\x00-\x1F]/g;

/**
 * Characters that should be replaced with dashes for readability
 */
const WHITESPACE = /\s+/g;

/**
 * Maximum filename length (conservative to work across filesystems)
 */
const MAX_LENGTH = 255;

/**
 * Sanitize a note title for use as a filename
 *
 * @param title - The note title to sanitize
 * @returns A filesystem-safe filename without extension
 */
export function sanitizeTitle(title: string): string {
  if (!title || title.trim().length === 0) {
    throw new Error("Title cannot be empty");
  }

  let sanitized = title.trim();

  // Remove illegal characters
  sanitized = sanitized.replace(ILLEGAL_CHARS, "");

  // Replace multiple whitespace with single space
  sanitized = sanitized.replace(WHITESPACE, " ");

  // Remove leading/trailing dots (problematic on some systems)
  sanitized = sanitized.replace(/^\.+|\.+$/g, "");

  // Truncate to max length
  if (sanitized.length > MAX_LENGTH) {
    sanitized = sanitized.substring(0, MAX_LENGTH).trim();
  }

  // Final check - if sanitization removed everything, throw error
  if (sanitized.length === 0) {
    throw new Error("Title contains only invalid characters");
  }

  return sanitized;
}

/**
 * Sanitize a folder path for filesystem usage
 *
 * @param folder - The folder path to sanitize
 * @returns A filesystem-safe folder path
 */
export function sanitizeFolderPath(folder: string): string {
  if (!folder || folder.trim().length === 0) {
    return "";
  }

  // Split by path separator, sanitize each part, rejoin
  const parts = folder.split("/").filter((part) => part.length > 0);
  const sanitizedParts = parts
    .map((part) => {
      // Apply similar rules as title but keep dots for folder names
      let sanitized = part.trim();
      sanitized = sanitized.replace(ILLEGAL_CHARS, "");
      sanitized = sanitized.replace(WHITESPACE, " ");

      // Remove leading/trailing dots
      sanitized = sanitized.replace(/^\.+|\.+$/g, "");

      return sanitized;
    })
    .filter((part) => part.length > 0);

  return sanitizedParts.join("/");
}

/**
 * Check if a path attempts to traverse outside the intended directory
 *
 * @param path - The path to check
 * @returns true if the path is safe, false if it contains traversal
 */
export function isPathSafe(path: string): boolean {
  // Check for path traversal attempts
  const normalized = path.replace(/\\/g, "/");

  // Reject absolute paths
  if (normalized.startsWith("/")) {
    return false;
  }

  // Reject paths with .. traversal
  if (normalized.includes("../") || normalized.includes("/..")) {
    return false;
  }

  // Reject paths starting with ..
  if (normalized.startsWith("..")) {
    return false;
  }

  return true;
}

/**
 * Create a full note filename from a title
 *
 * @param title - The note title
 * @returns Sanitized filename with .md extension
 */
export function createNoteFilename(title: string): string {
  const sanitized = sanitizeTitle(title);
  return `${sanitized}.md`;
}

/**
 * Validate and sanitize a complete note path
 *
 * @param folder - The folder path (can be empty)
 * @param title - The note title
 * @returns A safe, complete relative path with filename
 */
export function createNotePath(
  folder: string | undefined,
  title: string,
): string {
  const filename = createNoteFilename(title);

  if (!folder || folder.trim().length === 0) {
    return filename;
  }

  const sanitizedFolder = sanitizeFolderPath(folder);

  if (!isPathSafe(sanitizedFolder)) {
    throw new Error("Folder path contains invalid traversal");
  }

  return sanitizedFolder ? `${sanitizedFolder}/${filename}` : filename;
}
