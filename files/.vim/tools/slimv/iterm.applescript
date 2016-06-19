#! /usr/bin/osascript
-- joinList from Geert Vanderkelen @ bit.ly/1gRPYbH
-- toDo push new terminal to background after creation
to joinList(aList, delimiter)
    set retVal to ""
    set prevDelimiter to AppleScript's text item delimiters
    set AppleScript's text item delimiters to delimiter
    set retVal to aList as string
    set AppleScript's text item delimiters to prevDelimiter
    return retVal
end joinList

on run arg
  set thecommand to joinList(arg, " ")

  tell application "iTerm"
      tell the first terminal
      # tell the current terminal
        # set oldsession to the current session
        launch session "Default"
        tell the the current session
          set name to "Slimv REPL"
          write text thecommand
        end tell
        # select oldsession
     end tell
   end tell
   activate
end run
