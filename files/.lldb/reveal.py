# require the lldb lib to interact with LLDB
import lldb

# this is called when the file is added to the script runtime
# we'll invoke a lldb command that links the python functions to actual LLDB commands
# in this case, we're adding reveal_start_sim and reveal_stop
# note: the other functions in this file are no accessible from LLDB.
def __lldb_init_module (debugger, dict):
  debugger.HandleCommand('command script add -f reveal.reveal_start_sim reveal_start_sim')
  debugger.HandleCommand('command script add -f reveal.reveal_stop reveal_stop')

# This is the main entry point. This will load the reveal lib and then send a notification so that it will start
def reveal_start_sim(debugger, command, result, internal_dict):
  print "Installing reveal in the simulator"
  reveal_load_sim(debugger)
  reveal_start(debugger)

# Loads the libReveal.dylib from the Reveal app.
def reveal_load_sim(debugger):
  debugger.HandleCommand('expr (void*)dlopen("/Applications/Reveal.app/Contents/SharedSupport/iOS-Libraries/libReveal.dylib", 0x2);')

# Sends a notification to the reveal server so that it actually starts
def reveal_start(debugger):
  debugger.HandleCommand('expr [(NSNotificationCenter*)[NSNotificationCenter defaultCenter] postNotificationName:@"IBARevealRequestStart" object:nil];')

# Sends a notification to the reveal server so that it stops (if you'd want that)
def reveal_stop(debugger):
  debugger.HandleCommand('expr [(NSNotificationCenter*)[NSNotificationCenter defaultCenter] postNotificationName:@"IBARevealRequestStop" object:nil];')
