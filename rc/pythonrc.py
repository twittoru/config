##  pythonrc.py
##

import sys

if sys.stdin.isatty():
  import os
  HISTORYFILE=os.environ['HOME']+"/.python_history"
  try:
    import rlcompleter, readline
    # readline and rlcompleter available.
    readline.parse_and_bind("tab: complete")
    readline.parse_and_bind("set input-meta on")
    readline.parse_and_bind("set convert-meta off")
    readline.parse_and_bind("set output-meta on")
    try:
      # check if the history file is available.
      readline.read_history_file(HISTORYFILE)
    except (IOError, AttributeError):
      # file not found or not supported in Python 1.5.
      pass
    try:
      import atexit
      # atexit is available.
      atexit.register(lambda: readline.write_history_file(HISTORYFILE))
      del atexit
    except ImportError:
      # atexit is not available in Python1.5.
      pass
    del rlcompleter
  except ImportError:
    # neither readline nor rlcompleter is available.
    pass

del sys
