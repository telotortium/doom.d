#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
"""Export Org Pomodoro sessions to Google Calendar.

Installation:
    - Set up virtualenv at `venv` in the script directory:
    - Install dependencies in `venv` from `requirements.txt` in the script
      directory.

Last supported version is Python 3.8. In Python 3.9+, you get the following
stack trace:

```
Traceback (most recent call last):
  File "/Users/bytedance/.doom.d/org_pomodoro_calendar_log_sum.py", line 64, in <module>
    main(sys.argv)
  File "/Users/bytedance/.doom.d/org_pomodoro_calendar_log_sum.py", line 33, in main
    service, flags = sample_tools.init(
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/googleapiclient/sample_tools.py", line 98, in init
    service = discovery.build(name, version, http=http)
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/googleapiclient/_helpers.py", line 130, in positional_wrapper
    return wrapped(*args, **kwargs)
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/googleapiclient/discovery.py", line 219, in build
    requested_url = uritemplate.expand(discovery_url, params)
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/uritemplate/api.py", line 33, in expand
    return URITemplate(uri).expand(var_dict, **kwargs)
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/uritemplate/template.py", line 132, in expand
    return self._expand(_merge(var_dict, kwargs), False)
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/uritemplate/template.py", line 97, in _expand
    expanded.update(v.expand(expansion))
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/uritemplate/variable.py", line 338, in expand
    expanded = expansion(name, value, opts['explode'], opts['prefix'])
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/uritemplate/variable.py", line 278, in _string_expansion
    if dict_test(value) or tuples:
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/uritemplate/variable.py", line 363, in dict_test
    return isinstance(value, (dict, collections.MutableMapping))
AttributeError: module 'collections' has no attribute 'MutableMapping'
```

This is because `collections.MutableMapping`, deprecated in 3.3, was removed in 3.9:

```
collections.MutableMapping
__main__:1: DeprecationWarning: Using or importing the ABCs from 'collections' instead of from 'collections.abc' is deprecated since Python 3.3,and in 3.9 it will stop working
<class 'collections.abc.MutableMapping'>
```
"""

import os
import os.path
import sys

# Run script in correct virtualenv relative to script file.
if 'VIRTUAL_ENV' not in os.environ:
    venv_bin = os.path.join(os.path.dirname(__file__), 'venv', 'bin')
    venv_activate = os.path.join(venv_bin, 'activate')
    venv_python3 = os.path.join(venv_bin, 'python3')
    os.execvp('bash', [
        'bash', '-euc', 'source "$1"; shift; exec "$@"',
        'DUMMY_ARG0', venv_activate, venv_python3, __file__,
    ] + sys.argv[1:])

import argparse
import dateutil.parser
import html

from datetime import datetime, timedelta
from googleapiclient import sample_tools
from oauth2client import client


def main(argv):
    # Change to the directory of the script, so that we have a consistent
    # location for the `calendar.dat` file created by `sample_tools.init` below.
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    # Authenticate and construct service.
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument(
        '--calendar_id',
        help='Google Calendar ID of calendar on which to manage events')
    parser.add_argument(
        '--state',
        choices=[':pomodoro', ':short-break', ':long-break'],
        help='Pomodoro state',
    )
    parser.add_argument(
        '--start_timestamp',
        help='ISO Timestamp to start summing event logs')
    parser.add_argument(
        '--end_timestamp',
        help='ISO Timestamp to end summing event logs')
    service, flags = sample_tools.init(
        argv, 'calendar', 'v3', __doc__, __file__,
        scope='https://www.googleapis.com/auth/calendar',
        parents=[parser])

    try:
        events_result = service.events().list(
            calendarId=flags.calendar_id,
            timeMin=flags.start_timestamp,
            timeMax=flags.end_timestamp,
            singleEvents=True,
        ).execute()
        count = 0
        second_total = 0
        start = dateutil.parser.isoparse(flags.start_timestamp)
        end = dateutil.parser.isoparse(flags.end_timestamp)
        for event in events_result.get('items', []):
            if event['summary'] == flags.state:
                event_start = dateutil.parser.isoparse(event['start']['dateTime'])
                event_end = dateutil.parser.isoparse(event['end']['dateTime'])
                count += 1
                second_total += (
                    min(end, event_end) - max(start, event_start)
                ).total_seconds()
        print("{},{}".format(count, second_total))

    except client.AccessTokenRefreshError:
        print('The credentials have been revoked or expired, please re-run'
              'the application to re-authorize.')

if __name__ == '__main__':
    main(sys.argv)
