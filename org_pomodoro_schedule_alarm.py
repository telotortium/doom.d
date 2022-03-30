#!/usr/bin/env python3.8
# -*- coding: utf-8 -*-

"""Schedule alarms for org-pomodoro on Google Calendar.

Dependencies (MacPorts):

- py38-dateutil
- py38-google-api
- py38-oauth2client

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

import argparse
import os
import os.path
import sys

# To install these run
#   pip3 install --user --ignore-installed oauth2client google-api-python-client
from oauth2client import client
from googleapiclient import sample_tools


def main(argv):
    # Change to the directory of the script, so that we have a consistent
    # location for the `calendar.dat` file created by `sample_tools.init` below.
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    # Authenticate and construct service.
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument(
        '--title', help='Event title')
    parser.add_argument(
        '--timestamp',
        help='ISO Timestamp of alarm event (start and end are the same)')
    parser.add_argument(
        '--calendar_id',
        help='Google Calendar ID of calendar on which to create event')
    parser.add_argument(
        '--event_id',
        help=('Event ID of existing Pomodoro event to update. '
              'If not set, new event will be created.'),
    )
    parser.add_argument(
        '--remove',
        help=('If set, remove event with the event ID given in --event_id.'),
        action='store_true',
    )
    service, flags = sample_tools.init(
        argv, 'calendar', 'v3', __doc__, __file__,
        scope='https://www.googleapis.com/auth/calendar',
        parents=[parser])

    try:
        event = {
            'start': {
                'dateTime': flags.timestamp,
                'timeZone': 'America/Los_Angeles',
            },
            'end': {
                'dateTime': flags.timestamp,
                'timeZone': 'America/Los_Angeles',
            },
            'reminders': {
                'useDefault': False,
                'overrides': [
                  {'method': 'popup', 'minutes': 0},
                 ],
            },
        }
        if flags.title:
            event['summary'] = flags.title

        if flags.remove:
            event = service.events().delete(
                calendarId=flags.calendar_id,
                eventId=flags.event_id).execute()
        elif flags.event_id:
            event = service.events().patch(
                calendarId=flags.calendar_id,
                eventId=flags.event_id, body=event).execute()
        else:
            event = service.events().insert(
                calendarId=flags.calendar_id, body=event).execute()
        if not flags.remove:
            print(event['id'])

    except client.AccessTokenRefreshError:
        print('The credentials have been revoked or expired, please re-run'
              'the application to re-authorize.')

if __name__ == '__main__':
    main(sys.argv)
