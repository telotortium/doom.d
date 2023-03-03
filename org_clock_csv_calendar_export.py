#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
"""Export events in CSV file created using org-clock-csv to Google Calendar.

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

import csv
import logging

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
        '--org_clock_csv',
        help=('CSV file containing timestamp entries exported using the '
              'org-clock-csv package'))
    parser.add_argument(
        '--min_time',
        help=('ISO Timestamp before which Org-clock entries or Calendar '
              'entries will not be considered'),
        default=(datetime.now().astimezone() - timedelta(days=30)).isoformat())
    parser.add_argument(
        '--calendar_id',
        help='Google Calendar ID of calendar on which to manage events')
    service, flags = sample_tools.init(
        argv, 'calendar', 'v3', __doc__, __file__,
        scope='https://www.googleapis.com/auth/calendar',
        parents=[parser])


    # Map (title, start_time, end_time) => row
    logging.getLogger().setLevel(getattr(logging, flags.logging_level))
    org_clock_dict = dict()
    with open(flags.org_clock_csv) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            title = '{} ({})'.format(row['task'], row['parents'])
            start_time = datetime.strptime(
                row['start'], '%Y-%m-%d %H:%M').astimezone()
            end_time = datetime.strptime(
                row['end'], '%Y-%m-%d %H:%M').astimezone()
            event_key = (title, start_time, end_time)
            if start_time < datetime.fromisoformat(flags.min_time):
                logging.info("Skipping event {} - older then min_time {}"
                             .format(event_key_to_isoformat(event_key),
                                     flags.min_time))
                continue
            org_clock_dict[(title, start_time, end_time)] = row

    try:
        events = service.events().list(
            calendarId=flags.calendar_id, timeMin=flags.min_time,
        ).execute()['items']
        events_to_delete = []
        events_present = []
        for event in events:
            event_key = (
                event['summary'],
                datetime.fromisoformat(event['start']['dateTime']),
                datetime.fromisoformat(event['end']['dateTime']))
            if event_key in org_clock_dict:
                logging.info("Skipping event {} - already found in {}"
                             .format(event_key_to_isoformat(event_key),
                                     flags.org_clock_csv))
                del org_clock_dict[event_key]
            else:
                logging.info("Deleting event {} - not found in {}"
                             .format(event_key_to_isoformat(event_key),
                                     flags.org_clock_csv))
                events_to_delete.append(event)

        for event in events_to_delete:
            service.events().delete(calendarId=flags.calendar_id,
                                    eventId=event['id']).execute()

        for (title, start_time, end_time) in org_clock_dict.keys():
            event = {
                'summary': title,
                'start': {
                    'dateTime': start_time.isoformat(),
                    'timeZone': 'America/Los_Angeles',
                },
                'end': {
                    'dateTime': end_time.isoformat(),
                    'timeZone': 'America/Los_Angeles',
                },
                'reminders': {
                    'useDefault': False,
                    'overrides': [
                    {'method': 'popup', 'minutes': 0},
                    ],
                },
            }
            event = service.events().insert(
                calendarId=flags.calendar_id, body=event).execute()
            logging.info("event posted: {}".format(
                event_key_to_isoformat((title, start_time, end_time))))

    except client.AccessTokenRefreshError:
        print('The credentials have been revoked or expired, please re-run'
              'the application to re-authorize.')

def event_key_to_isoformat(event_key):
    return "{} [{}]-[{}]".format(
        event_key[0],
        event_key[1].isoformat(),
        event_key[2].isoformat(),
    )

if __name__ == '__main__':
    main(sys.argv)
