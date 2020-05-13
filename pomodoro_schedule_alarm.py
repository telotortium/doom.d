#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
"""Schedule alarms for org-pomodoro on Google Calendar."""

import argparse
import sys

from oauth2client import client
from googleapiclient import sample_tools


def main(argv):
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
    service, flags = sample_tools.init(
        argv, 'calendar', 'v3', __doc__, __file__,
        scope='https://www.googleapis.com/auth/calendar',
        parents=[parser])

    try:
        event = {
            'summary': flags.title,
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

        event = service.events().insert(
            calendarId=flags.calendar_id, body=event).execute()

    except client.AccessTokenRefreshError:
        print('The credentials have been revoked or expired, please re-run'
              'the application to re-authorize.')

if __name__ == '__main__':
    main(sys.argv)
