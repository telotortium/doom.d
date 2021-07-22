#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
"""Export Org Pomodoro sessions to Google Calendar."""

import argparse
import dateutil.parser
import html
import sys

from datetime import datetime, timedelta
from googleapiclient import sample_tools
from oauth2client import client


def main(argv):
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