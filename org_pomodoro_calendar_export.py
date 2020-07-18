#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
"""Export Org Pomodoro sessions to Google Calendar."""

import argparse
import csv
import html
import logging
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
        '--clocked_event',
        action='append',
        help=('Title of events to add to description of calendar event. '
              'Can be specified multiple times to add multiple event titles '
              'to the calendar event.'),
    )
    parser.add_argument(
        '--event_id',
        help=('Event ID of existing Pomodoro event to update. '
              'If not set, new event will be created.'),
    )
    parser.add_argument(
        '--start_timestamp',
        help='ISO Timestamp of event start')
    parser.add_argument(
        '--end_timestamp',
        help='ISO Timestamp of event end')
    service, flags = sample_tools.init(
        argv, 'calendar', 'v3', __doc__, __file__,
        scope='https://www.googleapis.com/auth/calendar',
        parents=[parser])

    try:
        event = {
            'summary': flags.state,
            'description': '<ul>{}</ul>'.format(
                ''.join('<li>{}</li>'.format(html.escape(x, quote=False))
                        for x in flags.clocked_event),
            ),
            'start': {
                'dateTime': flags.start_timestamp,
                'timeZone': 'America/Los_Angeles',
            },
            'end': {
                'dateTime': flags.end_timestamp,
                'timeZone': 'America/Los_Angeles',
            },
            'reminders': {
                'useDefault': False,
            },
        }
        if flags.event_id:
            event = service.events().update(
                calendarId=flags.calendar_id,
                eventId=flags.event_id, body=event).execute()
        else:
            event = service.events().insert(
                calendarId=flags.calendar_id, body=event).execute()
        print(event['id'])

    except client.AccessTokenRefreshError:
        print('The credentials have been revoked or expired, please re-run'
              'the application to re-authorize.')

if __name__ == '__main__':
    main(sys.argv)
