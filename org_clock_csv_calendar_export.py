#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
"""Export events in CSV file created using org-clock-csv to Google Calendar."""

import argparse
import csv
import logging
import sys

from datetime import datetime, timedelta
from googleapiclient import sample_tools
from oauth2client import client


def main(argv):
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
