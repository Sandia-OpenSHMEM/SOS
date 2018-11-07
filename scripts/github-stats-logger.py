#!/usr/bin/env python3

# Copyright (c) 2018 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

import requests, json, sys, os, argparse

## Note: This script uses simple password authentication. Fill in
##       username/password below.

user    = "username"
passwd  = "password"

### Argument Parsing ###

parser = argparse.ArgumentParser(description="Maintain a log file of GitHub traffic statistics over time.")
parser.add_argument("-l", "--listname", help="Name of list field (e.g. clones or views)", required=True)
parser.add_argument("-v", "--verbose", help="Display debugging output", action="store_true")
parser.add_argument("-d", "--dbase", help="Database file, stored in CSV format", required=True)
parser.add_argument("-u", "--url", help="Target URL, e.g. https://api.github.com/repos/[ORG]/[REPO]/traffic/clones", required=True)

args = parser.parse_args();

### Functions ###

def read_db(filename):
    data = []

    for line in open(args.dbase):
        fields = line.split(",")
        name   = fields[0]
        count  = int(fields[1])
        unique = int(fields[2])
        data.append([name, count, unique])

    return data

### Begin Script ###

## Pull updates from GitHub API
if args.verbose: print("Collecting from:", args.url)

try:
    r = requests.get(args.url, auth=(user, passwd))
    db_up = r.json()
except Exception as err:
    print("Collecting updates from,", args.url, "failed:", err)
    sys.exit(1)

if args.verbose: print(json.dumps(db_up))

## Check if GitHub returned an error message
if 'message' in db_up:
    if db_up['message'] == "Bad credentials" and user == "username":
        print("Error: Update username/password by editing this script")
    else:
        print("Error:", db_up['message'])
    sys.exit(1)

## Read contents of the database
if os.path.exists(args.dbase):
    db = read_db(args.dbase)
else:
    db = []

## Merge updates into the database
## Note: count for the oldest entry decreases over the course of the day and
## the newest increases over the course of the day.  So, we need to check if
## the count to determine whether to accept the update.
for l_up in db_up[args.listname]:
    found = False
    timestamp = l_up['timestamp'].replace('T00:00:00Z', '') # Timestamps are 0, so remove them

    for l in db:
        if l[0] == timestamp:
            found = True
            if l_up['count'] > l[1]:
                l[1] = l_up['count']
                l[2] = l_up['uniques']

    if not found:
        db.append((timestamp, int(l_up['count']), int(l_up['uniques'])))

if args.verbose: print(db)

## Write new info to the database
f = open(args.dbase, "w")
for l in db:
    f.write(l[0]+','+str(l[1])+','+str(l[2])+'\n')
f.close()
