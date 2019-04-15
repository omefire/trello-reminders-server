#!/usr/bin/env bash
createdb "trello-reminders-db"
psql "trello-reminders-db" < /omefire/backup.sql
