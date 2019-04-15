#!/bin/bash
pg_dump -U postgres "trello-reminders-db" -F p -W -c > backup.sql
