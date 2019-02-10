-- To backup
pg_dump -U postgres "trello-reminders-db" -F p -W -c > backup.sql


-- To restore
psql -U postgres "trello-reminders-db" -W < backup.sql
