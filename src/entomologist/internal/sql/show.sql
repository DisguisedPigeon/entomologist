select id, message, level, last_occurrence
from logs
where resolved = false and muted = false;
