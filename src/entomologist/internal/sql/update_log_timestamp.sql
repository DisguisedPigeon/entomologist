update logs
set last_occurrence = occurrences.timestamp
from occurrences
where logs.id = occurrences.log and occurrences.id = $1
