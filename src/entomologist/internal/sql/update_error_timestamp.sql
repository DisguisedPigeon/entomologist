update errors
set last_occurrence = occurrences.timestamp
from occurrences
where errors.id = occurrences.error and occurrences.id = $1
