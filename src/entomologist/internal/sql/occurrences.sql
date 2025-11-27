select id, timestamp, full_contents
from occurrences
where log = $1;
