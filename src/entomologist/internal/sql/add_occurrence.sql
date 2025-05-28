insert into
    occurrences(error, timestamp, full_contents)
values
    ($1, $2, $3)
returning id;
