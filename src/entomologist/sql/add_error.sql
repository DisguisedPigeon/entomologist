with error as (
    insert into errors (
        message, level, module, function, arity, file, line, last_occurrence
    ) values (
        $1, $2, $3, $4, $5, $6, $7, $8
    ) returning id, last_occurrence
)
insert into occurrences (
    error, timestamp, full_contents
) values (
    (select id from error), (select last_occurrence from error), $9
) returning id
