with log as (
    insert into logs (
        message, level, module, function, arity, file, line, last_occurrence
    ) values (
        $1, $2, $3, $4, $5, $6, $7, $8
    ) returning id, last_occurrence
)
insert into occurrences (
    log, timestamp, full_contents
) values (
    (select id from log), (select last_occurrence from log), $9
) returning id
