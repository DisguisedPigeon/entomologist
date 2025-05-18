with error_id as (
    insert into errors (
        message, level, module, function, resolved, last_occurrence, muted
    ) values (
        $1, $2, $3, $4, $5, $6, $7
    ) returning id
)
insert into occurrences (
    error,
    reason, context, module, function, arity, file, line
) values (
    (select id from error_id),
    $8, $9, $10, $11, $12, $13, $14
) returning id
