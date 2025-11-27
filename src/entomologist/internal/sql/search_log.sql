-- Since nullability is not detected by squirrel, I'll have to give up type-safety and implement this query in gleam on a custom function in custom_sql.gleam.
select id, message, level, last_occurrence
from logs
where ($1::text is null or LOWER(message) LIKE $1)
  and ($2::level is null or level = $2)
  and ($3::text is null or LOWER(module) LIKE $3)
  and ($4::text is null or LOWER(function) LIKE $4)
  and ($5::int is null or arity = $5)
  and ($6::text is null or LOWER(file) LIKE $6)
  and ($7::int is null or line = $7)
  and ($8::bool is null or resolved = $8)
  and ($9::bigint is null or last_occurrence = $9)
  and ($10::bool is null or muted = $10)
