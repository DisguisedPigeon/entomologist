-- Since nullability is not detected by squirrel, I'll have to give up type-safety and implement this query in gleam on a custom function. This will stay here in case the issue is ever fixed.
select * from logs
where ($1::level is null or level = $1)
  and ($2::text is null or module = $2)
  and ($3::text is null or function = $3)
  and ($4::int is null or arity = $4)
  and ($5::text is null or file = $5)
  and ($6::int is null or line = $6)
  and ($7::bool is null or resolved = $7)
  and ($8::bigint is null or last_occurrence = $8)
  and ($9::bool is null or snoozed = $9)
