-- Since nullability is not detected by squirrel, I'll have to give up type-safety and implement this query in gleam on a custom function in custom_sql.gleam.
select l.id, l.message, l.level, l.last_occurrence
from logs l
left join log2tag lt on lt.log = l.id
left join tags t on t.id = lt.tag
where ($1::text is null or LOWER(message) LIKE $1)
  and ($2::level is null or level = $2)
  and ($3::text is null or LOWER(l.module) LIKE $3)
  and ($4::text is null or LOWER(l.function) LIKE $4)
  and ($5::int is null or l.arity = $5)
  and ($6::text is null or LOWER(l.file) LIKE $6)
  and ($7::int is null or l.line = $7)
  and ($8::bool is null or l.resolved = $8)
  and ($9::bigint is null or l.last_occurrence > $9)
  and ($10::bigint is null or l.last_occurrence < $10)
  and ($11::bool is null or l.muted = $11)
group by l.id, l.message, l.level, l.last_occurrence
having $12::text[] is null -- no array
       or array_length($12, 1) is null -- empty array
       or COUNT(DISTINCT CASE
                  WHEN t.name = ANY($12) -- If the name is in the array of tags
                  THEN t.name -- return it
                END
          ) -- filter nulls and count them
     = array_length($12, 1); -- Ensure every tag is in there
