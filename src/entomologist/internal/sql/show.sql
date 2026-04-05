select l.id, message, level, last_occurrence, coalesce(array_remove(array_agg(t.name), null), '{}') as tags
from logs l
left join log2tag lt on l.id = lt.log
left join tags t on lt.tag = t.id
where resolved = false and muted = false
group by l.id;
