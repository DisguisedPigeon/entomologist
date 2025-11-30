select name
from tags as t
join log2tag as lt on t.id = lt.tag
join logs as l on lt.log = l.id
where l.id = $1;
