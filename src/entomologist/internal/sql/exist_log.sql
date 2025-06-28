select id
from logs
where message = $1
    and level = $2
    and function = $3
    and module = $4
    and arity = $5
    and resolved = false;
