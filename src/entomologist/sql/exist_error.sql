select count(1)
from errors
where message = $1
    and level = $2
    and function = $3
    and module = $4
    and resolved = false;
