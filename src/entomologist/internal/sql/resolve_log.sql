update logs
set (resolved , muted) = (true, false)
where id = $1;
