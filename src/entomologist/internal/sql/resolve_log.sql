update logs set (resolved , snoozed) = (true, false)
where id = $1;
