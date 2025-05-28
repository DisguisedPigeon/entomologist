update errors set (resolved , snoozed) = (true, false)
where id = $1;
