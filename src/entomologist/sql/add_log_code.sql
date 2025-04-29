insert into logs(
	level,
	title,
	code
)
values (
	$1, $2, $3
)
returning id;
