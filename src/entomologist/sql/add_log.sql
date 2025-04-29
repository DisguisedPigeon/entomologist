insert into logs(
	level,
	code,
	title,
	description
)
values (
	$1, $2, $3, $4
)
returning id;
