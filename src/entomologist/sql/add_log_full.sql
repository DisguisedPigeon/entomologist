insert into logs(
	level,
	title,
	description,
	code
)
values (
	$1, $2, $3, $4
)
returning id;
