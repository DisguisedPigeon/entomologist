insert into logs(
	level,
	title,
	description
)
values (
	$1, $2, $3
)
returning id;
