insert into logs(
	level,
	title
)
values (
	$1, $2
)
returning id;
