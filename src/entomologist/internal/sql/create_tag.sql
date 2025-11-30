insert into tags (name)
values ($1)
returning id;
