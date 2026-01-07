-- removes a link between a log and a tag and, if there is no more logs tagged, it gets deleted.
delete from log2tag
where log = $1 and tag = $2
