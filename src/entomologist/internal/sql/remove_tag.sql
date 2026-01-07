-- removes a link between a log and a tag and, if there is no more logs tagged, it gets deleted.
with _tag as (
  delete from log2tag
  where log = $1 and tag = $2
) delete from tags
where
(select count(*) from log2tag where tag = $2) = 0
and id = $2;
