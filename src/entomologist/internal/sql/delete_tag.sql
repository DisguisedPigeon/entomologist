delete from tags where
(select count(*) from log2tag where tag = $1) = 0 and id = $1;
