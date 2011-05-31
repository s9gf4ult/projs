start transaction;
create temporary table delete_files as (select distinct f.id  from filepack fp inner join files f on f.filepack_id = fp.id where exists(select ff.* from files ff where ff.nm = f.nm and ff.filepack_id = f.filepack_id and ff.id <> f.id) and f.dt <> (select max(ff.dt) from files ff where ff.filepack_id = f.filepack_id group by ff.filepack_id));
update files set rm = 1 where id in (select id from delete_files);
commit;
