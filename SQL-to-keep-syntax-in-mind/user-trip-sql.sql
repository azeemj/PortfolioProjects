Question :https://leetcode.com/problems/trips-and-users/

my answer:

select CAST(coalesce(b.uncompleted_total/a.total,0) AS DECIMAL(6,2)) As Cancellation_Rate,
 a.request_at

from(
                select count(t.id) as total, t.request_at
                from Trips t
                inner join Users u on t.driver_id = u.users_id
                where u.banned = 'No'
                group by t.request_at

) a

left join
 (
                select  count(t.id) as uncompleted_total, t.request_at

                from Trips t
                inner join Users u on t.driver_id = u.users_id
                where u.banned = 'No' and status !='completed'
                group by t.request_at
 )b
  on a.request_at = b.request_at