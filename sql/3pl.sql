select t1.sn_lr, t1.date_vr
from prescriptions t1, doctors drs, polyclinics pls
where t1.doctor_id = drs.id and drs.polyclinic_id = pls.id and upper(pls.m_namef) like '%ГОР%ПОЛ% 3%' and t1.status_id = 1 and t1.date_vr > '01.01.2010' and t1.date_vr < '30.06.2010'
order by t1.date_vr, t1.sn_lr;

select t1.sn_lr
from prescriptions t1, doctors drs, polyclinics pls
where t1.doctor_id = drs.id and drs.polyclinic_id = pls.id and upper(pls.m_namef) like '%ГОР%ПОЛ% 3%' and t1.status_id = 4;

select count(t1.sn_lr) as count
from prescriptions t1, doctors drs, polyclinics pls
where t1.doctor_id = drs.id and drs.polyclinic_id = pls.id and t1.status_id = 1 and exists (select * from prescriptions t2 where t2.sn_lr = t1.sn_lr and t2.status_id = 4)
order by t1.date_vr;