REPLACE INTO schedulers
(id, deleted, date_entered, date_modified, created_by, modified_user_id, name, job, date_time_start, date_time_end, job_interval, time_from, time_to, last_run, status, catch_up)
VALUES ('ca564b47-9a06-987d-a115-6442356ca768', 0, NOW(), NOW(), '1', '1', 'SinergiaCRM - Creación de registros de medicación', 'function::createMedicationLogs', NOW(), NULL, '*::1::*::*::*', NULL, NULL, NULL, 'Active', 0);
