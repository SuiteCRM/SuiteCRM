-- Eliminamos antes de insertar para evitar problemas de IDs duplicados en caso de que haya que relanzar el script de actualización
DELETE FROM schedulers WHERE id='98eb0c26-99dd-d656-ee73-611cc6994570';

-- Insertamos la tarea planificada SinergiaCRM - Purgar base de datos
INSERT INTO `schedulers` (`id`, `deleted`, `date_entered`, `date_modified`, `created_by`, `modified_user_id`, `name`, `job`, `date_time_start`, `date_time_end`, `job_interval`, `time_from`, `time_to`, `last_run`, `status`, `catch_up`) VALUES('98eb0c26-99dd-d656-ee73-611cc6994570', 0, NOW(), NOW(), '1', '1', 'SinergiaCRM - Purgar base de datos', 'function::sticPurgeDatabase', NOW(), NULL, '*::2::*::*::0', NULL, NULL, NULL, 'Active', 0);
