-- Eliminamos antes de insertar para evitar problemas de IDs duplicados en caso de que haya que relanzar el script de actualización
DELETE FROM schedulers WHERE id='4d0ac999-2bcb-cc4f-f65d-6192a21c4aff';
DELETE FROM stic_validation_actions WHERE id='ef33e1ee-1d8e-e054-0d6d-6193c473d5b9';
DELETE FROM stic_validation_actions_schedulers_c WHERE id='73031c41-d137-fd92-e776-6193c94ffaa8';



-- Insertamos la tarea planificada SinergiaCRM - Validación y actualización diaria de datos
INSERT INTO `schedulers` (`id`, `deleted`, `date_entered`, `date_modified`, `created_by`, `modified_user_id`, `name`, `job`, `date_time_start`, `date_time_end`, `job_interval`, `time_from`, `time_to`, `last_run`, `status`, `catch_up`) VALUES ('4d0ac999-2bcb-cc4f-f65d-6192a21c4aff', 0, NOW(), NOW(), '1', '1', 'SinergiaCRM - Validación y actualización mensual de datos', 'function::validationActions', NOW(), NULL, '*::3::1::*::*', NULL, NULL, NULL, 'Active', 0);

-- Insertamos la acción de validación SinergiaCRM - Eliminar relaciones inoperativas
INSERT INTO stic_validation_actions (id, name, date_entered, date_modified, modified_user_id, created_by, description, deleted, assigned_user_id, last_execution, `function`, report_always, priority) VALUES
('ef33e1ee-1d8e-e054-0d6d-6193c473d5b9', 'General - Eliminación de las relaciones obsoletas', NOW(), NOW(), '1', '1', NULL, 0, '1', NULL, 'ef33e1ee-1d8e-e054-0d6d-6193c473d5b9', 0, 50);

-- Insertamos la relación entre la tarea y la acción de validación
INSERT INTO stic_validation_actions_schedulers_c (id, date_modified, deleted, stic_validation_actions_schedulersstic_validation_actions_ida, stic_validation_actions_schedulersschedulers_idb) VALUES
('73031c41-d137-fd92-e776-6193c94ffaa8', NOW(), 0, 'ef33e1ee-1d8e-e054-0d6d-6193c473d5b9', '4d0ac999-2bcb-cc4f-f65d-6192a21c4aff');
