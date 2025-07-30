REPLACE INTO stic_validation_actions (id, name, date_entered, date_modified, modified_user_id, created_by, description, deleted, assigned_user_id, last_execution, `function`, report_always, priority) VALUES
('a8d6cdff-ff13-4a2d-b5af-dba7ed47f29c', 'Compromisos de pago - Revisión de la fecha de caducidad de las tarjetas', NOW(), NOW(), '1', '1', NULL, 0, '1', NULL, 'a8d6cdff-ff13-4a2d-b5af-dba7ed47f29c', 0, 65);

REPLACE INTO stic_validation_actions_schedulers_c (id, date_modified, deleted, stic_validation_actions_schedulersstic_validation_actions_ida, stic_validation_actions_schedulersschedulers_idb) VALUES
('a58bba60-c60f-11ee-98c0-0242ac140002', NOW(), 0, 'a8d6cdff-ff13-4a2d-b5af-dba7ed47f29c', 'a9bebf7f-8896-46dd-8d06-77e2b5256c83');

REPLACE INTO stic_settings (`id`, `date_entered`, `date_modified`, `modified_user_id`, `created_by`, `deleted`, `assigned_user_id`, `type`, `name`, `value`, `description`) VALUES
('b9755f82-72c9-d361-10b7-5b40699f14c4', NOW(), NOW(), '1', '1', 0, '1', 'TPV', 'TPV_EXPIRATION_MONTHS', '0', 'Una tarea programada avisa de los compromisos de pago recurrentes con tarjeta que tienen fecha de caducidad cercana. Este parámetro indica el mes de vencimiento de las tarjetas a revisar en función de la anticipación deseada (0 = Mes actual, 1 = Mes siguiente, etc.).');
