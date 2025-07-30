-- Insertamos la acción de validación
REPLACE INTO stic_validation_actions (id, name, date_entered, date_modified, modified_user_id, created_by, description, deleted, assigned_user_id, last_execution, `function`, report_always, priority) VALUES
('10fff3d4-5dc5-ef7a-3d7f-636bae661c14', 'Unidades familiares - Cálculo de registro activo/inactivo', NOW(), NOW(), '1', '1', NULL, 0, '1', NULL, '10fff3d4-5dc5-ef7a-3d7f-636bae661c14', 0, 30);

-- Insertamos la relación entre la tarea y la acción de validación
REPLACE INTO stic_validation_actions_schedulers_c (id, date_modified, deleted, stic_validation_actions_schedulersstic_validation_actions_ida, stic_validation_actions_schedulersschedulers_idb) VALUES
('d0d59fed-6419-a8eb-a7e2-636b906e5f36', NOW(), 0, '10fff3d4-5dc5-ef7a-3d7f-636bae661c14', 'b05bde8a-1309-4789-993b-bf85be389f07');
