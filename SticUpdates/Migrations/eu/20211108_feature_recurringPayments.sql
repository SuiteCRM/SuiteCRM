-- insert new validation action
INSERT INTO stic_validation_actions (id, name, date_entered, date_modified, modified_user_id, created_by, description, deleted, assigned_user_id, last_execution, `function`, report_always, priority) VALUES
('ac28533e-40ad-11ec-b2f2-0242ac150002', 'Compromisos de pago - Baja de las autorizaciones recurrentes incompletas', NOW(), NOW(), '1', '1', NULL, 0, '1', NULL, '8eeed951-4090-11ec-bd41-0242ac150002', 0, 30);

-- INSERT INTO stic_validation_actions_schedulers_c (id, date_modified, deleted, stic_validation_actions_schedulersstic_validation_actions_ida, stic_validation_actions_schedulersschedulers_idb) VALUES
-- ('6f82c1d8-d481-09b5-9bfb-618955029780', NOW(), 0, 'ac28533e-40ad-11ec-b2f2-0242ac150002', 'b05bde8a-1309-4789-993b-bf85be389f07');
