REPLACE INTO stic_validation_actions (id, name, date_entered, date_modified, modified_user_id, created_by, description, deleted, assigned_user_id, last_execution, `function`, report_always, priority) VALUES
('b53a08c5-23dc-96b7-2b31-6582cf7dbebc', 'Ayudas - Cálculo de registro activo/inactivo', NOW(), NOW(), '1', '1', NULL, 0, '1', NULL, 'b53a08c5-23dc-96b7-2b31-6582cf7dbebc', 0, 30);

REPLACE INTO stic_validation_actions_schedulers_c (id, date_modified, deleted, stic_validation_actions_schedulersstic_validation_actions_ida, stic_validation_actions_schedulersschedulers_idb) VALUES
('d0d59ped-6cd9-areb-a77a-6361606e5f36', NOW(), 0, 'b53a08c5-23dc-96b7-2b31-6582cf7dbebc', 'b05bde8a-1309-4789-993b-bf85be389f07');