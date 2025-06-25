-- Actualizamos las claves que se tradujeron por error

-- $app_list_strings['dha_plantillasdocumentos_category_dom']: 
UPDATE dha_plantillasdocumentos SET category_id = 'VENTAS' WHERE category_id = 'VENDAS';
UPDATE dha_plantillasdocumentos SET category_id = 'OTROS' WHERE category_id = 'OUTROS';

-- $app_list_strings['stic_incorpora_addr_street_type_list']:
UPDATE contacts_cstm SET inc_address_street_type_c = 'OTROS' WHERE inc_address_street_type_c = 'OUTROS';
UPDATE accounts_cstm SET inc_address_street_type_c = 'OTROS' WHERE inc_address_street_type_c = 'OUTROS';

-- $app_list_strings['stic_incorpora_cno_n2_list']
UPDATE stic_job_offers SET inc_cno_n2 = 'q_00' WHERE inc_cno_n2 = 'q_0';

-- $app_list_strings['stic_incorpora_collection_origin_list']
UPDATE accounts_cstm SET inc_collection_origin_c = 'OTROS' WHERE inc_collection_origin_c = 'OUTROS';

-- $app_list_strings['stic_incorpora_company_status_list']
UPDATE accounts_cstm SET inc_status_c = 'BAJA' WHERE inc_status_c = 'BAIXA';

-- $app_list_strings['stic_incorpora_disability_benefit_list'] // No se usa

-- $app_list_strings['stic_incorpora_offer_origin_list']
UPDATE stic_job_offers SET inc_offer_origin = 'OTROS' WHERE inc_offer_origin = 'OUTROS';

-- $app_list_strings['stic_incorpora_working_day_list']
UPDATE stic_job_offers SET inc_working_day = 'OTROS' WHERE inc_working_day = 'OUTROS';

-- $app_list_strings['campainglog_activity_type_dom']
UPDATE campaign_log SET activity_type = 'send error' WHERE activity_type = 'send erro';