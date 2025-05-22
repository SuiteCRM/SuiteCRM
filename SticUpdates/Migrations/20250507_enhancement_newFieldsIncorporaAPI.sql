INSERT INTO `fields_meta_data` (`id`, `custom_module`, `name`) VALUES
('Contactsinc_country_origin_c', 'Contacts', 'inc_country_origin_c');

update contacts_cstm set inc_nationality_c = '^011^' where inc_nationality_c = 'nacional';