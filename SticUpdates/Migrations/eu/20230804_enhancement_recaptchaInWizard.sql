-- Crear constantes RECAPTCHA con valor vacío. 
REPLACE INTO `stic_settings` (`id`, `date_entered`, `date_modified`, `modified_user_id`, `created_by`, `deleted`, `assigned_user_id`, `type`, `name`, `value`, `description`) VALUES
  ('696c690e-70ce-4dd1-855b-ff02d4302831', NOW(), NOW(), '1', '1', 0, '1', 'GENERAL', 'GENERAL_RECAPTCHA_VERSION', '', 'Versión de Google reCAPTCHA utilizada. Valores posibles: 2 o 3. Más información en: https://wikisuite.sinergiacrm.org/index.php?title=Google_reCAPTCHA'),
  ('572910d4-716b-457d-b66c-4fb54d8e39a5', NOW(), NOW(), '1', '1', 0, '1', 'GENERAL', 'GENERAL_RECAPTCHA_WEBKEY', '', 'Clave de sitio web proporcionada por Google. Más información en: https://wikisuite.sinergiacrm.org/index.php?title=Google_reCAPTCHA');
