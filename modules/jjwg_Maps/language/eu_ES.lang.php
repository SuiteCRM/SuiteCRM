<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$mod_strings['LBL_MAP'] = 'Mapa';
$mod_strings['LBL_MODULE_NAME'] = 'Mapas';
$mod_strings['LBL_MODULE_TITLE'] = 'Mapas: Inicio';
$mod_strings['LBL_MODULE_ID'] = 'Mapas';
$mod_strings['LBL_LIST_FORM_TITLE'] = 'Mapas de Venta';
$mod_strings['LBL_MAP_CUSTOM_MARKER'] = 'Marcador';
$mod_strings['LBL_MAP_CUSTOM_AREA'] = 'Espacio';
$mod_strings['LBL_HOMEPAGE_TITLE'] = 'Mi listado de mapas';

$mod_strings['LBL_FLEX_RELATE'] = 'Relacionado a la (Centro):';
$mod_strings['LBL_MODULE_TYPE'] = 'Tipo de módulo a visualizar:';
$mod_strings['LBL_DISTANCE'] = 'Distancia (Radio):';
$mod_strings['LBL_UNIT_TYPE'] = 'Tipo de Unidad:';

$mod_strings['LBL_MAP_DISPLAY'] = 'Visualización de Mapa';
$mod_strings['LBL_MAP_LEGEND'] = 'Leyenda:';
$mod_strings['LBL_MAP_USER_GROUPS'] = 'Grupos de Usuarios:';
$mod_strings['LBL_MAP_GROUP'] = 'Grupo';
$mod_strings['LBL_MAP_TYPE'] = 'Tipo';
$mod_strings['LBL_MAP_ASSIGNED_TO'] = 'Asignado a:';
$mod_strings['LBL_MAP_GET_DIRECTIONS'] = 'Obtener direcciones';
$mod_strings['LBL_MAP_GOOGLE_MAPS_VIEW'] = 'Vista de Mapas de Google';

$mod_strings['LNK_NEW_MAP'] = 'Añadir Nuevo Mapa';
$mod_strings['LNK_NEW_RECORD'] = 'Añadir Nuevo Mapa';
$mod_strings['LNK_MAP_LIST'] = 'Mapas Lista';

$mod_strings['LBL_MAP_ADDRESS_TEST'] = 'Prueba de Geocodificación';
$mod_strings['LBL_MAP_QUICK_RADIUS'] = 'Mapa Radio rápida';
$mod_strings['LBL_MAP_NULL_GROUP_NAME'] = 'Nada';
$mod_strings['LBL_MAP_ADDRESS'] = 'Dirección';
$mod_strings['LBL_MAP_PROCESS'] = 'Se Proceso!';

$mod_strings['LBL_MAP_LAST_STATUS'] = 'Estado Geocode Última';
$mod_strings['LBL_GEOCODED_COUNTS'] = 'Módulo Condes Geocodificadas';
$mod_strings['LBL_CRON_URL'] = 'Cron URL:';
$mod_strings['LBL_MODULE_HEADING'] = 'Módulo';

$mod_strings['LBL_N/A'] = 'No disponible';
$mod_strings['LBL_ZERO_RESULTS'] = 'No hay resultados';
$mod_strings['LBL_OK'] = 'Ok';
$mod_strings['LBL_INVALID_REQUEST'] = 'Solicitud no válida';
$mod_strings['LBL_APPROXIMATE'] = 'Aproximado';
$mod_strings['LBL_EMPTY'] = 'Vacío';

$mod_strings['LBL_MODULE_TOTAL_HEADING'] = 'Total';
$mod_strings['LBL_MODULE_RESET_HEADING'] = 'Restablecer';
$mod_strings['LBL_GEOCODED_COUNTS_DESCRIPTION'] = 'La tabla de abajo muestra el número de objetos de módulo geocodificada, agrupados por respuesta de geocodificación. Tenga en cuenta que el límite de uso estándar de mapas de Google es de 2500 solicitudes por día. Este módulo almacenará en caché la información de la geocodificación de direcciones durante el proceso para reducir el número total de solicitudes necesitada.';

$mod_strings['LBL_CRON_INSTRUCTIONS'] = 'Para procesar las solicitudes de geocodificación se recomienda configurar una tarea programada cada noche. Un punto de entrada personalizado se ha creado para este propósito y se puede acceder sin autenticación. La URL que se muestra a continuación está destinada a utilizarse con una tarea administrativa prevista. Por favor, consulte la documentación para obtener más información.';
$mod_strings['LBL_EXPORT_ADDRESS_URL'] = 'Exportación de URLs';
$mod_strings['LBL_EXPORT_INSTRUCTIONS'] = 'Utilice los vínculos siguientes para exportar direcciones completas que necesitan información de la herramienta de geocodeing. Luego utilizar una herramienta de geocodificación de lote en línea o sin conexión a geocodificar las direcciones. Cuando haya acabado la geocodificación, importar las direcciones en el módulo de caché de dirección para ser utilizado con los mapas. Tenga en cuenta que el módulo de caché de dirección es opcional. Toda la información geocoding es almacenada en el módulo representativo.';
$mod_strings['LBL_ADDRESS_CACHE'] = 'Caché de Direcciones';
$mod_strings['LBL_ADD_TO_TARGET_LIST'] = 'Añadir a la Lista de destinos';
$mod_strings['LBL_ADD_TO_TARGET_LIST_PROCESSING'] = 'Tratamiento...';


$mod_strings['LBL_CONFIG_TITLE'] = 'Configuración';
$mod_strings['LBL_CONFIG_SAVED'] = 'Configuraciones Guardadas con Exito!';
$mod_strings['LBL_BILLING_ADDRESS'] = 'Dirección de Facturación';
$mod_strings['LBL_SHIPPING_ADDRESS'] = 'Dirección de Envío';
$mod_strings['LBL_PRIMARY_ADDRESS'] = 'Dirección Primaria';
$mod_strings['LBL_ALTERNATIVE_ADDRESS'] = 'Dirección Alternativa';
$mod_strings['LBL_ADDRESS_FLEX_RELATE'] = 'Posiblemente Relacionado con';
$mod_strings['LBL_ADDRESS_ADDRESS'] = 'Dirección (Simple, Usuarios)';
$mod_strings['LBL_ADDRESS_CUSTOM'] = 'Personalizado (Lógica de Controlador Personalizado)';
$mod_strings['LBL_ENABLED'] = 'Habilitada';
$mod_strings['LBL_DISABLED'] = 'Deshabilitada';
$mod_strings['LBL_DEFAULT'] = 'Por Defecto:';
$mod_strings['LBL_CONFIG_DEFAULT'] = 'Por Defecto:';

$mod_strings['LBL_CONFIG_VALID_GEOCODE_MODULES'] = 'Módulos Geocodificar válidos:';
$mod_strings['LBL_CONFIG_VALID_GEOCODE_TABLES'] = 'Tablas Geocodificar válidos:';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_SETTINGS_TITLE'] = "Configuración de tipo de Dirección: Esto define los tipos de direcciones en los módulos que se desean utilizar como dirección a geocodificar. Valores aceptables: 'billing'; 'shipping'; 'primary'; 'alt'; 'flex_relate'";
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR'] = 'Tipo de dirección para ';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_ACCOUNTS'] = 'Tipos de direcciones para Cuentas:';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_CONTACTS'] = 'Tipos de direcciones para Contacts:';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_LEADS'] = 'Tipos de direcciones para Clientes Potenciales:';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_OPPORTUNITIES'] = 'Tipos de direcciones para Oportunidades:';
$mod_strings['LBL_CONFIG_OF_RELATED_ACCOUNT'] = '(de Cuenta Relacionada)';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_CASES'] = 'Tipos de direcciones para Casos:';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_PROJECTS'] = 'Tipos de direcciones para Proyectos:';
$mod_strings['LBL_CONFIG_OF_RELATED_ACCOUNT_OPPORTUNITY'] = '(de la Cuenta/Oportunidad relacionada)';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_MEETINGS'] = 'Tipos de direcciones para Reuniones:';
$mod_strings['LBL_CONFIG_ADDRESS_TYPE_FOR_PROSPECTS'] = 'Tipos de direcciones para Prospectos/Público Objetivo:';
$mod_strings['LBL_CONFIG_RELATED_OBJECT_THRU_FLEX_RELATE'] = 'Objetos relacionados a través de campo de relación flexible';

$mod_strings['LBL_CONFIG_MARKER_GROUP_FIELD_SETTINGS_TITLE'] = "Seteo de Marcador de Campo de Agrupamiento: Este define el 'campo' a ser uytilizado como parámetro de grupo cuando se muestran marcadores en un mapa. Ejemplos: assigned_user_name, industry, status, sales_stage, priority";
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR'] = 'Campo de grupo para ';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_ACCOUNTS'] = 'Campo de Agrupamiento para Cuentas:';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_CONTACTS'] = 'Campo de Agrupamiento para Contactos:';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_LEADS'] = 'Campo de Agrupamiento para Clientes Potenciales:';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_OPPORTUNITIES'] = 'Campo de Agrupamiento para Oportunidades:';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_CASES'] = 'Campo de Agrupamiento para Casos:';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_PROJECTS'] = 'Campo de Agrupamiento para para Proyectos:';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_MEETINGS'] = 'Campo de Agrupamiento para Reuniones:';
$mod_strings['LBL_CONFIG_GROUP_FIELD_FOR_PROSPECTS'] = 'Campo de Agrupamiento para Prospectos/Público Objetivo:';

$mod_strings['LBL_CONFIG_GEOCODING_SETTINGS_TITLE'] = 'Configuración de Geocodificación de Google:';
$mod_strings['LBL_CONFIG_GEOCODING_API_URL_TITLE'] = 'URL Geocodificación API:';
$mod_strings['LBL_CONFIG_GEOCODING_API_URL_DESC'] = 'La URL de Google Maps API V3 o Proxy';
$mod_strings['LBL_CONFIG_GEOCODING_API_SECRET_TITLE'] = 'Frase secreta para Proxy:';
$mod_strings['LBL_CONFIG_GEOCODING_API_SECRET_DESC'] = 'La frase secreta para ser utilizado con la comparación MD5 Proxy.';
$mod_strings['LBL_CONFIG_GEOCODING_LIMIT_TITLE'] = 'Límite de Geocodificación:';
$mod_strings['LBL_CONFIG_GEOCODING_LIMIT_DESC'] = "'geocoding_limit' establece el límite de consultas cuando se seleccionan registros para geocodificar.";
$mod_strings['LBL_CONFIG_GOOGLE_GEOCODING_LIMIT_TITLE'] = 'Límite de Geocodificación de Google:';
$mod_strings['LBL_CONFIG_GOOGLE_GEOCODING_LIMIT_DESC'] = "'google_geocoding_limit' establece el límite de solicitudes cuando se usann las APIs de Google Maps API.";
$mod_strings['LBL_CONFIG_EXPORT_ADDRESSES_LIMIT_TITLE'] = 'Exporta el límite de Direcciones:';
$mod_strings['LBL_CONFIG_EXPORT_ADDRESSES_LIMIT_DESC'] = "'export_addresses_limit' establece el limite de consultas cuando se selecciones registros para exportar.";
$mod_strings['LBL_CONFIG_ALLOW_APPROXIMATE_LOCATION_TYPE_TITLE'] = "Permitir a 'APPROXIMATE' Ubicación Tipos:";
$mod_strings['LBL_CONFIG_ALLOW_APPROXIMATE_LOCATION_TYPE_DESC'] = "'allow_approximate_location_type' - permite que los tipos de ubicación de 'APPROXIMATE' a tener en cuenta los resultados de geocodificación 'OK'.";

$mod_strings['LBL_CONFIG_ADDRESS_CACHE_SETTINGS_TITLE'] = 'Seteos de Caché de Direcciones:';
$mod_strings['LBL_CONFIG_ADDRESS_CACHE_GET_ENABLED_TITLE'] = 'Habilita el Cache de Direcciones (Get):';
$mod_strings['LBL_CONFIG_ADDRESS_CACHE_GET_ENABLED_DESC'] = "'address_cache_get_enabled' permite al módulo de caché de direcciones recuperar datos de la tabla de caché.";
$mod_strings['LBL_CONFIG_ADDRESS_CACHE_SAVE_ENABLED_TITLE'] = 'Habilita El guardado del Caché de Direcciones (Save):';
$mod_strings['LBL_CONFIG_ADDRESS_CACHE_SAVE_ENABLED_DESC'] = "'address_cache_save_enabled' permite que el módulo de caché de direcciones guarde datos en la tabla de caché.";

$mod_strings['LBL_CONFIG_LOGIC_HOOKS_SETTINGS_TITLE'] = 'Configuración de Logic Hooks:';
$mod_strings['LBL_CONFIG_LOGIC_HOOKS_ENABLED_TITLE'] = 'Habilita todos los Logic Hooks: ';
$mod_strings['LBL_CONFIG_LOGIC_HOOKS_ENABLED_DESC'] = "'logic_hooks_enabled' permite que los logic hooks actualicen automáticamente basado en los objetos relacionados. Se recomienda deshabilitar cuando se actualiza SuiteCRM.";

$mod_strings['LBL_CONFIG_MARKER_MAPPING_SETTINGS_TITLE'] = 'Configuración de Marcadores/Mapas:';
$mod_strings['LBL_CONFIG_MAP_MARKERS_LIMIT_TITLE'] = "Límite de Marcadores de Mapa:";
$mod_strings['LBL_CONFIG_MAP_MARKERS_LIMIT_DESC'] = "'map_markers_limit' establece el límite de consulta cuando se seleccionan registros para mostrar en un mapa.";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_CENTER_LATITUDE_TITLE'] = "Latitud Central por defecto en los Mapas:";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_CENTER_LATITUDE_DESC'] = "'map_default_center_latitude' establece la latitud central por defecto para los mapas.";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_CENTER_LONGITUDE_TITLE'] = "Longitud Central por defecto en los Mapas:";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_CENTER_LONGITUDE_DESC'] = "'map_default_center_longitude' establece la longitud central por defecto para los for mapas.";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_UNIT_TYPE_TITLE'] = "Tipo de Unidad por defecto en los Mapas:";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_UNIT_TYPE_DESC'] = "'map_default_unit_type' establece el tipo de unidad de medida por defecto para los calculos. Valores: 'mi' (millas) or 'km' (kilometros).";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_DISTANCE_TITLE'] = "Distancia por defecto en los Mapas:";
$mod_strings['LBL_CONFIG_MAP_DEFAULT_DISTANCE_DESC'] = "'map_default_distance' establece la unidad de meddida por defecto utilizada para las distancias.";
$mod_strings['LBL_CONFIG_MAP_DUPLICATE_MARKER_ADJUSTMENT_TITLE'] = "Ajuste de marca duplicada:";
$mod_strings['LBL_CONFIG_MAP_DUPLICATE_MARKER_ADJUSTMENT_DESC'] = "'map_duplicate_marker_adjustment' establece un ajuste de offset para ser agregado a la longitud y latitud en caso de posición de marcador duplicada.";
$mod_strings['LBL_CONFIG_MAP_CLUSTER_GRID_SIZE_TITLE'] = "Tamaño de Grilla de Marcadores de Clusters:";
$mod_strings['LBL_CONFIG_MAP_CLUSTER_GRID_SIZE_DESC'] = "'map_clusterer_grid_size' es usado para establecer el tamaño de la grilla para calcular los clusters de mapas.";
$mod_strings['LBL_CONFIG_MAP_MARKERS_CLUSTERER_MAX_ZOOM_TITLE'] = "Zoom máximo de Markers Clusters:";
$mod_strings['LBL_CONFIG_MAP_MARKERS_CLUSTERER_MAX_ZOOM_DESC'] = "'map_clusterer_max_zoom' es usado para establecer el nivel máximo de zoom  al cual el clusterizado no se aplica.";
$mod_strings['LBL_CONFIG_CUSTOM_CONTROLLER_DESC'] = "Nota Importante: Todas las configuraciones guardadas pueden ser encontradas en la tabla 'config' bao la categoría 'jjwg'. Nota: la utilización del archivo 'controller.php' personalizado para ignorar las configuraciones, queda obsoleto a partir de ahora.";
$mod_strings['LBL_JJWG_MAPS_JJWG_AREAS_FROM_JJWG_AREAS_TITLE'] = 'Map Áreas';
$mod_strings['LBL_JJWG_MAPS_JJWG_MARKERS_FROM_JJWG_MARKERS_TITLE'] = 'Marcadores';
$mod_strings['LBL_PARENT_ID'] = 'ID Padre';
$mod_strings['LBL_JJWP_PARTNERS'] = 'Socios JJWP';
$mod_strings['LBL_GET_GOOGLE_API_KEY'] = 'Obtener una clave';
$mod_strings['LBL_GOOGLE_API_KEY'] = 'Google Api Key';
$mod_strings['LBL_ERROR_NO_GOOGLE_API_KEY'] = 'Por favor seleccione el Google Api Key en el Panel administrativo de mapas de Google.';
