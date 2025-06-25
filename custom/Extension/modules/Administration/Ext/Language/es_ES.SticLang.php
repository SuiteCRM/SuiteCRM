<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

// Sección de administración de SinergiaCRM
$mod_strings['LBL_SINERGIACRM_TAB_TITLE'] = 'SinergiaCRM';
$mod_strings['LBL_SINERGIACRM_TAB_DESCRIPTION'] = 'Opciones de administración de SinergiaCRM';

// Elementos de la sección de administración (títulos y descripciones cortas)
$mod_strings['LBL_STIC_VALIDATION_ACTIONS_LINK_TITLE'] = 'Acciones de validación';
$mod_strings['LBL_STIC_VALIDATION_ACTIONS_DESCRIPTION'] = 'Gestiona las acciones de validación y permite vincularlas a las tareas programadas.';

$mod_strings['LBL_STIC_VALIDATION_RESULTS_LINK_TITLE'] = 'Resultados de validación';
$mod_strings['LBL_STIC_VALIDATION_RESULTS_DESCRIPTION'] = 'Gestiona y revisa los resultados de las acciones de validación.';

$mod_strings['LBL_STIC_CUSTOM_VIEWS_LINK_TITLE'] = 'Vistas personalizadas';
$mod_strings['LBL_STIC_CUSTOM_VIEWS_DESCRIPTION'] = 'Personalización condicional de las vistas de los módulos.';

$mod_strings['LBL_STIC_SETTINGS_LINK_TITLE'] = 'Configuración';
$mod_strings['LBL_STIC_SETTINGS_DESCRIPTION'] = 'Opciones de configuración de SinergiaCRM.';

$mod_strings['LBL_STIC_TEST_DATA_LINK_TITLE'] = 'Datos de prueba';
$mod_strings['LBL_STIC_TEST_DATA_DESCRIPTION'] = 'Cargar o eliminar datos de prueba.';

$mod_strings['LBL_STIC_SINERGIADA_LINK_TITLE'] = 'Sinergia Data Analytics';
$mod_strings['LBL_STIC_SINERGIADA_DESCRIPTION'] = 'Reconstruye la integración con Sinergia Data Analytics.';
$mod_strings['LBL_STIC_SINERGIADA_MAX_USERS_ERROR'] = 'Se ha excedido el límite de usuarios no administradores en SinergiaDA. Máximo permitido: <b>__max_users__</b>. Valor actual: <b>__enabled_users__</b>. Desactive los usuarios de más y reinténtelo.';

$mod_strings['LBL_STIC_MAIN_MENU_LINK_TITLE'] = 'Menú principal';
$mod_strings['LBL_STIC_MAIN_MENU_DESCRIPTION'] = 'Configuración de la estructura y el contenido del menú';

// Datos de prueba
$mod_strings['LBL_STIC_TEST_DATA_NOTICE'] = '<strong>Importante:</strong> Los registros de prueba cargados en los diferentes módulos no deben usarse para almacenar datos reales, ya que pueden ser eliminados en el futuro.';
$mod_strings['LBL_STIC_TEST_DATA_INSERT_LINK_TITLE'] = 'Cargar el conjunto de datos de prueba';
$mod_strings['LBL_STIC_TEST_DATA_INSERT_DESCRIPTION'] = 'Cargar un conjunto de datos de prueba para facilitar el aprendizaje del uso de SinergiaCRM. Estos datos podrán ser eliminados a voluntad en cualquier momento posterior.';
$mod_strings['LBL_STIC_TEST_DATA_INSERT_SUCCESS'] = 'Se han insertado los datos de prueba correctamente.';
$mod_strings['LBL_STIC_TEST_DATA_INSERT_ERROR'] = 'Se han producido errores al insertar los datos de prueba. Revise el <a target="_blank" href="index.php?action=LogView&module=Configurator&doaction=all&filter=action_insertSticData">log</a>.';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_LINK_TITLE'] = 'Eliminar el conjunto de datos de prueba';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_DESCRIPTION'] = 'Eliminar el conjunto de datos de prueba previamente cargado.';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_SUCCESS'] = 'Se han eliminado los datos de prueba correctamente.';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_ERROR'] = 'Se han producido errores al eliminar los datos de prueba. Revise el <a target="_blank" href="index.php?action=LogView&module=Configurator&doaction=all&filter=action_removeSticData">log</a>.';

// SinergiaDA
$mod_strings['LBL_STIC_RUN_SDA_ACTIONS_LINK_TITLE'] = 'Reconstruir ahora';
$mod_strings['LBL_STIC_RUN_SDA_ACTIONS_DESCRIPTION'] = 'Reconstruye y repara las vistas y los elementos necesarios para la integración con Sinergia Data Analytics. Añade nuevos campos si los hay.';
$mod_strings['LBL_STIC_GO_TO_SDA_LINK_TITLE'] = 'Ir a Sinergia Data Analytics';
$mod_strings['LBL_STIC_RUN_SDA_SUCCESS_MSG'] = 'La reconstrucción de Sinergia Data Analytics se ha completado con éxito.';
$mod_strings['LBL_STIC_RUN_SDA_ERROR_MSG'] = 'Durante la reconstrucción de Sinergia Data Analytics se han encontrado los siguientes errores. Contacte con el soporte técnico de SinergiaTIC si lo considera necesario.';

// Menú principal avanzado
$mod_strings['LBL_STIC_MENU_CONFIGURE_TITLE'] = 'Configuración del menú principal';
$mod_strings['LBL_STIC_MENU_ENABLED_NOT_INCLUDED'] = 'Módulos habilitados no incluidos en el menú';
$mod_strings['LBL_STIC_MENU_ENABLED_INCLUDED'] = 'Configuración del menú';
$mod_strings['LBL_STIC_MENU_SAVE'] = 'Guardar y aplicar';
$mod_strings['LBL_STIC_MENU_RESTORE'] = 'Restaurar';
$mod_strings['LBL_STIC_MENU_RESTORE_CONFIRM'] = '¿Restaurar el menu prederminado de SinergiaCRM?';
$mod_strings['LBL_STIC_MENU_INFO'] = 'El menú principal contiene dos tipos de elementos: por un lado, accesos directos a los diferentes módulos de SinergiaCRM y, por el otro, nodos de soporte que pueden usarse para agrupar módulos, enlazar con otros sitios web, etc. Estos últimos se identifican mediante una marca de color en la esquina inferior derecha. Para incluir un módulo en el menú principal debe estar <a href="index.php?module=Administration&action=ConfigureTabs" target="_blank">habilitado</a>. Si ya lo está, puede arrastrarse de la zona de módulos no incluidos (derecha) hacia el nodo del menú donde se desee que aparezca (izquierda). Para reorganizar el menú, arrastre cualquier elemento a la posición deseada. Con el botón derecho del ratón se puede mostrar el menú contextual asociado a cada nodo, que le permitirá crear nuevos nodos (que en el caso de los de soporte pueden apuntar a cualquier URL), duplicarlos, cambiar su nombre (solo en el caso de los nodos de soporte) o eliminarlos. La modificación de nombres de módulos debe hacerse en <a href="index.php?action=wizard&module=Studio&wizard=StudioWizard&option=RenameTabs">Renombrar pestañas</a>.';
$mod_strings['LBL_STIC_MENU_ICONS'] = 'Mostrar iconos de módulos';
$mod_strings['LBL_STIC_MENU_ALL'] = 'Mostrar la opción TODO';
$mod_strings['LBL_STIC_MENU_COMMAND_CREATE'] = 'Crear';
$mod_strings['LBL_STIC_MENU_COMMAND_CREATE_DEFAULT'] = 'Nuevo nodo';
$mod_strings['LBL_STIC_MENU_COMMAND_RENAME'] = 'Renombrar';
$mod_strings['LBL_STIC_MENU_COMMAND_EDITURL'] = 'Editar la URL';
$mod_strings['LBL_STIC_MENU_COMMAND_EDITURL_PROMPT'] = 'Escriba la URL';
$mod_strings['LBL_STIC_MENU_COMMAND_EDITURL_PROMPT_VALIDATE'] = 'Escriba una URL válida';
$mod_strings['LBL_STIC_MENU_COMMAND_REMOVE'] = 'Eliminar';
$mod_strings['LBL_STIC_MENU_COMMAND_DUPLICATE'] = 'Duplicar';
$mod_strings['LBL_STIC_MENU_COMMAND_NEW_MAIN_NODE'] = 'Nuevo nodo principal';
$mod_strings['LBL_STIC_MENU_COMMAND_EXPAND'] = 'Expandir árbol';
$mod_strings['LBL_STIC_MENU_COMMAND_COLLAPSE'] = 'Contraer árbol';

// Cadenas de SuiteCRM modificadas
$mod_strings['LBL_CONFIGURE_GROUP_TABS'] = 'Agrupación de subpaneles';
$mod_strings['LBL_CONFIGURE_GROUP_TABS_DESC'] = 'Configuración de la agrupación de los subpaneles en las vistas de detalle';

// Otras cadenas
$mod_strings['LBL_TRACKERS_TITLE'] = 'Monitorización';
$mod_strings['LBL_TRACKERS_DESCRIPTION'] = 'Registro de las sesiones de usuario y de las acciones sobre registros.';
$mod_strings['LBL_ADMIN_ACTIONS'] = 'Acciones de Administración';
$mod_strings['ERR_SYS_GEN_PWD_TPL_NOT_SELECTED'] = 'Especificar la plantilla de correo que se usará cuando el sistema genere la contraseña de un nuevo usuario.';
