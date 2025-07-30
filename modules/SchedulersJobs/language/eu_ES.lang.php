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

$mod_strings = array(
    'LBL_NAME' => 'Nombre de Tarea',
    'LBL_EXECUTE_TIME' => 'Hora de Ejecución',
    'LBL_SCHEDULER_ID' => 'Planificador',
    'LBL_STATUS' => 'Estado de tarea',
    'LBL_RESOLUTION' => 'Resolución',
    'LBL_MESSAGE' => 'Mensajes',
    'LBL_DATA' => 'Fecha de tarea',
    'LBL_REQUEUE' => 'Vuelver a intentar en caso de fallo',
    'LBL_RETRY_COUNT' => 'Intentos máximos',
    'LBL_FAIL_COUNT' => 'Fallos',
    'LBL_INTERVAL' => 'Intervalo mínimo entre intentos',
    'LBL_CLIENT' => 'Ser propietario de cliente',
    'LBL_PERCENT' => 'Porcentaje completado',
// Errors
    'ERR_CALL' => "No se puede llamar a la función: %s",
    'ERR_CURL' => "No CURL - no se puede ejecutar trabajos de URL",
    'ERR_FAILED' => "Error inesperado, por favor, consulte los registros de PHP y suitecrm.log",
    'ERR_PHP' => "%s [%d]: %s en %s on line %d",
    'ERR_NOUSER' => "No ID de usuario especificado para el trabajo",
    'ERR_NOSUCHUSER' => "ID %s de usuario no encontrado",
    'ERR_JOBTYPE' => "Tipo de tarea desconocido: %s",
    'ERR_TIMEOUT' => "Fracaso forzoso por tiempo de espera",
    'ERR_JOB_FAILED_VERBOSE' => 'Tarea %1$s (%2$s) fallo en la ejecución del CRON',
);
