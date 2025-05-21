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
// OOTB Scheduler Job Names:
    'LBL_OOTB_WORKFLOW' => 'Tasques de Procés de Workflow',
    'LBL_OOTB_REPORTS' => 'Executar Tasques Programadas de Generació d\'Informes',
    'LBL_OOTB_IE' => 'Comprovar Safata d\'Entrada',
    'LBL_OOTB_BOUNCE' => 'Executar procés nocturn de correus electrònics de campanya rebotats',
    'LBL_OOTB_CAMPAIGN' => 'Executar procés nocturn de campanyes de correu electrònics massiu',
    'LBL_OOTB_PRUNE' => 'Truncar Base de dades al Inici de Mes',
    'LBL_OOTB_TRACKER' => 'Netejar la Taula de Historial d\'Usuari a primer de Mes',
    'LBL_OOTB_SUITEFEEDS' => 'Retallar les taules Feed de SuiteCRM',
    'LBL_OOTB_LUCENE_INDEX' => 'Realitzar l\'índex de Lucene',
    'LBL_OOTB_OPTIMISE_INDEX' => 'Optimitzar l\'índex AOD',
    'LBL_OOTB_SEND_EMAIL_REMINDERS' => 'Executar les notificacions de recordatori per correu electrònic',
    'LBL_OOTB_CLEANUP_QUEUE' => 'Netejar la cua de tasques',
    'LBL_OOTB_REMOVE_DOCUMENTS_FROM_FS' => 'Eliminar documents del sistema de fitxers',
    'LBL_OOTB_GOOGLE_CAL_SYNC' => 'Sincronització amb Calendari de Google',
    'LBL_OOTB_ELASTIC_INDEX' => 'Realitzar Indexació d\'Elasticsearch',

// List Labels
    'LBL_LIST_JOB_INTERVAL' => 'Interval',
    'LBL_LIST_LIST_ORDER' => 'Programadors:',
    'LBL_LIST_NAME' => 'Planificador:',
    'LBL_LIST_RANGE' => 'Ranc:',
    'LBL_LIST_STATUS' => 'Estat:',
    'LBL_LIST_TITLE' => 'Llista de programacions:',
// human readable:
    'LBL_SUN' => 'Diumenge',
    'LBL_MON' => 'Dilluns',
    'LBL_TUE' => 'Dimarts',
    'LBL_WED' => 'Dimecres',
    'LBL_THU' => 'Dijous',
    'LBL_FRI' => 'Divendres',
    'LBL_SAT' => 'Dissabte',
    'LBL_ALL' => 'Cada dia',
    'LBL_EVERY' => 'Cada',
    'LBL_FROM' => 'des de',
    'LBL_ON_THE' => 'en el',
    'LBL_RANGE' => ' a ',
    'LBL_AND' => 'i',
    'LBL_MINUTES' => 'minuts',
    'LBL_HOUR' => 'hores',
    'LBL_HOUR_SING' => 'hora',
    'LBL_OFTEN' => 'Tan sovint com sigui possible.',
    'LBL_MIN_MARK' => 'marca per minut',


// crontabs
    'LBL_MINS' => 'min',
    'LBL_HOURS' => 'hrs',
    'LBL_DAY_OF_MONTH' => 'data',
    'LBL_MONTHS' => 'mes',
    'LBL_DAY_OF_WEEK' => 'dia',
    'LBL_CRONTAB_EXAMPLES' => 'El que es mostra a sobre utilitza la notació estàndard de crontab.',
// Labels
    'LBL_ALWAYS' => 'Sempre',
    'LBL_CATCH_UP' => 'Executar si falla',
    'LBL_CATCH_UP_WARNING' => 'des-sel·leccioni de l\'execució d\'aquesta tasca pot durar més d\'un moment.',
    'LBL_DATE_TIME_END' => 'Data i hora de finalització',
    'LBL_DATE_TIME_START' => "Data i hora d&#39;inici", // Excepció d'escapat
    'LBL_INTERVAL' => 'Interval',
    'LBL_JOB' => 'Treball',
    'LBL_JOB_URL' => 'URL del treball',
    'LBL_LAST_RUN' => 'Última execució exitosa',
    'LBL_MODULE_NAME' => 'Planificador de SuiteCRM',
    'LBL_MODULE_TITLE' => 'Planificacions',
    'LBL_NAME' => 'Nom del treball',
    'LBL_NEVER' => 'Mai',
    'LBL_NEW_FORM_TITLE' => 'Nova programació',
    'LBL_PERENNIAL' => 'perpetu',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca de programació',
    'LBL_SCHEDULER' => 'Programador:',
    'LBL_STATUS' => 'Estat',
    'LBL_TIME_FROM' => 'Actiu des de',
    'LBL_TIME_TO' => 'Actiu fins',
    'LBL_WARN_CURL_TITLE' => 'Avís cURL:',
    'LBL_WARN_CURL' => 'Advertència:',
    'LBL_WARN_NO_CURL' => 'Aquest sistema no té les llibreries cURL habilitades / compilades en el mòdul de PHP (--with-curl=/ruta/a/libreria_curl). Si us plau, contacteu amb l\'administrador per resoldre el problema. Sense la funcionalitat que proveeix cURL, el planificador no pot utilitzar fils amb les seves tasques.',
    'LBL_BASIC_OPTIONS' => 'Configuració bàsica',
    'LBL_ADV_OPTIONS' => 'Opcions avançades',
    'LBL_TOGGLE_ADV' => 'Mostrar les opcions avançades',
    'LBL_TOGGLE_BASIC' => 'Mostrar opcions bàsiques',
// Links
    'LNK_LIST_SCHEDULER' => 'Programadors',
    'LNK_NEW_SCHEDULER' => 'Crear programador',
// Messages
    'ERR_CRON_SYNTAX' => 'Sintaxi de cron invàlida',
    'NTC_LIST_ORDER' => 'Estableixi l\'ordre en que aquesta planificació apareixerà a les llistes desplegables de la secció del Planificador',
    'LBL_CRON_INSTRUCTIONS_WINDOWS' => 'Per a configurar el calendari de Windows',
    'LBL_CRON_INSTRUCTIONS_LINUX' => 'Per a configurar el Crontab',
    'LBL_CRON_LINUX_DESC1' => 'Per executar els planificadors de SuiteCRM, editar el arxiu crontab dels usuaris del servidor web amb aquest comandament: ',
    'LBL_CRON_LINUX_DESC2' => '... afegir la següent línia al fitxer crontab: ',
    'LBL_CRON_LINUX_DESC3' => 'Vostè hauria de fer això una vegada que la instal·lació ha finalitzat.',
    'LBL_CRON_WINDOWS_DESC' => 'Nota: Per executar els planificadors de SuiteCRM, creeu un fitxer de procés per lots a executar utilitzant els Tasques Programades de Windows. L\'arxiu de procés per lots hauria de contenir les següents comandes:',
// Subpanels
    'LBL_JOBS_SUBPANEL_TITLE' => 'Registre de treball',
    'LBL_EXECUTE_TIME' => 'Hora d\'execució',

//jobstrings
    'LBL_REFRESHJOBS' => 'Refrescar treballs',
    'LBL_POLLMONITOREDINBOXES' => 'Comprovar els comptes de correu d\'entrada',
    'LBL_PERFORMFULLFTSINDEX' => 'Sistema d\'índex de cerca de text complet',

    'LBL_RUNMASSEMAILCAMPAIGN' => 'Executar procés nocturn de campanyes de correu electrònics massiu',
    'LBL_POLLMONITOREDINBOXESFORBOUNCEDCAMPAIGNEMAILS' => 'Executar procés nocturn de correus electrònics de campanya rebotats',
    'LBL_PRUNEDATABASE' => 'Truncar Base de dades al Inici de Mes',
    'LBL_TRIMTRACKER' => 'Netejar la Taula de Historial d\'Usuari a primer de Mes',
    'LBL_TRIMSUGARFEEDS' => 'Retallar les taules Feed de SuiteCRM',
    'LBL_SENDEMAILREMINDERS' => 'Executar l\'enviament de notificacions d\'E-mail',
    'LBL_CLEANJOBQUEUE' => 'Neteja de la Cua de feines',
    'LBL_REMOVEDOCUMENTSFROMFS' => 'Eliminar documents del sistema de fitxers',

    'LBL_AODOPTIMISEINDEX' => 'Optimitzar l\'índex d\'Advanced OpenDiscovery',
    'LBL_AODINDEXUNINDEXED' => 'Indexar documents no indexats',
    'LBL_POLLMONITOREDINBOXESAOP' => 'AOP proposta de monitoritzat Inboxes',
    'LBL_AORRUNSCHEDULEDREPORTS' => 'Executar informes planificats',
    'LBL_PROCESSAOW_WORKFLOW' => 'Processar AOW flux de treball',

    'LBL_RUNELASTICSEARCHINDEXERSCHEDULER' => 'Indexador de Elasticsearch',

    'LBL_SCHEDULER_TIMES' => 'Horaris del planificador',
    'LBL_SYNCGOOGLECALENDAR' => 'Sincronitzar Calendaris de Google',
);

global $sugar_config;
