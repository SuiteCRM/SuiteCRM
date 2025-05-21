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
    'LBL_OOTB_WORKFLOW' => 'Procesar Tarefas de Fluxo de Traballo',
    'LBL_OOTB_REPORTS' => 'Executar Tarefas Programadas de Xeración de Informes',
    'LBL_OOTB_IE' => 'Comprobar Bandexas de Entrada',
    'LBL_OOTB_BOUNCE' => 'Executar Proceso Nocturno de Emails de Campaña Rebotados',
    'LBL_OOTB_CAMPAIGN' => 'Executar Proceso Nocturno de Campañas de Email Masivo',
    'LBL_OOTB_PRUNE' => 'Truncar Base de datos ao Inicio do Mes',
    'LBL_OOTB_TRACKER' => 'Truncar Táboas de Monitorización',
    'LBL_OOTB_SUITEFEEDS' => 'Limpar as Táboas de SuiteCRM Feed',
    'LBL_OOTB_LUCENE_INDEX' => 'Realizar o índice de Lucene',
    'LBL_OOTB_OPTIMISE_INDEX' => 'Optimizar o índice AOD',
    'LBL_OOTB_SEND_EMAIL_REMINDERS' => 'Executar Envío de Emails de Recordatorios',
    'LBL_OOTB_CLEANUP_QUEUE' => 'Limpar Cola de Traballos',
    'LBL_OOTB_REMOVE_DOCUMENTS_FROM_FS' => 'Extracción de documentos do sistema de arquivos',
    'LBL_OOTB_GOOGLE_CAL_SYNC' => 'Sincronización de Google Calendar',
    'LBL_OOTB_ELASTIC_INDEX' => 'Realizar Indexación de Elasticsearch',

// List Labels
    'LBL_LIST_JOB_INTERVAL' => 'Intervalo:',
    'LBL_LIST_LIST_ORDER' => 'Planificadores:',
    'LBL_LIST_NAME' => 'Planificador:',
    'LBL_LIST_RANGE' => 'Rango:',
    'LBL_LIST_STATUS' => 'Estado:',
    'LBL_LIST_TITLE' => 'Lista de Planificación:',
// human readable:
    'LBL_SUN' => 'Domingo',
    'LBL_MON' => 'Luns',
    'LBL_TUE' => 'Martes',
    'LBL_WED' => 'Mércores',
    'LBL_THU' => 'Xoves',
    'LBL_FRI' => 'Venres',
    'LBL_SAT' => 'Sábado',
    'LBL_ALL' => 'Todos os días',
    'LBL_EVERY' => 'Cada',
    'LBL_FROM' => 'Desde',
    'LBL_ON_THE' => 'No',
    'LBL_RANGE' => 'a',
    'LBL_AND' => 'e',
    'LBL_MINUTES' => 'minutos',
    'LBL_HOUR' => 'horas',
    'LBL_HOUR_SING' => 'hora',
    'LBL_OFTEN' => 'Tan a miúdo como sexa posible.',
    'LBL_MIN_MARK' => 'marca por minuto',


// crontabs
    'LBL_MINS' => 'min',
    'LBL_HOURS' => 'horas',
    'LBL_DAY_OF_MONTH' => 'data',
    'LBL_MONTHS' => 'me',
    'LBL_DAY_OF_WEEK' => 'día',
    'LBL_CRONTAB_EXAMPLES' => 'O arriba mostrado utiliza notación estándar de crontab.',
// Labels
    'LBL_ALWAYS' => 'Sempre',
    'LBL_CATCH_UP' => 'Executar Se Falla',
    'LBL_CATCH_UP_WARNING' => 'Desmarque se a execución desta tarefa pode durar máis dun momento.',
    'LBL_DATE_TIME_END' => 'Data e Hora de Fin',
    'LBL_DATE_TIME_START' => 'Data e Hora de Inicio',
    'LBL_INTERVAL' => 'Intervalo',
    'LBL_JOB' => 'Tarefa',
    'LBL_JOB_URL' => 'URL da tarefa',
    'LBL_LAST_RUN' => 'Última Execución Exitosa',
    'LBL_MODULE_NAME' => 'Planificador SuiteCRM',
    'LBL_MODULE_TITLE' => 'Planificadores',
    'LBL_NAME' => 'Nome de Tarefa',
    'LBL_NEVER' => 'Nunca',
    'LBL_NEW_FORM_TITLE' => 'Nova Planificación',
    'LBL_PERENNIAL' => 'continuo',
    'LBL_SEARCH_FORM_TITLE' => 'Busca de Planificación',
    'LBL_SCHEDULER' => 'Planificador:',
    'LBL_STATUS' => 'Estado',
    'LBL_TIME_FROM' => 'Activo Desde',
    'LBL_TIME_TO' => 'Activo Ata',
    'LBL_WARN_CURL_TITLE' => 'Aviso cURL:',
    'LBL_WARN_CURL' => 'Aviso:',
    'LBL_WARN_NO_CURL' => 'Este sistema non ten as librerías cURL habilitadas/compiladas no módulo de PHP (--with-curl=/ruta/a/libreria_curl).  Por favor, contacte co seu administrador para resolver o problema.  Sen a funcionalidade que provee cURL, o Planificador non pode utilizar fíos coas súas tarefas.',
    'LBL_BASIC_OPTIONS' => 'Configuración Básica',
    'LBL_ADV_OPTIONS' => 'Opcións Avanzadas',
    'LBL_TOGGLE_ADV' => 'Mostrar Opcións Avanzadas',
    'LBL_TOGGLE_BASIC' => 'Mostrar Opcións Básicas',
// Links
    'LNK_LIST_SCHEDULER' => 'Planificadores',
    'LNK_NEW_SCHEDULER' => 'Novo Planificador',
// Messages
    'ERR_CRON_SYNTAX' => 'Sintaxe de Cron inválida',
    'NTC_LIST_ORDER' => 'Estableza a orde na que esta planificación aparecerá nas listas despregables de selección de Planificador',
    'LBL_CRON_INSTRUCTIONS_WINDOWS' => 'Para configurar o Planificador de Windows',
    'LBL_CRON_INSTRUCTIONS_LINUX' => 'Para configurar Crontab',
    'LBL_CRON_LINUX_DESC1' => 'Para executar os planificadores de SuiteCRM, editar o arquivo crontab dos usuarios do servido web con este comando: ',
    'LBL_CRON_LINUX_DESC2' => 'e agregue a seguinte liña ao arquivo crontab: ',
    'LBL_CRON_LINUX_DESC3' => 'Vostede debería facer isto unha vez que a instalación finalice.',
    'LBL_CRON_WINDOWS_DESC' => 'Nota: Para executar os planificadores de SuiteCRM, cree un arquivo de proceso por lotes a executar utilizando as Tarefas Programadas de Windows. O arquivo de proceso por lotes debería conter os seguintes comandos:',
// Subpanels
    'LBL_JOBS_SUBPANEL_TITLE' => 'Rexistro de Tarefas',
    'LBL_EXECUTE_TIME' => 'Hora de Execución',

//jobstrings
    'LBL_REFRESHJOBS' => 'Refrescar traballos',
    'LBL_POLLMONITOREDINBOXES' => 'Comprobar contas de correo entrante',
    'LBL_PERFORMFULLFTSINDEX' => 'Sistema de índice de busca de texto completo',

    'LBL_RUNMASSEMAILCAMPAIGN' => 'Executar Proceso Nocturno de Campañas de Email Masivo',
    'LBL_POLLMONITOREDINBOXESFORBOUNCEDCAMPAIGNEMAILS' => 'Executar Proceso Nocturno de Emails de Campaña Rebotados',
    'LBL_PRUNEDATABASE' => 'Truncar Base de datos ao Inicio do Mes',
    'LBL_TRIMTRACKER' => 'Truncar Táboas de Monitorización',
    'LBL_TRIMSUGARFEEDS' => 'Limpar as Táboas de SuiteCRM Feed',
    'LBL_SENDEMAILREMINDERS' => 'Executar envío de correos recordatorios',
    'LBL_CLEANJOBQUEUE' => 'Limpar cola de traballos',
    'LBL_REMOVEDOCUMENTSFROMFS' => 'Extracción de documentos do sistema de arquivos',

    'LBL_AODOPTIMISEINDEX' => 'Optimizar índice de Advanced OpenDicsovery',
    'LBL_AODINDEXUNINDEXED' => 'Indexar documentos non indexados',
    'LBL_POLLMONITOREDINBOXESAOP' => 'Bandexas de entrada monitoreadas por Enquisas de AOP',
    'LBL_AORRUNSCHEDULEDREPORTS' => 'Executar informes programados',
    'LBL_PROCESSAOW_WORKFLOW' => 'Procesar fluxos de traballo de AOW',

    'LBL_RUNELASTICSEARCHINDEXERSCHEDULER' => 'Indizador de Elasticsearch',

    'LBL_SCHEDULER_TIMES' => 'Horarios do Calendarizador',
    'LBL_SYNCGOOGLECALENDAR' => 'Sincronizar os calendarios de Google',
);

global $sugar_config;
