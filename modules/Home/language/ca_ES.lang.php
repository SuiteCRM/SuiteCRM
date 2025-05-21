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
    'LBL_MODULE_NAME' => 'Inici',
    'LBL_NEW_FORM_TITLE' => 'Nou Contacte',
    'LBL_FIRST_NAME' => 'Nom:',
    'LBL_LAST_NAME' => 'Cognoms:',
    'LBL_LIST_LAST_NAME' => 'Cognoms',
    'LBL_PHONE' => 'Telèfon:',
    'LBL_EMAIL_ADDRESS' => 'Adreça de correu electrònic:',
    'LBL_MY_PIPELINE_FORM_TITLE' => 'El meu Objectiu',
    'LBL_PIPELINE_FORM_TITLE' => 'Objectiu per Etapa de Vendes',
    'LBL_RGraph_PIPELINE_FORM_TITLE' => 'Objectiu Per Etapa de Vendes',
    'LNK_NEW_CONTACT' => 'Nou Contacte',
    'LNK_NEW_ACCOUNT' => 'Nou Compte',
    'LNK_NEW_OPPORTUNITY' => 'Nova Oportunitat',
    'LNK_NEW_LEAD' => 'Nou Client Potencial',
    'LNK_NEW_CASE' => 'Nou Cas',
    'LNK_NEW_NOTE' => 'Nova Nota o Arxiu Adjunt',
    'LNK_NEW_CALL' => 'Programa una Trucada',
    'LNK_NEW_EMAIL' => 'Arxiva un Correu electrònic',
    'LNK_NEW_MEETING' => 'Programa una Reunió',
    'LNK_NEW_TASK' => 'Nova Tasca',
    'LNK_NEW_BUG' => 'Informa d\'una Incidència',
    'LNK_NEW_SEND_EMAIL' => 'Redacta un Correu electrònic',
    'LBL_NO_ACCESS' => 'No teniu accés a aquesta zona. Contacteu amb l\'administrador.',
    'LBL_NO_RESULTS_IN_MODULE' => '-- Sense resultats --',
    'LBL_NO_RESULTS' => '<h2>No s\'han trobat resultats. Realitzeu una nova cerca.</h2><br>',
    'LBL_NO_RESULTS_TIPS' => '<h3>Trucs per a la cerca:</h3><ul><li>Assegureu-vos d\'haver seleccionat les categories adequades.</li><li>Amplieu els criteris de cerca.</li><li>Si tot i així no obteniu resultats, proveu la cerca avançada.</li></ul>',

    'LBL_ADD_DASHLETS' => 'Afegeix Dashlets',
    'LBL_WEBSITE_TITLE' => 'Lloc web',
    'LBL_RSS_TITLE' => 'Font de notícies',
    'LBL_CLOSE_DASHLETS' => 'Tanca',
    'LBL_OPTIONS' => 'Opcions',
    // dashlet search fields
    'LBL_TODAY' => 'Avui',
    'LBL_YESTERDAY' => 'Ahir',
    'LBL_TOMORROW' => 'Demà',
    'LBL_NEXT_WEEK' => 'La setmana que ve',
    'LBL_LAST_7_DAYS' => 'Els darrers 7 dies',
    'LBL_NEXT_7_DAYS' => 'Els propers 7 dies',
    'LBL_LAST_MONTH' => 'El mes passat',
    'LBL_NEXT_MONTH' => 'El mes que ve',
    'LBL_LAST_YEAR' => 'L\'any passat',
    'LBL_NEXT_YEAR' => 'L\'any que ve',
    'LBL_LAST_30_DAYS' => 'Els darrers 30 dies',
    'LBL_NEXT_30_DAYS' => 'Els propers 30 dies',
    'LBL_THIS_MONTH' => 'Aquest mes',
    'LBL_THIS_YEAR' => 'Aquest any',

    'LBL_MODULES' => 'Mòduls',
    'LBL_CHARTS' => 'Gràfics',
    'LBL_TOOLS' => 'Eines',
    'LBL_WEB' => 'Web',
    'LBL_SEARCH_RESULTS' => 'Resultats de la cerca',

    // Dashlet Categories
    'dashlet_categories_dom' => array(
        'Module Views' => 'Vistes del mòdul',
        'Portal' => 'Portal',
        'Charts' => 'Gràfics',
        'Tools' => 'Eines',
        'Miscellaneous' => 'Altres'
    ),
    'LBL_ADDING_DASHLET' => 'Afegint el dashlet...',
    'LBL_ADDED_DASHLET' => 'S\'ha afegit el dashlet',
    'LBL_REMOVE_DASHLET_CONFIRM' => 'Segur que voleu eliminar el dashlet?',
    'LBL_REMOVING_DASHLET' => 'Eliminant el dashlet...',
    'LBL_REMOVED_DASHLET' => 'S\'ha eliminat el Dashlet',
    'LBL_DASHLET_CONFIGURE_GENERAL' => 'General',
    'LBL_DASHLET_CONFIGURE_FILTERS' => 'Filtres',
    'LBL_DASHLET_CONFIGURE_MY_ITEMS_ONLY' => 'Només els meus elements',
    'LBL_DASHLET_CONFIGURE_TITLE' => 'Títol',
    'LBL_DASHLET_CONFIGURE_DISPLAY_ROWS' => 'Files a mostrar',

    'LBL_DASHLET_DELETE' => 'Elimina el dashlet',
    'LBL_DASHLET_REFRESH' => 'Actualitza el dashlet',
    'LBL_DASHLET_EDIT' => 'Edita el dashlet',

    // Default out-of-box names for tabs
    'LBL_HOME_PAGE_1_NAME' => 'El meu CRM',
    'LBL_CLOSE_SITEMAP' => 'Tanca',

    'LBL_SEARCH' => 'Cerca',
    'LBL_CLEAR' => 'Neteja',

    'LBL_BASIC_CHARTS' => 'Gràfics bàsics',

    'LBL_DASHLET_SEARCH' => 'Cerca els dashlets',

//ABOUT page
    'LBL_VERSION' => 'Versió',
    'LBL_BUILD' => 'Compilació',

    'LBL_SOURCE_SUGAR' => 'SugarCRM Inc - proveïdor del CE framework',

    'LBL_DASHLET_TITLE' => 'Portal',
    'LBL_DASHLET_OPT_TITLE' => 'Títol',
    'LBL_DASHLET_INCORRECT_URL' => 'La ubicació especificada del web és incorrecta',
    'LBL_DASHLET_OPT_URL' => 'Adreça del lloc web',
    'LBL_DASHLET_OPT_HEIGHT' => 'Altura del dashlet (en píxels)',
    'LBL_DASHLET_SUITE_NEWS' => 'Notícies de SuiteCRM',
    'LBL_DASHLET_DISCOVER_SUITE' => 'Descobreix SuiteCRM',
    'LBL_BASIC_SEARCH' => 'Filtre ràpid' /*for 508 compliance fix*/,
    'LBL_ADVANCED_SEARCH' => 'Filtre avançat' /*for 508 compliance fix*/,
    'LBL_TOUR_HOME' => 'Icona de la pàgina d\'inici',
    'LBL_TOUR_HOME_DESCRIPTION' => 'Torneu a la pàgina d\'inici en un sol clic.',
    'LBL_TOUR_MODULES' => 'Mòdul',
    'LBL_TOUR_MODULES_DESCRIPTION' => 'Tots els mòduls importants són aquí.',
    'LBL_TOUR_MORE' => 'Més mòduls',
    'LBL_TOUR_MORE_DESCRIPTION' => 'Els altres mòduls són aquí.',
    'LBL_TOUR_SEARCH' => 'Cerca de text complet',
    'LBL_TOUR_SEARCH_DESCRIPTION' => 'La cerca és ara molt millor.',
    'LBL_TOUR_NOTIFICATIONS' => 'Notificacions',
    'LBL_TOUR_NOTIFICATIONS_DESCRIPTION' => 'Les notificacions de SuiteCRM es veuran aquí.',
    'LBL_TOUR_PROFILE' => 'Perfil',
    'LBL_TOUR_PROFILE_DESCRIPTION' => 'Accedeix al perfil, la configuració i a finalitzar la sessió.',
    'LBL_TOUR_QUICKCREATE' => 'Creació ràpida',
    'LBL_TOUR_QUICKCREATE_DESCRIPTION' => 'Creació ràpida de registres sense canviar de lloc.',
    'LBL_TOUR_FOOTER' => 'Peu de pàgina plegable',
    'LBL_TOUR_FOOTER_DESCRIPTION' => 'Plegueu i desplegueu el peu de pàgina facilment.',
    'LBL_TOUR_CUSTOM' => 'Integracions personalitzades',
    'LBL_TOUR_CUSTOM_DESCRIPTION' => 'Les integracions personalitzades haurien de ser aquí.',
    'LBL_TOUR_BRAND' => 'La vostra marca',
    'LBL_TOUR_BRAND_DESCRIPTION' => 'El vostre logo va aquí. Passeu el ratolí per sobre per a més informació.',
    'LBL_TOUR_WELCOME' => 'Benvingut a SuiteCRM',
    'LBL_TOUR_WATCH' => 'Mostra les novetats de SuiteCRM',
    'LBL_TOUR_FEATURES' => '<ul style=""><li class="icon-ok">Nova barra de navegació simplificada</li><li class="icon-ok">Nou peu plegable</li><li class="icon-ok">Millora de la cerca</li><li class="icon-ok">Actualització del menú d\'accions</li></ul><p>I molt més!</p>',
    'LBL_TOUR_VISIT' => 'Per a més informació visiteu la nostra aplicació',
    'LBL_TOUR_DONE' => 'Ja està!',
    'LBL_TOUR_REFERENCE_1' => 'Sempre podeu referenciar la nostra',
    'LBL_TOUR_REFERENCE_2' => 'a través de l\'enllaç de suport a la pestanya del perfil.',
    'LNK_TOUR_DOCUMENTATION' => 'documentació',
    'LBL_TOUR_CALENDAR_URL_1' => 'Compartiu el vostre calendari de SuiteCRM amb aplicacions de tercers com Microsoft Outlook o Exchange? Si és així, teniu una adreça URL nova. Aquesta nova URL, més segura, inclou una clau personal que evitarà la publicació no autoritzada del calendari.',
    'LBL_TOUR_CALENDAR_URL_2' => 'Recupereu la URL del nou calendari compartit',
    'LBL_CONTRIBUTORS' => 'Col·laboradors',
    'LBL_ABOUT_SUITE' => 'Quant a SuiteCRM',
    'LBL_PARTNERS' => 'Socis',
    'LBL_FEATURING' => 'Els mòduls AOS, AOW, AOR, AOP, AOE i Replanificació són de SalesAgility',

    'LBL_CONTRIBUTOR_SUITECRM' => 'SuiteCRM - CRM de codi obert per a tot el món',
    'LBL_CONTRIBUTOR_SECURITY_SUITE' => 'SecuritySuite de Jason Eggers',
    'LBL_CONTRIBUTOR_JJW_GMAPS' => 'JJWDesign Google Maps de Jeffrey J.Walters',
    'LBL_CONTRIBUTOR_CONSCIOUS' => 'SuiteCRM LOGO proporcionat per Conscious Solutions',
    'LBL_CONTRIBUTOR_RESPONSETAP' => 'Contribució de ResponseTap a SuiteCRM 7.3',
    'LBL_CONTRIBUTOR_GMBH' => 'Camps calculats dels fluxos de treball aportats per diligent technology & business consulting GmbH',

    'LBL_LANGUAGE_ABOUT' => 'Quant a les traduccions de SuiteCRM',
    'LBL_LANGUAGE_COMMUNITY_ABOUT' => 'Traducció col·laborativa de la comunitat de SuiteCRM',
    'LBL_LANGUAGE_COMMUNITY_PACKS' => 'Traducció creada utilitzant Crowdin',

    'LBL_ABOUT_SUITE_2' => 'SuiteCRM està publicat sota la llicència de codi obert AGPLv3',
    'LBL_ABOUT_SUITE_4' => 'Tot el codi desenvolupat i administrat pel projecte SuiteCRM serà publicat com a codi obert sota la llicència AGPLv3',
    'LBL_ABOUT_SUITE_5' => 'SuiteCRM compta amb serveis de suport tant gratuïts com de pagament',

    'LBL_SUITE_PARTNERS' => 'Hi ha socis lleials de SuiteCRM que són apassionats del codi obert. Per veure\'n la llista completa, visiteu el nostre web.',

    'LBL_SAVE_BUTTON' => 'Desa',
    'LBL_DELETE_BUTTON' => 'Elimina',
    'LBL_APPLY_BUTTON' => 'Aplica',
    'LBL_SEND_INVITES' => 'Envia invitacions',
    'LBL_CANCEL_BUTTON' => 'Cancel·la',
    'LBL_CLOSE_BUTTON' => 'Tanca',

    'LBL_CREATE_NEW_RECORD' => 'Crea una activitat',
    'LBL_CREATE_CALL' => 'Programa una Trucada',
    'LBL_CREATE_MEETING' => 'Programa una Reunió',

    'LBL_GENERAL_TAB' => 'Detalls',
    'LBL_PARTICIPANTS_TAB' => 'Assistents',
    'LBL_REPEAT_TAB' => 'Repetició',

    'LBL_REPEAT_TYPE' => 'Repeteix',
    'LBL_REPEAT_INTERVAL' => 'Cada',
    'LBL_REPEAT_END' => 'Últim',
    'LBL_REPEAT_END_AFTER' => 'Després de',
    'LBL_REPEAT_OCCURRENCES' => 'recurrències',
    'LBL_REPEAT_END_BY' => 'Per',
    'LBL_REPEAT_DOW' => 'En',
    'LBL_REPEAT_UNTIL' => 'Repeteix fins',
    'LBL_REPEAT_COUNT' => 'Nombre de recurrències',
    'LBL_REPEAT_LIMIT_ERROR' => 'La vostra sol·licitud hauria creat més de $limit reunions.',

    //Events
    'LNK_EVENT' => 'Esdeveniment',
    'LNK_EVENT_VIEW' => 'Mostra l\'esdeveniment',
    'LBL_DATE' => 'Data: ',
    'LBL_DURATION' => 'Durada: ',
    'LBL_NAME' => 'Títol: ',
    'LBL_HOUR_ABBREV' => 'hora',
    'LBL_HOURS_ABBREV' => 'hores',
    'LBL_MINSS_ABBREV' => 'minuts',
    'LBL_LOCATION' => 'Lloc:',
    'LBL_STATUS' => 'Estat:',
    'LBL_DESCRIPTION' => 'Descripció: ',
    //End Events

    'LBL_ELASTIC_SEARCH_EXCEPTION_SEARCH_INVALID_REQUEST' => 'S\'ha produït un error mentre es realitzava la cerca. La sintaxi de la consulta podria no ser vàlida.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_SEARCH_ENGINE_NOT_FOUND' => 'No es pot trobar el motor de cerca sol·licitat. Proveu de tornar a fer la cerca.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_NO_NODES_AVAILABLE' => 'No s\'ha pogut connectar amb el servidor d\'Elasticsearch.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_SEARCH' => 'S\'ha produït un error intern a la cerca.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_DEFAULT' => 'S\'ha produït un error desconegut mentre es realitzava la cerca.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_END_MESSAGE' => 'Si el problema persisteix, contacteu amb un administrador. Més informació al registre de l\'aplicació.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_MISSING_INDEX' => 'No s\'ha pogut trobar l\'índex de cerca per a un o més mòduls. Si us plau, assegureu-vos que el crontab està configurat i en funcionament, obriu la configuració d\'Elasticsearch i feu clic a "Planificar indexació completa" i considereu crear una tasca del Scheduler "Indexador d\'Elasticsearch" si no existeix.',

    'LBL_ELASTIC_SEARCH_DEFAULT' => 'No hi ha resultats que coincideixin amb els criteris de cerca. Proveu d\'ampliar la cerca.',

    'LNK_TASK_VIEW' => 'Veure Tasca',
);
