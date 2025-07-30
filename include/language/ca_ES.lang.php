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



$app_list_strings = array(
    'language_pack_name' => 'Català (Catalan) - ca_ES',
    'moduleList' => array(
        'Home' => 'Inici',
        'ResourceCalendar' => 'Calendari del recurs',
        'Contacts' => 'Contactes',
        'Accounts' => 'Comptes',
        'Alerts' => 'Alertes',
        'Opportunities' => 'Oportunitats',
        'Cases' => 'Casos',
        'Notes' => 'Notes',
        'Calls' => 'Trucades',
        'TemplateSectionLine' => 'Línia de secció de plantilla',
        'Calls_Reschedule' => 'Replanifica la trucada',
        'Emails' => 'Correus electrònics',
        'EAPM' => 'EAPM',
        'Meetings' => 'Reunions',
        'Tasks' => 'Tasques',
        'Calendar' => 'Calendari',
        'Leads' => 'Clients Potencials',
        'Currencies' => 'Monedes',
        'Activities' => 'Activitats',
        'Bugs' => 'Incidències',
        'Feeds' => 'Fonts RSS',
        'iFrames' => 'Portal',
        'TimePeriods' => 'Administració de Períodes de Temps',
        'ContractTypes' => 'Tipus de Contractes',
        'Schedulers' => 'Planificacions',
        'Project' => 'Projectes',
        'ProjectTask' => 'Tasques de Projecte',
        'Campaigns' => 'Campanyes',
        'CampaignLog' => 'Registre de Campanyes',
        'Documents' => 'Documents',
        'DocumentRevisions' => 'Versions',
        'Connectors' => 'Configuració de Connectors',
        'Roles' => 'Rols',
        'Notifications' => 'Notificacions',
        'Sync' => 'Sincronitzacions',
        'Users' => 'Usuaris',
        'Employees' => 'Empleats',
        'Administration' => 'Administració',
        'ACLRoles' => 'Rols',
        'InboundEmail' => 'Correu electrònic entrant',
        'Releases' => 'Llançaments',
        'Prospects' => 'Públic Objectiu',
        'Queues' => 'Cues',
        'EmailMarketing' => 'Màrqueting per correu electrònic',
        'EmailTemplates' => 'Plantilles de correu electrònic',
        'ProspectLists' => 'Llistes de Públic Objectiu',
        'SavedSearch' => 'Cerques desades',
        'UpgradeWizard' => "Assistent d'actualització",
        'Trackers' => 'Monitoratge',
        'TrackerSessions' => 'Sessions de Monitoratge',
        'TrackerQueries' => 'Consultes de Monitoratge',
        'FAQ' => 'Preguntes freqüents',
        'Newsletters' => 'Butlletins de Notícies',
        'SugarFeed' => 'Feed de SuiteCRM',
        'SugarFavorites' => 'Favorits de SuiteCRM',

        'OAuthKeys' => 'Claus OAuth',
        'OAuthTokens' => 'Tokens OAuth',
        'OAuth2Clients' => 'Clients OAuth',
        'OAuth2Tokens' => 'Tokens OAuth',
    ),

    'moduleListSingular' => array(
        'Home' => 'Inici',
        'Dashboard' => 'Gràfic',
        'Contacts' => 'Contacte',
        'Accounts' => 'Compte',
        'Opportunities' => 'Oportunitat',
        'Cases' => 'Cas',
        'Notes' => 'Nota',
        'Calls' => 'Trucada',
        'Emails' => 'Correu electrònic',
        'EmailTemplates' => 'Plantilla de correu electrònic',
        'Meetings' => 'Reunió',
        'Tasks' => 'Tasca',
        'Calendar' => 'Calendari',
        'Leads' => 'Client Potencial',
        'Activities' => 'Activitat',
        'Bugs' => 'Incidència',
        'KBDocuments' => 'Base de Coneixement',
        'Feeds' => 'Font RSS',
        'iFrames' => 'Favorit Web',
        'TimePeriods' => 'Període de Temps',
        'Project' => 'Projecte',
        'ProjectTask' => 'Tasca de Projecte',
        'Prospects' => 'Públic Objectiu',
        'Campaigns' => 'Campanya',
        'Documents' => 'Document',
        'Sync' => 'Sincronització',
        'Users' => 'Usuari',
        'SugarFavorites' => 'Favorit de SuiteCRM',

    ),

    'checkbox_dom' => array(
        '' => '',
        '1' => 'Sí',
        '2' => 'No',
    ),
    'account_type_dom' => array(
        '' => '',
        'Analyst' => 'Analista',
        'Competitor' => 'Competidor',
        'Customer' => 'Client',
        'Integrator' => 'Integrador',
        'Investor' => 'Inversor',
        'Partner' => 'Soci',
        'Press' => 'Premsa',
        'Prospect' => 'Prospector',
        'Reseller' => 'Revenedor',
        'Other' => 'Altres',
    ),
    'industry_dom' => array(
        '' => '',
        'Apparel' => 'Tèxtil',
        'Banking' => 'Banca',
        'Biotechnology' => 'Biotecnologia',
        'Chemicals' => 'Química',
        'Communications' => 'Comunicacions',
        'Construction' => 'Construcció',
        'Consulting' => 'Consultoria',
        'Education' => 'Educació',
        'Electronics' => 'Electrònica',
        'Energy' => 'Energia',
        'Engineering' => 'Enginyeria',
        'Entertainment' => 'Entretenimient',
        'Environmental' => 'Medi ambient',
        'Finance' => 'Finances',
        'Government' => 'Govern',
        'Healthcare' => 'Sanitat',
        'Hospitality' => 'Atenció social',
        'Insurance' => 'Assegurances',
        'Machinery' => 'Maquinària',
        'Manufacturing' => 'Fabricació',
        'Media' => 'Mitjans de comunicació',
        'Not For Profit' => 'Sense ànim de lucre',
        'Recreation' => 'Lleure',
        'Retail' => 'Venda minorista',
        'Shipping' => 'Enviaments',
        'Technology' => 'Tecnologia',
        'Telecommunications' => 'Telecomunicacions',
        'Transportation' => 'Transport',
        'Utilities' => 'Serveis públics',
        'Other' => 'Altres',
    ),
    'lead_source_default_key' => 'Autogenerada',
    'lead_source_dom' => array(
        '' => '',
        'Cold Call' => 'Trucada freda',
        'Existing Customer' => 'Client existent',
        'Self Generated' => 'Autogenerat',
        'Employee' => 'Empleat',
        'Partner' => 'Soci',
        'Public Relations' => 'Relacions públiques',
        'Direct Mail' => 'Correu directe',
        'Conference' => 'Conferència',
        'Trade Show' => 'Exposició',
        'Web Site' => 'Web',
        'Word of mouth' => 'Recomanació',
        'Email' => 'Correu electrònic',
        'Campaign' => 'Campanya',
        'Other' => 'Altres',
    ),
    'language_dom' => array(
        'af' => 'Africà',
        'ar-EG' => 'Àrab (Egipte)',
        'ar-SA' => 'Àrab (Aràbia Saudita)',
        'az' => 'Àzeri',
        'bg' => 'Búlgar',
        'bn' => 'Bengalí',
        'bs' => 'Bosnià',
        'ca' => 'Català',
        'ceb' => 'Cebuà',
        'cs' => 'Txec',
        'da' => 'Danès',
        'de' => 'Alemany',
        'de-CH' => 'Alemany (Suïssa)',
        'el' => 'Grec',
        'en-GB' => 'Anglès (Regne Unit)',
        'en-US' => 'Anglès (Estats Units)',
        'es-ES' => 'Espanyol (Espanya)',
        'es-MX' => 'Espanyol (Mèxic)',
        'es-PY' => 'Espanyol (Paraguai)',
        'es-VE' => 'Espanyol (Veneçuela)',
        'et' => 'Estonià',
        'eu' => 'Basc',
        'fa' => 'Persa',
        'fi' => 'Filipí',
        'fil' => 'Finlandès',
        'fr' => 'Francès',
        'fr-CA' => 'Francès (Canadà)',
        'gu-IN' => 'Gujarati',
        'he' => 'Hebreu',
        'hi' => 'Hindi',
        'hr' => 'Croat',
        'hu' => 'Hongarès',
        'hy-AM' => 'Armeni',
        'id' => 'Indonesi',
        'it' => 'Italià',
        'ja' => 'Japonès',
        'ka' => 'Georgià',
        'ko' => 'Coreà',
        'lt' => 'Lituà',
        'lv' => 'Letó',
        'mk' => 'Macedònia',
        'nb' => 'Bokmal noruec',
        'nl' => 'Holandès',
        'pcm' => 'Nigèria Pidgin',
        'pl' => 'Polonès',
        'pt-BR' => 'Portuguès (Brasil)',
        'pt-PT' => 'Portuguès (Portugal)',
        'ro' => 'Romanès',
        'ru' => 'Rus',
        'si-LK' => 'Singalès',
        'sk' => 'Eslovac',
        'sl' => 'Eslovè',
        'sq' => 'Albanès',
        'sr-CS' => 'Serbi (llatí)',
        'sv-SE' => 'Suec',
        'th' => 'Tailandès',
        'tl' => 'Tagal',
        'tr' => 'Turc',
        'uk' => 'Ucraïnès',
        'ur-IN' => 'Urdú (Índia)',
        'ur-PK' => 'Urdú (Pakistan)',
        'vi' => 'Vietnamita',
        'yo' => 'Ioruba',
        'zh-CN' => 'Xinès simplificat',
        'zh-TW' => 'Xinès tradicional',
        'other' => 'Altres',
    ),
    'opportunity_type_dom' => array(
        '' => '',
        'Existing Business' => 'Negocis existents',
        'New Business' => 'Negocis nous',
    ),
    'roi_type_dom' => array(
        'Revenue' => 'Ingressos',
        'Investment' => 'Inversió',
        'Expected_Revenue' => 'Ingressos esperats',
        'Budget' => 'Pressupost',

    ),
    // Note: do not translate opportunity_relationship_type_default_key
    // it is the key for the default opportunity_relationship_type_dom value
    'opportunity_relationship_type_default_key' => 'Primary Decision Maker',
    'opportunity_relationship_type_dom' => array(
        '' => '',
        'Primary Decision Maker' => 'Decisor principal',
        'Business Decision Maker' => 'Decisor de negoci',
        'Business Evaluator' => 'Avaluador de negoci',
        'Technical Decision Maker' => 'Decisor tècnic',
        'Technical Evaluator' => 'Avaluador tècnic',
        'Executive Sponsor' => 'Patrocinador executiu',
        'Influencer' => 'Influenciador',
        'Other' => 'Altres',
    ),
    // Note: do not translate case_relationship_type_default_key
    // it is the key for the default case_relationship_type_dom value
    'case_relationship_type_default_key' => 'Primary Contact',
    'case_relationship_type_dom' => array(
        '' => '',
        'Primary Contact' => 'Contacte principal',
        'Alternate Contact' => 'Contacte alternatiu',
    ),
    'payment_terms' => array(
        '' => '',
        'Net 15' => '15 dies',
        'Net 30' => '30 dies',
    ),
    'sales_stage_default_key' => 'Prospecting',
    'sales_stage_dom' => array(
        'Prospecting' => 'Prospecció',
        'Qualification' => 'Qualificació',
        'Needs Analysis' => 'Necessita anàlisi',
        'Value Proposition' => 'Proposta de valor',
        'Id. Decision Makers' => 'Identificació dels decisors',
        'Perception Analysis' => 'Anàlisi de percepció',
        'Proposal/Price Quote' => 'Proposta/Pressupost',
        'Negotiation/Review' => 'Negociació/Revisió',
        'Closed Won' => 'Tancat guanyat',
        'Closed Lost' => 'Tancat perdut',
    ),
    'sales_probability_dom' => // keys must be the same as sales_stage_dom
        array(
            'Prospecting' => '10',
            'Qualification' => '20',
            'Needs Analysis' => '25',
            'Value Proposition' => '30',
            'Id. Decision Makers' => '40',
            'Perception Analysis' => '50',
            'Proposal/Price Quote' => '65',
            'Negotiation/Review' => '80',
            'Closed Won' => '100',
            'Closed Lost' => '0',
        ),
    'activity_dom' => array(
        'Call' => 'Trucada',
        'Meeting' => 'Reunió',
        'Task' => 'Tasca',
        'Email' => 'Correu electrònic',
        'Note' => 'Nota',
    ),
    'salutation_dom' => array(
        '' => '',
        'Mr.' => 'Sr.',
        'Ms.' => 'Sra.',
        'Mrs.' => 'Sra.',
        'Miss' => 'Sra.',
        'Dr.' => 'Dr.',
        'Prof.' => 'Prof.',
    ),
    //time is in seconds; the greater the time the longer it takes;
    'reminder_max_time' => 90000,
    'reminder_time_options' => array(
        60 => '1 minut abans',
        300 => '5 minuts abans',
        600 => '10 minuts abans',
        900 => '15 minuts abans',
        1800 => '30 minuts abans',
        3600 => '1 hora abans',
        7200 => '2 hores abans',
        10800 => '3 hores abans',
        18000 => '5 hores abans',
        86400 => '1 dia abans',
    ),

    'task_priority_default' => 'Mitja',
    'task_priority_dom' => array(
        'High' => 'Alta',
        'Medium' => 'Mitja',
        'Low' => 'Baixa',
    ),
    'task_status_default' => 'Not Started',
    'task_status_dom' => array(
        'Not Started' => 'No iniciada',
        'In Progress' => 'En progrés',
        'Completed' => 'Completada',
        'Pending Input' => "Pendent d'informació",
        'Deferred' => 'Ajornada',
    ),
    'meeting_status_default' => 'Planned',
    'meeting_status_dom' => array(
        'Planned' => 'Planificada',
        'Held' => 'Realitzada',
        'Not Held' => 'No realitzada',
    ),
    'extapi_meeting_password' => array(
        'WebEx' => 'WebEx',
    ),
    'meeting_type_dom' => array(
        'Other' => 'Altres',
        'Sugar' => 'SuiteCRM',
    ),
    'call_status_default' => 'Planned',
    'call_status_dom' => array(
        'Planned' => 'Planificada',
        'Held' => 'Realitzada',
        'Not Held' => 'No realitzada',
    ),
    'call_direction_default' => 'Outbound',
    'call_direction_dom' => array(
        'Inbound' => 'Entrant',
        'Outbound' => 'Sortint',
    ),
    'lead_status_dom' => array(
        '' => '',
        'New' => 'Nou',
        'Assigned' => 'Assignat',
        'In Process' => 'En procés',
        'Converted' => 'Convertit',
        'Recycled' => 'Reciclat',
        'Dead' => 'Mort',
    ),
    'case_priority_default_key' => 'P2',
    'case_priority_dom' => array(
        'P1' => 'Alta',
        'P2' => 'Mitja',
        'P3' => 'Baixa',
    ),
    'user_type_dom' => array(
        'RegularUser' => 'Usuari regular',
        'Administrator' => 'Administrador',
    ),
    'user_status_dom' => array(
        'Active' => 'Actiu',
        'Inactive' => 'Inactiu',
    ),
    'user_factor_auth_interface_dom' => array(
        'FactorAuthEmailCode' => 'Codi de correu electrònic',
    ),
    'employee_status_dom' => array(
        'Active' => 'Actiu',
        'Terminated' => 'Baixa',
        'Leave of Absence' => 'En excedència',
    ),
    'messenger_type_dom' => array(
        '' => '',
        'MSN' => 'MSN',
        'Yahoo!' => 'Yahoo!',
        'AOL' => 'AOL',
    ),
    'project_task_priority_default' => 'Medium',
    'project_task_priority_options' => array(
        'High' => 'Alta',
        'Medium' => 'Mitja',
        'Low' => 'Baixa',
    ),
    'project_task_status_options' => array(
        'Not Started' => 'No iniciada',
        'In Progress' => 'En progrés',
        'Completed' => 'Completada',
        'Pending Input' => "Pendent d'informació",
        'Deferred' => 'Ajornada',
    ),
    'project_task_utilization_options' => array(
        '0' => '0',
        '25' => '25',
        '50' => '50',
        '75' => '75',
        '100' => '100',
    ),
    'project_status_default' => 'Draft',
    'project_status_dom' => array(
        'Draft' => 'Esborrany',
        'In Review' => 'En revisió',
        'Underway' => 'En procés',
        'On_Hold' => 'En espera',
        'Completed' => 'Completat',
    ),

    'project_duration_units_dom' => array(
        'Days' => 'Dies',
        'Hours' => 'Hores',
    ),

    'activity_status_type_dom' => array(
        '' => '',
        'active' => 'Actiu',
        'inactive' => 'Inactiu',
    ),

    // Note: do not translate record_type_default_key
    // it is the key for the default record_type_module value
    'record_type_default_key' => 'Accounts',
    'record_type_display' => array(
        '' => '',
        'Accounts' => 'Comptes',
        'Opportunities' => 'Oportunitats',
        'Cases' => 'Casos',
        'Leads' => 'Clients Potencials',
        'Contacts' => 'Contactes',
        'Bugs' => 'Incidències',
        'Project' => 'Projectes',
        'Prospects' => 'Públic Objectiu',
        'ProjectTask' => 'Tasques de Projecte',
        'Tasks' => 'Tasques',
        'AOS_Contracts' => 'Contractes',
        'AOS_Invoices' => 'Factures',
        'AOS_Quotes' => 'Pressupostos',
        'AOS_Products' => 'Productes',
    ),
// PR 4606
    'record_type_display_notes' => array(
        'Accounts' => 'Comptes',
        'Contacts' => 'Contactes',
        'Opportunities' => 'Oportunitats',
        'Campaigns' => 'Campanyes',
        'Tasks' => 'Tasques',
        'Emails' => 'Correu electrònic',
        'Bugs' => 'Incidències',
        'Project' => 'Projectes',
        'ProjectTask' => 'Tasques de Projecte',
        'Prospects' => 'Públic Objectiu',
        'Cases' => 'Casos',
        'Leads' => 'Clients Potencials',
        'Meetings' => 'Reunions',
        'Calls' => 'Trucades',
        'AOS_Contracts' => 'Contractes',
        'AOS_Invoices' => 'Factures',
        'AOS_Quotes' => 'Pressupostos',
        'AOS_Products' => 'Productes',
    ),

    'parent_type_display' => array(
        'Accounts' => 'Comptes',
        'Contacts' => 'Contactes',
        'Tasks' => 'Tasques',
        'Opportunities' => 'Oportunitats',
        'Bugs' => 'Incidències',
        'Cases' => 'Casos',
        'Leads' => 'Clients Potencials',
        'Project' => 'Projectes',
        'ProjectTask' => 'Tasques de Projecte',
        'Prospects' => 'Públic Objectiu',
        'AOS_Contracts' => 'Contractes',
        'AOS_Invoices' => 'Factures',
        'AOS_Quotes' => 'Pressupostos',
        'AOS_Products' => 'Productes', 
    ),
    'parent_line_items' => array(
        'AOS_Quotes' => 'Pressupostos',
        'AOS_Invoices' => 'Factures',
        'AOS_Contracts' => 'Contractes',
    ),
    'issue_priority_default_key' => 'Medium',
    'issue_priority_dom' => array(
        'Urgent' => 'Urgent',
        'High' => 'Alta',
        'Medium' => 'Mitja',
        'Low' => 'Baixa',
    ),
    'issue_resolution_default_key' => '',
    'issue_resolution_dom' => array(
        '' => '',
        'Accepted' => 'Acceptat',
        'Duplicate' => 'Duplicat',
        'Closed' => 'Tancat',
        'Out of Date' => 'Caducat',
        'Invalid' => 'Invàlid',
    ),

    'issue_status_default_key' => 'New',
    'issue_status_dom' => array(
        'New' => 'Nou',
        'Assigned' => 'Assignat',
        'Closed' => 'Tancat',
        'Pending' => 'Pendent',
        'Rejected' => 'Refusat',
    ),

    'bug_priority_default_key' => 'Medium',
    'bug_priority_dom' => array(
        'Urgent' => 'Urgent',
        'High' => 'Alta',
        'Medium' => 'Mitja',
        'Low' => 'Baixa',
    ),
    'bug_resolution_default_key' => '',
    'bug_resolution_dom' => array(
        '' => '',
        'Accepted' => 'Acceptat',
        'Duplicate' => 'Duplicat',
        'Fixed' => 'Resolt',
        'Out of Date' => 'Caducat',
        'Invalid' => 'Invàlid',
        'Later' => 'Ajornat',
    ),
    'bug_status_default_key' => 'New',
    'bug_status_dom' => array(
        'New' => 'Nou',
        'Assigned' => 'Assignat',
        'Closed' => 'Tancat',
        'Pending' => 'Pendent',
        'Rejected' => 'Refusat',
    ),
    'bug_type_default_key' => 'Bug',
    'bug_type_dom' => array(
        'Bug' => 'Incidència',
        'Defect' => 'Defecte',
        'Feature' => 'Característica',
    ),
    'case_type_dom' => array(
        'Administration' => 'Administració',
        'Product' => 'Producte',
        'User' => 'Usuari',
    ),

    'source_default_key' => '',
    'source_dom' => array(
        '' => '',
        'Internal' => 'Intern',
        'Forum' => 'Fòrum',
        'Web' => 'Web',
        'InboundEmail' => 'Correu electrònic',
    ),

    'product_category_default_key' => '',
    'product_category_dom' => array(
        '' => '',
        'Accounts' => 'Comptes',
        'Activities' => 'Activitats',
        'Bugs' => 'Incidències',
        'Calendar' => 'Calendari',
        'Calls' => 'Trucades',
        'Campaigns' => 'Campanyes',
        'Cases' => 'Casos',
        'Contacts' => 'Contactes',
        'Currencies' => 'Monedes',
        'Dashboard' => 'Gràfics',
        'Documents' => 'Documents',
        'Emails' => 'Correus electrònics',
        'Feeds' => 'Canals electrònics',
        'Forecasts' => 'Objectiu',
        'Help' => 'Ajuda',
        'Home' => 'Inici',
        'Leads' => 'Clients Potencials',
        'Meetings' => 'Reunions',
        'Notes' => 'Notes',
        'Opportunities' => 'Oportunitats',
        'Outlook Plugin' => 'Plugin de Outlook',
        'Projects' => 'Projectes',
        'Quotes' => 'Pressupostos',
        'Releases' => 'Llançaments',
        'RSS' => 'Fonts RSS',
        'Studio' => 'Estudi',
        'Upgrade' => 'Actualització',
        'Users' => 'Usuaris',
    ),
    /*Added entries 'Queued' and 'Sending' for 4.0 release..*/
    'campaign_status_dom' => array(
        '' => '',
        'Planning' => 'Planificació',
        'Active' => 'Actiu',
        'Inactive' => 'Inactiu',
        'Complete' => 'Completa',
        //'In Queue' => 'In Queue',
        //'Sending' => 'Sending',
    ),
    'campaign_type_dom' => array(
        '' => '',
        'Telesales' => 'Televenda',
        'Mail' => 'Correu',
        'Email' => 'Correu electrònic',
        'Print' => 'Impressió',
        'Web' => 'Web',
        'Radio' => 'Ràdio',
        'Television' => 'Televisió',
        'NewsLetter' => 'Butlletí de Notícies',
    ),

    'newsletter_frequency_dom' => array(
        '' => '',
        'Weekly' => 'Setmanal',
        'Monthly' => 'Mensual',
        'Quarterly' => 'Trimestral',
        'Annually' => 'Anual',
    ),

    'notifymail_sendtype' => array(
        'SMTP' => 'SMTP',
    ),
    'dom_cal_month_long' => array(
        '0' => '',
        '1' => 'Gener',
        '2' => 'Febrer',
        '3' => 'Març',
        '4' => 'Abril',
        '5' => 'Maig',
        '6' => 'Juny',
        '7' => 'Juliol',
        '8' => 'Agost',
        '9' => 'Setembre',
        '10' => 'Octubre',
        '11' => 'Novembre',
        '12' => 'Desembre',
    ),
    'dom_cal_month_short' => array(
        '0' => '',
        '1' => 'gen.',
        '2' => 'febr.',
        '3' => 'març',
        '4' => 'abr.',
        '5' => 'maig',
        '6' => 'juny',
        '7' => 'jul.',
        '8' => 'ag.',
        '9' => 'set.',
        '10' => 'oct.',
        '11' => 'nov.',
        '12' => 'des.',
    ),
    'dom_cal_day_long' => array(
        '0' => '',
        '1' => 'Diumenge',
        '2' => 'Dilluns',
        '3' => 'Dimarts',
        '4' => 'Dimecres',
        '5' => 'Dijous',
        '6' => 'Divendres',
        '7' => 'Dissabte',
    ),
    'dom_cal_day_short' => array(
        '0' => '',
        '1' => 'dg.',
        '2' => 'dl.',
        '3' => 'dt.',
        '4' => 'dc.',
        '5' => 'dj.',
        '6' => 'dv.',
        '7' => 'ds.',
    ),
    'dom_meridiem_lowercase' => array(
        'am' => 'am',
        'pm' => 'pm',
    ),
    'dom_meridiem_uppercase' => array(
        'AM' => 'AM',
        'PM' => 'PM',
    ),

    'dom_inbound_email_account_types' => [
        'personal' => 'Personal',
        'group' => 'Grup',
        'bounce' => 'Rebot',
    ],

    'dom_inbound_email_auth_types' => [
        'basic' => 'Autenticació Bàsica',
        'oauth' => 'OAuth',
    ],

    'dom_external_oauth_connection_types' => [
        'personal' => 'Personal',
        'group' => 'Grup',
    ],

    'dom_external_oauth_provider_types' => [
        'personal' => 'Personal',
        'group' => 'Grup',
    ],

    'dom_outbound_email_account_types' => [
        'user' => 'Personal',
        'group' => 'Grup',
        'system' => 'Sistema',
        'system-override' => 'Anul·lació del sistema',
    ],

    'dom_inbound_email_account_status' => [
        'Active' => 'Actiu',
        'Inactive' => 'Inactiu',
    ],

    'dom_email_body_filtering_option' => [
      'multi' => 'Cerca de Múltiples Paraules',
      'single' => "Cerca d'una Paraula",
    ],

    'dom_email_types' => array(
        'out' => 'Enviat',
        'archived' => 'Arxivat',
        'draft' => 'Esborrany',
        'inbound' => 'Entrant',
        'campaign' => 'Campanya',
    ),
    'dom_email_status' => array(
        'archived' => 'Arxivat',
        'closed' => 'Tancat',
        'draft' => 'Esborrany',
        'read' => 'Llegit',
        'replied' => 'Respost',
        'sent' => 'Enviat',
        'send_error' => 'Error d\'Enviament',
        'unread' => 'No Llegit',
    ),
    'dom_email_archived_status' => array(
        'archived' => 'Arxivat',
    ),

    'dom_email_server_type' => array(
        '' => '--Cap--',
        'imap' => 'IMAP',
    ),
    'dom_mailbox_type' => array(/*''           => '--None Specified--',*/
        'pick' => '--Cap--',
        'createcase' => 'Nou Cas',
        'bounce' => 'Gestió de Rebots',
    ),
    'dom_email_distribution' => array(
        '' => '--Cap--',
        'direct' => 'Assignació Directa',
        'roundRobin' => 'Round Robin',
        'leastBusy' => 'Menys-Ocupat',
    ),
    'dom_email_errors' => array(
        1 => 'Seleccioneu només un usuari quan assigneu elements directament.',
        2 => 'Heu d\'assignar només elements marcats quan assignació directa d\'elements.',
    ),
    'dom_email_bool' => array(
        'bool_true' => 'Si',
        'bool_false' => 'No',
    ),
    'dom_int_bool' => array(
        1 => 'Si',
        0 => 'No',
    ),
    'dom_switch_bool' => array(
        'on' => 'Si',
        'off' => 'No',
        '' => '--Cap--',
    ),

    'dom_email_link_type' => array(
        'sugar' => 'Client de correu electrònic de SuiteCRM',
        'mailto' => 'Client de correu electrònic extern',
    ),

    'dom_editor_type' => array(
        'none' => 'HTML Directe',
        'tinymce' => 'Editor TinyMCE',
        'mozaik' => 'Mozaik',
    ),

    'dom_email_editor_option' => array(
        '' => 'Format d\'Email per defecte',
        'html' => 'Correu electrònic HTML',
        'plain' => 'Correu electrònic amb text plà',
    ),

    'schedulers_times_dom' => array(
        'not run' => 'Hora d\'Execució Passada, No Executat',
        'ready' => 'Llest',
        'in progress' => 'En Progrés',
        'failed' => 'Fallat',
        'completed' => 'Completat',
        'no curl' => 'No executat: cURL no està disponible',
    ),

    'scheduler_status_dom' => array(
        'Active' => 'Actiu',
        'Inactive' => 'Inactiu',
    ),

    'scheduler_period_dom' => array(
        'min' => 'Minuts',
        'hour' => 'Hores',
    ),
    'document_category_dom' => array(
        '' => '',
        'Marketing' => 'Màrqueting',
        'Knowledege Base' => 'Base de Coneixement',
        'Sales' => 'Vendes',
    ),

    'email_category_dom' => array(
        '' => '',
        'Archived' => 'Arxivat',
        // TODO: add more categories here...
    ),

    'document_subcategory_dom' => array(
        '' => '',
        'Marketing Collateral' => 'Impresos de Màrqueting',
        'Product Brochures' => 'Fullets de Producte',
        'FAQ' => 'Preguntes freqüents',
    ),

    'document_status_dom' => array(
        'Active' => 'Actiu',
        'Draft' => 'Esborrany',
        'FAQ' => 'Preguntes freqüents',
        'Expired' => 'Caducat',
        'Under Review' => 'En Revisió',
        'Pending' => 'Pendent',
    ),
    'document_template_type_dom' => array(
        '' => '',
        'mailmerge' => 'Combinar Correspondència',
        'eula' => 'CLUF',
        'nda' => 'ANR',
        'license' => 'Contracte de Llicència',
    ),
    'dom_meeting_accept_options' => array(
        'accept' => 'Acceptar',
        'decline' => 'Refusar',
        'tentative' => 'Temptativa',
    ),
    'dom_meeting_accept_status' => array(
        'accept' => 'Acceptat',
        'decline' => 'Rebutjat',
        'tentative' => 'Temptativa',
        'none' => 'Cap',
    ),
    'duration_intervals' => array(
        '0' => '00',
        '15' => '15',
        '30' => '30',
        '45' => '45',
    ),
    'repeat_type_dom' => array(
        '' => '--Cap--',
        'Daily' => 'Diari',
        'Weekly' => 'Setmanal',
        'Monthly' => 'Mensual',
        'Yearly' => 'Anual',
    ),

    'repeat_intervals' => array(
        '' => '',
        'Daily' => 'die(s)',
        'Weekly' => 'setmana(es)',
        'Monthly' => 'mes(os)',
        'Yearly' => 'any(s)',
    ),

    'duration_dom' => array(
        '' => '--Cap--',
        '900' => '15 minuts',
        '1800' => '30 minuts',
        '2700' => '45 minuts',
        '3600' => '1 hora',
        '5400' => '1,5 hores',
        '7200' => '2 hores',
        '10800' => '3 hores',
        '21600' => '6 hores',
        '86400' => '1 dia',
        '172800' => '2 dies',
        '259200' => '3 dies',
        '604800' => '1 setmana',
    ),


//prospect list type dom
    'prospect_list_type_dom' => array(
        'default' => 'Per defecte',
        'seed' => 'El primer de la llista',
        'exempt_domain' => "Llista d'exclusió - Per domini",
        'exempt_address' => "Llista d'exclusió - Per adreça de correu electrònic",
        'exempt' => "Llista d'exclusió - Per Id",
        'test' => 'Prova',
    ),

    'email_settings_num_dom' => array(
        '10' => '10',
        '20' => '20',
        '50' => '50',
    ),
    'email_marketing_status_dom' => array(
        '' => '',
        'active' => 'Actiu',
        'inactive' => 'Inactiu',
    ),

    'campainglog_activity_type_dom' => array(
        '' => '',
        'targeted' => 'Missatge enviat',
        'send error' => 'Missatge no enviat (altres causes)',
        'invalid email' => 'Missatge no enviat (adreça no vàlida)',
        'link' => 'Enllaç clicat',
        'viewed' => 'Missatge vist',
        'removed' => 'Baixa',
        'lead' => 'Client potencial creat',
        'contact' => 'Contacte creat',
        'blocked' => 'Destinatari exclòs per adreça o domini',
        'Survey' => 'Resposta a enquesta',
    ),

    'campainglog_target_type_dom' => array(
        'Contacts' => 'Contactes',
        'Users' => 'Usuaris',
        'Prospects' => 'Públic Objectiu',
        'Leads' => 'Clients Potencials',
        'Accounts' => 'Comptes',
    ),
    'merge_operators_dom' => array(
        'like' => 'Conté',
        'exact' => 'Exactamente',
        'start' => 'Comença amb',
    ),

    'custom_fields_importable_dom' => array(
        'true' => 'Sí',
        'false' => 'No',
        'required' => 'Requerit',
    ),

    'custom_fields_merge_dup_dom' => array(
        0 => 'Deshabilitat',
        1 => 'Habilitat',
        2 => 'Al filtre',
        3 => 'Filtre seleccionat per defecte',
        4 => 'Només al filtre',
    ),

    'projects_priority_options' => array(
        'high' => 'Alta',
        'medium' => 'Mitja',
        'low' => 'Baixa',
    ),

    'projects_status_options' => array(
        'notstarted' => 'No iniciat',
        'inprogress' => 'En progrés',
        'completed' => 'Completat',
    ),
    // strings to pass to Flash charts
    'chart_strings' => array(
        'expandlegend' => 'Expandeix la llegenda',
        'collapselegend' => 'Contrau la llegenda',
        'clickfordrilldown' => 'Cliqueu per aprofundir',
        'detailview' => 'Més detalls...',
        'piechart' => 'Gràfic Circular',
        'groupchart' => 'Gràfic Agrupat',
        'stackedchart' => 'Gràfic Apilat',
        'barchart' => 'Gràfic de Barres',
        'horizontalbarchart' => 'Gràfic de Barres Horitzontal',
        'linechart' => 'Gràfic de Línies',
        'noData' => 'Dades no disponibles',
        'print' => 'Imprimeix',
        'pieWedgeName' => 'seccions',
    ),
    'release_status_dom' => array(
        'Active' => 'Actiu',
        'Inactive' => 'Inactiu',
    ),
    'email_settings_for_ssl' => array(
        '0' => '',
        '1' => 'SSL',
        '2' => 'TTL',
    ),
    'import_enclosure_options' => array(
        '\'' => 'Cometes simples (&#39;)',
        '"' => 'Cometes dobles (&#34;)',
        '' => '--Cap--',
        'other' => 'Altre:',
    ),
    'import_delimeter_options' => array(
        ',' => ',',
        ';' => ';',
        '\t' => '\t',
        '.' => '.',
        ':' => ':',
        '|' => '|',
        'other' => 'Altre:',
    ),
    'link_target_dom' => array(
        '_blank' => 'Nova finestra',
        '_self' => 'A la mateix finestra',
    ),
    'dashlet_auto_refresh_options' => array(
        '-1' => 'No refresquis automàticament',
        '30' => 'Cada 30 segons',
        '60' => 'Cada hora',
        '180' => 'Cada 3 minuts',
        '300' => 'Cada 5 minuts',
        '600' => 'Cada 10 minuts',
    ),
    'dashlet_auto_refresh_options_admin' => array(
        '-1' => 'Mai',
        '30' => 'Cada 30 segons',
        '60' => 'Cada hora',
        '180' => 'Cada 3 minuts',
        '300' => 'Cada 5 minuts',
        '600' => 'Cada 10 minuts',
    ),
    'date_range_search_dom' => array(
        '=' => 'Igual a',
        'not_equal' => 'Diferent de',
        'greater_than' => 'Després de',
        'less_than' => 'Abans de',
        'last_7_days' => 'Els darrers 7 dies',
        'next_7_days' => 'Els propers 7 dies',
        'last_30_days' => 'Els darrers 30 dies',
        'next_30_days' => 'Els propers 30 dies',
        'last_month' => 'El mes passat',
        'this_month' => 'Aquest mes',
        'next_month' => 'El mes que ve',
        'last_year' => "L'any passat",
        'this_year' => 'Aquest any',
        'next_year' => "L'any que ve",
        'between' => 'Entre',
    ),
    'numeric_range_search_dom' => array(
        '=' => 'Igual a',
        'not_equal' => 'Diferent de',
        'greater_than' => 'Més gran que',
        'greater_than_equals' => 'Més gran o igual que',
        'less_than' => 'Més petit que',
        'less_than_equals' => 'Més petit o igual que',
        'between' => 'Entre',
    ),
    'lead_conv_activity_opt' => array(
        'copy' => 'Copia',
        'move' => 'Mou',
        'donothing' => 'No facis res',
    ),
    // PR 6009
    'inboundmail_assign_replies_to_admin' => array(
        'donothing' => 'No facis res',
        'repliedtoowner' => 'Respon al correu electrònic del propietari',
        'recordowner' => 'Propietari associat del registre',
    ),
);

$app_strings = array(
    'LBL_SEARCH_REAULTS_TITLE' => 'Resultats',
    'ERR_SEARCH_INVALID_QUERY' => 'S\'ha produït un error mentre es realitzava la cerca. La sintaxi de la consulta podria no ser vàlida.',
    'ERR_SEARCH_NO_RESULTS' => 'No hi ha resultats que coincideixin amb els criteris de cerca. Mireu d\'ampliar la cerca.',
    'LBL_SEARCH_PERFORMED_IN' => 'Cerca realitzada en',
    'LBL_EMAIL_CODE' => 'Codi de correu electrònic:',
    'LBL_SEND' => 'Envia',
    'LBL_LOGOUT' => 'Surt',
    'LBL_TOUR_NEXT' => 'Següent',
    'LBL_TOUR_SKIP' => 'Salta',
    'LBL_TOUR_BACK' => 'Enrere',
    'LBL_TOUR_TAKE_TOUR' => 'Visita guiada',
    'LBL_MOREDETAIL' => 'Més detalls' /*for 508 compliance fix*/,
    'LBL_EDIT_INLINE' => 'Edita en línia' /*for 508 compliance fix*/,
    'LBL_VIEW_INLINE' => 'Mostra' /*for 508 compliance fix*/,
    'LBL_BASIC_SEARCH' => 'Filtre' /*for 508 compliance fix*/,
    'LBL_Blank' => ' ' /*for 508 compliance fix*/,
    'LBL_ID_FF_ADD' => 'Afegir' /*for 508 compliance fix*/,
    'LBL_ID_FF_ADD_EMAIL' => 'Afegeix una adreça de correu electrònic' /*for 508 compliance fix*/,
    'LBL_HIDE_SHOW' => 'Amaga/Mostra' /*for 508 compliance fix*/,
    'LBL_DELETE_INLINE' => 'Elimina' /*for 508 compliance fix*/,
    'LBL_ID_FF_CLEAR' => 'Neteja' /*for 508 compliance fix*/,
    'LBL_ID_FF_VCARD' => 'vCard' /*for 508 compliance fix*/,
    'LBL_ID_FF_REMOVE' => 'Eliminar' /*for 508 compliance fix*/,
    'LBL_ID_FF_REMOVE_EMAIL' => 'Borrar l\'adreça de correu electrònic' /*for 508 compliance fix*/,
    'LBL_ID_FF_OPT_OUT' => 'Optar per',
    'LBL_ID_FF_OPT_IN' => 'Autoritzat a enviar',
    'LBL_ID_FF_INVALID' => 'Fer invàlid',
    'LBL_ADD' => 'Afegir' /*for 508 compliance fix*/,
    'LBL_COMPANY_LOGO' => 'Logo de la companyia' /*for 508 compliance fix*/,
    'LBL_CONNECTORS_POPUPS' => 'Connector de finestres emergents',
    'LBL_CLOSEINLINE' => 'Tancar',
    'LBL_VIEWINLINE' => 'Veure',
    'LBL_INFOINLINE' => 'Informació',
    'LBL_PRINT' => 'Imprimir',
    'LBL_HELP' => 'Ajuda',
    'LBL_ID_FF_SELECT' => 'Seleccionar',
    'DEFAULT' => 'Bàsic', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_SORT' => 'Ordenar',
    'LBL_EMAIL_SMTP_SSL_OR_TLS' => 'Habilitar SMTP sobre SSL o TLS?',
    'LBL_NO_ACTION' => 'No hi ha cap acció amb aquest nom: %s',
    'LBL_NO_SHORTCUT_MENU' => 'No hi han accion disponibles.',
    'LBL_NO_DATA' => 'No hi ha dades',

    'LBL_ERROR_UNDEFINED_BEHAVIOR' => 'S\'ha produït un error desconegut.', //PR 3669
    'LBL_ERROR_UNHANDLED_VALUE' => 'Un valor ha no s\'ha transmès correctament i impedeix que un procés pugui continuar.', //PR 3669
    'LBL_ERROR_UNUSABLE_VALUE' => 'Un valor inutilitzable impedeix que un procés pugui continuar.', //PR 3669
    'LBL_ERROR_INVALID_TYPE' => 'El tipus de valor és diferent al que s\'esperava.', //PR 3669

    'LBL_ROUTING_FLAGGED' => 'conjunt de marques de seguiment',
    'LBL_NOTIFICATIONS' => 'Notificacions',

    'LBL_ROUTING_TO' => 'a',
    'LBL_ROUTING_TO_ADDRESS' => "a l'adreça",
    'LBL_ROUTING_WITH_TEMPLATE' => 'con la plantilla',

    'NTC_OVERWRITE_ADDRESS_PHONE_CONFIRM' => 'Els camps Telèfon i Adreça del seu formulari ja tenen valor assignat. Per sobreescriure aquests valors amb el telèfon/adreça del Compte que ha seleccionat, faci clic en "Acceptar". Per mantenir els valors actuals, faci clic en "Cancel·lar".',
    'LBL_DROP_HERE' => '[Deixar Anar Aquí]',
    'LBL_EMAIL_ACCOUNTS_GMAIL_DEFAULTS' => 'Establir configuració per a Gmail',
    'LBL_EMAIL_ACCOUNTS_NAME' => 'Nom',
    'LBL_EMAIL_ACCOUNTS_OUTBOUND' => 'Propietats del servidor de correu sortint',
    'LBL_EMAIL_ACCOUNTS_SMTPPASS' => 'Clau de pas SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPPORT' => 'Port SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPSERVER' => 'Servidor SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPUSER' => 'Nom d\'usuari SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPDEFAULT' => 'Per defecte',
    'LBL_EMAIL_WARNING_MISSING_USER_CREDS' => 'Alerta: Falta l\'usuari i la contrasenya de la compta de correu de sortida.',
    'LBL_EMAIL_ACCOUNTS_SUBTITLE' => 'Configurar comptes de correu electrònic per a veure correus electrònics entrants de les seves comptes de correu.',
    'LBL_EMAIL_ACCOUNTS_OUTBOUND_SUBTITLE' => 'Proporcionar informació del servidor de correu SMTP a utilitzar per al correu sortint en les comptes de correu.',

    'LBL_EMAIL_ADDRESS_BOOK_ADD' => 'Fet',
    'LBL_EMAIL_ADDRESS_BOOK_CLEAR' => 'Netejar',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_TO' => 'Per:',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_CC' => 'CC:',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_BCC' => 'CCO:',
    'LBL_EMAIL_ADDRESS_BOOK_ADRRESS_TYPE' => 'Per/CC/CCO',
    'LBL_EMAIL_ADDRESS_BOOK_EMAIL_ADDR' => 'Adreça de correu electrònic',
    'LBL_EMAIL_ADDRESS_BOOK_FILTER' => 'Filtre',
    'LBL_EMAIL_ADDRESS_BOOK_NAME' => 'Nom',
    'LBL_EMAIL_ADDRESS_BOOK_NOT_FOUND' => 'No s\'ha trobat cap adreça',
    'LBL_EMAIL_ADDRESS_BOOK_SAVE_AND_ADD' => "Desa i afegeix a la llibreta d'adreces",
    'LBL_EMAIL_ADDRESS_BOOK_SELECT_TITLE' => "Seleccionar Entrades de la llibreta d'adreces",
    'LBL_EMAIL_ADDRESS_BOOK_TITLE' => "Llibreta d'adreces",
    'LBL_EMAIL_REPORTS_TITLE' => 'Informes',
    'LBL_EMAIL_REMOVE_SMTP_WARNING' => 'Avís! El compte de sortida que està intentant eliminar està associat a un compte d\'entrada actual. Esteu segur que voleu continuar?',
    'LBL_EMAIL_ADDRESSES' => 'Correu electrònic',
    'LBL_EMAIL_ADDRESS_PRIMARY' => 'Adreça de correu electrònic',
    'LBL_EMAIL_ADDRESS_OPT_IN' => 'Han confirmat que l\'adreça electrònica ha estat activada és: ',
    'LBL_EMAIL_ADDRESS_OPT_IN_ERR' => 'No es pot confirmar l\'adreça de correu electrònic',
    'LBL_EMAIL_ARCHIVE_TO_SUITE' => 'Importar a SuiteCRM',
    'LBL_EMAIL_ASSIGNMENT' => 'Assignació',
    'LBL_EMAIL_ATTACH_FILE_TO_EMAIL' => 'Adjuntar',
    'LBL_EMAIL_ATTACHMENT' => 'Adjuntar',
    'LBL_EMAIL_ATTACHMENTS' => 'Adjuntar Arxius',
    'LBL_EMAIL_ATTACHMENTS2' => 'Des de documents de SuiteCRM',
    'LBL_EMAIL_ATTACHMENTS3' => 'Adjunts de Plantilla',
    'LBL_EMAIL_ATTACHMENTS_FILE' => 'Arxiu',
    'LBL_EMAIL_ATTACHMENTS_DOCUMENT' => 'Document',
    'LBL_EMAIL_BCC' => 'CCO',
    'LBL_EMAIL_CANCEL' => 'Cancelar',
    'LBL_EMAIL_CC' => 'CC',
    'LBL_EMAIL_CHARSET' => 'Joc de Caràcters',
    'LBL_EMAIL_CHECK' => 'Comprovar correu',
    'LBL_EMAIL_CHECKING_NEW' => 'Comprovant nous correus electrònics',
    'LBL_EMAIL_CHECKING_DESC' => 'Un moment, si us plau... <br><br>Si és la primera comprovació per aquesta compta de correu, pot tardar una estona.',
    'LBL_EMAIL_CLOSE' => 'Tancar',
    'LBL_EMAIL_COFFEE_BREAK' => 'Comprovant correus electrònics nous. <br><br>Els comptes de correu amb gran volum poden tardar una quantitat considerable de temps.',

    'LBL_EMAIL_COMPOSE' => 'Correu electrònic',
    'LBL_EMAIL_COMPOSE_ERR_NO_RECIPIENTS' => 'Introduïu els destinataris d\'aquest correu electrònic.',
    'LBL_EMAIL_COMPOSE_NO_BODY' => 'El cos d\'aquest missatge és buit. Enviar de tota manera?',
    'LBL_EMAIL_COMPOSE_NO_SUBJECT' => 'Aquest missatge no té assumpte.  Enviar de tota manera?',
    'LBL_EMAIL_COMPOSE_NO_SUBJECT_LITERAL' => '(sense assumpte)',
    'LBL_EMAIL_COMPOSE_INVALID_ADDRESS' => 'Introduïu una adreça de correu electrònica vàlida per als camps: Per a, CC i CCO',

    'LBL_EMAIL_CONFIRM_CLOSE' => 'Descartar aquest correu electrònic?',
    'LBL_EMAIL_CONFIRM_DELETE_SIGNATURE' => '¿Està segur que desitja eliminar aquesta firma?',

    'LBL_EMAIL_SENT_SUCCESS' => 'Correu electrònic enviat',

    'LBL_EMAIL_CREATE_NEW' => '--Crear Al Desar--',
    'LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS' => 'Múltiple',
    'LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS_EMPTY' => 'Buit',
    'LBL_EMAIL_DATE_SENT_BY_SENDER' => 'Data d\'Enviament per Remitent',
    'LBL_EMAIL_DATE_TODAY' => 'Avui',
    'LBL_EMAIL_DELETE' => 'Esborrar',
    'LBL_EMAIL_DELETE_CONFIRM' => 'Esborrar missatges seleccionats?',
    'LBL_EMAIL_DELETE_SUCCESS' => 'Correu electrònic eliminat satisfactòriament.',
    'LBL_EMAIL_DELETING_MESSAGE' => 'Eliminant Missatge',
    'LBL_EMAIL_DETAILS' => 'Detalls',

    'LBL_EMAIL_EDIT_CONTACT_WARN' => "Només es farà servir l'adreça principal de cada contacte.",

    'LBL_EMAIL_EMPTYING_TRASH' => 'Buidant Paperera',
    'LBL_EMAIL_DELETING_OUTBOUND' => 'Eliminant servidor sortint',
    'LBL_EMAIL_CLEARING_CACHE_FILES' => 'Netejant arxius de la memòria cau',
    'LBL_EMAIL_EMPTY_MSG' => 'Cap email per mostrar.',
    'LBL_EMAIL_EMPTY_ADDR_MSG' => 'No hi ha adreces electròniques per mostrar.',

    'LBL_EMAIL_ERROR_ADD_GROUP_FOLDER' => 'El nom de la carpeta ha de ser únic i no pot estar buit. Torneu-ho a intentar.',
    'LBL_EMAIL_ERROR_DELETE_GROUP_FOLDER' => 'No pot esborrar-se la carpeta. O la carpeta o els seus arbres tenen una safata de correu electrònic associada.',
    'LBL_EMAIL_ERROR_CANNOT_FIND_NODE' => 'No s\'ha pogut determinar la carpeta pretesa a partir del context. Ho intenti de nou.',
    'LBL_EMAIL_ERROR_CHECK_IE_SETTINGS' => 'Comproveu la configuració.',
    'LBL_EMAIL_ERROR_DESC' => 'S\'han detectat errors: ',
    'LBL_EMAIL_DELETE_ERROR_DESC' => 'No teniu accés a aquesta àrea. Contacteu amb l\'administrador del lloc per obtenir-hi accés.',
    'LBL_EMAIL_ERROR_DUPE_FOLDER_NAME' => 'Els noms de les carpetes de SuiteCRM han de ser únics.',
    'LBL_EMAIL_ERROR_EMPTY' => 'Introduïu algun criteri de cerca.',
    'LBL_EMAIL_ERROR_GENERAL_TITLE' => 'Ha ocorregut un error',
    'LBL_EMAIL_ERROR_MESSAGE_DELETED' => 'Missatge eliminat del servidor',
    'LBL_EMAIL_ERROR_IMAP_MESSAGE_DELETED' => 'O el missatge s\'ha eliminat al servidor o ha estat mogut a una altra carpeta',
    'LBL_EMAIL_ERROR_MAILSERVERCONNECTION' => 'La connexió amb el servidor de correu ha fallat. Contacteu amb el vostre administrador.',
    'LBL_EMAIL_ERROR_MOVE' => 'De moment no està suportat moure correus electrònics entre servidors i/o comptes de correu.',
    'LBL_EMAIL_ERROR_MOVE_TITLE' => 'Error al Moure',
    'LBL_EMAIL_ERROR_NAME' => 'Es requereix un nom.',
    'LBL_EMAIL_ERROR_FROM_ADDRESS' => "Es requereix l'adreça del remitent.",
    'LBL_EMAIL_ERROR_NO_FILE' => 'Proporcioneu un arxiu.',
    'LBL_EMAIL_ERROR_SERVER' => 'Es requereix una adreça de servidor de correu.',
    'LBL_EMAIL_ERROR_SAVE_ACCOUNT' => 'El compte de correu pot no haver estat guardat.',
    'LBL_EMAIL_ERROR_TIMEOUT' => 'Ha ocorregut un error en la comunicació amb el servidor de correu.',
    'LBL_EMAIL_ERROR_USER' => 'Es requereix un nom d\'inici de sessió.',
    'LBL_EMAIL_ERROR_PORT' => 'Es requereix un port del servidor de correu.',
    'LBL_EMAIL_ERROR_PROTOCOL' => 'Es requereix un protocol al servidor.',
    'LBL_EMAIL_ERROR_MONITORED_FOLDER' => 'Es requereix una Carpeta Monitoritzada.',
    'LBL_EMAIL_ERROR_TRASH_FOLDER' => 'Es requereix una Carpeta de Paperera.',
    'LBL_EMAIL_ERROR_VIEW_RAW_SOURCE' => 'Aquesta informació no està disponible',
    'LBL_EMAIL_ERROR_NO_OUTBOUND' => 'No hi ha cap servidor de correu sortint especificat.',
    'LBL_EMAIL_ERROR_SENDING' => 'Error enviant el missatge. Contacteu amb l\'administrador per obtenir ajuda.',
    'LBL_EMAIL_FOLDERS' => SugarThemeRegistry::current()->getImage('icon_email_folder', 'align=absmiddle border=0', null, null, '.gif', '') . 'Carpetes',
    'LBL_EMAIL_FOLDERS_SHORT' => SugarThemeRegistry::current()->getImage('icon_email_folder', 'align=absmiddle border=0', null, null, '.gif', ''),
    'LBL_EMAIL_FOLDERS_ADD' => 'Afegir',
    'LBL_EMAIL_FOLDERS_ADD_DIALOG_TITLE' => 'Afegir Nova Carpeta',
    'LBL_EMAIL_FOLDERS_RENAME_DIALOG_TITLE' => 'Renombrar Carpeta',
    'LBL_EMAIL_FOLDERS_ADD_NEW_FOLDER' => 'Desa',
    'LBL_EMAIL_FOLDERS_ADD_THIS_TO' => 'Afegir aquesta carpeta a',
    'LBL_EMAIL_FOLDERS_CHANGE_HOME' => 'Aquesta carpeta no pot ser canviada',
    'LBL_EMAIL_FOLDERS_DELETE_CONFIRM' => 'Està segur que vol eliminar aquesta carpeta?\nAquest procés no pot ser tornat enrera.\nLa eliminació de carpetes s\'aplicarà en cascada a totes les carpetes contingudes.',
    'LBL_EMAIL_FOLDERS_NEW_FOLDER' => 'Nom de la Nova Carpeta',
    'LBL_EMAIL_FOLDERS_NO_VALID_NODE' => 'Si us plau, seleccioni una carpeta abans de realitzar aquesta acció.',
    'LBL_EMAIL_FOLDERS_TITLE' => 'Administració de Carpetes de SuiteCRM',

    'LBL_EMAIL_FORWARD' => 'Reenviar',
    'LBL_EMAIL_DELIMITER' => '::;::',
    'LBL_EMAIL_DOWNLOAD_STATUS' => 'Descarregats [[count]] de [[total]] correus electrònics',
    'LBL_EMAIL_FROM' => 'De',
    'LBL_EMAIL_GROUP' => 'Grup',
    'LBL_EMAIL_UPPER_CASE_GROUP' => 'Grup',
    'LBL_EMAIL_HOME_FOLDER' => 'Inici',
    'LBL_EMAIL_IE_DELETE' => 'Esborrant el compte',
    'LBL_EMAIL_IE_DELETE_SIGNATURE' => 'Esborrant la firma',
    'LBL_EMAIL_IE_DELETE_CONFIRM' => 'Segur que voleu eliminar aquest compte de correu?',
    'LBL_EMAIL_IE_DELETE_SUCCESSFUL' => 'Eliminació correcta.',
    'LBL_EMAIL_IE_SAVE' => 'Desant informació del compte',
    'LBL_EMAIL_IMPORTING_EMAIL' => 'Important correu electrònic',
    'LBL_EMAIL_IMPORT_EMAIL' => 'Importa a SuiteCRM',
    'LBL_EMAIL_IMPORT_SETTINGS' => 'Configuració d\'importació',
    'LBL_EMAIL_INVALID' => 'No vàlid',
    'LBL_EMAIL_LOADING' => 'Carregant...',
    'LBL_EMAIL_MARK' => 'Marcar',
    'LBL_EMAIL_MARK_FLAGGED' => 'Com a etiquetat',
    'LBL_EMAIL_MARK_READ' => 'Com a llegit',
    'LBL_EMAIL_MARK_UNFLAGGED' => 'Com a no etiquetat',
    'LBL_EMAIL_MARK_UNREAD' => 'Com a no llegit',
    'LBL_EMAIL_ASSIGN_TO' => 'Assigna a',

    'LBL_EMAIL_MENU_ADD_FOLDER' => 'Crea una carpeta',
    'LBL_EMAIL_MENU_COMPOSE' => 'Redacta per a',
    'LBL_EMAIL_MENU_DELETE_FOLDER' => 'Esborra la carpeta',
    'LBL_EMAIL_MENU_EMPTY_TRASH' => 'Buida la paperera',
    'LBL_EMAIL_MENU_SYNCHRONIZE' => 'Sincronitza',
    'LBL_EMAIL_MENU_CLEAR_CACHE' => 'Neteja els arxius de la memòria cau',
    'LBL_EMAIL_MENU_REMOVE' => 'Elimina',
    'LBL_EMAIL_MENU_RENAME_FOLDER' => 'Canvia el nom de la carpeta',
    'LBL_EMAIL_MENU_RENAMING_FOLDER' => 'Canviant el nom de la carpeta',
    'LBL_EMAIL_MENU_MAKE_SELECTION' => 'Feu una selecció abans d\'intentar aquesta operació.',

    'LBL_EMAIL_MENU_HELP_ADD_FOLDER' => 'Crear una carpeta (remota o a SuiteCRM)',
    'LBL_EMAIL_MENU_HELP_DELETE_FOLDER' => 'Eliminar una carpeta (remota o a SuiteCRM)',
    'LBL_EMAIL_MENU_HELP_EMPTY_TRASH' => 'Buida totes les carpetes de paperera dels seus comptes de correu',
    'LBL_EMAIL_MENU_HELP_MARK_READ' => 'Marcar aquests correus electrònics com a llegits',
    'LBL_EMAIL_MENU_HELP_MARK_UNFLAGGED' => 'Marcar aquests correus electrònics com a no etiquetats',
    'LBL_EMAIL_MENU_HELP_RENAME_FOLDER' => 'Renombrar una carpeta (remota o a SuiteCRM)',

    'LBL_EMAIL_MESSAGES' => 'missatges',

    'LBL_EMAIL_ML_NAME' => 'Nom de Llista',
    'LBL_EMAIL_ML_ADDRESSES_1' => 'Adreces de Llista seleccionades',
    'LBL_EMAIL_ML_ADDRESSES_2' => 'Adreces de Llista disponibles',

    'LBL_EMAIL_MULTISELECT' => '<b>Ctrl-Clic</b> per seleccionar múltiples<br />(els usuaris de Mac poden usar <b>CMD-Clic</b>)',

    'LBL_EMAIL_NO' => 'No',
    'LBL_EMAIL_NOT_SENT' => 'El sistema és incapaç de processar la vostra petició. Contacteu amb l\'administrador.',

    'LBL_EMAIL_OK' => 'Accepta',
    'LBL_EMAIL_ONE_MOMENT' => 'Un moment, si us plau...',
    'LBL_EMAIL_OPEN_ALL' => 'Obre múltiples missatges',
    'LBL_EMAIL_OPTIONS' => 'Opcions',
    'LBL_EMAIL_QUICK_COMPOSE' => 'Redacció ràpida',
    'LBL_EMAIL_OPT_OUT' => 'Refusat',
    'LBL_EMAIL_OPT_IN' => 'Autoritzat a enviar',
    'LBL_EMAIL_OPT_IN_AND_INVALID' => 'Autoritzat a enviar i invàlid',
    'LBL_EMAIL_OPT_OUT_AND_INVALID' => 'Optar fora i no és vàlid',
    'LBL_EMAIL_PERFORMING_TASK' => 'Realitzant tasca',
    'LBL_EMAIL_PRIMARY' => 'Principal',
    'LBL_EMAIL_PRINT' => 'Imprimeix',

    'LBL_EMAIL_QC_BUGS' => 'Incidència',
    'LBL_EMAIL_QC_CASES' => 'Cas',
    'LBL_EMAIL_QC_LEADS' => 'Client Potencial',
    'LBL_EMAIL_QC_CONTACTS' => 'Contacte',
    'LBL_EMAIL_QC_TASKS' => 'Tasca',
    'LBL_EMAIL_QC_OPPORTUNITIES' => 'Oportunitat',
    'LBL_EMAIL_QUICK_CREATE' => 'Creació Ràpida',

    'LBL_EMAIL_REBUILDING_FOLDERS' => 'Reconstruint Carpetes',
    'LBL_EMAIL_RELATE_TO' => 'Relacionar amb',
    'LBL_EMAIL_VIEW_RELATIONSHIPS' => 'Vure Relacions',
    'LBL_EMAIL_RECORD' => 'Registre de correu electrònic',
    'LBL_EMAIL_REMOVE' => 'Eliminar',
    'LBL_EMAIL_REPLY' => 'Contestar',
    'LBL_EMAIL_REPLY_ALL' => 'Contestar a Tots',
    'LBL_EMAIL_REPLY_TO' => 'Contestar a',
    'LBL_EMAIL_RETRIEVING_MESSAGE' => 'Recuperant Missatge',
    'LBL_EMAIL_RETRIEVING_RECORD' => 'Recuperant registre de correu electrònic',
    'LBL_EMAIL_SELECT_ONE_RECORD' => 'Si us plau, seleccioni un únic registre de correu electrònic',
    'LBL_EMAIL_RETURN_TO_VIEW' => 'Tornar a Mòdul Anterior?',
    'LBL_EMAIL_REVERT' => 'Revertir',
    'LBL_EMAIL_RELATE_EMAIL' => 'Relacionar correu electrònic',

    'LBL_EMAIL_RULES_TITLE' => 'Administració de Regles',

    'LBL_EMAIL_SAVE' => 'Desar',
    'LBL_EMAIL_SAVE_AND_REPLY' => 'Desar i Contestar',
    'LBL_EMAIL_SAVE_DRAFT' => 'Desar Esborrany',
    'LBL_EMAIL_DRAFT_SAVED' => 'L\'esborrany s\'ha guardat correctament',

    'LBL_EMAIL_SEARCH' => SugarThemeRegistry::current()->getImage('Search', 'align=absmiddle border=0', null, null, '.gif', ''),
    'LBL_EMAIL_SEARCH_SHORT' => SugarThemeRegistry::current()->getImage('Search', 'align=absmiddle border=0', null, null, '.gif', ''),
    'LBL_EMAIL_SEARCH_DATE_FROM' => 'Data Desde',
    'LBL_EMAIL_SEARCH_DATE_UNTIL' => 'Data Fis a',
    'LBL_EMAIL_SEARCH_NO_RESULTS' => 'No hi ha resultats per els seus criteris de cerca.',
    'LBL_EMAIL_SEARCH_RESULTS_TITLE' => 'Resultats de la cerca',

    'LBL_EMAIL_SELECT' => 'Seleccionar',

    'LBL_EMAIL_SEND' => 'Enviar',
    'LBL_EMAIL_SENDING_EMAIL' => 'Enviant correu electrònic',

    'LBL_EMAIL_SETTINGS' => 'Configuració',
    'LBL_EMAIL_SETTINGS_ACCOUNTS' => 'Comptes de correu',
    'LBL_EMAIL_SETTINGS_ADD_ACCOUNT' => 'Netejar Formulari',
    'LBL_EMAIL_SETTINGS_CHECK_INTERVAL' => 'Comprovar nou correu',
    'LBL_EMAIL_SETTINGS_FROM_ADDR' => 'Adreça del Remitent',
    'LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR' => 'Adreça de correu electrònic per a notificació de prova:',
    'LBL_EMAIL_SETTINGS_FROM_NAME' => 'Nom del Remitent',
    'LBL_EMAIL_SETTINGS_REPLY_TO_ADDR' => 'Respondre a l\'adreça',
    'LBL_EMAIL_SETTINGS_FULL_SYNC' => 'Sincronitzar tots els comptes de correu',
    'LBL_EMAIL_TEST_NOTIFICATION_SENT' => 'Un correu electrònic va ser enviat a l\'adreça de correu electrònic especificada utilitzant la configuració del correu sortint proporcionada. Si us plau, comproveu si s\'ha rebut el correu electrònic per verificar que la configuració és correcta.',
    'LBL_EMAIL_TEST_SEE_FULL_SMTP_LOG' => 'Vegi el registre de SMTP complert',
    'LBL_EMAIL_SETTINGS_FULL_SYNC_WARN' => 'Realitzar una sincronització completa?\nPer a comptes de correu grans, pot durar diversos minuts.',
    'LBL_EMAIL_SUBSCRIPTION_FOLDER_HELP' => 'Faci clic en la Tecla Shift o en la tecla Ctrl per seleccionar carpetes múltiples.',
    'LBL_EMAIL_SETTINGS_GENERAL' => 'General',
    'LBL_EMAIL_SETTINGS_GROUP_FOLDERS_CREATE' => 'Crear Carpetes de Grup',

    'LBL_EMAIL_SETTINGS_GROUP_FOLDERS_EDIT' => 'Editar Carpetes de Grup',

    'LBL_EMAIL_SETTINGS_NAME' => 'Nom',
    'LBL_EMAIL_SETTINGS_REQUIRE_REFRESH' => 'Aquestes opcions poden requerir un refresc de pàgina per ser activades.',
    'LBL_EMAIL_SETTINGS_RETRIEVING_ACCOUNT' => 'Recuperant compta de correu',
    'LBL_EMAIL_SETTINGS_SAVED' => 'S\'ha aplicat la configuració correctament.',
    'LBL_EMAIL_SETTINGS_SEND_EMAIL_AS' => 'Enviar només correus electrònics de text pla',
    'LBL_EMAIL_SETTINGS_SHOW_NUM_IN_LIST' => 'Número de correus electrònics per pàgina',
    'LBL_EMAIL_SETTINGS_TITLE_LAYOUT' => 'Configuració Visual',
    'LBL_EMAIL_SETTINGS_TITLE_PREFERENCES' => 'Preferències',
    'LBL_EMAIL_SETTINGS_USER_FOLDERS' => 'Carpetes d\'Usuari Disponibles',
    'LBL_EMAIL_ERROR_PREPEND' => 'S\'ha produït un error en el correu electrònic:',
    'LBL_EMAIL_INVALID_PERSONAL_OUTBOUND' => 'El servidor de correu sortint seleccionat per al compte de correu que utilitzeu no és vàlid. Comproveu la configuració o seleccioneu un servidor de correu diferent per al compte de correu.',
    'LBL_EMAIL_INVALID_SYSTEM_OUTBOUND' => 'No hi ha un servidor de correu de sortida configurat per a enviar correus electrònics. Si us plau, configureu un servidor de correu de sortida o seleccioneu un correu de sortida per la compta que esteu utilitzant a Configuració >> Compte de correu.',
    'LBL_DEFAULT_EMAIL_SIGNATURES' => 'Signatura per defecte',
    'LBL_EMAIL_SIGNATURES' => 'Firmes',
    'LBL_SMTPTYPE_GMAIL' => 'Gmail',
    'LBL_SMTPTYPE_YAHOO' => 'Yahoo! Mail',
    'LBL_SMTPTYPE_EXCHANGE' => 'Microsoft Exchange',
    'LBL_SMTPTYPE_OTHER' => 'Altre',
    'LBL_EMAIL_SPACER_MAIL_SERVER' => '[ Carpetes Remotes ]',
    'LBL_EMAIL_SPACER_LOCAL_FOLDER' => '[ Carpetes de SuiteCRM ]',
    'LBL_EMAIL_SUBJECT' => 'Assumpte',
    'LBL_EMAIL_SUCCESS' => 'Fet',
    'LBL_EMAIL_SUITE_FOLDER' => 'Carpeta de SuiteCRM',
    'LBL_EMAIL_TEMPLATE_EDIT_PLAIN_TEXT' => 'El cos de la plantilla de correu electrònic és vuit',
    'LBL_EMAIL_TEMPLATES' => 'Plantilles',
    'LBL_EMAIL_TO' => 'Per a',
    'LBL_EMAIL_VIEW' => 'Veure',
    'LBL_EMAIL_VIEW_HEADERS' => 'Mostrar Capceleres',
    'LBL_EMAIL_VIEW_RAW' => 'Mostrar codi font del correu electrònic',
    'LBL_EMAIL_VIEW_UNSUPPORTED' => 'Aquesta característica no està suportada quan s\'usa amb POP3.',
    'LBL_DEFAULT_LINK_TEXT' => 'Text d\'enllaç per defecte.',
    'LBL_EMAIL_YES' => 'Si',
    'LBL_EMAIL_TEST_OUTBOUND_SETTINGS' => 'Enviar correu electrònic de prova',
    'LBL_EMAIL_TEST_OUTBOUND_SETTINGS_SENT' => 'Correu electrònic de prova enviat',
    'LBL_EMAIL_MESSAGE_NO' => 'Missatge Nº', // Counter. Message number xx
    'LBL_EMAIL_IMPORT_SUCCESS' => 'Importació Correcta',
    'LBL_EMAIL_IMPORT_FAIL' => 'Importació Fallida a causa que el missatge ja ha estat importat o eliminat del servidor',

    'LBL_LINK_NONE' => 'Cap',
    'LBL_LINK_ALL' => 'Tots',
    'LBL_LINK_RECORDS' => 'Registres',
    'LBL_LINK_SELECT' => 'Seleccionar',
    'LBL_LINK_ACTIONS' => 'Accions', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CLOSE_ACTIVITY_HEADER' => 'Confirmar',
    'LBL_CLOSE_ACTIVITY_CONFIRM' => 'Vol tancar aquest #module#?',
    'LBL_INVALID_FILE_EXTENSION' => 'La extensió del fitxer no és vàlida',

    'ERR_AJAX_LOAD' => 'Ha succeit un error:',
    'ERR_AJAX_LOAD_FAILURE' => 'S\'ha produït un error al atendre la seva petició, si us plau, torni-ho a intentar més tard.',
    'ERR_AJAX_LOAD_FOOTER' => 'Si aquest error persisteix, si us plau, digui-li al seu administrador que deshabiliti Ajax per aquest mòdul',
    'ERR_DECIMAL_SEP_EQ_THOUSANDS_SEP' => 'No pot utilitzar-se el mateix caràcter com a separador decimal que l\'utilitzat com a separador de miles.\n\n Si us plau, canviï els valors.',
    'ERR_DELETE_RECORD' => 'Ha d\'especificar un número de registre per eliminar el contacte.',
    'ERR_EXPORT_DISABLED' => 'Exportació deshabilitada.',
    'ERR_EXPORT_TYPE' => 'Error exportando ',
    'ERR_INVALID_EMAIL_ADDRESS' => 'no es una adreça de correu vàlida.',
    'ERR_INVALID_FILE_REFERENCE' => 'Referència a arxiu no vàlid',
    'ERR_NO_HEADER_ID' => 'Aquesta funcionalitat no està disponible amb aquest tema.',
    'ERR_NOT_ADMIN' => 'Accés no autoritzat a l\'administració.',
    'ERR_MISSING_REQUIRED_FIELDS' => 'Falta camp requerit:',
    'ERR_INVALID_REQUIRED_FIELDS' => 'Camp requerit invàlid:',
    'ERR_INVALID_VALUE' => 'Valor no vàlid:',
    'ERR_NO_SUCH_FILE' => 'L\'arxiu no existeix en el sistema',
    'ERR_FILE_EMPTY' => 'El fitxer està buit', // PR 6672
    'ERR_NO_SINGLE_QUOTE' => 'No podem utilitzar les cometes simples per a ',
    'ERR_NOTHING_SELECTED' => 'Si us plau, realitzi una selecció abans de procedir.',
    'ERR_SELF_REPORTING' => 'Un usuari no pot ser informador de si mateix.',
    'ERR_SQS_NO_MATCH_FIELD' => "No s\'han trobat coincidències per al camp: ", // Excepció d'escapat 
    'ERR_SQS_NO_MATCH' => 'Sense coincidències',
    'ERR_ADDRESS_KEY_NOT_SPECIFIED' => 'Si us plau, especifiqui l\'índex \'clau\' en l\'atribut displayParams per a la definició de Metadades',
    'ERR_EXISTING_PORTAL_USERNAME' => 'Error: El Nom de Portal ja ha estat assignat a un altre contacte.',
    'ERR_COMPATIBLE_PRECISION_VALUE' => 'El valor del camp no és compatible amb el tipus de precisió',
    'ERR_EXTERNAL_API_SAVE_FAIL' => 'Ha succeït un error al intentar desar a una compta externa.',
    'ERR_NO_DB' => 'No es pot establir la connexió amb la base de dades. Si us plau, consulti els registres d\'errors de SuiteCRM per a més detalls.',
    'ERR_DB_FAIL' => 'Error de base de dades. Consulteu suitecrm.log per a més detalls.',
    'ERR_DB_VERSION' => 'Els fitxers {0} de SuiteCRM només poden ser utilitzats amb una base de dades {1} de SuiteCRM.',

    'LBL_ACCOUNT' => 'Compte',
    'LBL_ACCOUNTS' => 'Comptes',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activitats',
    'LBL_ACCUMULATED_HISTORY_BUTTON_KEY' => 'H',
    'LBL_ACCUMULATED_HISTORY_BUTTON_LABEL' => 'Veure Resum',
    'LBL_ACCUMULATED_HISTORY_BUTTON_TITLE' => 'Veure Resum',
    'LBL_ADD_BUTTON' => 'Afegir',
    'LBL_ADD_DOCUMENT' => 'Afegir Document',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_KEY' => 'L',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL' => 'Afegir a Llista de Públic Objectiu',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL_ACCOUNTS_CONTACTS' => 'Afegir contactes a la llista de distribució',
    'LBL_ADDITIONAL_DETAILS_CLOSE_TITLE' => 'Clic per tancar',
    'LBL_ADDITIONAL_DETAILS' => 'Detalls addicionals',
    'LBL_ADMIN' => 'Administració',
    'LBL_ALT_HOT_KEY' => '',
    'LBL_ARCHIVE' => 'Arxiu',
    'LBL_ASSIGNED_TO_USER' => 'Assignat a Usuari',
    'LBL_ASSIGNED_TO' => 'Assignat a:',
    'LBL_BACK' => 'Enrere',
    'LBL_BILLING_ADDRESS' => 'Adreça de facturació',
    'LBL_QUICK_CREATE' => 'Crear ',
    'LBL_BROWSER_TITLE' => 'SuiteCRM - CRM de codi obert',
    'LBL_BUGS' => 'Incidències',
    'LBL_BY' => 'per',
    'LBL_CALLS' => 'Trucades',
    'LBL_CAMPAIGNS_SEND_QUEUED' => 'Envia els missatges de la cua de correu',
    'LBL_SUBMIT_BUTTON_LABEL' => 'Envia',
    'LBL_CASE' => 'Cas',
    'LBL_CASES' => 'Casos',
    'LBL_CHANGE_PASSWORD' => 'Modifica la contrasenya',
    'LBL_CHARSET' => 'UTF-8',
    'LBL_CHECKALL' => 'Marcar tots',
    'LBL_CITY' => 'Ciutat',
    'LBL_CLEAR_BUTTON_LABEL' => 'Netejar',
    'LBL_CLEAR_BUTTON_TITLE' => 'Netejar',
    'LBL_CLEARALL' => 'Desmarcar Tots',
    'LBL_CLOSE_BUTTON_TITLE' => 'Tancar', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_LABEL' => 'Tancar i Crear Nou', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_TITLE' => 'Tancar i Crear Nou', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_KEY' => 'C',
    'LBL_OPEN_ITEMS' => 'Elements oberts:',
    'LBL_COMPOSE_EMAIL_BUTTON_KEY' => 'L',
    'LBL_COMPOSE_EMAIL_BUTTON_LABEL' => 'Redactar correu electrònic',
    'LBL_COMPOSE_EMAIL_BUTTON_TITLE' => 'Redactar correu electrònic',
    'LBL_SEARCH_DROPDOWN_YES' => 'Si',
    'LBL_SEARCH_DROPDOWN_NO' => 'No',
    'LBL_CONTACT_LIST' => 'Llista de Contactes',
    'LBL_CONTACT' => 'Contacte',
    'LBL_CONTACTS' => 'Contactes',
    'LBL_CONTRACT' => 'Contracte',
    'LBL_CONTRACTS' => 'Contractes',
    'LBL_COUNTRY' => 'País:',
    'LBL_CREATE_BUTTON_LABEL' => 'Crea', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CREATED_BY_USER' => 'Creat per Usuari',
    'LBL_CREATED_USER' => 'Creat per Usuari',
    'LBL_CREATED' => 'Creat per',
    'LBL_CURRENT_USER_FILTER' => 'Només els meus elements:',
    'LBL_CURRENCY' => 'Moneda:',
    'LBL_DOCUMENTS' => 'Documents',
    'LBL_DATE_ENTERED' => 'Data de Creació:',
    'LBL_DATE_MODIFIED' => 'Última Modificació:',
    'LBL_EDIT_BUTTON' => 'Edita',
    // STIC-Custom 20240214 JBL - QuickEdit view
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/93
    'LBL_QUICKEDIT_BUTTON' => '↙ Edita',
    // END STIC-Custom
    'LBL_DUPLICATE_BUTTON' => 'Duplica',
    'LBL_DELETE_BUTTON' => 'Esborra',
    'LBL_DELETE' => 'Esborra',
    'LBL_DELETED' => 'Esborrat',
    'LBL_DIRECT_REPORTS' => 'Informa a',
    'LBL_DONE_BUTTON_LABEL' => 'Fet',
    'LBL_DONE_BUTTON_TITLE' => 'Fet',
    'LBL_FAVORITES' => 'Favorits',
    'LBL_VCARD' => 'vCard',
    'LBL_EMPTY_VCARD' => 'Si us plau, seleccioni un fitxer vCard',
    'LBL_EMPTY_REQUIRED_VCARD' => 'vCard no té tots els camps obligatoris per a aquest mòdul. Consulteu suitecrm.log per a detalls.',
    'LBL_VCARD_ERROR_FILESIZE' => 'El fitxer que heu intentat pujar excedeix el límit de 30000 bytes que està especificat al formulari HTML.',
    'LBL_VCARD_ERROR_DEFAULT' => 'S\'ha produït un error al pujar el fitxer vCard. Si us plau, consulti el fitxer suitecrm.log per a més detalls.',
    'LBL_IMPORT_VCARD' => 'Importar vCard:',
    'LBL_IMPORT_VCARD_BUTTON_LABEL' => 'Importar vCard',
    'LBL_IMPORT_VCARD_BUTTON_TITLE' => 'Importar vCard',
    'LBL_VIEW_BUTTON' => 'Veure',
    'LBL_EMAIL_PDF_BUTTON_LABEL' => 'Envia com a PDF',
    'LBL_EMAIL_PDF_BUTTON_TITLE' => 'Envia per correu electrònic com a PDF',
    'LBL_EMAILS' => 'Correus electrònics',
    'LBL_EMPLOYEES' => 'Empleats',
    'LBL_ENTER_DATE' => 'Introdueix la data',
    'LBL_EXPORT' => 'Exportar',
    'LBL_FAVORITES_FILTER' => 'Els meus preferits:',
    'LBL_GO_BUTTON_LABEL' => 'Endavant',
    'LBL_HIDE' => 'Amagar',
    'LBL_ID' => 'ID',
    'LBL_IMPORT' => 'Importar',
    'LBL_IMPORT_STARTED' => 'Importació iniciada: ',
    'LBL_LAST_VIEWED' => 'Recents',
    'LBL_LEADS' => 'Clients Potencials',
    'LBL_LESS' => 'menys',
    'LBL_CAMPAIGN' => 'Campanya:',
    'LBL_CAMPAIGNS' => 'Campanyes',
    'LBL_CAMPAIGNLOG' => 'Registre de Campanyes',
    'LBL_CAMPAIGN_CONTACT' => 'Campanyes',
    'LBL_CAMPAIGN_ID' => 'campaign_id',
    'LBL_CAMPAIGN_NONE' => 'Cap',
    'LBL_THEME' => 'Tema:',
    'LBL_FOUND_IN_RELEASE' => 'Trobat en Versió',
    'LBL_FIXED_IN_RELEASE' => 'Corregit en Versió',
    'LBL_LIST_ACCOUNT_NAME' => 'Nom del Compte',
    'LBL_LIST_ASSIGNED_USER' => 'Usuari',
    'LBL_LIST_CONTACT_NAME' => 'Nom Contacte',
    'LBL_LIST_CONTACT_ROLE' => 'Rol Contacte',
    'LBL_LIST_DATE_ENTERED' => 'Data de Creació',
    'LBL_LIST_EMAIL' => 'Correu electrònic',
    'LBL_LIST_NAME' => 'Nom',
    'LBL_LIST_OF' => 'de',
    'LBL_LIST_PHONE' => 'Telèfon',
    'LBL_LIST_RELATED_TO' => 'Relacionat amb:',
    'LBL_LIST_USER_NAME' => 'Nom d\'Usuari',
    'LBL_LISTVIEW_NO_SELECTED' => 'Si us plau, seleccioni almenys 1 registre per procedir.',
    'LBL_LISTVIEW_TWO_REQUIRED' => 'Si us plau, seleccioni almenys 2 registres per procedir.',
    'LBL_LISTVIEW_OPTION_SELECTED' => 'Registres Seleccionats',
    'LBL_LISTVIEW_SELECTED_OBJECTS' => 'Seleccionats: ',

    'LBL_LOCALE_NAME_EXAMPLE_FIRST' => 'Joan',
    'LBL_LOCALE_NAME_EXAMPLE_LAST' => 'Pérez',
    'LBL_LOCALE_NAME_EXAMPLE_SALUTATION' => 'Sr.',
    'LBL_LOCALE_NAME_EXAMPLE_TITLE' => 'Code Monkey Extraordinaire',
    'LBL_CANCEL' => 'Cancelar',
    'LBL_VERIFY' => 'Verificar',
    'LBL_RESEND' => 'Reenviar',
    'LBL_PROFILE' => 'Perfil',
    'LBL_MAILMERGE' => 'Combinar Correspondència',
    'LBL_MASS_UPDATE' => 'Actualització Massiva',
    // STIC-Custom - 20220704 - JCH - Duplicate & Mass Update
    // STIC#776
    'LBL_MASS_DUPLICATE_UPDATE' => 'Duplicació i Actualització Massiva',
    'LBL_MASS_DUPLICATE_REMOVE_NAME' => 'Buida el Nom dels nous registres per tal que pugui ser reconstruït automàticament',
    'LBL_MASS_DUPLICATE_UPDATE_CONFIRMATION_NUM' => 'Segur que voleu duplicar i actualitzar el(s) ',
    'LBL_MASS_DUPLICATE_UPDATE_BTN' => 'Duplica i Actualitza',
    // END STIC
    'LBL_NO_MASS_UPDATE_FIELDS_AVAILABLE' => 'No hi ha camps disponibles per a la operació d\'actualització massiva',
    'LBL_OPT_OUT_FLAG_PRIMARY' => 'Refusar per correu electrònic principal',
    'LBL_OPT_IN_FLAG_PRIMARY' => 'Autoritzat a enviar a l\'E-mail primari',
    'LBL_MEETINGS' => 'Reunions',
    'LBL_MEETING_GO_BACK' => 'Tornar a la reunió',
    'LBL_MEMBERS' => 'Membres',
    'LBL_MEMBER_OF' => 'Membre de',
    'LBL_MODIFIED_BY_USER' => 'Modificat per Usuari',
    'LBL_MODIFIED_USER' => 'Modificat per Usuari',
    'LBL_MODIFIED' => 'Modificat per',
    'LBL_MODIFIED_NAME' => 'Modificat per Nom',
    'LBL_MORE' => 'més',
    'LBL_MY_ACCOUNT' => 'El Meu Compte',
    'LBL_NAME' => 'Nom',
    'LBL_NEW_BUTTON_KEY' => 'N',
    'LBL_NEW_BUTTON_LABEL' => 'Nou',
    'LBL_NEW_BUTTON_TITLE' => 'Crear',
    'LBL_NEXT_BUTTON_LABEL' => 'Següent',
    'LBL_NONE' => '--Cap--',
    'LBL_NOTES' => 'Notes',
    'LBL_OPPORTUNITIES' => 'Oportunitats',
    'LBL_OPPORTUNITY_NAME' => 'Nom de l\'Oportunitat',
    'LBL_OPPORTUNITY' => 'Oportunitat',
    'LBL_OR' => 'O',
    'LBL_PANEL_OVERVIEW' => 'Visió general', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PANEL_ASSIGNMENT' => 'Altre', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PANEL_ADVANCED' => 'Més Informació', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PARENT_TYPE' => 'Tipus de Pare',
    'LBL_PERCENTAGE_SYMBOL' => '%',
    'LBL_POSTAL_CODE' => 'Codi postal:',
    'LBL_PRIMARY_ADDRESS_CITY' => 'Ciutat principal:',
    'LBL_PRIMARY_ADDRESS_COUNTRY' => 'País principal:',
    'LBL_PRIMARY_ADDRESS_POSTALCODE' => 'Codi postal principal:',
    'LBL_PRIMARY_ADDRESS_STATE' => 'Estat/Província principal:',
    'LBL_PRIMARY_ADDRESS_STREET_2' => 'Carrer principal 2:',
    'LBL_PRIMARY_ADDRESS_STREET_3' => 'Carrer principal 3:',
    'LBL_PRIMARY_ADDRESS_STREET' => 'Carrer principal:',
    'LBL_PRIMARY_ADDRESS' => 'Adreça principal:',

    'LBL_PROSPECTS' => 'Perspectives',
    'LBL_PRODUCTS' => 'Productes',
    'LBL_PROJECT_TASKS' => 'Tasques de Projecte',
    'LBL_PROJECTS' => 'Projectes',
    'LBL_QUOTES' => 'Pressupostos',

    'LBL_RELATED' => 'Relacionats',
    'LBL_RELATED_RECORDS' => 'Registres Relacionats',
    'LBL_REMOVE' => 'Eliminar',
    'LBL_REPORTS_TO' => 'Informa a',
    'LBL_REQUIRED_SYMBOL' => '*',
    'LBL_REQUIRED_TITLE' => 'Indica un camp requerit',
    'LBL_EMAIL_DONE_BUTTON_LABEL' => 'Fet',
    'LBL_FULL_FORM_BUTTON_KEY' => 'F',
    'LBL_FULL_FORM_BUTTON_LABEL' => 'Formulari Complert',
    'LBL_FULL_FORM_BUTTON_TITLE' => 'Formulari Complert',
    'LBL_SAVE_NEW_BUTTON_LABEL' => 'Desar i Crear Nou',
    'LBL_SAVE_NEW_BUTTON_TITLE' => 'Desar i Crear Nou',
    'LBL_SAVE_OBJECT' => 'Desar {0}',
    'LBL_SEARCH_BUTTON_KEY' => 'C',
    'LBL_SEARCH_BUTTON_LABEL' => 'Cerca',
    'LBL_SEARCH_BUTTON_TITLE' => 'Cerca',
    'LBL_FILTER' => 'Filtre',
    'LBL_SEARCH' => 'Cerca',
    'LBL_SEARCH_ALT' => '',
    'LBL_SEARCH_MORE' => 'més',
    'LBL_UPLOAD_IMAGE_FILE_INVALID' => 'Format de fitxer no vàlid, només es poden  pujar imatges.',
    'LBL_SELECT_BUTTON_KEY' => 'T',
    'LBL_SELECT_BUTTON_LABEL' => 'Seleccionar',
    'LBL_SELECT_BUTTON_TITLE' => 'Seleccionar',
    'LBL_BROWSE_DOCUMENTS_BUTTON_LABEL' => 'Explorar Documents',
    'LBL_BROWSE_DOCUMENTS_BUTTON_TITLE' => 'Explorar Documents',
    'LBL_SELECT_CONTACT_BUTTON_KEY' => 'T',
    'LBL_SELECT_CONTACT_BUTTON_LABEL' => 'Seleccionar Contacte',
    'LBL_SELECT_CONTACT_BUTTON_TITLE' => 'Seleccionar Contacte',
    'LBL_SELECT_REPORTS_BUTTON_LABEL' => 'Seleccionar desde Informes',
    'LBL_SELECT_REPORTS_BUTTON_TITLE' => 'Seleccionar Informes',
    'LBL_SELECT_USER_BUTTON_KEY' => 'U',
    'LBL_SELECT_USER_BUTTON_LABEL' => 'Seleccionar Usuari',
    'LBL_SELECT_USER_BUTTON_TITLE' => 'Seleccionar Usuari',
    // Clear buttons take up too many keys, lets default the relate and collection ones to be empty
    'LBL_ACCESSKEY_CLEAR_RELATE_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_RELATE_TITLE' => 'Netejar Selecció',
    'LBL_ACCESSKEY_CLEAR_RELATE_LABEL' => 'Netejar Selecció',
    'LBL_ACCESSKEY_CLEAR_COLLECTION_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_COLLECTION_TITLE' => 'Netejar Selecció',
    'LBL_ACCESSKEY_CLEAR_COLLECTION_LABEL' => 'Netejar Selecció',
    'LBL_ACCESSKEY_SELECT_FILE_KEY' => 'F',
    'LBL_ACCESSKEY_SELECT_FILE_TITLE' => 'Seleccionar Arxiu',
    'LBL_ACCESSKEY_SELECT_FILE_LABEL' => 'Seleccionar Arxiu',
    'LBL_ACCESSKEY_CLEAR_FILE_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_FILE_TITLE' => 'Netejar Arxiu',
    'LBL_ACCESSKEY_CLEAR_FILE_LABEL' => 'Netejar Arxiu',

    'LBL_ACCESSKEY_SELECT_USERS_KEY' => 'U',
    'LBL_ACCESSKEY_SELECT_USERS_TITLE' => 'Seleccionar Usuari',
    'LBL_ACCESSKEY_SELECT_USERS_LABEL' => 'Seleccionar Usuari',
    'LBL_ACCESSKEY_CLEAR_USERS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_USERS_TITLE' => 'Netejar usuari',
    'LBL_ACCESSKEY_CLEAR_USERS_LABEL' => 'Netejar usuari',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_KEY' => 'A',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_TITLE' => 'Seleccionar Compte',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_LABEL' => 'Seleccionar Compte',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_TITLE' => 'Netejar Compte',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_LABEL' => 'Netejar Compte',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_KEY' => 'M',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_TITLE' => 'Seleccionar Campanya',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_LABEL' => 'Seleccionar Campanya',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_TITLE' => 'Netejar Campanya',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_LABEL' => 'Netejar Campanya',
    'LBL_ACCESSKEY_SELECT_CONTACTS_KEY' => 'C',
    'LBL_ACCESSKEY_SELECT_CONTACTS_TITLE' => 'Seleccionar Contacte',
    'LBL_ACCESSKEY_SELECT_CONTACTS_LABEL' => 'Seleccionar Contacte',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_TITLE' => 'Netejar Contacte',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_LABEL' => 'Netejar Contacte',
    'LBL_ACCESSKEY_SELECT_TEAMSET_KEY' => 'Z',
    'LBL_ACCESSKEY_SELECT_TEAMSET_TITLE' => 'Seleccionar Equip',
    'LBL_ACCESSKEY_SELECT_TEAMSET_LABEL' => 'Seleccionar Equip',
    'LBL_ACCESSKEY_CLEAR_TEAMS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_TEAMS_TITLE' => 'Netejar Equip',
    'LBL_ACCESSKEY_CLEAR_TEAMS_LABEL' => 'Netejar Equip',
    'LBL_SERVER_RESPONSE_RESOURCES' => 'Recursos usats per construir aquesta pàgina (consultes,arxius)',
    'LBL_SERVER_RESPONSE_TIME_SECONDS' => 'segons.',
    'LBL_SERVER_RESPONSE_TIME' => 'Temps de resposta del servidor:',
    'LBL_SERVER_MEMORY_BYTES' => 'bytes',
    'LBL_SERVER_MEMORY_USAGE' => 'Ús de memòria del servidor: {0} ({1})',
    'LBL_SERVER_MEMORY_LOG_MESSAGE' => 'Ús: - mòdul: {0} - acció: {1}',
    'LBL_SERVER_PEAK_MEMORY_USAGE' => 'Pic de memòria utilitzada pel servidor: {0} ({1})',
    'LBL_SHIPPING_ADDRESS' => 'Adreça d\'enviament',
    'LBL_SHOW' => 'Mostrar',
    'LBL_STATE' => 'Estat:', //Used for Case State, situation, condition
    'LBL_STATUS_UPDATED' => 'El seu estat per a aquest esdeveniment ha estat actualitzat!',
    'LBL_STATUS' => 'Estat:',
    'LBL_STREET' => 'Carrer',
    'LBL_SUBJECT' => 'Assumpte',

    'LBL_INBOUNDEMAIL_ID' => 'Id de correu electrònic entrant',

    'LBL_SCENARIO_SALES' => 'Vendes',
    'LBL_SCENARIO_MARKETING' => 'Màrqueting',
    'LBL_SCENARIO_FINANCE' => 'Finances',
    'LBL_SCENARIO_SERVICE' => 'Servei',
    'LBL_SCENARIO_PROJECT' => 'Gestió de projectes',

    'LBL_SCENARIO_SALES_DESCRIPTION' => 'Aquesta situació facilita la gestió de vendes d\'elements',
    'LBL_SCENARIO_MAKETING_DESCRIPTION' => 'Aquesta situació facilita la gestió d\'elements de màrqueting',
    'LBL_SCENARIO_FINANCE_DESCRIPTION' => 'Aquesta situació facilita la gestió d\'elements relacionats de Finances ',
    'LBL_SCENARIO_SERVICE_DESCRIPTION' => 'Aquesta situació facilita la gestió del servei d\'elements relacionats',
    'LBL_SCENARIO_PROJECT_DESCRIPTION' => 'Aquest escenari posa la gestió del projecte elements relacionats',

    'LBL_SYNC' => 'Sincronitzar',
    'LBL_TABGROUP_ALL' => 'Tot',
    'LBL_TABGROUP_ACTIVITIES' => 'Activitats',
    'LBL_TABGROUP_COLLABORATION' => 'Col·laboració',
    'LBL_TABGROUP_MARKETING' => 'Màrqueting',
    'LBL_TABGROUP_OTHER' => 'Altre',
    'LBL_TABGROUP_SALES' => 'Vendes',
    'LBL_TABGROUP_SUPPORT' => 'Suport',
    'LBL_TASKS' => 'Tasques',
    'LBL_THOUSANDS_SYMBOL' => 'K',
    'LBL_TRACK_EMAIL_BUTTON_LABEL' => 'Arxivar correu electrònic',
    'LBL_TRACK_EMAIL_BUTTON_TITLE' => 'Arxivar correu electrònic',
    'LBL_UNDELETE_BUTTON_LABEL' => 'Restaurar',
    'LBL_UNDELETE_BUTTON_TITLE' => 'Restaurar',
    'LBL_UNDELETE_BUTTON' => 'Restaurar',
    'LBL_UNDELETE' => 'Restaurar',
    'LBL_UNSYNC' => 'Desincronitzar',
    'LBL_UPDATE' => 'Actualitzar',
    'LBL_USER_LIST' => 'Llista d\'Usuaris',
    'LBL_USERS' => 'Usuaris',
    'LBL_VERIFY_EMAIL_ADDRESS' => 'Comprovant l\'entrada de correu electrònic actual...',
    'LBL_VERIFY_PORTAL_NAME' => 'Comprovant el nom de portal actual...',
    'LBL_VIEW_IMAGE' => 'veure',

    'LNK_ABOUT' => 'Quant a',
    'LNK_ADVANCED_FILTER' => 'Filtre avançat',
    'LNK_BASIC_FILTER' => 'Filtre ràpid',
    'LBL_ADVANCED_SEARCH' => 'Filtre avançat',
    'LBL_QUICK_FILTER' => 'Filtre ràpid',
    'LNK_SEARCH_NONFTS_VIEW_ALL' => 'Mostrar Tot',
    'LNK_CLOSE' => 'Tancament',
    'LBL_MODIFY_CURRENT_FILTER' => 'Modificar el filtre actual',
    'LNK_SAVED_VIEWS' => 'Cerca i dissenys desats',
    'LNK_DELETE' => 'Eliminar',
    'LNK_EDIT' => 'Editar',
    'LNK_GET_LATEST' => 'Obtenir última',
    'LNK_GET_LATEST_TOOLTIP' => 'Reemplaçar amb última versió',
    'LNK_HELP' => 'Ajuda',
    'LNK_CREATE' => 'Crear',
    'LNK_LIST_END' => 'Últim',
    'LNK_LIST_NEXT' => 'Següent',
    'LNK_LIST_PREVIOUS' => 'Anterior',
    'LNK_LIST_RETURN' => 'Tornar a Llista',
    'LNK_LIST_START' => 'Inici',
    'LNK_LOAD_SIGNED' => 'Firmar',
    'LNK_LOAD_SIGNED_TOOLTIP' => 'Reemplaçar amb document firmat',
    'LNK_PRINT' => 'Imprimir',
    'LNK_BACKTOTOP' => 'Tornar a l\'inici',
    'LNK_REMOVE' => 'Eliminar',
    'LNK_RESUME' => 'Continuar',
    'LNK_VIEW_CHANGE_LOG' => 'Veure Registre de Canvis',

    'NTC_CLICK_BACK' => 'Si us plau, pressioni el botó anterior del navegador i corregeixi l\'error.',
    'NTC_DATE_FORMAT' => '(aaaa-mm-dd)',
    'NTC_DELETE_CONFIRMATION_MULTIPLE' => 'Segur que voleu eliminar els registres seleccionats?',
    'NTC_TEMPLATE_IS_USED' => 'La plantilla s\'està utilitzant almenys en un registre de màrqueting per correu electrònic. Està segur que desitja eliminar-la?',
    'NTC_TEMPLATES_IS_USED' => 'Les següents plantilles són utilitzades en registres de màrqueting per correu electrònic. Esteu segur que voleu suprimir-les?' . PHP_EOL,
    'NTC_DELETE_CONFIRMATION' => 'Segur que voleu eliminar el registre?',
    'NTC_DELETE_CONFIRMATION_NUM' => 'Segur que voleu eliminar el(s) ',
    'NTC_UPDATE_CONFIRMATION_NUM' => 'Segur que voleu actualitzar el(s) ',
    'NTC_DELETE_SELECTED_RECORDS' => ' registre(s) seleccionat(s)?',
    'NTC_LOGIN_MESSAGE' => 'Introduïu el nom d\'usuari i la contrasenya:',
    'NTC_NO_ITEMS_DISPLAY' => 'Cap',
    'NTC_REMOVE_CONFIRMATION' => 'Segur que voleu esborrar aquesta relació? Només s\'esborrarà la relació. Les dades no s\'esborraran.',
    'NTC_REQUIRED' => 'Indica un camp requerit',
    'NTC_TIME_FORMAT' => '(24:00)',
    'NTC_WELCOME' => 'Benvingut',
    'NTC_YEAR_FORMAT' => '(aaaa)',
    'WARN_UNSAVED_CHANGES' => 'Està a punt d\'abandonar aquest registre sense desar els canvis que hagi pogut realitzar. Està segur que vol sortir d\'aquest registre?',
    'ERROR_NO_RECORD' => 'Error al recuperar el registre. Pot ser que estigui eliminat o que no esteu autoritzat a veure\'l.',
    'WARN_BROWSER_VERSION_WARNING' => '<b>Atenció:</b> La versió del navegador ja no és compatible o esteu utilitzant un navegador incompatible.<p></p>Es recomanen les següents versions de navegadors:<p></p><ul><li>Internet Explorer 10 (la vista de compatibilitat no és compatible)<li>Firefox 32.0<li>Safari 5.1<li>Chrome 37</ul>',
    'WARN_BROWSER_IE_COMPATIBILITY_MODE_WARNING' => '<b>Atenció:</b> El seu navegador està en mode vista compatible IE. Aquest mode no és compatible.',
    'ERROR_TYPE_NOT_VALID' => 'Error. Aquest tipus no es vàlid.',
    'ERROR_NO_BEAN' => 'obtenció del bean fallida',
    'LBL_DUP_MERGE' => 'Cerca duplicats',
    'LBL_MANAGE_SUBSCRIPTIONS' => 'Administra les subscripcions',
    'LBL_MANAGE_SUBSCRIPTIONS_FOR' => 'Administra les subscripcions a ',
    // Ajax status strings
    'LBL_LOADING' => 'Carregant...',
    'LBL_SEARCHING' => 'Cercant...',
    'LBL_SAVING_LAYOUT' => 'Desant el disseny...',
    'LBL_SAVED_LAYOUT' => "El disseny s'ha desat.",
    'LBL_SAVED' => 'Desat',
    'LBL_SAVING' => 'Desant',
    'LBL_DISPLAY_COLUMNS' => 'Mostra les columnes',
    'LBL_HIDE_COLUMNS' => 'Amaga les columnes',
    'LBL_SEARCH_CRITERIA' => 'Criteris de cerca',
    'LBL_SAVED_VIEWS' => 'Visualitzacions desades',
    'LBL_PROCESSING_REQUEST' => 'Processant...',
    'LBL_REQUEST_PROCESSED' => 'Fet',
    'LBL_AJAX_FAILURE' => 'Error d\'Ajax',
    'LBL_MERGE_DUPLICATES' => 'Combina els duplicats',
    'LBL_SAVED_FILTER_SHORTCUT' => 'Els meus filtres',
    'LBL_SEARCH_POPULATE_ONLY' => 'Realitzar una cerca utilitzant el formulari de cerca anterior',
    'LBL_DETAILVIEW' => 'Vista de detall',
    'LBL_LISTVIEW' => 'Vista de llista',
    'LBL_EDITVIEW' => 'Vista d\'edició',
    'LBL_BILLING_STREET' => 'Carrer:',
    'LBL_SHIPPING_STREET' => 'Carrer:',
    'LBL_SEARCHFORM' => 'Formulari de cerca',
    'LBL_SAVED_SEARCH_ERROR' => 'Introduïu un nom per a aquesta vista.',
    'LBL_DISPLAY_LOG' => 'Mostra el registre',
    'ERROR_JS_ALERT_SYSTEM_CLASS' => 'Sistema',
    'ERROR_JS_ALERT_TIMEOUT_TITLE' => 'Tancament de la Sessió',
    'ERROR_JS_ALERT_TIMEOUT_MSG_1' => 'La seva sessió expirarà en 2 minuts. Si us plau, guardi el seu treball.',
    'ERROR_JS_ALERT_TIMEOUT_MSG_2' => 'La seva sessió ha expirat.',
    'MSG_JS_ALERT_MTG_REMINDER_AGENDA' => "\nAgenda: ",
    'MSG_JS_ALERT_MTG_REMINDER_MEETING' => 'Reunió',
    'MSG_JS_ALERT_MTG_REMINDER_CALL' => 'Trucada',
    'MSG_JS_ALERT_MTG_REMINDER_TIME' => 'Hora: ',
    'MSG_JS_ALERT_MTG_REMINDER_LOC' => 'Lloc: ',
    'MSG_JS_ALERT_MTG_REMINDER_DESC' => 'Descripció: ',
    'MSG_JS_ALERT_MTG_REMINDER_STATUS' => 'Estat: ',
    'MSG_JS_ALERT_MTG_REMINDER_RELATED_TO' => 'Relacionat amb:',
    'MSG_JS_ALERT_MTG_REMINDER_CALL_MSG' => "\nCliqui Acceptar per veure aquesta trucada o cliqui Cancel·lar per tancar aquest missatge.",
    'MSG_JS_ALERT_MTG_REMINDER_MEETING_MSG' => "\nCliqui Acceptar per veure aquesta reunió o cliqui Cancel·lar per tancar aquest missatge.",
    'MSG_JS_ALERT_MTG_REMINDER_NO_EVENT_NAME' => 'Esdeveniment',
    'MSG_JS_ALERT_MTG_REMINDER_NO_DESCRIPTION' => 'L\'esdeveniment no està establert.',
    'MSG_JS_ALERT_MTG_REMINDER_NO_LOCATION' => 'La ubicació no està configurada.',
    'MSG_JS_ALERT_MTG_REMINDER_NO_START_DATE' => 'La data d\'inici no està definida.',
    'MSG_LIST_VIEW_NO_RESULTS_BASIC' => 'No s\'han trobat resultats.',
    'MSG_LIST_VIEW_NO_RESULTS_CHANGE_CRITERIA' => 'No hi ha resultats... Vol canviar els seus criteris de cerca i torna a intentar-ho?',
    'MSG_LIST_VIEW_NO_RESULTS' => 'No s\'han trobat resultats per <item1>',
    'MSG_LIST_VIEW_NO_RESULTS_SUBMSG' => 'Crear <item1> com a nou <item2>',
    'MSG_LIST_VIEW_CHANGE_SEARCH' => 'o canviar els seus criteris de cerca',
    'MSG_EMPTY_LIST_VIEW_NO_RESULTS' => 'Actualment no té registres desats. <item2> o <item3> ara.',

    'LBL_CLICK_HERE' => 'Cliqui aquí',
    // contextMenu strings
    'LBL_ADD_TO_FAVORITES' => 'Agfegir als Meus Favorits',
    'LBL_CREATE_CONTACT' => 'Nou Contacte',
    'LBL_CREATE_CASE' => 'Nou Cas',
    'LBL_CREATE_NOTE' => 'Nova Nota',
    'LBL_CREATE_OPPORTUNITY' => 'Nova Oportunitat',
    'LBL_SCHEDULE_CALL' => 'Programar Trucada',
    'LBL_SCHEDULE_MEETING' => 'Programar Reunió',
    'LBL_CREATE_TASK' => 'Nova Tasca',
    //web to lead
    'LBL_GENERATE_WEB_TO_LEAD_FORM' => 'Generar Formulari',
    'LBL_SAVE_WEB_TO_LEAD_FORM' => 'Desar formulari Web',
    'LBL_AVAILABLE_FIELDS' => 'Camps Disponibles',
    'LBL_FIRST_FORM_COLUMN' => 'Primera columna del formulari',
    'LBL_SECOND_FORM_COLUMN' => 'Segona columna del formulari',
    'LBL_ASSIGNED_TO_REQUIRED' => 'Falten camps obligatoris: Assignat a',
    'LBL_RELATED_CAMPAIGN_REQUIRED' => 'Falten camps obligatoris: campanya relacionada',
    'LBL_TYPE_OF_PERSON_FOR_FORM' => 'Formulari web per crear ',
    'LBL_TYPE_OF_PERSON_FOR_FORM_DESC' => 'Enviar per crear el formulari ',

    'LBL_ADD_ALL_LEAD_FIELDS' => 'Afegir Tots els Camps',
    'LBL_RESET_ALL_LEAD_FIELDS' => 'Restablir tots els camps',
    'LBL_REMOVE_ALL_LEAD_FIELDS' => 'Borrar Tots els Camps',
    'LBL_NEXT_BTN' => 'Següent',
    'LBL_ONLY_IMAGE_ATTACHMENT' => 'Només pot incloure\'s un adjunt de tipus imatge',
    'LBL_TRAINING' => 'Fòrum de suport',
    'ERR_MSSQL_DB_CONTEXT' => 'Canviat el context de base de dades a',
    'ERR_MSSQL_WARNING' => 'Advertència:',

    //Meta-Data framework
    'ERR_CANNOT_CREATE_METADATA_FILE' => 'Error: No existeix l\'arxiu [[file]].  No s\'ha pogut crear perquè l\'arxiu amb el HTML corresponent no ha estat trobat.',
    'ERR_CANNOT_FIND_MODULE' => 'Error: El mòdul [module] no existeix.',
    'LBL_ALT_ADDRESS' => 'Una altra adreça:',
    'ERR_SMARTY_UNEQUAL_RELATED_FIELD_PARAMETERS' => 'Error: Hi ha un nombre d\'arguments desigual per als elements \'key\' i \'copy\' en el array displayParams.',

    /* MySugar Framework (for Home and Dashboard) */
    'LBL_DASHLET_CONFIGURE_GENERAL' => 'General',
    'LBL_DASHLET_CONFIGURE_FILTERS' => 'Filtres',
    'LBL_DASHLET_CONFIGURE_MY_ITEMS_ONLY' => 'Només els Meus Elements',
    'LBL_DASHLET_CONFIGURE_TITLE' => 'Títol',
    'LBL_DASHLET_CONFIGURE_DISPLAY_ROWS' => 'Mostrar Files',

    // MySugar status strings
    'LBL_MAX_DASHLETS_REACHED' => 'Ha assolit el nombre màxim de Dashlets de SuiteCRM establert pel seu administrador. Si us plau, elimini un Dashlet de SuiteCRM per a poder afegir-ne més.',
    'LBL_ADDING_DASHLET' => 'Afegint Dashlet de SuiteCRM...',
    'LBL_ADDED_DASHLET' => 'Dashlet de SuiteCRM afegit',
    'LBL_REMOVE_DASHLET_CONFIRM' => 'Està segur que vol eliminar el Dashlet de SuiteCRM?',
    'LBL_REMOVING_DASHLET' => 'Eliminant Dashlet de SuiteCRM...',
    'LBL_REMOVED_DASHLET' => 'Dashlet de SuiteCRM eliminat',
    'LBL_MAX_DASHLET_COLUMNS' => "Es poden mostrar un màxim de %s columnes.",
    'LBL_RETRIEVING_XML_DATA' => "Hi ha hagut un problema en recuperar les dades XML:\n",

    // MySugar Menu Options

    'LBL_LOADING_PAGE' => 'Carregant pàgina, esperi si us plau...',

    'LBL_RELOAD_PAGE' => 'Si us plau, <a href="javascript: window.location.reload()">refresqui la pàgina</a> per a utilitzar aquest Dashlet de SuiteCRM.',
    'LBL_ADD_DASHLETS' => 'Afegir Dashlets',
    'LBL_CLOSE_DASHLETS' => 'Tancar',
    'LBL_OPTIONS' => 'Opcions',
    'LBL_1_COLUMN' => '1 Columna',
    'LBL_2_COLUMN' => '2 Columnes',
    'LBL_3_COLUMN' => '3 Columnes',
    'LBL_PAGE_NAME' => 'Nom de pàgina',

    'LBL_SEARCH_RESULTS' => 'Resultats de la cerca',
    'LBL_SEARCH_MODULES' => 'Mòduls',
    'LBL_SEARCH_TOOLS' => 'Eines',
    'LBL_SEARCH_HELP_TITLE' => 'Treballant amb Seleccions múltiples i Recerques Guardades',
    /* End MySugar Framework strings */

    'LBL_NO_IMAGE' => 'Sense Imatge',

    //adding a label for address copy from left
    'LBL_COPY_ADDRESS_FROM_LEFT' => 'Copiar adreça de l\'esquerra:',
    'LBL_SAVE_AND_CONTINUE' => 'Desa i continua',

    'LBL_SEARCH_HELP_TEXT' => '<p><br /><strong>Controls Multiselecció</strong></p><ul><li>Faci clic en els valors per seleccionar un atribut.</li><li>Ctrl-clic&nbsp;per a&nbsp;seleccionar múltiples. Els usuaris de Mac fan servir CMD-clic.</li><li>Per seleccionar tots els valors entre dos atributs,&nbsp; faci clic en el primer valor&nbsp;i després shift-clic en l\'últim.</li></ul><p><strong>Recerca Avançada i Opcions de Presentació</strong><br><br>Usant l\'opció <b>Recerques Guardades i Presentació</b>, pot guardar un conujunt de criteris de recerca i/o un diseny de Vista en Llista per ràpidament obtenir els resultats de recerca desitjats en un futur. Pot guardar un número il·limitat de recerques i disenys. Totes les recerques guardades apareixen amb el seu nom en la llista de Recerques Guardades, amb l\'última recerca guardada en la part superior de la llista.<br><br>Per personalitzar el disseny de Vista en Llista, utilitzi els quadres d\'Ocultar Columnes i Mostrar Columnes per seleccionar els camps a mostrar en els resultats de la cerca. Per exemple, pot veure o ocultar detalls com el nom del registre, l\'usuari assignat i l\'equip assignat en els resultats de la cerca. Per agregar una columna a la Vista en Llista, seleccioni el camp de la llista Ocultar Columnes i utilitzi la fletxa esquerra per moure\'l a la llista de Mostrar Columnes. Per eliminar una columna de la Vista en Llista, la seleccioni de la llista Mostrar Columnes i utilitzi la fletxa dreta per moure-la a la llista Ocultar Columnes.<br><br>Si guarda la configuració de disseny, podrà carregar-la en qualsevol moment per veure els resultats de la cerca amb la presentació personalitzada.<br><br>Per guardar i actualitzar una recerca i/o disseny:<ol><li>Introdueixi un nom per als resultats de recerca en el camp <b>Guardar aquesta recerca com a</b> i faci clic a <b>Guardar</b>.Després d\'això, el nom es mostrarà en la llista de Recerques Guardades al costat del botó <b>Netejar</b>.</li><li>Per veure una recerca guardada, seleccioni-la de la llista de Recerques Guardades. Els resultats de recerca es mostraran en la Llista de Vista.</li><li>Per actualitzar les propietats d\'una recerca guardada, seleccioni la recerca guardada de la llista, introdueixi els nous criteris de recerca i/o les opcions de disseny en l\'àrea de Recerca Avançada, i faci clic a<b>Actualitzar</b> al costat de <b>Modificar Recerca Actual</b>.</li><li>Per esborrar una recerca guardada, seleccioni-la en la llista de Recerques Guardades, faci clic a <b>Esborrar</b> al costat de <b>Modificar Recerca Actual</b>, i faci clic a <b>Acceptar</b> per confirmar l\'esborrament.</li></ol><p><strong>Consells</strong><br><br>Utilitzant % a com a comodí pot fer la seva cerca més àmplia. Per exemple, en comptes de cercar únicament resultats iguals a "Pomes", pot canviar la cerca per "Pomes%" i es retornaran tots els resultats que començen per Pomes però contenen altres caràcters.</p>',

    //resource management
    'ERR_QUERY_LIMIT' => 'Error: S\'ha assolit el límit $limit de consultes per el mòdul $module.',
    'ERROR_NOTIFY_OVERRIDE' => 'Error: Es te que sobreescribir ResourceObserver->notify().',

    //tracker labels
    'ERR_MONITOR_FILE_MISSING' => 'Error: No s\'ha pogut crear un monitor perquè l\'arxiu de metadades està buit o no existeix.',
    'ERR_MONITOR_NOT_CONFIGURED' => 'Error: No hi ha cap monitor configurat per el nom solicitat',
    'ERR_UNDEFINED_METRIC' => 'Error: No s\'ha pogut establir un valor per una mètrica no definida',
    'ERR_STORE_FILE_MISSING' => 'Error: No s\'ha pogut trobar l\'arxiu d\'implementació d\'Emmagatzemament',

    'LBL_MONITOR_ID' => 'Id del Monitor',
    'LBL_USER_ID' => "Id de l'Usuari",
    'LBL_MODULE' => 'Mòdul',
    'LBL_ITEM_ID' => "Id de l'Element",
    'LBL_ITEM_SUMMARY' => "Resum de l'Element",
    'LBL_ACTION' => 'Acció',
    'LBL_SESSION_ID' => 'Id de la Sessió',
    'LBL_BREADCRUMBSTACK_CREATED' => 'Pluja d\'idees creada per l\'usuari {0}',
    'LBL_VISIBLE' => 'Registre visible',
    'LBL_DATE_LAST_ACTION' => 'Data',

    //jc:#12287 - For javascript validation messages
    'MSG_IS_NOT_BEFORE' => 'no és abans de',
    'MSG_IS_MORE_THAN' => 'és més que',
    'MSG_IS_LESS_THAN' => 'és menys que',
    'MSG_SHOULD_BE' => 'hauria de ser',
    'MSG_OR_GREATER' => 'o més gran',

    'LBL_LIST' => 'Llista',
    'LBL_CREATE_BUG' => 'Nova Incidència',

    'LBL_OBJECT_IMAGE' => 'imatge d\'objecte',
    //jchi #12300
    'LBL_MASSUPDATE_DATE' => 'Seleccioni Data',

    'LBL_VALUE' => 'valor',
    'LBL_VALIDATE_RANGE' => 'no està dins del rang vàlid',
    'LBL_CHOOSE_START_AND_END_DATES' => "Si us plau, triï una data d\'inici i de finalització", // Excepció d'escapat 
    'LBL_CHOOSE_START_AND_END_ENTRIES' => "Si us plau, triï un període d\'inici i de finalització", // Excepció d'escapat 

    //jchi #  20776
    'LBL_DROPDOWN_LIST_ALL' => 'Tot',

    //Connector
    'ERR_CONNECTOR_FILL_BEANS_SIZE_MISMATCH' => 'Error: El número d\'Arrays del paràmetre bean no coincideix amb el número d\'Arrays dels resultats.',
    'ERR_MISSING_MAPPING_ENTRY_FORM_MODULE' => 'Error: Falta l\'entrada de mapeig per el mòdul.',
    'ERROR_UNABLE_TO_RETRIEVE_DATA' => 'Error: No ha estat possible obtenir les dades per el connector.',

    // fastcgi checks
    'LBL_FASTCGI_LOGGING' => 'Per a una millor experiència amb ISS/FastCGI sapi, configuri el valor de fastcgi.logging a 0 al fitxer php.ini',

    //Collection Field
    'LBL_COLLECTION_NAME' => 'Nom',
    'LBL_COLLECTION_PRIMARY' => 'Principal',
    'ERROR_MISSING_COLLECTION_SELECTION' => 'Camp obligatori buit',

    //MB -Fixed Bug #32812 -Max
    'LBL_ASSIGNED_TO_NAME' => 'Assignat a',
    'LBL_DESCRIPTION' => 'Descripció',

    'LBL_YESTERDAY' => 'ahir',
    'LBL_NOW' => 'ara',
    'LBL_TODAY' => 'avui',
    'LBL_TOMORROW' => 'demà',
    'LBL_NEXT_WEEK' => 'la setmana vinent',
    'LBL_NEXT_MONDAY' => 'el dilluns vinent',
    'LBL_NEXT_FRIDAY' => 'el divendres vinent',
    'LBL_TWO_WEEKS' => "d'aquí a dues setmanes",
    'LBL_NEXT_MONTH' => 'el mes vinent',
    'LBL_FIRST_DAY_OF_NEXT_MONTH' => 'el primer dia del mes vinent',
    'LBL_THREE_MONTHS' => "d'aquí a tres mesos",
    'LBL_SIXMONTHS' => "d'aquí a sis mesos",
    'LBL_NEXT_YEAR' => "l'any vinent",

    //Datetimecombo fields
    'LBL_HOURS' => 'Hores',
    'LBL_MINUTES' => 'Minuts',
    'LBL_MERIDIEM' => 'Meridiem',
    'LBL_DATE' => 'Data',
    'LBL_DASHLET_CONFIGURE_AUTOREFRESH' => 'Autorefrescar',

    'LBL_DURATION_DAY' => 'dia',
    'LBL_DURATION_HOUR' => 'hora',
    'LBL_DURATION_MINUTE' => 'minut',
    'LBL_DURATION_DAYS' => 'dies',
    'LBL_DURATION_HOURS' => 'Hores de durada',
    'LBL_DURATION_MINUTES' => 'Minuts de durada',

    //Calendar widget labels
    'LBL_CHOOSE_MONTH' => 'Trieu el mes',
    'LBL_ENTER_YEAR' => "Trieu l'any",
    'LBL_ENTER_VALID_YEAR' => 'Indiqueu un any vàlid',

    //File write error label
    'ERR_FILE_WRITE' => 'Error: No es pot escriure el fitxer {0}. Comproveu els permisos del sistema i del servidor web.',
    'ERR_FILE_NOT_FOUND' => 'Error: No es pot llegir el fitxer {0}. Comproveu els permisos del sistema i del servidor web.',

    'LBL_AND' => 'I',

    // File fields
    'LBL_SEARCH_EXTERNAL_API' => 'Arxiu de font externa',
    'LBL_EXTERNAL_SECURITY_LEVEL' => 'Seguretat',

    //IMPORT SAMPLE TEXT
    'LBL_IMPORT_SAMPLE_FILE_TEXT' => '
"Aquest és un fitxer d\'importació d\'exemple que proporciona un exemple dels continguts que s\'esperava d\'un arxiu que està a punt per a la importació". "L\'arxiu és un fitxer CSV delimitat per comes, utilitzant cometes com el qualificador de camp". "La fila de capçalera és la fila més superior a l\'arxiu i conté les etiquetes de camp com vols veure-los en l\'aplicació". "Aquestes etiquetes s\'utilitzen per assignar les dades a l\'arxiu als camps en l\'aplicació." "Notes: els noms de la base de dades també podria ser utilitzat en la fila de capçalera. Això és útil quan està utilitzant phpMyAdmin o una altra eina de base de dades per proporcionar una llista de dades per importar exportat." "L\'ordre de columna no és crítica en el procés d\'importació coincideix amb les dades en els camps apropiats basats en la fila de capçalera". "Per utilitzar aquest fitxer com a plantilla, feu el següent:" "1. eliminar les files Mostra de dades" "2. Treure el text d\'ajuda que estàs llegint ara mateix""3. Aportació de les seves dades en el corresponents files i columnes""4. Salvi l\'arxiu a un lloc conegut en el seu sistema""5. Feu clic a l\'opció importa del menú accions en l\'aplicació i seleccioneu el fitxer per carregar"   ',
    //define labels to be used for overriding local values during import/export

    'LBL_NOTIFICATIONS_NONE' => 'No hi ha notificacions actuals',
    'LBL_ALT_SORT_DESC' => 'Ordena descendentment',
    'LBL_ALT_SORT_ASC' => 'Ordena ascendentment',
    'LBL_ALT_SORT' => 'Ordenar',
    'LBL_ALT_SHOW_OPTIONS' => 'Mostra les opcions',
    'LBL_ALT_HIDE_OPTIONS' => 'Oculta les opcions',
    'LBL_ALT_MOVE_COLUMN_LEFT' => 'Mou l\'entrada seleccionada a la llista de l\'esquerra',
    'LBL_ALT_MOVE_COLUMN_RIGHT' => 'Mou l\'entrada seleccionada a la llista de la dreta',
    'LBL_ALT_MOVE_COLUMN_UP' => 'Moure selecció cap amunt en l\'ordre de la llista',
    'LBL_ALT_MOVE_COLUMN_DOWN' => 'Moure selecció cap avall en l\'ordre de la llista',
    'LBL_ALT_INFO' => 'Informació',
    'MSG_DUPLICATE' => 'El registre {0} que està a punt de crear pot ser un duplicat d\'un registre {0} que ja existeix. Els {1} registres que contenen noms similars es mostren a continuació.<br />Faci clic a Crear {1} per a continuar amb la creació d\'aquest nou {0}, o seleccioni un {0} existent de la següent llista.',
    'MSG_SHOW_DUPLICATES' => 'El registre {0} que està a punt de crear pot ser un duplicat d\'un registre {0} que ja existeix. Els {1} registres que contenen noms similars es mostren a continuació. Faci clic a Desar per a continuar amb la creació d\'aquest nou {0}, o faci clic a Cancel·lar per a tornar al mòdul sense crear {0}.',
    'LBL_EMAIL_TITLE' => 'adreça de correu electrònic',
    'LBL_EMAIL_OPT_TITLE' => 'adreça de correu electrònic rebutjada',
    'LBL_EMAIL_INV_TITLE' => 'adreça de correu electrònic invàlida',
    'LBL_EMAIL_PRIM_TITLE' => 'Fer l\'adreça electrònica principal',
    'LBL_SELECT_ALL_TITLE' => 'Seleccionar tot',
    'LBL_SELECT_THIS_ROW_TITLE' => 'Seleccionar aquesta fila',

    //for upload errors
    'UPLOAD_ERROR_TEXT' => 'ERROR: S\'ha produït un error durant la pujada. Codi d\'error: {0} - {1}',
    'UPLOAD_ERROR_TEXT_SIZEINFO' => 'ERROR: S\'ha produït un error durant la pujada. Codi d\'error: {0} - {1}. La mida màxima de pujada és {2}',
    'UPLOAD_ERROR_HOME_TEXT' => 'ERROR: S\'ha produït un error durant la pujada, si us plau, contacti amb el seu administrador per obtenir ajuda.',
    'UPLOAD_MAXIMUM_EXCEEDED' => 'La mida de la pujada ({0} bytes) excedeix la mida màxima permesa: {1} bytes',
    'UPLOAD_REQUEST_ERROR' => 'S\'ha produït un error. Si us plau, recargui la pàgina i torni-ho a provar.',

    //508 used Access Keys
    'LBL_EDIT_BUTTON_KEY' => 'i',
    'LBL_EDIT_BUTTON_LABEL' => 'Editar',
    'LBL_EDIT_BUTTON_TITLE' => 'Editar',
    'LBL_DUPLICATE_BUTTON_KEY' => 'u',
    'LBL_DUPLICATE_BUTTON_LABEL' => 'Duplicar',
    'LBL_DUPLICATE_BUTTON_TITLE' => 'Duplicar',
    'LBL_DELETE_BUTTON_KEY' => 'd',
    'LBL_DELETE_BUTTON_LABEL' => 'Esborrar',
    'LBL_DELETE_BUTTON_TITLE' => 'Eliminar',
    'LBL_BULK_ACTION_BUTTON_LABEL' => 'Acció en Bloc', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_BULK_ACTION_BUTTON_LABEL_MOBILE' => 'Acció', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_SAVE_BUTTON_KEY' => 'S',
    'LBL_SAVE_BUTTON_LABEL' => 'Desar',
    'LBL_SAVE_BUTTON_TITLE' => 'Desar',
    'LBL_CANCEL_BUTTON_KEY' => 'X',
    'LBL_CANCEL_BUTTON_LABEL' => 'Cancel·lar',
    'LBL_CANCEL_BUTTON_TITLE' => 'Cancel·lar',
    'LBL_FIRST_INPUT_EDIT_VIEW_KEY' => '7',
    'LBL_ADV_SEARCH_LNK_KEY' => '8',
    'LBL_FIRST_INPUT_SEARCH_KEY' => '9',

    'ERR_CONNECTOR_NOT_ARRAY' => 'la matriu de connector a {0} està definida incorrectament o és buida i no podria ser utilitzada.',
    'ERR_SUHOSIN' => 'El flux de pujada està bloquejat per Suhosin, si us plau, afegeixi &quot;upload&quot; a suhosin.executor.include.whitelist (Veure el registre suitecrm.log per a més informació)',
    'ERR_BAD_RESPONSE_FROM_SERVER' => 'Resposta incorrecta del servidor',
    'LBL_ACCOUNT_PRODUCT_QUOTE_LINK' => 'Pressupost',
    'LBL_ACCOUNT_PRODUCT_SALE_PRICE' => 'Preu de venda',
    'LBL_EMAIL_CHECK_INTERVAL_DOM' => array(
        '-1' => 'Manualment',
        '5' => 'Cada 5 minuts',
        '15' => 'Cada 15 minuts',
        '30' => 'Cada 30 minuts',
        '60' => 'Cada hora',
    ),

    'ERR_A_REMINDER_IS_EMPTY_OR_INCORRECT' => 'Un recordatori és buit o incorrecte.',
    'ERR_REMINDER_IS_NOT_SET_POPUP_OR_EMAIL' => 'Recordatori no està definit per un desplegable o correu electrònic.',
    'ERR_NO_INVITEES_FOR_REMINDER' => 'No hi ha convidats per al recordatori.',
    'LBL_DELETE_REMINDER_CONFIRM' => 'El recordatori no inclou algun dels convidats, voleu treure el recordatori?',
    'LBL_DELETE_REMINDER' => 'Suprimir recordatori',
    'LBL_OK' => 'Ok',

    'LBL_COLUMNS_FILTER_HEADER_TITLE' => 'Trieu les columnes',
    'LBL_COLUMN_CHOOSER' => 'Selector de columnes',
    'LBL_SAVE_CHANGES_BUTTON_TITLE' => 'Desa els canvis',
    'LBL_DISPLAYED' => 'Visualització',
    'LBL_HIDDEN' => 'Ocult',
    'ERR_EMPTY_COLUMNS_LIST' => 'Almenys, un element necessari',

    'LBL_FILTER_HEADER_TITLE' => 'Filtre',

    'LBL_CATEGORY' => 'Categoría',
    'LBL_LIST_CATEGORY' => 'Categoría',
    'ERR_FACTOR_TPL_INVALID' => 'Missatge de factor d\'autenticació no és vàlid, contacteu amb l\'administrador.',
    'LBL_SUBTHEMES' => 'Estil',
    'LBL_SUBTHEME_OPTIONS_DAWN' => 'Alba',
    'LBL_SUBTHEME_OPTIONS_DAY' => 'Dia',
    'LBL_SUBTHEME_OPTIONS_DUSK' => 'Capvespre',
    'LBL_SUBTHEME_OPTIONS_NIGHT' => 'Nit',
    'LBL_SUBTHEME_OPTIONS_NOON' => 'Migdia', 

    'LBL_CONFIRM_DISREGARD_DRAFT_TITLE' => 'Descarta l\'esborrany',
    'LBL_CONFIRM_DISREGARD_DRAFT_BODY' => 'Amb aquesta operació se suprimirà aquest missatge, voleu continuar?',
    'LBL_CONFIRM_DISREGARD_EMAIL_TITLE' => 'Sortir del diàleg del compositor',
    'LBL_CONFIRM_DISREGARD_EMAIL_BODY' => 'Sortint del diàleg del compositor tota l\'informació entrada es perdrà, segur que vols continuar sortint?',
    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE' => 'Aplicar una plantilla de missatge',
    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_BODY' => 'Aquesta operació esborrarà el camp cos del missatge, ¿vol continuar?',

    'LBL_CONFIRM_OPT_IN_TITLE' => 'Confirmat Autoritzat a enviar',
    'LBL_OPT_IN_TITLE' => 'Autoritzat a enviar',
    'LBL_CONFIRM_OPT_IN_DATE' => 'Confirmat Autoritzat a enviar en data',
    'LBL_CONFIRM_OPT_IN_SENT_DATE' => 'Confirmat Autoritzat a enviar en data d\'enviament',
    'LBL_CONFIRM_OPT_IN_FAIL_DATE' => 'Confirmat Autoritzat per enviar en data errònia',
    'LBL_CONFIRM_OPT_IN_TOKEN' => 'Confirmar Autoritzat a enviar al Token',
    'ERR_OPT_IN_TPL_NOT_SET' => 'La plantilla d\'email per Autoritzat a enviar no està configurada. Si us plau configureu a la configuració del correu electrònic.',
    'ERR_OPT_IN_RELATION_INCORRECT' => 'Autoritzar a enviar requereix l\'e-mail a compte/contacte/Potencial/Objectiu',

    'LBL_SECURITYGROUP_NONINHERITABLE' => 'Grup no heretable',
    'LBL_PRIMARY_GROUP' => "Grup principal",

    // footer
    'LBL_SUITE_TOP' => 'Torna a l\'inici',
    'LBL_SUITE_SUPERCHARGED' => 'Sobrealimentat per SuiteCRM',
    'LBL_SUITE_POWERED_BY' => 'Desenvolupat per SugarCRM',
    'LBL_SUITE_DESC1' => 'SuiteCRM ha estat desenvolupat per <a href="https://salesagility.com">SalesAgility</a>. El programari es proporciona TAL COM ÉS, sense cap garantia. Sota llicència AGPLv3.',
    'LBL_SUITE_DESC2' => 'Aquest programa és programari lliure. Podeu redistribuir-lo i/o modificar-lo segons els termes de la Llicència Pública General Affero de GNU versió 3, publicada per la Free Software Foundation, incloent-hi qualsevol permís addicional indicat a la capçalera del codi font.',
    'LBL_SUITE_DESC3' => 'SuiteCRM és una marca registrada de SalesAgility Ltd. Tots els noms d\'altres empreses i productes poden ser marques registrades de les respectives empreses amb les quals s\'associen.',
    'LBL_GENERATE_PASSWORD_BUTTON_TITLE' => 'Restableix la contrasenya',
    'LBL_SEND_CONFIRM_OPT_IN_EMAIL' => 'Enviar confirmació Autoritzat a enviar a l\'E-mail',
    'LBL_CONFIRM_OPT_IN_ONLY_FOR_PERSON' => 'Confirmar Autoritzat a enviar per enviament de correu electrònic només per a comptes/contactes/Potencials/Prospectes',
    'LBL_CONFIRM_OPT_IN_IS_DISABLED' => 'Confirmar Autoritzat a enviar per enviament de correu electrònic està inhabilitada, permetre confirmar Autoritzat a enviar opció en configuració d\'E-mail o contactar amb l\'administrador.',
    'LBL_CONTACT_HAS_NO_PRIMARY_EMAIL' => 'Confirmar l\'enviament de Autoritzat a enviar a E-mail no és possible perquè el contacte ha adreça E-mail no principal',
    'LBL_CONFIRM_EMAIL_SENDING_FAILED' => 'Confirmar Autoritzat a enviar E-mail d\'enviament ha fallat',
    'LBL_CONFIRM_EMAIL_SENT' => 'Confirmació d\'Autoritzat a enviar l\'E-mail enviat amb èxit',
);

$app_list_strings['moduleList']['Library'] = 'Biblioteca';
$app_list_strings['moduleList']['EmailAddresses'] = 'Adreces de correu electrònic';
$app_list_strings['project_priority_default'] = 'Mitja';
$app_list_strings['project_priority_options'] = array(
    'High' => 'Alta',
    'Medium' => 'Mitja',
    'Low' => 'Baixa',
);

//GDPR lawful basis options
$app_list_strings['lawful_basis_dom'] = array(
    '' => '',
    'consent' => 'Consentiment',
    'contract' => 'Contracte',
    'legal_obligation' => 'Obligació legal',
    'protection_of_interest' => 'Protecció d\'interès',
    'public_interest' => 'Interès públic',
    'legitimate_interest' => 'Interès legítim',
    'withdrawn' => 'Retirat',
);
//End GDPR lawful basis options

//GDPR lawful basis source options
$app_list_strings['lawful_basis_source_dom'] = array(
    '' => '',
    'website' => 'Lloc Web',
    'phone' => 'Telèfon',
    'given_to_user' => 'Donat a l\'usuari',
    'email' => 'Correu electrònic',
    'third_party' => 'Tercers',
);
//End GDPR lawful basis source options

$app_list_strings['moduleList']['KBDocuments'] = 'Base de Coneixement';

$app_list_strings['countries_dom'] = array(
    '' => '',
    'ABU DHABI' => 'ABU DHABI',
    'ADEN' => 'ADEN',
    'AFGHANISTAN' => 'AFGANISTAN',
    'ALBANIA' => 'ALBANIA',
    'ALGERIA' => 'ALGERIA',
    'AMERICAN SAMOA' => 'AMERICAN SAMOA',
    'ANDORRA' => 'ANDORRA',
    'ANGOLA' => 'ANGOLA',
    'ANTARCTICA' => 'ANTARCTICA',
    'ANTIGUA' => 'ANTIGUA',
    'ARGENTINA' => 'ARGENTINA',
    'ARMENIA' => 'ARMENIA',
    'ARUBA' => 'ARUBA',
    'AUSTRALIA' => 'AUSTRALIA',
    'AUSTRIA' => 'AUSTRIA',
    'AZERBAIJAN' => 'AZERBAIJAN',
    'BAHAMAS' => 'BAHAMAS',
    'BAHRAIN' => 'BAHRAIN',
    'BANGLADESH' => 'BANGLADESH',
    'BARBADOS' => 'BARBADOS',
    'BELARUS' => 'BELARUS',
    'BELGIUM' => 'BELGIUM',
    'BELIZE' => 'BELIZE',
    'BENIN' => 'BENIN',
    'BERMUDA' => 'BERMUDA',
    'BHUTAN' => 'BHUTAN',
    'BOLIVIA' => 'BOLIVIA',
    'BOSNIA' => 'BOSNIA',
    'BOTSWANA' => 'BOTSWANA',
    'BOUVET ISLAND' => 'BOUVET ISLAND',
    'BRAZIL' => 'BRAZIL',
    'BRITISH ANTARCTICA TERRITORY' => 'BRITISH ANTARCTICA TERRITORY',
    'BRITISH INDIAN OCEAN TERRITORY' => 'BRITISH INDIAN OCEAN TERRITORY',
    'BRITISH VIRGIN ISLANDS' => 'ILLES VERGES BRITÀNIQUES',
    'BRITISH WEST INDIES' => 'ÍNDIES BRITÀNIQUES',
    'BRUNEI' => 'BRUNEI',
    'BULGARIA' => 'BULGÀRIA',
    'BURKINA FASO' => 'BURKINA FASO',
    'BURUNDI' => 'BURUNDI',
    'CAMBODIA' => 'CAMBODJA',
    'CAMEROON' => 'CAMERUN',
    'CANADA' => 'CANADÀ',
    'CANAL ZONE' => 'ZONA DEL CANAL',
    'CANARY ISLAND' => 'ILLES CANÀRIES',
    'CAPE VERDI ISLANDS' => 'ILLES CAPE VERDI',
    'CAYMAN ISLANDS' => 'ILLES CAIMAN',
    'CHAD' => 'TXAD',
    'CHANNEL ISLAND UK' => 'ILLA DEL CANAL UK',
    'CHILE' => 'XILE',
    'CHINA' => 'XINA',
    'CHRISTMAS ISLAND' => 'ILLA CHRISTMAS',
    'COCOS (KEELING) ISLAND' => 'ILLA DEL COCO (VATICÀ)',
    'COLOMBIA' => 'COLÒMBIA',
    'COMORO ISLANDS' => 'ILLES COMORES',
    'CONGO' => 'CONGO',
    'CONGO KINSHASA' => 'CONGO KINSHASA',
    'COOK ISLANDS' => 'ILLES COOK',
    'COSTA RICA' => 'COSTA RICA',
    'CROATIA' => 'CROÀCIA',
    'CUBA' => 'CUBA',
    'CURACAO' => 'CURAÇAO',
    'CYPRUS' => 'XIPRE',
    'CZECH REPUBLIC' => 'REPÚBLICA TXECA',
    'DAHOMEY' => 'DAHOMEY',
    'DENMARK' => 'DINAMARCA',
    'DJIBOUTI' => 'DJIBOUTI',
    'DOMINICA' => 'DOMINICA',
    'DOMINICAN REPUBLIC' => 'REPÚBLICA DOMINICANA',
    'DUBAI' => 'DUBAI',
    'ECUADOR' => 'L\'EQUADOR',
    'EGYPT' => 'EGYPT',
    'EL SALVADOR' => 'EL SALVADOR',
    'EQUATORIAL GUINEA' => 'EQUATORIAL GUINEA',
    'ESTONIA' => 'ESTONIA',
    'ETHIOPIA' => 'ETHIOPIA',
    'FAEROE ISLANDS' => 'FAEROE ISLANDS',
    'FALKLAND ISLANDS' => 'FALKLAND ISLANDS',
    'FIJI' => 'FIJI',
    'FINLAND' => 'FINLAND',
    'FRANCE' => 'FRANCE',
    'FRENCH GUIANA' => 'FRANCE',
    'FRENCH POLYNESIA' => 'FRENCH POLYNESIA',
    'GABON' => 'GABON',
    'GAMBIA' => 'GAMBIA',
    'GEORGIA' => 'GEORGIA',
    'GERMANY' => 'GERMANY',
    'GHANA' => 'GHANA',
    'GIBRALTAR' => 'GIBRALTAR',
    'GREECE' => 'GIBRALTAR',
    'GREENLAND' => 'GREENLAND',
    'GUADELOUPE' => 'GUADELOUPE',
    'GUAM' => 'GUAM',
    'GUATEMALA' => 'GUAM',
    'GUINEA' => 'GUINEA',
    'GUYANA' => 'GUYANA',
    'HAITI' => 'GUYANA',
    'HONDURAS' => 'HONDURAS',
    'HONG KONG' => 'HONDURAS',
    'HUNGARY' => 'HONG KONG',
    'ICELAND' => 'HUNGARY',
    'IFNI' => 'ICELAND',
    'INDIA' => 'INDIA',
    'INDONESIA' => 'INDIA',
    'IRAN' => 'INDONESIA',
    'IRAQ' => 'IRAN',
    'IRELAND' => 'IRAQ',
    'ISRAEL' => 'IRELAND',
    'ITALY' => 'ISRAEL',
    'IVORY COAST' => 'IVORY COAST',
    'JAMAICA' => 'JAMAICA',
    'JAPAN' => 'JAMAICA',
    'JORDAN' => 'JORDAN',
    'KAZAKHSTAN' => 'JORDAN',
    'KENYA' => 'KAZAKHSTAN',
    'KOREA' => 'KOREA',
    'KOREA, SOUTH' => 'KOREA',
    'KUWAIT' => 'KUWAIT',
    'KYRGYZSTAN' => 'KYRGYZSTAN',
    'LAOS' => 'LAOS',
    'LATVIA' => 'LATVIA',
    'LEBANON' => 'LÍBAN',
    'LEEWARD ISLANDS' => 'ILLES LEEWARD',
    'LESOTHO' => 'LESOTHO',
    'LIBYA' => 'LÍBIA',
    'LIECHTENSTEIN' => 'LIECHTENSTEIN',
    'LITHUANIA' => 'LITUÀNIA',
    'LUXEMBOURG' => 'LUXEMBURG',
    'MACAO' => 'MACAU',
    'MACEDONIA' => 'MACEDÒNIA',
    'MADAGASCAR' => 'MADAGASCAR',
    'MALAWI' => 'MALAWI',
    'MALAYSIA' => 'MALÀISIA',
    'MALDIVES' => 'MALDIVES',
    'MALI' => 'MALI',
    'MALTA' => 'MALTA',
    'MARTINIQUE' => 'MARTINICA',
    'MAURITANIA' => 'MAURITÀNIA',
    'MAURITIUS' => 'MAURICI',
    'MELANESIA' => 'MELANÈSIA',
    'MEXICO' => 'MÈXIC',
    'MOLDOVIA' => 'MOLDÀVIA',
    'MONACO' => 'MÒNACO',
    'MONGOLIA' => 'MONGÒLIA',
    'MOROCCO' => 'MARROC',
    'MOZAMBIQUE' => 'MOÇAMBIC',
    'MYANAMAR' => 'MYANAMAR',
    'NAMIBIA' => 'NAMÍBIA',
    'NEPAL' => 'NEPAL',
    'NETHERLANDS' => 'PAÏSOS BAIXOS',
    'NETHERLANDS ANTILLES' => 'ANTILLES HOLANDESES',
    'NETHERLANDS ANTILLES NEUTRAL ZONE' => 'PAÏSOS BAIXOS ANTILLES NEUTRAL ZONE',
    'NEW CALADONIA' => 'CALADONIA NOU',
    'NEW HEBRIDES' => 'NOVES HÈBRIDES',
    'NEW ZEALAND' => 'NOVA ZELANDA',
    'NICARAGUA' => 'NICARAGUA',
    'NIGER' => 'NÍGER',
    'NIGERIA' => 'NIGÈRIA',
    'NORFOLK ISLAND' => 'NORFOLK ISLAND',
    'NORWAY' => 'NORUEGA',
    'OMAN' => 'OMAN',
    'OTHER' => 'ALTRES',
    'PACIFIC ISLAND' => 'ILLA DEL PACÍFIC',
    'PAKISTAN' => 'PAKISTAN',
    'PANAMA' => 'PANAMÀ',
    'PAPUA NEW GUINEA' => 'PAPUA NOVA GUINEA',
    'PARAGUAY' => 'PARAGUAI',
    'PERU' => 'PERÚ',
    'PHILIPPINES' => 'FILIPINES',
    'POLAND' => 'POLÒNIA',
    'PORTUGAL' => 'PORTUGAL',
    'PORTUGUESE TIMOR' => 'TIMOR ORIENTAL',
    'PUERTO RICO' => 'PUERTO RICO',
    'QATAR' => 'QATAR',
    'REPUBLIC OF BELARUS' => 'REPÚBLICA DE BIELORÚSSIA',
    'REPUBLIC OF SOUTH AFRICA' => 'REPÚBLICA DE SUD-ÀFRICA',
    'REUNION' => 'REUNIÓ',
    'ROMANIA' => 'ROMANIA',
    'RUSSIA' => 'RÚSSIA',
    'RWANDA' => 'RWANDA',
    'RYUKYU ISLANDS' => 'ILLES RYUKYU',
    'SABAH' => 'SABAH',
    'SAN MARINO' => 'SAN MARINO',
    'SAUDI ARABIA' => 'ARÀBIA SAUDITA',
    'SENEGAL' => 'SENEGAL',
    'SERBIA' => 'SÈRBIA',
    'SEYCHELLES' => 'SEYCHELLES',
    'SIERRA LEONE' => 'SIERRA LEONA',
    'SINGAPORE' => 'SINGAPUR',
    'SLOVAKIA' => 'ESLOVÀQUIA',
    'SLOVENIA' => 'ESLOVÈNIA',
    'SOMALILIAND' => 'SOMALILIAND',
    'SOUTH AFRICA' => 'SUD-ÀFRICA',
    'SOUTH YEMEN' => 'IEMEN DEL SUD',
    'SPAIN' => 'ESPANYA',
    'SPANISH SAHARA' => 'SÀHARA ESPANYOL',
    'SRI LANKA' => 'SRI LANKA',
    'ST. KITTS AND NEVIS' => 'SANT CRISTÒFOL I NEVIS',
    'ST. LUCIA' => 'SANTA LLÚCIA',
    'SUDAN' => 'SUDAN',
    'SURINAM' => 'SURINAM',
    'SW AFRICA' => 'ÀFRICA SW',
    'SWAZILAND' => 'SWAZILÀNDIA',
    'SWEDEN' => 'SUÈCIA',
    'SWITZERLAND' => 'SUÏSSA',
    'SYRIA' => 'SÍRIA',
    'TAIWAN' => 'TAIWAN',
    'TAJIKISTAN' => 'TADJIKISTAN',
    'TANZANIA' => 'TANZÀNIA',
    'THAILAND' => 'TAILÀNDIA',
    'TONGA' => 'TONGA',
    'TRINIDAD' => 'TRINITAT',
    'TUNISIA' => 'TUNÍSIA',
    'TURKEY' => 'TURQUIA',
    'UGANDA' => 'UGANDA',
    'UKRAINE' => 'UCRAÏNA',
    'UNITED ARAB EMIRATES' => 'UNIÓ DELS EMIRATS ÀRABS',
    'UNITED KINGDOM' => 'REGNE UNIT',
    'URUGUAY' => 'L\'URUGUAI',
    'US PACIFIC ISLAND' => 'ILLA DEL PACÍFIC',
    'US VIRGIN ISLANDS' => 'ILLES VERGES AMERICANES',
    'USA' => 'USA',
    'UZBEKISTAN' => 'UZBEKISTAN',
    'VANUATU' => 'VANUATU',
    'VATICAN CITY' => 'VATICAN CITY',
    'VENEZUELA' => 'VENEZUELA',
    'VIETNAM' => 'VIETNAM',
    'WAKE ISLAND' => 'VIETNAM',
    'WEST INDIES' => 'WEST INDIES',
    'WESTERN SAHARA' => 'WEST INDIES',
    'YEMEN' => 'WESTERN SAHARA',
    'ZAIRE' => 'ZAIRE',
    'ZAMBIA' => 'ZAMBIA',
    'ZIMBABWE' => 'ZAMBIA',
);

$app_list_strings['charset_dom'] = array(
    'BIG-5' => 'BIG-5 (Taiwan y Hong Kong)',
    /*'CP866'     => 'CP866', // ms-dos Cyrillic */
    /*'CP949'     => 'CP949 (Microsoft Korean)', */
    'CP1251' => 'CP1251 (Cirílico de MS)',
    'CP1252' => 'CP1252 (Europa Occidental i EEUU de Ms)',
    'EUC-CN' => 'EUC-CN (Chines Simplificat GB2312)',
    'EUC-JP' => 'EUC-JP (Japonés Unix)',
    'EUC-KR' => 'EUC-KR (Corea)',
    'EUC-TW' => 'EUC-TW (Taiwanés)',
    'ISO-2022-JP' => 'ISO-2022-JP (Japonés)',
    'ISO-2022-KR' => 'ISO-2022-KR (Corea)',
    'ISO-8859-1' => 'ISO-8859-1 (Europa Occidental i EEUU)',
    'ISO-8859-2' => 'ISO-8859-2 (Centroeuropa i Europa del Est)',
    'ISO-8859-3' => 'ISO-8859-3 (Llatí 3)',
    'ISO-8859-4' => 'ISO-8859-4 (Llatí 4)',
    'ISO-8859-5' => 'ISO-8859-5 (Cirílic)',
    'ISO-8859-6' => 'ISO-8859-6 (Árab)',
    'ISO-8859-7' => 'ISO-8859-7 (Grec)',
    'ISO-8859-8' => 'ISO-8859-8 (Hebreu)',
    'ISO-8859-9' => 'ISO-8859-9 (Llatí 5)',
    'ISO-8859-10' => 'ISO-8859-10 (Llatí 6)',
    'ISO-8859-13' => 'ISO-8859-13 (Llatí 7)',
    'ISO-8859-14' => 'ISO-8859-14 (Llatí 8)',
    'ISO-8859-15' => 'ISO-8859-15 (Llatí 9)',
    'KOI8-R' => 'KOI8-R (Cirílic Rus)',
    'KOI8-U' => 'KOI8-U (Cirílic Ucrania)',
    'SJIS' => 'SJIS (Japonés de MS)',
    'UTF-8' => 'UTF-8',
);

$app_list_strings['timezone_dom'] = array(

    'Africa/Algiers' => 'Africa/Algiers',
    'Africa/Luanda' => 'Africa/Luanda',
    'Africa/Porto-Novo' => 'Africa/Porto-Novo',
    'Africa/Gaborone' => 'Africa/Gaborone',
    'Africa/Ouagadougou' => 'Africa/Gaborone',
    'Africa/Bujumbura' => 'Africa/Bujumbura',
    'Africa/Douala' => 'Africa/Douala',
    'Atlantic/Cape_Verde' => 'Atlantic/Cape Verde',
    'Africa/Bangui' => 'Atlantic/Cape Verde',
    'Africa/Ndjamena' => 'Africa/Bangui',
    'Indian/Comoro' => 'Africa/Ndjamena',
    'Africa/Kinshasa' => 'Indian/Comoro',
    'Africa/Lubumbashi' => 'Africa/Lubumbashi',
    'Africa/Brazzaville' => 'Africa/Brazzaville',
    'Africa/Abidjan' => 'Africa/Abidjan',
    'Africa/Djibouti' => 'Africa/Djibouti',
    'Africa/Cairo' => 'Africa/Cairo',
    'Africa/Malabo' => 'Africa/Malabo',
    'Africa/Asmera' => 'Africa/Asmera',
    'Africa/Addis_Ababa' => 'Africa/Addis Ababa',
    'Africa/Libreville' => 'Africa/Libreville',
    'Africa/Banjul' => 'Africa/Banjul',
    'Africa/Accra' => 'Africa/Accra',
    'Africa/Conakry' => 'Africa/Conakry',
    'Africa/Bissau' => 'Africa/Bissau',
    'Africa/Nairobi' => 'Africa/Bissau',
    'Africa/Maseru' => 'Africa/Maseru',
    'Africa/Monrovia' => 'Africa/Monrovia',
    'Africa/Tripoli' => 'Africa/Tripoli',
    'Indian/Antananarivo' => 'Indian/Antananarivo',
    'Africa/Blantyre' => 'Africa/Blantyre',
    'Africa/Bamako' => 'Africa/Bamako',
    'Africa/Nouakchott' => 'Africa/Nouakchott',
    'Indian/Mauritius' => 'Africa/Nouakchott',
    'Indian/Mayotte' => 'Indian/Mayotte',
    'Africa/Casablanca' => 'Africa/Casablanca',
    'Africa/El_Aaiun' => 'Africa/El Aaiun',
    'Africa/Maputo' => 'Àfrica/Maputo',
    'Africa/Windhoek' => 'Àfrica/Windhoek',
    'Africa/Niamey' => 'Àfrica/Niamey',
    'Africa/Lagos' => 'Àfrica/Lagos',
    'Indian/Reunion' => 'Índia/Reunión',
    'Africa/Kigali' => 'Àfrica/Kigali',
    'Atlantic/St_Helena' => 'Atlàntic i St Helena',
    'Africa/Sao_Tome' => 'Àfrica/Sao prenc',
    'Africa/Dakar' => 'Àfrica-Dakar',
    'Indian/Mahe' => 'Índia/Mahe',
    'Africa/Freetown' => 'Àfrica/Freetown',
    'Africa/Mogadishu' => 'Àfrica/Mogadiscio',
    'Africa/Johannesburg' => 'Àfrica/Johannesburg',
    'Africa/Khartoum' => 'Àfrica/Khartum',
    'Africa/Mbabane' => 'Àfrica/Mbabane',
    'Africa/Dar_es_Salaam' => 'Àfrica/Dar es Salaam',
    'Africa/Lome' => 'Àfrica/Lome',
    'Africa/Tunis' => 'Àfrica/Tunísia',
    'Africa/Kampala' => 'Àfrica/Kampala',
    'Africa/Lusaka' => 'Àfrica/Lusaka',
    'Africa/Harare' => 'Àfrica/Harare',
    'Antarctica/Casey' => 'L\'Antàrtida/Casey',
    'Antarctica/Davis' => 'L\'Antàrtida/Davis',
    'Antarctica/Mawson' => 'L\'Antàrtida/Mawson',
    'Indian/Kerguelen' => 'Índia/Kerguelen',
    'Antarctica/DumontDUrville' => 'L\'Antàrtida/DumontDUrville',
    'Antarctica/Syowa' => 'L\'Antàrtida/Syowa',
    'Antarctica/Vostok' => 'L\'Antàrtida/Vostok',
    'Antarctica/Rothera' => 'L\'Antàrtida/Rothera',
    'Antarctica/Palmer' => 'L\'Antàrtida/Palmer',
    'Antarctica/McMurdo' => 'L\'Antàrtida/McMurdo',
    'Asia/Kabul' => 'Kabul/Àsia',
    'Asia/Yerevan' => 'Yerevan/Àsia',
    'Asia/Baku' => 'Baku/Àsia',
    'Asia/Bahrain' => 'Bahrain/Àsia',
    'Asia/Dhaka' => 'Dhaka/Àsia',
    'Asia/Thimphu' => 'Thimphu/Àsia',
    'Indian/Chagos' => 'Chagos/Índia',
    'Asia/Brunei' => 'Brunei/Àsia',
    'Asia/Rangoon' => 'Rangoon/Àsia',
    'Asia/Phnom_Penh' => 'Phnom Penh/Àsia',
    'Asia/Beijing' => 'Beijing/Àsia',
    'Asia/Harbin' => 'Harbin/Àsia',
    'Asia/Shanghai' => 'Shanghai/Àsia',
    'Asia/Chongqing' => 'Chongqing/Àsia',
    'Asia/Urumqi' => 'Urumqi/Àsia',
    'Asia/Kashgar' => 'Kashgar/Àsia',
    'Asia/Hong_Kong' => 'Hong Kong/Àsia',
    'Asia/Taipei' => 'Taipei/Àsia',
    'Asia/Macau' => 'Macau/Àsia',
    'Asia/Nicosia' => 'Asia/Nicosia',
    'Asia/Tbilisi' => 'Asia/Nicosia',
    'Asia/Dili' => 'Asia/Dili',
    'Asia/Calcutta' => 'Asia/Calcutta',
    'Asia/Jakarta' => 'Asia/Jakarta',
    'Asia/Pontianak' => 'Asia/Pontianak',
    'Asia/Makassar' => 'Asia/Makassar',
    'Asia/Jayapura' => 'Asia/Jayapura',
    'Asia/Tehran' => 'Asia/Tehran',
    'Asia/Baghdad' => 'Asia/Tehran',
    'Asia/Jerusalem' => 'Asia/Baghdad',
    'Asia/Tokyo' => 'Asia/Jerusalem',
    'Asia/Amman' => 'Asia/Amman',
    'Asia/Almaty' => 'Asia/Almaty',
    'Asia/Qyzylorda' => 'Asia/Qyzylorda',
    'Asia/Aqtobe' => 'Asia/Aqtobe',
    'Asia/Aqtau' => 'Asia/Aqtau',
    'Asia/Oral' => 'Asia/Oral',
    'Asia/Bishkek' => 'Asia/Bishkek',
    'Asia/Seoul' => 'Asia/Seoul',
    'Asia/Pyongyang' => 'Asia/Pyongyang',
    'Asia/Kuwait' => 'Asia/Kuwait',
    'Asia/Vientiane' => 'Asia/Vientiane',
    'Asia/Beirut' => 'Beirut/Àsia',
    'Asia/Kuala_Lumpur' => 'Kuala Lumpur/Àsia',
    'Asia/Kuching' => 'Kuching/Àsia',
    'Indian/Maldives' => 'Maldives/Índia',
    'Asia/Hovd' => 'Hovd/Àsia',
    'Asia/Ulaanbaatar' => 'Ulaanbaatar/Àsia',
    'Asia/Choibalsan' => 'Choibalsan/Àsia',
    'Asia/Katmandu' => 'Katmandu/Àsia',
    'Asia/Muscat' => 'Muscat/Àsia',
    'Asia/Karachi' => 'Karachi/Àsia',
    'Asia/Gaza' => 'Gaza/Àsia',
    'Asia/Manila' => 'Manila/Àsia',
    'Asia/Qatar' => 'Qatar/Àsia',
    'Asia/Riyadh' => 'Riyadh/Àsia',
    'Asia/Singapore' => 'Singapur/Àsia',
    'Asia/Colombo' => 'Colombo/Àsia',
    'Asia/Damascus' => 'Damasc/Àsia',
    'Asia/Dushanbe' => 'Duixanbe/Àsia',
    'Asia/Bangkok' => 'Bangkok/Àsia',
    'Asia/Ashgabat' => 'Ashgabat/Àsia',
    'Asia/Dubai' => 'Dubai/Àsia',
    'Asia/Samarkand' => 'Samarcanda/Àsia',
    'Asia/Tashkent' => 'Taixkent/Àsia',
    'Asia/Saigon' => 'Saigon/Àsia',
    'Asia/Aden' => 'Aden/Àsia',
    'Australia/Darwin' => 'Darwin/Austràlia',
    'Australia/Perth' => 'Perth/Austràlia',
    'Australia/Brisbane' => 'Brisbane/Austràlia',
    'Australia/Lindeman' => 'Lindeman/Austràlia',
    'Australia/Adelaide' => 'Austràlia/Adelaida',
    'Australia/Hobart' => 'Austràlia/Hobart',
    'Australia/Currie' => 'Currie/Austràlia',
    'Australia/Melbourne' => 'Austràlia/Melbourne',
    'Australia/Sydney' => 'Austràlia/Sydney',
    'Australia/Broken_Hill' => 'Broken Hill/Austràlia',
    'Indian/Christmas' => 'Índia/Nadal',
    'Pacific/Rarotonga' => 'Pacific/Rarotonga',
    'Indian/Cocos' => 'Índia/coco',
    'Pacific/Fiji' => 'Pacific/Fiji',
    'Pacific/Gambier' => 'Pacific/Gambier',
    'Pacific/Marquesas' => 'Pacific/Marqueses',
    'Pacific/Tahiti' => 'Pacific/Tahiti',
    'Pacific/Guam' => 'Guam/Pacífic',
    'Pacific/Tarawa' => 'Pacific/Tarawa',
    'Pacific/Enderbury' => 'Pacific/Enderbury',
    'Pacific/Kiritimati' => 'Pacific/Kiritimati',
    'Pacific/Saipan' => 'Pacific/Saipan',
    'Pacific/Majuro' => 'Pacific/Majuro',
    'Pacific/Kwajalein' => 'Pacific/Kwajalein',
    'Pacific/Truk' => 'Pacific/Truk',
    'Pacific/Pohnpei' => 'Pohnpei/Pacífic',
    'Pacific/Kosrae' => 'Pacific/Kosrae',
    'Pacific/Nauru' => 'Pacific/Nauru',
    'Pacific/Noumea' => 'Pacific/Noumea',
    'Pacific/Auckland' => 'Pacific/Auckland',
    'Pacific/Chatham' => 'Pacific/Chatham',
    'Pacific/Niue' => 'Pacific/Niue',
    'Pacific/Norfolk' => 'Pacific/Norfolk',
    'Pacific/Palau' => 'Pacific/Palau',
    'Pacific/Port_Moresby' => 'Pacific/Port Moresby',
    'Pacific/Pitcairn' => 'Pacific/Pitcairn',
    'Pacific/Pago_Pago' => 'Pacific/Pago Pago',
    'Pacific/Apia' => 'Pacific/Apia',
    'Pacific/Guadalcanal' => 'Pacific/Guadalcanal',
    'Pacific/Fakaofo' => 'Pacific/Fakaofo',
    'Pacific/Tongatapu' => 'Pacific/Tongatapu',
    'Pacific/Funafuti' => 'Pacific/Funafuti',
    'Pacific/Johnston' => 'Pacific/Johnston',
    'Pacific/Midway' => 'Pacific/Midway',
    'Pacific/Wake' => 'Pacific/Wake',
    'Pacific/Efate' => 'Pacific/Efate',
    'Pacific/Wallis' => 'Pacific/Wallis',
    'Europe/London' => 'Europa/Londres',
    'Europe/Dublin' => 'Europa/Dublín',
    'WET' => 'Wet',
    'CET' => 'CET',
    'MET' => 'Met',
    'EET' => 'EET',
    'Europe/Tirane' => 'Europa/Tirana',
    'Europe/Andorra' => 'Andorra/Europa',
    'Europe/Vienna' => 'Vienna/Europa',
    'Europe/Minsk' => 'Minsk/Europa',
    'Europe/Brussels' => 'Brussel·les/Europa',
    'Europe/Sofia' => 'Sofia/Europa',
    'Europe/Prague' => 'Praga/Europa',
    'Europe/Copenhagen' => 'Copenhaguen/Europa',
    'Atlantic/Faeroe' => 'Fèroe/Atlàntic',
    'America/Danmarkshavn' => 'Danmarkshavn/Amèrica',
    'America/Scoresbysund' => 'Scoresbysund/Amèrica',
    'America/Godthab' => 'Godthab/Amèrica',
    'America/Thule' => 'Thule/Amèrica',
    'Europe/Tallinn' => 'Tallinn/Europa',
    'Europe/Helsinki' => 'Helsinki/Europa',
    'Europe/Paris' => 'París/Europa',
    'Europe/Berlin' => 'Berlín/Europa',
    'Europe/Gibraltar' => 'Gibraltar/Europa',
    'Europe/Athens' => 'Atenes/Europa',
    'Europe/Budapest' => 'Budapest/Europa',
    'Atlantic/Reykjavik' => 'Reykjavik/Atlàntic',
    'Europe/Rome' => 'Roma/Europa',
    'Europe/Riga' => 'Riga/Europa',
    'Europe/Vaduz' => 'Vaduz/Europa',
    'Europe/Vilnius' => 'Vílnius/Europa',
    'Europe/Luxembourg' => 'Luxemburg/Europa',
    'Europe/Malta' => 'Malta/Europa',
    'Europe/Chisinau' => 'Chisinau/Europa',
    'Europe/Monaco' => 'Mònaco/Europa',
    'Europe/Amsterdam' => 'Amsterdam/Europa,',
    'Europe/Oslo' => 'Oslo/Europa',
    'Europe/Warsaw' => 'Varsòvia/Europa',
    'Europe/Lisbon' => 'Lisboa/Europa',
    'Atlantic/Azores' => 'Azores/Atlàntic',
    'Atlantic/Madeira' => 'Madeira/Atlàntic',
    'Europe/Bucharest' => 'Bucarest/Europa',
    'Europe/Kaliningrad' => 'Kaliningrad/Europa',
    'Europe/Moscow' => 'Moscou/Europa',
    'Europe/Samara' => 'Samara/Europa',
    'Asia/Yekaterinburg' => 'Iekaterinburg/Àsia',
    'Asia/Omsk' => 'Omsk/Àsia',
    'Asia/Novosibirsk' => 'Novosibirsk/Àsia',
    'Asia/Krasnoyarsk' => 'Krasnoyarsk/Àsia',
    'Asia/Irkutsk' => 'Irkutsk/Àsia',
    'Asia/Yakutsk' => 'Yakutsk/Àsia',
    'Asia/Vladivostok' => 'Vladivostok/Àsia',
    'Asia/Sakhalin' => 'Sakhalín/Àsia',
    'Asia/Magadan' => 'Magadan/Àsia',
    'Asia/Kamchatka' => 'Kamchatka/Àsia',
    'Asia/Anadyr' => 'Anadyr/Àsia',
    'Europe/Belgrade' => 'Belgrad/Europa',
    'Europe/Madrid' => 'Europa/Madrid',
    'Africa/Ceuta' => 'Àfrica/Ceuta',
    'Atlantic/Canary' => 'Atlantic/Gran Canària',
    'Europe/Stockholm' => 'Europa/estocolm',
    'Europe/Zurich' => 'Europa/Zurich',
    'Europe/Istanbul' => 'Europa/Istanbul',
    'Europe/Kiev' => 'Europa/Kiev',
    'Europe/Uzhgorod' => 'Europa/Uzhgorod',
    'Europe/Zaporozhye' => 'Europa/Zaporozhye',
    'Europe/Simferopol' => 'Europa/Simferopol',
    'America/New_York' => 'Amèrica / Nova York',
    'America/Chicago' => 'Estats Units/Chicago',
    'America/North_Dakota/Center' => 'Amèrica del nord/Dakota(centre)',
    'America/Denver' => 'Amèrica/Denver',
    'America/Los_Angeles' => 'Amèrica / Los Angeles',
    'America/Juneau' => 'Amèrica/Juneau',
    'America/Yakutat' => 'Amèrica/Yakutat',
    'America/Anchorage' => 'Amèrica/Anchorage',
    'America/Nome' => 'Amèrica/Nome',
    'America/Adak' => 'Amèrica/Adak',
    'Pacific/Honolulu' => 'Pacific/Honolulu',
    'America/Phoenix' => 'Amèrica/Phoenix',
    'America/Boise' => 'Amèrica/Boise',
    'America/Indiana/Indianapolis' => 'Indiana/Indianapolis/Amèrica',
    'America/Indiana/Marengo' => 'Amèrica/Indiana/Marengo',
    'America/Indiana/Knox' => 'Indiana/Amèrica/Knox',
    'America/Indiana/Vevay' => 'Amèrica/Indiana/Vevay',
    'America/Kentucky/Louisville' => 'Kentucky/Louisville/Amèrica',
    'America/Kentucky/Monticello' => 'Monticello/Kentucky/Amèrica',
    'America/Detroit' => 'Amèrica/Detroit',
    'America/Menominee' => 'Amèrica/Menominee',
    'America/St_Johns' => 'Amèrica/St Johns',
    'America/Goose_Bay' => 'Amèrica/Goose_Bay',
    'America/Halifax' => 'Amèrica/Halifax',
    'America/Glace_Bay' => 'Amèrica/Glace Bay',
    'America/Montreal' => 'Amèrica/Montreal',
    'America/Toronto' => 'Amèrica/Toronto',
    'America/Thunder_Bay' => 'Amèrica/Thunder Bay',
    'America/Nipigon' => 'Amèrica/Nipigon',
    'America/Rainy_River' => 'Riu Amèrica/plujós',
    'America/Winnipeg' => 'Amèrica/Winnipeg',
    'America/Regina' => 'Amèrica/Regina',
    'America/Swift_Current' => 'Amèrica/Swift Current',
    'America/Edmonton' => 'Amèrica/Edmonton',
    'America/Vancouver' => 'Amèrica/Vancouver',
    'America/Dawson_Creek' => 'Amèrica/Dawson Creek',
    'America/Pangnirtung' => 'Amèrica/Pangnirtung',
    'America/Iqaluit' => 'Amèrica/Iqaluit',
    'America/Coral_Harbour' => 'Amèrica/Coral Harbour',
    'America/Rankin_Inlet' => 'Amèrica/Rankin Inlet',
    'America/Cambridge_Bay' => 'Amèrica/Cambridge badia',
    'America/Yellowknife' => 'Yellowknife/Amèrica',
    'America/Inuvik' => 'Inuvik/Amèrica',
    'America/Whitehorse' => 'Whitehorse/Amèrica',
    'America/Dawson' => 'Dawson/Amèrica',
    'America/Cancun' => 'Cancún/Amèrica',
    'America/Merida' => 'Mèrida/Amèrica',
    'America/Monterrey' => 'Monterrey/Amèrica',
    'America/Mexico_City' => 'Ciutat de Mèxic/Amèrica',
    'America/Chihuahua' => 'Chihuahua/Amèrica',
    'America/Hermosillo' => 'Hermosillo/Amèrica',
    'America/Mazatlan' => 'Mazatlán/Amèrica',
    'America/Tijuana' => 'Tijuana/Amèrica',
    'America/Anguilla' => 'Anguilla/Amèrica',
    'America/Antigua' => 'Antigua/Amèrica',
    'America/Nassau' => 'Nassau/Amèrica',
    'America/Barbados' => 'Barbados/Amèrica',
    'America/Belize' => 'Belize/Amèrica',
    'Atlantic/Bermuda' => 'Bermudes/Atlàntic',
    'America/Cayman' => 'Caiman/Amèrica',
    'America/Costa_Rica' => 'Costa Rica/Amèrica',
    'America/Havana' => 'Havana/Amèrica',
    'America/Dominica' => 'Dominica/Amèrica',
    'America/Santo_Domingo' => 'Santo Domingo/Amèrica',
    'America/El_Salvador' => 'El Salvador/Amèrica',
    'America/Grenada' => 'Grenada/Amèrica',
    'America/Guadeloupe' => 'Guadalupe/Amèrica',
    'America/Guatemala' => 'Guatemala/Amèrica',
    'America/Port-au-Prince' => 'Port-au-Prince/Amèrica',
    'America/Tegucigalpa' => 'Tegucigalpa/Amèrica',
    'America/Jamaica' => 'Jamaica/Amèrica',
    'America/Martinique' => 'Martinica/Amèrica',
    'America/Montserrat' => 'Montserrat/Amèrica',
    'America/Managua' => 'Managua/Amèrica',
    'America/Panama' => 'Panamà/Amèrica',
    'America/Puerto_Rico' => 'Puerto_Rico/Amèrica',
    'America/St_Kitts' => 'St_Kitts/Amèrica',
    'America/St_Lucia' => 'St_Lucia/Amèrica',
    'America/Miquelon' => 'Miquelon/Amèrica',
    'America/St_Vincent' => 'Sant Vicenç/Amèrica',
    'America/Grand_Turk' => 'Grand Turk/Amèrica',
    'America/Tortola' => 'Tortola/Amèrica',
    'America/St_Thomas' => 'St Thomas/Amèrica',
    'America/Argentina/Buenos_Aires' => 'Buenos Aires/Argentina/Amèrica',
    'America/Argentina/Cordoba' => 'Còrdova/Argentina/Amèrica',
    'America/Argentina/Tucuman' => 'Tucuman/Argentina/Amèrica',
    'America/Argentina/La_Rioja' => 'La_Rioja/Argentina/Amèrica',
    'America/Argentina/San_Juan' => 'San Juan/Argentina/Amèrica',
    'America/Argentina/Jujuy' => 'Jujuy/Argentina/Amèrica',
    'America/Argentina/Catamarca' => 'Catamarca/Argentina/Amèrica',
    'America/Argentina/Mendoza' => 'Amèrica/Argentina/Mendoza',
    'America/Argentina/Rio_Gallegos' => 'Amèrica/Argentina/Rio Gallegos',
    'America/Argentina/Ushuaia' => 'Amèrica/Argentina/Ushuaia',
    'America/Aruba' => 'Amèrica/Aruba',
    'America/La_Paz' => 'Amèrica/La Paz',
    'America/Noronha' => 'Amèrica/Noronha',
    'America/Belem' => 'Amèrica/Belem',
    'America/Fortaleza' => 'Amèrica/Fortaleza',
    'America/Recife' => 'Amèrica/Recife',
    'America/Araguaina' => 'Amèrica/Araguaina',
    'America/Maceio' => 'Amèrica/Maceio',
    'America/Bahia' => 'Amèrica/Bahia',
    'America/Sao_Paulo' => 'Amèrica/Sao Paulo',
    'America/Campo_Grande' => 'Amèrica/Campo Grande',
    'America/Cuiaba' => 'Amèrica/Cuiaba',
    'America/Porto_Velho' => 'Amèrica/Porto_Velho',
    'America/Boa_Vista' => 'Amèrica/Boa Vista',
    'America/Manaus' => 'Amèrica/Manaus',
    'America/Eirunepe' => 'Amèrica/Eirunepe',
    'America/Rio_Branco' => 'Amèrica/Rio Branco',
    'America/Santiago' => 'Amèrica/Santiago',
    'Pacific/Easter' => 'Pacific/Pasqua',
    'America/Bogota' => 'Amèrica/Bogotà',
    'America/Curacao' => 'Amèrica/Curaçao',
    'America/Guayaquil' => 'Amèrica/Guayaquil',
    'Pacific/Galapagos' => 'Pacific/Galápagos',
    'Atlantic/Stanley' => 'Atlàntic/Stanley',
    'America/Cayenne' => 'Amèrica/Cayenne',
    'America/Guyana' => 'Amèrica/Guyana',
    'America/Asuncion' => 'Amèrica/Asunción',
    'America/Lima' => 'Amèrica/Lima',
    'Atlantic/South_Georgia' => 'Atlàntic/Geòrgia Sud',
    'America/Paramaribo' => 'Amèrica/Paramaribo',
    'America/Port_of_Spain' => 'Amèrica/Port de espanya',
    'America/Montevideo' => 'Amèrica/Montevideo',
    'America/Caracas' => 'Amèrica/Caracas',
);

$app_list_strings['eapm_list'] = array(
    'Sugar' => 'SuiteCRM',
    'WebEx' => 'WebEx',
    'GoToMeeting' => 'Anar a la reunió',
    'IBMSmartCloud' => 'IBM SmartCloud',
    'Google' => 'Google',
    'Box' => 'Box.net',
    'Facebook' => 'Facebook',
    'Twitter' => 'Twitter',
);
$app_list_strings['eapm_list_import'] = array(
    'Google' => 'Contactes de Google',
);
$app_list_strings['eapm_list_documents'] = array(
    'Google' => 'Google Drive',
);
$app_list_strings['token_status'] = array(
    1 => 'Petició',
    2 => 'Accés',
    3 => 'No Vàlid',
);
// PR 5464
$app_list_strings ['emailTemplates_type_list'] = array(
    '' => '',
    'campaign' => 'Campanya',
    'email' => 'Correu electrònic',
    'event' => 'Esdeveniment',
);

$app_list_strings ['emailTemplates_type_list_campaigns'] = array(
    '' => '',
    'campaign' => 'Campanya',
);

$app_list_strings ['emailTemplates_type_list_no_workflow'] = array(
    '' => '',
    'campaign' => 'Campanya',
    'email' => 'Correu electrònic',
    'event' => 'Esdeveniment',
    'system' => 'Sistema',
);

// knowledge base
$app_list_strings['moduleList']['AOK_KnowledgeBase'] = 'Base de Coneixement'; // Shows in the ALL menu entries
$app_list_strings['moduleList']['AOK_Knowledge_Base_Categories'] = 'Categories Base de coneixement'; // Shows in the ALL menu entries
$app_list_strings['aok_status_list']['Draft'] = 'Esborrany';
$app_list_strings['aok_status_list']['Expired'] = 'Caducat';
$app_list_strings['aok_status_list']['In_Review'] = 'En revisió';
//$app_list_strings['aok_status_list']['Published'] = 'Publicat';
$app_list_strings['aok_status_list']['published_private'] = 'Privat';
$app_list_strings['aok_status_list']['published_public'] = 'Públic';

$app_list_strings['moduleList']['FP_events'] = 'Esdeveniments';
$app_list_strings['moduleList']['FP_Event_Locations'] = 'Ubicacions';

//events
$app_list_strings['fp_event_invite_status_dom']['Invited'] = 'Convidats';
$app_list_strings['fp_event_invite_status_dom']['Not Invited'] = 'No Convidats';
$app_list_strings['fp_event_invite_status_dom']['Attended'] = 'Assistents ';
$app_list_strings['fp_event_invite_status_dom']['Not Attended'] = 'No Assistents';
$app_list_strings['fp_event_status_dom']['Accepted'] = 'Acceptat';
$app_list_strings['fp_event_status_dom']['Declined'] = 'Rebutjat';
$app_list_strings['fp_event_status_dom']['No Response'] = 'Sense resposta';

$app_strings['LBL_STATUS_EVENT'] = 'Estat de la invitació';
$app_strings['LBL_ACCEPT_STATUS'] = 'Estat d\'acceptació';
$app_strings['LBL_LISTVIEW_OPTION_CURRENT'] = 'Pàgina actual';
$app_strings['LBL_LISTVIEW_OPTION_ENTIRE'] = 'Tots els registres';
$app_strings['LBL_LISTVIEW_NONE'] = 'Res';

//aod
$app_list_strings['moduleList']['AOD_IndexEvent'] = 'Esdeveniment índex';
$app_list_strings['moduleList']['AOD_Index'] = 'Índex';

$app_list_strings['moduleList']['AOP_Case_Events'] = 'Esdeveniments de casos';
$app_list_strings['moduleList']['AOP_Case_Updates'] = 'Actualitzacions de casos';
$app_strings['LBL_AOP_EMAIL_REPLY_DELIMITER'] = '= = = Si us plau respongui damunt aquesta línia = = =';


//aop PR 5426
$app_list_strings['moduleList']['JAccount'] = 'JAccount';

$app_list_strings['case_state_default_key'] = 'Open';
$app_list_strings['case_state_dom'] =
    array(
        'Open' => 'Obert',
        'Closed' => 'Tancat',
    );
$app_list_strings['case_status_default_key'] = 'Open_New';
$app_list_strings['case_status_dom'] =
    array(
        'Open_New' => 'Nou',
        'Open_Assigned' => 'Assignat',
        'Closed_Closed' => 'Tancat',
        'Open_Pending Input' => 'Pendent d\'Informació',
        'Closed_Rejected' => 'Refusat',
        'Closed_Duplicate' => 'Duplicat',
    );
$app_list_strings['contact_portal_user_type_dom'] =
    array(
        'Single' => 'Usuari individual',
        'Account' => 'Compta d\'usuari',
    );
$app_list_strings['dom_email_distribution_for_auto_create'] = array(
    'AOPDefault' => 'Utilitza el AOP predeterminat',
    'singleUser' => 'Usuari individual',
    'roundRobin' => 'Round Robin',
    'leastBusy' => 'Menys-Ocupat',
    'random' => 'Aleatori ',
);

//aor
$app_list_strings['moduleList']['AOR_Reports'] = 'Informes';
$app_list_strings['moduleList']['AOR_Conditions'] = 'Condicions d\'Informe';
$app_list_strings['moduleList']['AOR_Charts'] = 'Gràfics d\'Informe';
$app_list_strings['moduleList']['AOR_Fields'] = 'Camps d\'Informes';
$app_list_strings['moduleList']['AOR_Scheduled_Reports'] = 'Informes programats';
$app_list_strings['aor_operator_list']['Equal_To'] = 'Igual a';
$app_list_strings['aor_operator_list']['Not_Equal_To'] = 'No igual a';
$app_list_strings['aor_operator_list']['Greater_Than'] = 'Més gran que';
$app_list_strings['aor_operator_list']['Less_Than'] = 'Més petit que';
$app_list_strings['aor_operator_list']['Greater_Than_or_Equal_To'] = 'Més gran o igual a';
$app_list_strings['aor_operator_list']['Less_Than_or_Equal_To'] = 'Més petit o igual a';
$app_list_strings['aor_operator_list']['Contains'] = 'Conté';
$app_list_strings['aor_operator_list']['Not_Contains'] = 'No conté';
$app_list_strings['aor_operator_list']['Starts_With'] = 'Comença amb';
$app_list_strings['aor_operator_list']['Ends_With'] = 'Finalitza amb';
$app_list_strings['aor_format_options'][''] = '';
$app_list_strings['aor_format_options']['Y-m-d'] = 'Y-m-d';
$app_list_strings['aor_format_options']['m-d-Y'] = 'm-d-A';
$app_list_strings['aor_format_options']['d-m-Y'] = 'd-m-A';
$app_list_strings['aor_format_options']['Y/m/d'] = 'A/m/d';
$app_list_strings['aor_format_options']['m/d/Y'] = 'm/d/A';
$app_list_strings['aor_format_options']['d/m/Y'] = 'd/m/Y';
$app_list_strings['aor_format_options']['Y.m.d'] = 'A.m.d';
$app_list_strings['aor_format_options']['m.d.Y'] = 'm.d.A';
$app_list_strings['aor_format_options']['d.m.Y'] = 'd.m.A';
$app_list_strings['aor_format_options']['Ymd'] = 'Ymd';
$app_list_strings['aor_format_options']['Y-m'] = 'Y-m';
$app_list_strings['aor_format_options']['Y'] = 'Y';
$app_list_strings['aor_condition_operator_list']['And'] = 'I';
$app_list_strings['aor_condition_operator_list']['OR'] = 'O';
$app_list_strings['aor_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aor_condition_type_list']['Field'] = 'Camp';
$app_list_strings['aor_condition_type_list']['Date'] = 'Data';
$app_list_strings['aor_condition_type_list']['Multi'] = 'Un de';
$app_list_strings['aor_condition_type_list']['Period'] = 'Període';
$app_list_strings['aor_condition_type_list']['CurrentUserID'] = 'Usuari actual';
$app_list_strings['aor_date_type_list'][''] = '';
$app_list_strings['aor_date_type_list']['minute'] = 'Minuts';
$app_list_strings['aor_date_type_list']['hour'] = 'Hores';
$app_list_strings['aor_date_type_list']['day'] = 'Dies';
$app_list_strings['aor_date_type_list']['week'] = 'Setmanes';
$app_list_strings['aor_date_type_list']['month'] = 'Messos';
$app_list_strings['aor_date_type_list']['business_hours'] = 'Hores Laborals';
$app_list_strings['aor_date_options']['now'] = 'Ara';
$app_list_strings['aor_date_options']['field'] = 'Aquest camp';
$app_list_strings['aor_date_operator']['now'] = '';
$app_list_strings['aor_date_operator']['plus'] = '+';
$app_list_strings['aor_date_operator']['minus'] = '-';
$app_list_strings['aor_sort_operator'][''] = '';
$app_list_strings['aor_sort_operator']['ASC'] = 'Ascendent';
$app_list_strings['aor_sort_operator']['DESC'] = 'Descendent';
$app_list_strings['aor_function_list'][''] = '';
$app_list_strings['aor_function_list']['COUNT'] = 'Total';
$app_list_strings['aor_function_list']['MIN'] = 'Mínim';
$app_list_strings['aor_function_list']['MAX'] = 'Màxim';
$app_list_strings['aor_function_list']['SUM'] = 'Suma';
$app_list_strings['aor_function_list']['AVG'] = 'Mitjana';
$app_list_strings['aor_total_options'][''] = '';
$app_list_strings['aor_total_options']['COUNT'] = 'Total';
$app_list_strings['aor_total_options']['SUM'] = 'Suma';
$app_list_strings['aor_total_options']['AVG'] = 'Mitjana';
$app_list_strings['aor_chart_types']['bar'] = 'Gràfic de barres';
$app_list_strings['aor_chart_types']['line'] = 'Gràfic de línies';
$app_list_strings['aor_chart_types']['pie'] = 'Gràfic de sectors';
$app_list_strings['aor_chart_types']['radar'] = 'Gràfic radial';
$app_list_strings['aor_chart_types']['stacked_bar'] = 'Barra apilada';
$app_list_strings['aor_chart_types']['grouped_bar'] = 'Barra agrupada';
$app_list_strings['aor_scheduled_report_schedule_types']['monthly'] = 'Mensual';
$app_list_strings['aor_scheduled_report_schedule_types']['weekly'] = 'Setmanal';
$app_list_strings['aor_scheduled_report_schedule_types']['daily'] = 'Diari';
$app_list_strings['aor_scheduled_reports_status_dom']['active'] = 'Actiu';
$app_list_strings['aor_scheduled_reports_status_dom']['inactive'] = 'Inactiu';
$app_list_strings['aor_email_type_list']['Email Address'] = 'Correu electrònic';
$app_list_strings['aor_email_type_list']['Specify User'] = 'Usuari';
$app_list_strings['aor_email_type_list']['Users'] = 'Usuaris';
$app_list_strings['aor_assign_options']['all'] = 'Tots els usuaris';
$app_list_strings['aor_assign_options']['role'] = 'Tots els usuaris de rol';
$app_list_strings['aor_assign_options']['security_group'] = 'Tots els usuaris del Grup de Seguretat';
$app_list_strings['date_time_period_list']['today'] = 'Avui';
$app_list_strings['date_time_period_list']['yesterday'] = 'Ahir';
$app_list_strings['date_time_period_list']['this_week'] = 'Aquesta setmana';
$app_list_strings['date_time_period_list']['last_week'] = 'La Setmana Pasada';
$app_list_strings['date_time_period_list']['last_month'] = 'Últim Mes';
$app_list_strings['date_time_period_list']['this_month'] = 'Aquest Mes';
$app_list_strings['date_time_period_list']['this_quarter'] = 'Aquest Trimestre';
$app_list_strings['date_time_period_list']['last_quarter'] = 'Últim Trimestre';
$app_list_strings['date_time_period_list']['this_year'] = 'Aquest any';
$app_list_strings['date_time_period_list']['last_year'] = 'L\'any passat';
$app_strings['LBL_CRON_ON_THE_MONTHDAY'] = 'en el';
$app_strings['LBL_CRON_ON_THE_WEEKDAY'] = 'en';
$app_strings['LBL_CRON_AT'] = 'en';
$app_strings['LBL_CRON_RAW'] = 'Avançat';
$app_strings['LBL_CRON_MIN'] = 'Min';
$app_strings['LBL_CRON_HOUR'] = 'Hora';
$app_strings['LBL_CRON_DAY'] = 'Dia';
$app_strings['LBL_CRON_MONTH'] = 'Mes';
$app_strings['LBL_CRON_DOW'] = 'DOW';
$app_strings['LBL_CRON_DAILY'] = 'Diari';
$app_strings['LBL_CRON_WEEKLY'] = 'Setmanal';
$app_strings['LBL_CRON_MONTHLY'] = 'Mensual';

//aos
$app_list_strings['moduleList']['AOS_Contracts'] = 'Contractes';
$app_list_strings['moduleList']['AOS_Invoices'] = 'Factures';
$app_list_strings['moduleList']['AOS_PDF_Templates'] = 'Plantilles PDF';
$app_list_strings['moduleList']['AOS_Product_Categories'] = 'Categories de Productes';
$app_list_strings['moduleList']['AOS_Products'] = 'Productes';
$app_list_strings['moduleList']['AOS_Products_Quotes'] = 'Línies de Pressupost';
$app_list_strings['moduleList']['AOS_Line_Item_Groups'] = 'Grups';
$app_list_strings['moduleList']['AOS_Quotes'] = 'Pressupostos';
$app_list_strings['aos_quotes_type_dom'][''] = '';
$app_list_strings['aos_quotes_type_dom']['Analyst'] = 'Analista';
$app_list_strings['aos_quotes_type_dom']['Competitor'] = 'Competidor';
$app_list_strings['aos_quotes_type_dom']['Customer'] = 'Client';
$app_list_strings['aos_quotes_type_dom']['Integrator'] = 'Integrador';
$app_list_strings['aos_quotes_type_dom']['Investor'] = 'Inversor';
$app_list_strings['aos_quotes_type_dom']['Partner'] = 'Soci';
$app_list_strings['aos_quotes_type_dom']['Press'] = 'Premsa';
$app_list_strings['aos_quotes_type_dom']['Prospect'] = 'Perspectives';
$app_list_strings['aos_quotes_type_dom']['Reseller'] = 'Revenedor';
$app_list_strings['aos_quotes_type_dom']['Other'] = 'Altre';
$app_list_strings['template_ddown_c_list'][''] = '';
$app_list_strings['quote_stage_dom']['Draft'] = 'Esborrany';
$app_list_strings['quote_stage_dom']['Negotiation'] = 'Negociació';
$app_list_strings['quote_stage_dom']['Delivered'] = 'Enviat';
$app_list_strings['quote_stage_dom']['On Hold'] = 'En espera';
$app_list_strings['quote_stage_dom']['Confirmed'] = 'Confirmat';
$app_list_strings['quote_stage_dom']['Closed Accepted'] = 'Tancat Acceptat';
$app_list_strings['quote_stage_dom']['Closed Lost'] = 'Perdut';
$app_list_strings['quote_stage_dom']['Closed Dead'] = 'Tancat Mort';
$app_list_strings['quote_term_dom']['Net 15'] = '15 dies';
$app_list_strings['quote_term_dom']['Net 30'] = '30 dies';
$app_list_strings['quote_term_dom'][''] = '';
$app_list_strings['approval_status_dom']['Approved'] = 'Aprovat';
$app_list_strings['approval_status_dom']['Not Approved'] = 'No Aprovat';
$app_list_strings['approval_status_dom'][''] = '';
$app_list_strings['vat_list']['0.0'] = '0%';
$app_list_strings['vat_list']['5.0'] = '5%';
$app_list_strings['vat_list']['7.5'] = '7,5%';
$app_list_strings['vat_list']['17.5'] = '17,5%';
$app_list_strings['vat_list']['20.0'] = '20%';
$app_list_strings['discount_list']['Percentage'] = 'per cent';
$app_list_strings['discount_list']['Amount'] = 'quantitat';
$app_list_strings['aos_invoices_type_dom'][''] = '';
$app_list_strings['aos_invoices_type_dom']['Analyst'] = 'Analista';
$app_list_strings['aos_invoices_type_dom']['Competitor'] = 'Competidor';
$app_list_strings['aos_invoices_type_dom']['Customer'] = 'Client';
$app_list_strings['aos_invoices_type_dom']['Integrator'] = 'Integrador';
$app_list_strings['aos_invoices_type_dom']['Investor'] = 'Inversor';
$app_list_strings['aos_invoices_type_dom']['Partner'] = 'Soci';
$app_list_strings['aos_invoices_type_dom']['Press'] = 'Premsa';
$app_list_strings['aos_invoices_type_dom']['Prospect'] = 'Perspectives';
$app_list_strings['aos_invoices_type_dom']['Reseller'] = 'Revenedor';
$app_list_strings['aos_invoices_type_dom']['Other'] = 'Altre';
$app_list_strings['invoice_status_dom']['Paid'] = 'Pagat';
$app_list_strings['invoice_status_dom']['Unpaid'] = 'No Pagat';
$app_list_strings['invoice_status_dom']['Cancelled'] = 'Cancel·lat';
$app_list_strings['invoice_status_dom'][''] = '';
$app_list_strings['quote_invoice_status_dom']['Not Invoiced'] = 'No Facturat';
$app_list_strings['quote_invoice_status_dom']['Invoiced'] = 'Facturat';
$app_list_strings['product_code_dom']['XXXX'] = 'Codi producte';
$app_list_strings['product_code_dom']['YYYY'] = 'AAAA';
$app_list_strings['product_category_dom']['Laptops'] = 'Portàtils';
$app_list_strings['product_category_dom']['Desktops'] = 'Escriptoris';
$app_list_strings['product_category_dom'][''] = '';
$app_list_strings['product_type_dom']['Good'] = 'Bé';
$app_list_strings['product_type_dom']['Service'] = 'Servei';
$app_list_strings['product_quote_parent_type_dom']['AOS_Quotes'] = 'Pressupostos';
$app_list_strings['product_quote_parent_type_dom']['AOS_Invoices'] = 'Factures';
$app_list_strings['product_quote_parent_type_dom']['AOS_Contracts'] = 'Contractes';
// STIC-Custom 20220124 MHP - Delete the values of the pdf_template_type_dom 
// STIC#564               
// $app_list_strings['pdf_template_type_dom']['AOS_Quotes'] = 'Pressupostos';
// $app_list_strings['pdf_template_type_dom']['AOS_Invoices'] = 'Factures';
// $app_list_strings['pdf_template_type_dom']['AOS_Contracts'] = 'Contractes';
// $app_list_strings['pdf_template_type_dom']['Accounts'] = 'Comptes';
// $app_list_strings['pdf_template_type_dom']['Contacts'] = 'Contactes';
// $app_list_strings['pdf_template_type_dom']['Leads'] = 'Clients Potencials';
// END STIC-Custom
$app_list_strings['pdf_template_sample_dom'][''] = '';
$app_list_strings['contract_status_list']['Not Started'] = 'No iniciat';
$app_list_strings['contract_status_list']['In Progress'] = 'En progrés';
$app_list_strings['contract_status_list']['Signed'] = 'Firmat';
$app_list_strings['contract_type_list']['Type'] = 'Tipus';
$app_strings['LBL_PRINT_AS_PDF'] = 'Genera document PDF';
$app_strings['LBL_SELECT_TEMPLATE'] = 'Seleccioneu una plantilla';
$app_strings['LBL_NO_TEMPLATE'] = "ERROR: No s\'han trobat plantilles. Aneu al mòdul Plantilles PDF i creeu-hi una plantilla"; // Excepció d'escapat 

//aow PR 5775
$app_list_strings['moduleList']['AOW_WorkFlow'] = 'Fluxos de treball';
$app_list_strings['moduleList']['AOW_Conditions'] = 'Condicions dels Fluxos de treball';
$app_list_strings['moduleList']['AOW_Processed'] = 'Auditoria de Processos';
$app_list_strings['moduleList']['AOW_Actions'] = 'Accions dels Fluxos de treball';
$app_list_strings['aow_status_list']['Active'] = 'Actiu';
$app_list_strings['aow_status_list']['Inactive'] = 'Inactiu';
$app_list_strings['aow_operator_list']['Equal_To'] = 'Igual a';
$app_list_strings['aow_operator_list']['Not_Equal_To'] = 'No igual a';
$app_list_strings['aow_operator_list']['Greater_Than'] = 'Més gran que';
$app_list_strings['aow_operator_list']['Less_Than'] = 'Més petit que';
$app_list_strings['aow_operator_list']['Greater_Than_or_Equal_To'] = 'Més gran o igual que';
$app_list_strings['aow_operator_list']['Less_Than_or_Equal_To'] = 'Més petit o igual que';
$app_list_strings['aow_operator_list']['Contains'] = 'Conté';
$app_list_strings['aow_operator_list']['Not_Contains'] = 'No conté';
$app_list_strings['aow_operator_list']['Starts_With'] = 'Comença amb';
$app_list_strings['aow_operator_list']['Ends_With'] = 'Finalitza amb';
$app_list_strings['aow_operator_list']['is_null'] = 'És nul';
$app_list_strings['aow_operator_list']['is_not_null'] = 'No és nul';
$app_list_strings['aow_operator_list']['Anniversary'] = 'Aniversaris';
$app_list_strings['aow_process_status_list']['Complete'] = 'Completat';
$app_list_strings['aow_process_status_list']['Running'] = 'Corrent';
$app_list_strings['aow_process_status_list']['Pending'] = 'Pendent';
$app_list_strings['aow_process_status_list']['Failed'] = 'Fallat';
$app_list_strings['aow_condition_operator_list']['And'] = 'I';
$app_list_strings['aow_condition_operator_list']['OR'] = 'O';
$app_list_strings['aow_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aow_condition_type_list']['Field'] = 'Camp';
$app_list_strings['aow_condition_type_list']['Any_Change'] = 'Qualsevol canvi';
$app_list_strings['aow_condition_type_list']['SecurityGroup'] = 'En el Grup de Seguretat';
$app_list_strings['aow_condition_type_list']['Date'] = 'Data';
$app_list_strings['aow_condition_type_list']['Multi'] = 'Un de';
$app_list_strings['aow_action_type_list']['Value'] = 'Valor';
$app_list_strings['aow_action_type_list']['Field'] = 'Camp';
$app_list_strings['aow_action_type_list']['Date'] = 'Data';
$app_list_strings['aow_action_type_list']['Round_Robin'] = 'Torn rotatori';
$app_list_strings['aow_action_type_list']['Least_Busy'] = 'Menys ocupat';
$app_list_strings['aow_action_type_list']['Random'] = 'Aleatori';
$app_list_strings['aow_rel_action_type_list']['Value'] = 'Valor';
$app_list_strings['aow_rel_action_type_list']['Field'] = 'Camp';
$app_list_strings['aow_date_type_list'][''] = '';
$app_list_strings['aow_date_type_list']['minute'] = 'Minuts';
$app_list_strings['aow_date_type_list']['hour'] = 'Hores';
$app_list_strings['aow_date_type_list']['day'] = 'Dies';
$app_list_strings['aow_date_type_list']['week'] = 'Setmanes';
$app_list_strings['aow_date_type_list']['month'] = 'Mesos';
$app_list_strings['aow_date_type_list']['business_hours'] = 'Horari laboral';
$app_list_strings['aow_date_options']['now'] = 'Ara';
$app_list_strings['aow_date_options']['today'] = 'Avui';
$app_list_strings['aow_date_options']['field'] = 'Aquest camp';
$app_list_strings['aow_date_operator']['now'] = '';
$app_list_strings['aow_date_operator']['plus'] = '+';
$app_list_strings['aow_date_operator']['minus'] = '-';
$app_list_strings['aow_assign_options']['all'] = 'Tots els usuaris';
$app_list_strings['aow_assign_options']['role'] = 'Tots els usuaris de rol';
$app_list_strings['aow_assign_options']['security_group'] = 'Tots els usuaris del Grup de Seguretat';
$app_list_strings['aow_email_type_list']['Email Address'] = 'Correu electrònic';
$app_list_strings['aow_email_type_list']['Record Email'] = 'Registre de correu electrònic';
$app_list_strings['aow_email_type_list']['Related Field'] = 'Camp relacionat';
$app_list_strings['aow_email_type_list']['Specify User'] = 'Usuari';
$app_list_strings['aow_email_type_list']['Users'] = 'Usuaris';
$app_list_strings['aow_email_to_list']['to'] = 'Per a';
$app_list_strings['aow_email_to_list']['cc'] = 'Cc';
$app_list_strings['aow_email_to_list']['bcc'] = 'Cco';
$app_list_strings['aow_run_on_list']['All_Records'] = 'Tots els registres';
$app_list_strings['aow_run_on_list']['New_Records'] = 'Nous registres';
$app_list_strings['aow_run_on_list']['Modified_Records'] = 'Registres modificats';
$app_list_strings['aow_run_when_list']['Always'] = 'Sempre';
$app_list_strings['aow_run_when_list']['On_Save'] = 'Només al desar';
$app_list_strings['aow_run_when_list']['In_Scheduler'] = 'Només al planificador';

//gant
$app_list_strings['moduleList']['AM_ProjectTemplates'] = 'Plantilles de Projecte';
$app_list_strings['moduleList']['AM_TaskTemplates'] = 'Plantilles de Tasques de Projecte';
$app_list_strings['relationship_type_list']['FS'] = 'de principi a fi';
$app_list_strings['relationship_type_list']['SS'] = 'començar per començar';
$app_list_strings['duration_unit_dom']['Days'] = 'Dies';
$app_list_strings['duration_unit_dom']['Hours'] = 'Hores';
$app_strings['LBL_GANTT_BUTTON_LABEL'] = 'Veure gantt';
$app_strings['LBL_DETAIL_BUTTON_LABEL'] = 'Mostra els detalls';
$app_strings['LBL_CREATE_PROJECT'] = 'Crear Projecte';

//gmaps
$app_strings['LBL_MAP'] = 'Mapa';

$app_strings['LBL_JJWG_MAPS_LNG'] = 'Longitud';
$app_strings['LBL_JJWG_MAPS_LAT'] = 'Latitud';
$app_strings['LBL_JJWG_MAPS_GEOCODE_STATUS'] = 'Estat de Geocodificació';
$app_strings['LBL_JJWG_MAPS_ADDRESS'] = 'Adreça';

$app_list_strings['moduleList']['jjwg_Maps'] = 'Mapes';
$app_list_strings['moduleList']['jjwg_Markers'] = 'Marcadors de Mapa';
$app_list_strings['moduleList']['jjwg_Areas'] = 'Mapa d\'àrees';
$app_list_strings['moduleList']['jjwg_Address_Cache'] = 'Mapes - adreça memòria cau';

$app_list_strings['moduleList']['jjwp_Partners'] = 'JJWP socis';

$app_list_strings['map_unit_type_list']['mi'] = 'Milles';
$app_list_strings['map_unit_type_list']['km'] = 'Kilòmetres';

$app_list_strings['map_module_type_list']['Accounts'] = 'Comptes';
$app_list_strings['map_module_type_list']['Contacts'] = 'Contactes';
$app_list_strings['map_module_type_list']['Cases'] = 'Casos';
$app_list_strings['map_module_type_list']['Leads'] = 'Clients Potencials';
$app_list_strings['map_module_type_list']['Meetings'] = 'Reunions';
$app_list_strings['map_module_type_list']['Opportunities'] = 'Oportunitats';
$app_list_strings['map_module_type_list']['Project'] = 'Projectes';
$app_list_strings['map_module_type_list']['Prospects'] = 'Públic Objectiu';

$app_list_strings['map_relate_type_list']['Accounts'] = 'Comptes';
$app_list_strings['map_relate_type_list']['Contacts'] = 'Contactes';
$app_list_strings['map_relate_type_list']['Cases'] = 'Casos';
$app_list_strings['map_relate_type_list']['Leads'] = 'Clients Potencials';
$app_list_strings['map_relate_type_list']['Meetings'] = 'Reunions';
$app_list_strings['map_relate_type_list']['Opportunities'] = 'Oportunitats';
$app_list_strings['map_relate_type_list']['Project'] = 'Projectes';
$app_list_strings['map_relate_type_list']['Prospects'] = 'Públic Objectiu';

$app_list_strings['marker_image_list']['accident'] = 'Accident';
$app_list_strings['marker_image_list']['administration'] = 'Administració';
$app_list_strings['marker_image_list']['agriculture'] = 'Agricultura';
$app_list_strings['marker_image_list']['aircraft_small'] = 'Avioneta';
$app_list_strings['marker_image_list']['airplane_tourism'] = 'Avió Turístic';
$app_list_strings['marker_image_list']['airport'] = 'Aeroport';
$app_list_strings['marker_image_list']['amphitheater'] = 'Amfiteatre';
$app_list_strings['marker_image_list']['apartment'] = 'Apartament';
$app_list_strings['marker_image_list']['aquarium'] = 'Aquari';
$app_list_strings['marker_image_list']['arch'] = 'arc';
$app_list_strings['marker_image_list']['atm'] = 'atm';
$app_list_strings['marker_image_list']['audio'] = 'Àudio';
$app_list_strings['marker_image_list']['bank'] = 'Banc';
$app_list_strings['marker_image_list']['bank_euro'] = 'Banc Euro';
$app_list_strings['marker_image_list']['bank_pound'] = 'Banc Lliura';
$app_list_strings['marker_image_list']['bar'] = 'Barra';
$app_list_strings['marker_image_list']['beach'] = 'Platja';
$app_list_strings['marker_image_list']['beautiful'] = 'Bell';
$app_list_strings['marker_image_list']['bicycle_parking'] = 'Aparcament de bicicletes';
$app_list_strings['marker_image_list']['big_city'] = 'Gran Ciutat';
$app_list_strings['marker_image_list']['bridge'] = 'Pont';
$app_list_strings['marker_image_list']['bridge_modern'] = 'Pont Modern';
$app_list_strings['marker_image_list']['bus'] = 'Bus';
$app_list_strings['marker_image_list']['cable_car'] = 'Telefèric';
$app_list_strings['marker_image_list']['car'] = 'Cotxe';
$app_list_strings['marker_image_list']['car_rental'] = 'Lloguer de cotxes';
$app_list_strings['marker_image_list']['carrepair'] = 'Reparació d\'automòbils';
$app_list_strings['marker_image_list']['castle'] = 'Castell';
$app_list_strings['marker_image_list']['cathedral'] = 'Catedral';
$app_list_strings['marker_image_list']['chapel'] = 'Capella';
$app_list_strings['marker_image_list']['church'] = 'Esglèsia';
$app_list_strings['marker_image_list']['city_square'] = 'Àrea Central';
$app_list_strings['marker_image_list']['cluster'] = 'Clúster';
$app_list_strings['marker_image_list']['cluster_2'] = 'Clúster 2';
$app_list_strings['marker_image_list']['cluster_3'] = 'Clúster 3';
$app_list_strings['marker_image_list']['cluster_4'] = 'Clúster 4';
$app_list_strings['marker_image_list']['cluster_5'] = 'Clúster 5';
$app_list_strings['marker_image_list']['coffee'] = 'Cafè';
$app_list_strings['marker_image_list']['community_centre'] = 'Centre Comunitari';
$app_list_strings['marker_image_list']['company'] = 'Empresa';
$app_list_strings['marker_image_list']['conference'] = 'Conferència';
$app_list_strings['marker_image_list']['construction'] = 'Construcció';
$app_list_strings['marker_image_list']['convenience'] = 'Convivència';
$app_list_strings['marker_image_list']['court'] = 'Jutjat';
$app_list_strings['marker_image_list']['cruise'] = 'Creuer';
$app_list_strings['marker_image_list']['currency_exchange'] = 'Canvi de moneda';
$app_list_strings['marker_image_list']['customs'] = 'Aduana';
$app_list_strings['marker_image_list']['cycling'] = 'Ciclisme';
$app_list_strings['marker_image_list']['dam'] = 'presa';
$app_list_strings['marker_image_list']['dentist'] = 'Dentista';
$app_list_strings['marker_image_list']['deptartment_store'] = 'Grans Magatzems';
$app_list_strings['marker_image_list']['disability'] = 'Discapacitat';
$app_list_strings['marker_image_list']['disabled_parking'] = 'Aparcament per a discapacitats';
$app_list_strings['marker_image_list']['doctor'] = 'Doctor';
$app_list_strings['marker_image_list']['dog_leash'] = 'Corretja de gossos';
$app_list_strings['marker_image_list']['down'] = 'A baix';
$app_list_strings['marker_image_list']['down_left'] = 'A baix a l\'esquerra';
$app_list_strings['marker_image_list']['down_right'] = 'A baix a la dreta';
$app_list_strings['marker_image_list']['down_then_left'] = 'A baix i després a l\'esquerra';
$app_list_strings['marker_image_list']['down_then_right'] = 'A baix i després a la dreta';
$app_list_strings['marker_image_list']['drugs'] = 'Drogues';
$app_list_strings['marker_image_list']['elevator'] = 'Elevador';
$app_list_strings['marker_image_list']['embassy'] = 'Embaixada';
$app_list_strings['marker_image_list']['expert'] = 'Expert';
$app_list_strings['marker_image_list']['factory'] = 'Fàbrica';
$app_list_strings['marker_image_list']['falling_rocks'] = 'Zona de desprendiments';
$app_list_strings['marker_image_list']['fast_food'] = 'Menjar ràpid';
$app_list_strings['marker_image_list']['festival'] = 'Festival';
$app_list_strings['marker_image_list']['fjord'] = 'Fiord';
$app_list_strings['marker_image_list']['forest'] = 'Bosc';
$app_list_strings['marker_image_list']['fountain'] = 'Font';
$app_list_strings['marker_image_list']['friday'] = 'Divendres';
$app_list_strings['marker_image_list']['garden'] = 'Jardí';
$app_list_strings['marker_image_list']['gas_station'] = 'Benzinera';
$app_list_strings['marker_image_list']['geyser'] = 'Guèiser';
$app_list_strings['marker_image_list']['gifts'] = 'Regals';
$app_list_strings['marker_image_list']['gourmet'] = 'Gourmet';
$app_list_strings['marker_image_list']['grocery'] = 'Magatzem';
$app_list_strings['marker_image_list']['hairsalon'] = 'Perruqueria';
$app_list_strings['marker_image_list']['helicopter'] = 'Helicòpter';
$app_list_strings['marker_image_list']['highway'] = 'Autopista';
$app_list_strings['marker_image_list']['historical_quarter'] = 'Casc antic';
$app_list_strings['marker_image_list']['home'] = 'Inici';
$app_list_strings['marker_image_list']['hospital'] = 'Hospital';
$app_list_strings['marker_image_list']['hostel'] = 'Hostal';
$app_list_strings['marker_image_list']['hotel'] = 'Hotel';
$app_list_strings['marker_image_list']['hotel_1_star'] = 'Hotel 1 Estrella';
$app_list_strings['marker_image_list']['hotel_2_stars'] = 'Hotel 2 Estrelles';
$app_list_strings['marker_image_list']['hotel_3_stars'] = 'Hotel 3 Estrelles';
$app_list_strings['marker_image_list']['hotel_4_stars'] = 'Hotel 4 Estrelles';
$app_list_strings['marker_image_list']['hotel_5_stars'] = 'Hotel 5 Estrelles';
$app_list_strings['marker_image_list']['info'] = 'Informació';
$app_list_strings['marker_image_list']['justice'] = 'Jutjat';
$app_list_strings['marker_image_list']['lake'] = 'Llac';
$app_list_strings['marker_image_list']['laundromat'] = 'Bugaderia';
$app_list_strings['marker_image_list']['left'] = 'Esquerra';
$app_list_strings['marker_image_list']['left_then_down'] = 'Esquerra i després a baix';
$app_list_strings['marker_image_list']['left_then_up'] = 'Esquerra i després a dalt';
$app_list_strings['marker_image_list']['library'] = 'Biblioteca';
$app_list_strings['marker_image_list']['lighthouse'] = 'Il·luminació';
$app_list_strings['marker_image_list']['liquor'] = 'Licor';
$app_list_strings['marker_image_list']['lock'] = 'Candau';
$app_list_strings['marker_image_list']['main_road'] = 'Carretera principal';
$app_list_strings['marker_image_list']['massage'] = 'Massatge';
$app_list_strings['marker_image_list']['mobile_phone_tower'] = 'Torre de telefonia mòbil';
$app_list_strings['marker_image_list']['modern_tower'] = 'Torre Moderna';
$app_list_strings['marker_image_list']['monastery'] = 'Monestir';
$app_list_strings['marker_image_list']['monday'] = 'Dilluns';
$app_list_strings['marker_image_list']['monument'] = 'Monument';
$app_list_strings['marker_image_list']['mosque'] = 'Mesquita';
$app_list_strings['marker_image_list']['motorcycle'] = 'Moto';
$app_list_strings['marker_image_list']['museum'] = 'Museu';
$app_list_strings['marker_image_list']['music_live'] = 'Música en directe';
$app_list_strings['marker_image_list']['oil_pump_jack'] = 'Jack bomba d\'oli';
$app_list_strings['marker_image_list']['pagoda'] = 'Pagoda';
$app_list_strings['marker_image_list']['palace'] = 'Palau';
$app_list_strings['marker_image_list']['panoramic'] = 'Panoràmica';
$app_list_strings['marker_image_list']['park'] = 'Parc';
$app_list_strings['marker_image_list']['park_and_ride'] = 'Parc i passeig';
$app_list_strings['marker_image_list']['parking'] = 'Aparcament';
$app_list_strings['marker_image_list']['photo'] = 'Foto';
$app_list_strings['marker_image_list']['picnic'] = 'Pícnic';
$app_list_strings['marker_image_list']['places_unvisited'] = 'Llocs no visitats';
$app_list_strings['marker_image_list']['places_visited'] = 'Llocs visitats';
$app_list_strings['marker_image_list']['playground'] = 'Plaça';
$app_list_strings['marker_image_list']['police'] = 'Policia';
$app_list_strings['marker_image_list']['port'] = 'Port';
$app_list_strings['marker_image_list']['postal'] = 'Postal';
$app_list_strings['marker_image_list']['power_line_pole'] = 'Pal de línia elèctrica';
$app_list_strings['marker_image_list']['power_plant'] = 'Planta d\'energia';
$app_list_strings['marker_image_list']['power_substation'] = 'Subestació d\'energia';
$app_list_strings['marker_image_list']['public_art'] = 'Art públic';
$app_list_strings['marker_image_list']['rain'] = 'Pluja';
$app_list_strings['marker_image_list']['real_estate'] = 'Estat real';
$app_list_strings['marker_image_list']['regroup'] = 'Reagrupament';
$app_list_strings['marker_image_list']['resort'] = 'resort';
$app_list_strings['marker_image_list']['restaurant'] = 'Restaurant';
$app_list_strings['marker_image_list']['restaurant_african'] = 'Restaurant Africà';
$app_list_strings['marker_image_list']['restaurant_barbecue'] = 'Restaurant Barbacoa';
$app_list_strings['marker_image_list']['restaurant_buffet'] = 'Restaurant de bufet';
$app_list_strings['marker_image_list']['restaurant_chinese'] = 'Restaurant Xinès';
$app_list_strings['marker_image_list']['restaurant_fish'] = 'Restaurant Marisc';
$app_list_strings['marker_image_list']['restaurant_fish_chips'] = 'Restaurant Chips de Peix';
$app_list_strings['marker_image_list']['restaurant_gourmet'] = 'Restaurant Gourmet';
$app_list_strings['marker_image_list']['restaurant_greek'] = 'Restaurant Grec';
$app_list_strings['marker_image_list']['restaurant_indian'] = 'Restaurant Indi';
$app_list_strings['marker_image_list']['restaurant_italian'] = 'Restaurant Italià';
$app_list_strings['marker_image_list']['restaurant_japanese'] = 'Restaurant Japonès';
$app_list_strings['marker_image_list']['restaurant_kebab'] = 'Restaurant Kebab';
$app_list_strings['marker_image_list']['restaurant_korean'] = 'Restaurant Coreà';
$app_list_strings['marker_image_list']['restaurant_mediterranean'] = 'Restaurant Mediterrani';
$app_list_strings['marker_image_list']['restaurant_mexican'] = 'Restaurant Mexicà';
$app_list_strings['marker_image_list']['restaurant_romantic'] = 'Restaurant Romàntic';
$app_list_strings['marker_image_list']['restaurant_thai'] = 'Restaurant Tailandès';
$app_list_strings['marker_image_list']['restaurant_turkish'] = 'Restaurant Turc';
$app_list_strings['marker_image_list']['right'] = 'Dreta';
$app_list_strings['marker_image_list']['right_then_down'] = 'A la dreta i després a baix';
$app_list_strings['marker_image_list']['right_then_up'] = 'A la dreta i després a dalt';
$app_list_strings['marker_image_list']['saturday'] = 'Dissabte';
$app_list_strings['marker_image_list']['school'] = 'Escola';
$app_list_strings['marker_image_list']['shopping_mall'] = 'Centre Comercial';
$app_list_strings['marker_image_list']['shore'] = 'Apuntalament';
$app_list_strings['marker_image_list']['sight'] = 'Vista';
$app_list_strings['marker_image_list']['small_city'] = 'Ciutat petita';
$app_list_strings['marker_image_list']['snow'] = 'Neu';
$app_list_strings['marker_image_list']['spaceport'] = 'Port espacial';
$app_list_strings['marker_image_list']['speed_100'] = 'Velocitat 100';
$app_list_strings['marker_image_list']['speed_110'] = 'Velocitat 110';
$app_list_strings['marker_image_list']['speed_120'] = 'Velocitat 120';
$app_list_strings['marker_image_list']['speed_130'] = 'Velocitat 130';
$app_list_strings['marker_image_list']['speed_20'] = 'Velocitat 20';
$app_list_strings['marker_image_list']['speed_30'] = 'Velocitat 30';
$app_list_strings['marker_image_list']['speed_40'] = 'Velocitat 40';
$app_list_strings['marker_image_list']['speed_50'] = 'Velocitat 50';
$app_list_strings['marker_image_list']['speed_60'] = 'Velocitat 60';
$app_list_strings['marker_image_list']['speed_70'] = 'Velocitat 70';
$app_list_strings['marker_image_list']['speed_80'] = 'Velocitat 80';
$app_list_strings['marker_image_list']['speed_90'] = 'Velocitat 90';
$app_list_strings['marker_image_list']['speed_hump'] = 'velocitat hump';
$app_list_strings['marker_image_list']['stadium'] = 'Estadi';
$app_list_strings['marker_image_list']['statue'] = 'Estatua';
$app_list_strings['marker_image_list']['steam_train'] = 'Tren a Vapor';
$app_list_strings['marker_image_list']['stop'] = 'Stop';
$app_list_strings['marker_image_list']['stoplight'] = 'Semàfor';
$app_list_strings['marker_image_list']['subway'] = 'Subterrani';
$app_list_strings['marker_image_list']['sun'] = 'dg.';
$app_list_strings['marker_image_list']['sunday'] = 'Diumenge';
$app_list_strings['marker_image_list']['supermarket'] = 'Supermercat';
$app_list_strings['marker_image_list']['synagogue'] = 'Sinagoga';
$app_list_strings['marker_image_list']['tapas'] = 'Tapes';
$app_list_strings['marker_image_list']['taxi'] = 'Taxi';
$app_list_strings['marker_image_list']['taxiway'] = 'via de taxi';
$app_list_strings['marker_image_list']['teahouse'] = 'Casa de te';
$app_list_strings['marker_image_list']['telephone'] = 'Telèfon';
$app_list_strings['marker_image_list']['temple_hindu'] = 'Temple Hindú';
$app_list_strings['marker_image_list']['terrace'] = 'Terrassa';
$app_list_strings['marker_image_list']['text'] = 'Text';
$app_list_strings['marker_image_list']['theater'] = 'Teatre';
$app_list_strings['marker_image_list']['theme_park'] = 'Parc Temàtic';
$app_list_strings['marker_image_list']['thursday'] = 'Dijous';
$app_list_strings['marker_image_list']['toilets'] = 'Lavabos';
$app_list_strings['marker_image_list']['toll_station'] = 'Peatge';
$app_list_strings['marker_image_list']['tower'] = 'Torre';
$app_list_strings['marker_image_list']['traffic_enforcement_camera'] = 'Control de Velocitat';
$app_list_strings['marker_image_list']['train'] = 'Tren';
$app_list_strings['marker_image_list']['tram'] = 'Tramvia';
$app_list_strings['marker_image_list']['truck'] = 'Camió';
$app_list_strings['marker_image_list']['tuesday'] = 'Dimarts';
$app_list_strings['marker_image_list']['tunnel'] = 'Túnel';
$app_list_strings['marker_image_list']['turn_left'] = 'Gir a l\'esquerra';
$app_list_strings['marker_image_list']['turn_right'] = 'Gir a la dreta';
$app_list_strings['marker_image_list']['university'] = 'Universitat';
$app_list_strings['marker_image_list']['up'] = 'A dalt';
$app_list_strings['marker_image_list']['up_left'] = 'A dalt a la esquerra';
$app_list_strings['marker_image_list']['up_right'] = 'A dalt a la dreta';
$app_list_strings['marker_image_list']['up_then_left'] = 'A dalt i després a l\'esquerra';
$app_list_strings['marker_image_list']['up_then_right'] = 'A dalt i després a la dreta';
$app_list_strings['marker_image_list']['vespa'] = 'Vespa';
$app_list_strings['marker_image_list']['video'] = 'Vídeo';
$app_list_strings['marker_image_list']['villa'] = 'Vila';
$app_list_strings['marker_image_list']['water'] = 'Aigua';
$app_list_strings['marker_image_list']['waterfall'] = 'Cascada';
$app_list_strings['marker_image_list']['watermill'] = 'Molí d\'aigua';
$app_list_strings['marker_image_list']['waterpark'] = 'Parc aquàtic';
$app_list_strings['marker_image_list']['watertower'] = 'Torre d\'aigua';
$app_list_strings['marker_image_list']['wednesday'] = 'Dimecres';
$app_list_strings['marker_image_list']['wifi'] = 'WiFi';
$app_list_strings['marker_image_list']['wind_turbine'] = 'Aerogenerador';
$app_list_strings['marker_image_list']['windmill'] = 'Molí de vent';
$app_list_strings['marker_image_list']['winery'] = 'Celler';
$app_list_strings['marker_image_list']['work_office'] = 'Oficina de Treball';
$app_list_strings['marker_image_list']['world_heritage_site'] = 'Patrimoni de la Humanitat';
$app_list_strings['marker_image_list']['zoo'] = 'Zoològic';

//Reschedule
$app_list_strings['call_reschedule_dom'][''] = '';
$app_list_strings['call_reschedule_dom']['Out of Office'] = 'Fora d\'Oficina';
$app_list_strings['call_reschedule_dom']['In a Meeting'] = 'A una reunió';

$app_strings['LBL_RESCHEDULE_LABEL'] = 'Replanificar';
$app_strings['LBL_RESCHEDULE_TITLE'] = 'Si us plau, ingressi les dades de la replanificació ';
$app_strings['LBL_RESCHEDULE_DATE'] = 'Data:';
$app_strings['LBL_RESCHEDULE_REASON'] = 'Raó:';
$app_strings['LBL_RESCHEDULE_ERROR1'] = 'Si us plau, seleccioni una data vàlida';
$app_strings['LBL_RESCHEDULE_ERROR2'] = 'Si us plau, seleccioni una raó';

$app_strings['LBL_RESCHEDULE_PANEL'] = 'Replanificar';
$app_strings['LBL_RESCHEDULE_HISTORY'] = 'Historial d\'intents de trucada';
$app_strings['LBL_RESCHEDULE_COUNT'] = 'Intents de trucada';

//SecurityGroups
$app_list_strings['moduleList']['SecurityGroups'] = 'Grups de Seguretat';
$app_strings['LBL_SECURITYGROUP'] = 'Grup de Seguretat';

$app_list_strings['moduleList']['OutboundEmailAccounts'] = 'Comptes d\'E-mail sortints';

//social
$app_strings['FACEBOOK_USER_C'] = 'Facebook';
$app_strings['TWITTER_USER_C'] = 'Twitter';
$app_strings['LBL_PANEL_SOCIAL_FEED'] = 'Detalls del social feed';

$app_strings['LBL_SUBPANEL_FILTER_LABEL'] = 'Filtre';

$app_strings['LBL_COLLECTION_TYPE'] = 'Tipus';

$app_strings['LBL_ADD_TAB'] = 'Afegir pestanya';
$app_strings['LBL_EDIT_TAB'] = 'Editar pestanyes';
$app_strings['LBL_SUITE_DASHBOARD'] = 'Tauler de control de SuiteCRM'; //Can be translated in all caps. This string will be used by SuiteP template menu actions
$app_strings['LBL_ENTER_DASHBOARD_NAME'] = 'Entrar el nom del tauler de control:';
$app_strings['LBL_NUMBER_OF_COLUMNS'] = 'Nombre de columnes:';
$app_strings['LBL_DELETE_DASHBOARD1'] = 'Està segur que vol eliminar el';
$app_strings['LBL_DELETE_DASHBOARD2'] = 'tauler de control?';
$app_strings['LBL_ADD_DASHBOARD_PAGE'] = 'Afegir una pàgina de tauler de control';
$app_strings['LBL_DELETE_DASHBOARD_PAGE'] = 'Eliminar la pàgina de tauler de control actual';
$app_strings['LBL_RENAME_DASHBOARD_PAGE'] = 'Renombrar pàgina de control';
$app_strings['LBL_SUITE_DASHBOARD_ACTIONS'] = 'Accions'; //Can be translated in all caps. This string will be used by SuiteP template menu actions

$app_list_strings['collection_temp_list'] = array(
    'Tasks' => 'Tasques',
    'Meetings' => 'Reunions',
    'Calls' => 'Trucades',
    'Notes' => 'Notes',
    'Emails' => 'Correus electrònics'
);

$app_list_strings['moduleList']['TemplateEditor'] = 'Editor de plantilla';
$app_strings['LBL_CONFIRM_CANCEL_INLINE_EDITING'] = "Vostè ha fet clic fora del camp que estàveu editant sense desar-lo. Clic correcte si està d'acord a perdre el seu canvi, o cancel·li si voleu continuar editant-lo";
$app_strings['LBL_LOADING_ERROR_INLINE_EDITING'] = "Hi va haver un error en carregar el camp. La sessió pot haver-se exhaurit. Si us plau, torneu a iniciar per arreglar això";

//SuiteSpots
$app_list_strings['spots_areas'] = array(
    'getSalesSpotsData' => 'Vendes',
    'getAccountsSpotsData' => 'Comptes',
    'getLeadsSpotsData' => 'Clients Potencials',
    'getServiceSpotsData' => 'Servei',
    'getMarketingSpotsData' => 'Màrqueting',
    'getMarketingActivitySpotsData' => 'Activitat de màrqueting',
    'getActivitiesSpotsData' => 'Activitats',
    'getQuotesSpotsData' => 'Pressupostos'
);

$app_list_strings['moduleList']['Spots'] = 'Gràfiques';

$app_list_strings['moduleList']['AOBH_BusinessHours'] = 'Hores Laborals';
$app_list_strings['business_hours_list']['0'] = '12 am';
$app_list_strings['business_hours_list']['1'] = '1 am';
$app_list_strings['business_hours_list']['2'] = '2 am';
$app_list_strings['business_hours_list']['3'] = '3 am';
$app_list_strings['business_hours_list']['4'] = '4 am';
$app_list_strings['business_hours_list']['5'] = '5 am';
$app_list_strings['business_hours_list']['6'] = '6: 00';
$app_list_strings['business_hours_list']['7'] = '7: 00';
$app_list_strings['business_hours_list']['8'] = '8: 00';
$app_list_strings['business_hours_list']['9'] = '9h';
$app_list_strings['business_hours_list']['10'] = '10: 00';
$app_list_strings['business_hours_list']['11'] = '11h';
$app_list_strings['business_hours_list']['12'] = '12h';
$app_list_strings['business_hours_list']['13'] = '1 pm';
$app_list_strings['business_hours_list']['14'] = '2 pm';
$app_list_strings['business_hours_list']['15'] = '3 pm';
$app_list_strings['business_hours_list']['16'] = '4 pm';
$app_list_strings['business_hours_list']['17'] = '5 pm';
$app_list_strings['business_hours_list']['18'] = '6 pm';
$app_list_strings['business_hours_list']['19'] = '19: 00';
$app_list_strings['business_hours_list']['20'] = '20: 00';
$app_list_strings['business_hours_list']['21'] = '21:00';
$app_list_strings['business_hours_list']['22'] = '22:00';
$app_list_strings['business_hours_list']['23'] = '23:00';
$app_list_strings['day_list']['Monday'] = 'Dilluns';
$app_list_strings['day_list']['Tuesday'] = 'Dimarts';
$app_list_strings['day_list']['Wednesday'] = 'Dimecres';
$app_list_strings['day_list']['Thursday'] = 'Dijous';
$app_list_strings['day_list']['Friday'] = 'Divendres';
$app_list_strings['day_list']['Saturday'] = 'Dissabte';
$app_list_strings['day_list']['Sunday'] = 'Diumenge';
$app_list_strings['pdf_page_size_dom']['A4'] = 'A4';
$app_list_strings['pdf_page_size_dom']['Letter'] = 'Carta';
$app_list_strings['pdf_page_size_dom']['Legal'] = 'Legal';
$app_list_strings['pdf_orientation_dom']['Portrait'] = 'Retrat';
$app_list_strings['pdf_orientation_dom']['Landscape'] = 'Paisatge';
$app_list_strings['run_when_dom']['When True'] = 'Avalua al desar'; // PR 6143
$app_list_strings['run_when_dom']['Once True'] = 'Perpetu - (camp a auditar)';
$app_list_strings['sa_status_list']['Complete'] = 'Completa';
$app_list_strings['sa_status_list']['In_Review'] = 'En revisió';
$app_list_strings['sa_status_list']['Issue_Resolution'] = 'Resolució del problema';
$app_list_strings['sa_status_list']['Pending_Apttus_Submission'] = 'Registre d\'Apptus Pendent';
$app_list_strings['sharedGroupRule']['none'] = 'Sense Accés';
$app_list_strings['sharedGroupRule']['view'] = 'Només visualització';
$app_list_strings['sharedGroupRule']['view_edit'] = 'Veure i editar';
$app_list_strings['sharedGroupRule']['view_edit_delete'] = 'Veure, editar i esborrar';
$app_list_strings['moduleList']['SharedSecurityRulesFields'] = 'Compartir camps de normes de seguretat';
$app_list_strings['moduleList']['SharedSecurityRules'] = 'Compartir camps de normes de seguretat';
$app_list_strings['moduleList']['SharedSecurityRulesActions'] = 'Compartir accions de camps de normes de seguretat';
$app_list_strings['shared_email_type_list'][''] = '';
$app_list_strings['shared_email_type_list']['Specify User'] = 'Usuari';
$app_list_strings['shared_email_type_list']['Users'] = 'Usuaris';
$app_list_strings['aow_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aow_condition_type_list']['Field'] = 'Camp';
$app_list_strings['aow_condition_type_list']['Any_Change'] = 'Qualsevol canvi';
$app_list_strings['aow_condition_type_list']['SecurityGroup'] = 'En el Grup de Seguretat';
$app_list_strings['aow_condition_type_list']['currentUser'] = 'Usuari actiu';
$app_list_strings['aow_condition_type_list']['Date'] = 'Data';
$app_list_strings['aow_condition_type_list']['Multi'] = 'Un de';


$app_list_strings['moduleList']['SurveyResponses'] = 'Respostes a enquestes';
$app_list_strings['moduleList']['Surveys'] = 'Enquestes';
$app_list_strings['moduleList']['SurveyQuestionResponses'] = 'Respostes a pregunta d\'enquesta';
$app_list_strings['moduleList']['SurveyQuestions'] = 'Preguntes de l\'enquesta';
$app_list_strings['moduleList']['SurveyQuestionOptions'] = 'Opcions de pregunta d\'enquesta';
$app_list_strings['survey_status_list']['Draft'] = 'Esborrany';
$app_list_strings['survey_status_list']['Public'] = 'Públic';
$app_list_strings['survey_status_list']['Closed'] = 'Tancat';
$app_list_strings['surveys_question_type']['Text'] = 'Text';
$app_list_strings['surveys_question_type']['Textbox'] = 'Quadre de text';
$app_list_strings['surveys_question_type']['Checkbox'] = 'Casella de verificació';
$app_list_strings['surveys_question_type']['Radio'] = 'Ràdio';
$app_list_strings['surveys_question_type']['Dropdown'] = 'Llista desplegable';
$app_list_strings['surveys_question_type']['Multiselect'] = 'Multi-selecció';
$app_list_strings['surveys_question_type']['Matrix'] = 'Matriu';
$app_list_strings['surveys_question_type']['DateTime'] = 'Data i hora';
$app_list_strings['surveys_question_type']['Date'] = 'Data';
$app_list_strings['surveys_question_type']['Scale'] = 'Escala';
$app_list_strings['surveys_question_type']['Rating'] = 'Puntuació';
$app_list_strings['surveys_matrix_options'][0] = 'Satisfet';
$app_list_strings['surveys_matrix_options'][1] = 'Ni satisfet ni insatisfet';
$app_list_strings['surveys_matrix_options'][2] = 'Insatisfet';

$app_strings['LBL_OPT_IN_PENDING_EMAIL_NOT_SENT'] = 'Pendent de confirmar Autoritzat a enviar, confirmar Autoritzat a enviar no enviat';
$app_strings['LBL_OPT_IN_PENDING_EMAIL_FAILED'] = 'Confirmar Autoritzat a enviar a l\'e-mail d\'enviament ha fallat';
$app_strings['LBL_OPT_IN_PENDING_EMAIL_SENT'] = 'Pendent de confirmar Autoritzat a enviar, confirmar Autoritzat a enviar per enviaments';
$app_strings['LBL_OPT_IN'] = 'Autoritzat a enviar';
$app_strings['LBL_OPT_IN_CONFIRMED'] = 'Autoritzat a enviar Confirmat';
$app_strings['LBL_OPT_IN_OPT_OUT'] = 'Missatge Esborrat';
$app_strings['LBL_OPT_IN_INVALID'] = 'No Vàlid';

/** @see SugarEmailAddress */
$app_list_strings['email_settings_opt_in_dom'] = array(
    'not-opt-in' => 'Deshabilitat',
    'opt-in' => 'Autoritzat a enviar',
    'confirmed-opt-in' => 'Confirmat Autoritzat a enviar'
);

$app_list_strings['email_confirmed_opt_in_dom'] = array(
    'not-opt-in' => 'No Autoritzat a enviar',
    'opt-in' => 'Autoritzat a enviar',
    'confirmed-opt-in' => 'Confirmat Autoritzat a enviar'
);

$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL'] = 'El correu electrònic de confirmació per Autoritzat a enviar s\'ha afegit a la cua de correu electrònic d\'adreces d\'e-mail %s';
$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_NOT_OPT_IN'] = 'No es pot enviar correu electrònic a les adreces d\'e-mail de %s , perquè no estan Autoritzats a enviar. ';
$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_MISSING_EMAIL_ADDRESS_ID'] = 'L\'adreça de correu electrònic %s no té un id vàlid. ';

$app_strings['ERR_TWO_FACTOR_FAILED'] = 'L\'Autenticació en dos passos ha fallat';
$app_strings['ERR_TWO_FACTOR_CODE_SENT'] = 'S\'ha enviat el codi de l\'autenticació en dos passos.';
$app_strings['ERR_TWO_FACTOR_CODE_FAILED'] = 'El codi d\'autenticació de Factor doble no s\'ha pogut per enviar.';
$app_strings['LBL_THANKS_FOR_SUBMITTING'] = 'Moltes gràcies per haver enviat una pregunta.';

$app_strings['ERR_IP_CHANGE'] = 'La sessió va ser acabada per canvi significatiu de la seva adreça IP';
$app_strings['ERR_RETURN'] = 'Tornar a l\'Inici';


$app_list_strings['oauth2_grant_type_dom'] = array(
    'password' => 'Clau d\'autorització',
    'client_credentials' => 'Credencials de client',
    'implicit' => 'Implícit',
    'authorization_code' => 'Codi d\'autorització'
);

$app_list_strings['oauth2_duration_units'] = [
    'minute' => 'minuts',
    'hour' => 'hores',
    'day' => 'dies',
    'week' => 'setmanes',
    'month' => 'mesos',
];

$app_list_strings['search_controllers'] = [
    'Search' => 'Cerca (nou)',
    'UnifiedSearch' => 'Cerca Global Unificada'
];


$app_strings['LBL_DEFAULT_API_ERROR_TITLE'] = 'Error d\'API de JSON';
$app_strings['LBL_DEFAULT_API_ERROR_DETAIL'] = 'S\'ha produït l\'Error d\'API de JSON.';
$app_strings['LBL_API_EXCEPTION_DETAIL'] = 'Versió Api: 8';
$app_strings['LBL_BAD_REQUEST_EXCEPTION_DETAIL'] = 'Assegureu-vos que omplir els camps obligatoris';
$app_strings['LBL_EMPTY_BODY_EXCEPTION_DETAIL'] = 'JSON API espera el cos de la sol·licitud per ser JSON';
$app_strings['LBL_INVALID_JSON_API_REQUEST_EXCEPTION_DETAIL'] = 'Incapaç de validar la petició càrrega Api Json';
$app_strings['LBL_INVALID_JSON_API_RESPONSE_EXCEPTION_DETAIL'] = 'Incapaç de validar la resposta de càrrega d\'Api de Json';
$app_strings['LBL_MODULE_NOT_FOUND_EXCEPTION_DETAIL'] = 'JSON API no pot trobar recursos';
$app_strings['LBL_NOT_ACCEPTABLE_EXCEPTION_DETAIL'] = 'JSON API espera la capçalera "Accepta" per ser application/vnd.api+json';
$app_strings['LBL_UNSUPPORTED_MEDIA_TYPE_EXCEPTION_DETAIL'] = 'JSON API espera la capçalera "Content-Type" a application/vnd.api+json';

$app_strings['MSG_BROWSER_NOTIFICATIONS_ENABLED'] = 'Notificacions d\'escriptori ara estan habilitades per a aquest navegador web.';
$app_strings['MSG_BROWSER_NOTIFICATIONS_DISABLED'] = 'Escriptori notificacions estan inhabilitades per a aquest navegador web. Utilitzar les preferències per permetre\'ls una altra vegada.';
$app_strings['MSG_BROWSER_NOTIFICATIONS_UNSUPPORTED'] = 'Aquest navegador no dóna suport notificacions d\'escriptori.';

$app_strings['LBL_GOOGLE_SYNC_ERR'] = 'SuiteCRM Google Sync - ERROR';
$app_strings['LBL_THERE_WAS_AN_ERR'] = 'S\'ha produït un error: ';
$app_strings['LBL_CLICK_HERE'] = 'Cliqui aquí';
$app_strings['LBL_TO_CONTINUE'] = ' per continuar.';

$app_strings['IMAP_HANDLER_ERROR'] = 'ERROR: {error}; la clau és: "{key}".';
$app_strings['IMAP_HANDLER_SUCCESS'] = 'OK: paràmetres de prova canviats a "{key}"';
$app_strings['IMAP_HANDLER_ERROR_INVALID_REQUEST'] = 'Petició incorrecta, utilitzeu el valor "{var}".';
$app_strings['IMAP_HANDLER_ERROR_UNKNOWN_BY_KEY'] = 'Ha ocorregut un error desconegut, la clau "{key}" no s\'ha guardat.';
$app_strings['IMAP_HANDLER_ERROR_NO_TEST_SET'] = 'Els paràmetres de prova no existeixen.';
$app_strings['IMAP_HANDLER_ERROR_NO_KEY'] = 'Clau no trobada.';
$app_strings['IMAP_HANDLER_ERROR_KEY_SAVE'] = 'Error guardant la clau.';
$app_strings['IMAP_HANDLER_ERROR_UNKNOWN'] = 'Error desconegut';
$app_strings['LBL_SEARCH_TITLE']                   = 'Cerca';
$app_strings['LBL_SEARCH_TEXT_FIELD_TITLE_ATTR']   = 'Criteri de cerca';
$app_strings['LBL_SEARCH_SUBMIT_FIELD_TITLE_ATTR'] = 'Cerca';
$app_strings['LBL_SEARCH_SUBMIT_FIELD_VALUE']      = 'Cerca';
$app_strings['LBL_SEARCH_QUERY']                   = 'Consulta a cercar: ';
$app_strings['LBL_SEARCH_RESULTS_PER_PAGE']        = 'Resultats per pàgina: ';
$app_strings['LBL_SEARCH_ENGINE']                  = 'Motor: ';
$app_strings['LBL_SEARCH_TOTAL'] = 'Total de resultats: ';
$app_strings['LBL_SEARCH_PREV'] = 'Anterior';
$app_strings['LBL_SEARCH_NEXT'] = 'Següent';
$app_strings['LBL_SEARCH_PAGE'] = 'Pàgina ';
$app_strings['LBL_SEARCH_OF'] = ' de '; // Usage: Page 1 of 5
$app_strings['LBL_USE_ADVANCED_SEARCH'] = 'Fes servir la cerca avançada';
$app_strings['LBL_USE_BASIC_SEARCH'] = 'Fes servir la cerca bàsica';

// PDF Engines
$app_strings['LBL_LEGACY_MPDF_ENGINE'] = 'Motor MPDF obsolet';
$app_strings['LBL_TCPDF_ENGINE'] = 'Motor TCPDF';

$app_strings['ERR_INVALID_FILE_NAME'] = 'El nom del fitxer no és vàlid:';
$app_strings['LBL_LOGGER_VALID_FILENAME_CHARACTERS'] = 'Només pot contenir caràcters alfanumèrics, el punt (.), el guionet (-) o el guió baix (_).';
$app_strings['LBL_LOGGER_INVALID_FILENAME'] = "El nom del fitxer d'importació no és vàlid";

$app_strings['LBL_PASSWORD_SET_NEW_VALUE_TO_RESET'] = 'Contrasenya establerta. Introduïu un valor per establir-ne una de nova.';
$app_strings['LBL_VALUE_SET_PLACEHOLDER'] = 'Valor establert. Introduïu-ne un de nou per sobreescriure el valor actual.';
