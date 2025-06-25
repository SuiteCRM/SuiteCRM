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
//e.g. auf Deutsch 'Contacts'=>'Contakten',
    'language_pack_name' => 'Galego (España) - gl_ES',
    'moduleList' => array(
        'Home' => 'Inicio',
        'ResourceCalendar' => 'Calendario de recursos',
        'Contacts' => 'Contactos',
        'Accounts' => 'Contas',
        'Alerts' => 'Alertas',
        'Opportunities' => 'Oportunidades',
        'Cases' => 'Casos',
        'Notes' => 'Notas',
        'Calls' => 'Chamadas',
        'TemplateSectionLine' => 'Liña de sección de plantilla',
        'Calls_Reschedule' => 'Reprogramación de chamadas',
        'Emails' => 'Correos',
        'EAPM' => 'EAPM',
        'Meetings' => 'Reunións',
        'Tasks' => 'Tarefas',
        'Calendar' => 'Calendario',
        'Leads' => 'Clientes Potenciais',
        'Currencies' => 'Moedas',
        'Activities' => 'Actividades',
        'Bugs' => 'Incidencias',
        'Feeds' => 'RSS',
        'iFrames' => 'Os Meus Sitios',
        'TimePeriods' => 'Períodos de Tempo',
        'ContractTypes' => 'Tipos de Contrato',
        'Schedulers' => 'Planificadores',
        'Project' => 'Proxectos',
        'ProjectTask' => 'Tarefas de Proxecto',
        'Campaigns' => 'Campañas',
        'CampaignLog' => 'Rexistro de Campañas',
        'Documents' => 'Documentos',
        'DocumentRevisions' => 'Versións de documento',
        'Connectors' => 'Conectores',
        'Roles' => 'Roles',
        'Notifications' => 'Notificacións',
        'Sync' => 'Sincronizar',
        'Users' => 'Usuarios',
        'Employees' => 'Empregados',
        'Administration' => 'Administración',
        'ACLRoles' => 'Roles',
        'InboundEmail' => 'Correo Entrante',
        'Releases' => 'Lanzamentos',
        'Prospects' => 'Público Obxectivo',
        'Queues' => 'Colas',
        'EmailMarketing' => 'Marketing por Email',
        'EmailTemplates' => 'Correo electrónico - Plantillas',
        'ProspectLists' => 'Público Obxectivo - Listas',
        'SavedSearch' => 'Buscas Gardadas',
        'UpgradeWizard' => 'Asistente de Actualizacións',
        'Trackers' => 'Monitorización',
        'TrackerSessions' => 'Monitorización de Sesións',
        'TrackerQueries' => 'Consultas de Monitorización',
        'FAQ' => 'FAQ',
        'Newsletters' => 'Boletíns de Noticias',
        'SugarFeed' => 'SuiteCRM alimentación',
        'SugarFavorites' => 'Favoritos',

        'OAuthKeys' => 'Claves do Consumidor OAuth',
        'OAuthTokens' => 'Tokens OAuth',
        'OAuth2Clients' => 'Clientes de OAuth',
        'OAuth2Tokens' => 'Tokens OAuth',
    ),

    'moduleListSingular' => array(
        'Home' => 'Inicio',
        'Dashboard' => 'Cadro de Mando',
        'Contacts' => 'Contacto',
        'Accounts' => 'Conta',
        'Opportunities' => 'Oportunidade',
        'Cases' => 'Caso',
        'Notes' => 'Nota',
        'Calls' => 'Chamada',
        'Emails' => 'Email',
        'EmailTemplates' => 'Plantilla de Email',
        'Meetings' => 'Reunión',
        'Tasks' => 'Tarefa',
        'Calendar' => 'Calendario',
        'Leads' => 'Cliente Potencial',
        'Activities' => 'Actividades',
        'Bugs' => 'Incidencia',
        'KBDocuments' => 'Base de Coñecemento',
        'Feeds' => 'RSS',
        'iFrames' => 'Os Meus Sitios',
        'TimePeriods' => 'Período de Tempo',
        'Project' => 'Proxecto',
        'ProjectTask' => 'Tarefa de Proxecto',
        'Prospects' => 'Público Obxectivo',
        'Campaigns' => 'Campaña',
        'Documents' => 'Documentos',
        'Sync' => 'Sincronización',
        'Users' => 'Usuarios',
        'SugarFavorites' => 'Favoritos',

    ),

    'checkbox_dom' => array(
        '' => '',
        '1' => 'Si',
        '2' => 'Non',
    ),

    //e.g. en francais 'Analyst'=>'Analyste',
    'account_type_dom' => array(
        '' => '',
        'Analyst' => 'Analista',
        'Competitor' => 'Competidor',
        'Customer' => 'Cliente',
        'Integrator' => 'Integrador',
        'Investor' => 'Inversor',
        'Partner' => 'Socio',
        'Press' => 'Prensa',
        'Prospect' => 'Prospecto',
        'Reseller' => 'Revendedor',
        'Other' => 'Outro',
    ),
    //e.g. en espanol 'Apparel'=>'Ropa',
    'industry_dom' => array(
        '' => '',
        'Apparel' => 'Textil',
        'Banking' => 'Banca',
        'Biotechnology' => 'Biotecnoloxía',
        'Chemicals' => 'Química',
        'Communications' => 'Comunicacións',
        'Construction' => 'Construción',
        'Consulting' => 'Consultoría',
        'Education' => 'Educación',
        'Electronics' => 'Electronica',
        'Energy' => 'Enerxía',
        'Engineering' => 'Enxeñería',
        'Entertainment' => 'Entretemento',
        'Environmental' => 'Medio ambiente',
        'Finance' => 'Finanzas',
        'Government' => 'Goberno',
        'Healthcare' => 'Sanidade',
        'Hospitality' => 'Caridade',
        'Insurance' => 'Seguros',
        'Machinery' => 'Maquinaria',
        'Manufacturing' => 'Fabricación',
        'Media' => 'Medios de comunicación',
        'Not For Profit' => 'Sen ánimo de lucro',
        'Recreation' => 'Ocio',
        'Retail' => 'Minoristas',
        'Shipping' => 'Envíos',
        'Technology' => 'Tecnoloxía',
        'Telecommunications' => 'Telecomunicacións',
        'Transportation' => 'Transporte',
        'Utilities' => 'Servizos públicos',
        'Other' => 'Outro',
    ),
    'lead_source_default_key' => 'Self Generated',
    'lead_source_dom' => array(
        '' => '',
        'Cold Call' => 'Chamada en Frío',
        'Existing Customer' => 'Cliente Existente',
        'Self Generated' => 'Auto Xerado',
        'Employee' => 'Empregado',
        'Partner' => 'Socio',
        'Public Relations' => 'Relacións Públicas',
        'Direct Mail' => 'Correo Directo',
        'Conference' => 'Conferencia',
        'Trade Show' => 'Exposición',
        'Web Site' => 'Sitio Web',
        'Word of mouth' => 'Recomendación',
        'Email' => 'Email',
        'Campaign' => 'Campaña',
        'Other' => 'Outro',
    ),
    'language_dom' => array(
        'af' => 'Africano',
        'ar-EG' => 'Árabe (Exipto)',
        'ar-SA' => 'Árabe (Arabia Saudita)',
        'az' => 'Azerí',
        'bg' => 'Búlgaro',
        'bn' => 'Bengalí',
        'bs' => 'Bosnio',
        'ca' => 'Catalán',
        'ceb' => 'Cebuano',
        'cs' => 'Checo',
        'da' => 'Danés',
        'de' => 'Alemán',
        'de-CH' => 'Alemán (Suiza)',
        'el' => 'Grego',
        'en-GB' => 'Inglés, Reino Unido',
        'en-US' => 'Inglés, Estados Unidos',
        'es-ES' => 'Español',
        'es-MX' => 'Español, México',
        'es-PY' => 'Español, Paraguai',
        'es-VE' => 'Español, Venezuela',
        'et' => 'Estonio',
        'eu' => 'Vasco',
        'fa' => 'Persa',
        'fi' => 'Filipino',
        'fil' => 'Finés',
        'fr' => 'Francés',
        'fr-CA' => 'Francés, Canadá',
        'gu-IN' => 'Gujarati',
        'he' => 'Hebreo',
        'hi' => 'Hindi',
        'hr' => 'Croata',
        'hu' => 'Húngaro',
        'hy-AM' => 'Armenio',
        'id' => 'Indonesio',
        'it' => 'Italiano',
        'ja' => 'Xaponés',
        'ka' => 'Xeorxiano',
        'ko' => 'Coreano',
        'lt' => 'Lituano',
        'lv' => 'Letón',
        'mk' => 'Macedonio',
        'nb' => 'Noruego (Bokmal)',
        'nl' => 'Holandés',
        'pcm' => 'Pidgin nigeriano',
        'pl' => 'Polaco',
        'pt-BR' => 'Portugués, Brasileño',
        'pt-PT' => 'Portugués',
        'ro' => 'Rumano',
        'ru' => 'Ruso',
        'si-LK' => 'Cingalés',
        'sk' => 'Eslovaco',
        'sl' => 'Esloveno',
        'sq' => 'Albanés',
        'sr-CS' => 'Serbio (Latín)',
        'sv-SE' => 'Sueco',
        'th' => 'Tailandés',
        'tl' => 'Tagalo',
        'tr' => 'Turco',
        'uk' => 'Ucraniano',
        'ur-IN' => 'Urdu (India)',
        'ur-PK' => 'Urdu (Pakistán)',
        'vi' => 'Vietnamita',
        'yo' => 'Yoruba',
        'zh-CN' => 'Chino simplificado',
        'zh-TW' => 'Chino tradicional',
        'other' => 'Outro',
    ),
    'opportunity_type_dom' => array(
        '' => '',
        'Existing Business' => 'Negocios Existentes',
        'New Business' => 'Novos Negocios',
    ),
    'roi_type_dom' => array(
        'Revenue' => 'Ingresos',
        'Investment' => 'Inversión',
        'Expected_Revenue' => 'Ingresos Esperados',
        'Budget' => 'Presuposto',

    ),
    //Note:  do not translate opportunity_relationship_type_default_key
//       it is the key for the default opportunity_relationship_type_dom value
    'opportunity_relationship_type_default_key' => 'Primary Decision Maker',
    'opportunity_relationship_type_dom' => array(
        '' => '',
        'Primary Decision Maker' => 'Tomador de Decisión Principal',
        'Business Decision Maker' => 'Tomador de Decisión de Negocio',
        'Business Evaluator' => 'Evaluador de Negocio',
        'Technical Decision Maker' => 'Tomador de Decisión Técnica',
        'Technical Evaluator' => 'Evaluador Técnico',
        'Executive Sponsor' => 'Patrocinador Executivo',
        'Influencer' => 'Influenciador',
        'Other' => 'Outro',
    ),
    //Note:  do not translate case_relationship_type_default_key
//       it is the key for the default case_relationship_type_dom value
    'case_relationship_type_default_key' => 'Primary Contact',
    'case_relationship_type_dom' => array(
        '' => '',
        'Primary Contact' => 'Contacto Principal',
        'Alternate Contact' => 'Contacto Alternativo',
    ),
    'payment_terms' => array(
        '' => '',
        'Net 15' => 'Neto 15',
        'Net 30' => 'Neto 30',
    ),
    'sales_stage_default_key' => 'Prospecting',
    'sales_stage_dom' => array(
        'Prospecting' => 'Prospecto',
        'Qualification' => 'Cualificación',
        'Needs Analysis' => 'Necesita Análise',
        'Value Proposition' => 'Proposta de Valor',
        'Id. Decision Makers' => 'Identificar aos tomadores de decisión',
        'Perception Analysis' => 'Análise de Percepción',
        'Proposal/Price Quote' => 'Proposta/presuposto',
        'Negotiation/Review' => 'Negociación/Revisión',
        'Closed Won' => 'Gañado',
        'Closed Lost' => 'Perdido',
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
        'Call' => 'Chamada',
        'Meeting' => 'Reunión',
        'Task' => 'Tarefa',
        'Email' => 'Email',
        'Note' => 'Nota',
    ),
    'salutation_dom' => array(
        '' => '',
        'Mr.' => 'Sr.',
        'Ms.' => 'Sra.',
        'Mrs.' => 'Sra.',
        'Miss' => 'Srta.',
        'Dr.' => 'Dr.',
        'Prof.' => 'Prof.',
    ),
    //time is in seconds; the greater the time the longer it takes;
    'reminder_max_time' => 90000,
    'reminder_time_options' => array(
        60 => '1 minuto antes',
        300 => '5 minutos antes',
        600 => '10 minutos antes',
        900 => '15 minutos antes',
        1800 => '30 minutos antes',
        3600 => '1 hora antes',
        7200 => '2 horas antes',
        10800 => '3 horas antes',
        18000 => '5 horas antes',
        86400 => '1 día antes',
    ),

    'task_priority_default' => 'Media',
    'task_priority_dom' => array(
        'High' => 'Alta',
        'Medium' => 'Media',
        'Low' => 'Baixa',
    ),
    'task_status_default' => 'Non Iniciado',
    'task_status_dom' => array(
        'Not Started' => 'Non Iniciada',
        'In Progress' => 'En Progreso',
        'Completed' => 'Completada',
        'Pending Input' => 'Pendente de Información',
        'Deferred' => 'Aprazada',
    ),
    'meeting_status_default' => 'Planned',
    'meeting_status_dom' => array(
        'Planned' => 'Planificada',
        'Held' => 'Realizada',
        'Not Held' => 'Non Realizada',
    ),
    'extapi_meeting_password' => array(
        'WebEx' => 'WebEx',
    ),
    'meeting_type_dom' => array(
        'Other' => 'Outro',
        'Sugar' => 'SuiteCRM',
    ),
    'call_status_default' => 'Planificada',
    'call_status_dom' => array(
        'Planned' => 'Planificada',
        'Held' => 'Realizada',
        'Not Held' => 'Non Realizada',
    ),
    'call_direction_default' => 'Outbound',
    'call_direction_dom' => array(
        'Inbound' => 'Entrante',
        'Outbound' => 'Saínte',
    ),
    'lead_status_dom' => array(
        '' => '',
        'New' => 'Novo',
        'Assigned' => 'Asignado',
        'In Process' => 'En Proceso',
        'Converted' => 'Convertido',
        'Recycled' => 'Reciclado',
        'Dead' => 'Morto',
    ),
    'case_priority_default_key' => 'P2',
    'case_priority_dom' => array(
        'P1' => 'Alta',
        'P2' => 'Media',
        'P3' => 'Baixa',
    ),
    'user_type_dom' => array(
        'RegularUser' => 'Usuario Normal',
        'Administrator' => 'Administrador',
    ),
    'user_status_dom' => array(
        'Active' => 'Activo',
        'Inactive' => 'Inactivo',
    ),
    'user_factor_auth_interface_dom' => array(
        'FactorAuthEmailCode' => 'Código de correo electrónico',
    ),
    'employee_status_dom' => array(
        'Active' => 'Activo',
        'Terminated' => 'Despedido',
        'Leave of Absence' => 'Excedencia',
    ),
    'messenger_type_dom' => array(
        '' => '',
        'MSN' => 'MSM',
        'Yahoo!' => 'Yahoo!',
        'AOL' => 'AOL',
    ),
    'project_task_priority_options' => array(
        'High' => 'Alta',
        'Medium' => 'Media',
        'Low' => 'Baixa',
    ),
    'project_task_priority_default' => 'Media',

    'project_task_status_options' => array(
        'Not Started' => 'Non Iniciada',
        'In Progress' => 'En Progreso',
        'Completed' => 'Completada',
        'Pending Input' => 'Pendente de Información',
        'Deferred' => 'Retrasada',
    ),
    'project_task_utilization_options' => array(
        '0' => 'ningún',
        '25' => '25',
        '50' => '50',
        '75' => '75',
        '100' => '100',
    ),

    'project_status_dom' => array(
        'Draft' => 'Borrador',
        'In Review' => 'En Revisión',
        'Underway' => 'En Curso',
        'On_Hold' => 'En Espera',
        'Completed' => 'Completada',
    ),
    'project_status_default' => 'Borrador',

    'project_duration_units_dom' => array(
        'Days' => 'Días',
        'Hours' => 'Horas',
    ),

    'activity_status_type_dom' => array(
        '' => '--Ningún--',
        'active' => 'Activo',
        'inactive' => 'Inactivo',
    ),

    // Note:  do not translate record_type_default_key
    //        it is the key for the default record_type_module value
    'record_type_default_key' => 'Contas',
    'record_type_display' => array(
        '' => '',
        'Accounts' => 'Conta',
        'Opportunities' => 'Oportunidades',
        'Cases' => 'Casos',
        'Leads' => 'Clientes Potenciais',
        'Contacts' => 'Contactos', // cn (11/22/2005) added to support Emails

        'Bugs' => 'Incidencia',
        'Project' => 'Proxectos',

        'Prospects' => 'Público Obxectivo',
        'ProjectTask' => 'Tarefas de Proxecto',

        'Tasks' => 'Tarefas',

        'AOS_Contracts' => 'Contrato',
        'AOS_Invoices' => 'Factura',
        'AOS_Quotes' => 'Presuposto',
        'AOS_Products' => 'Produto',

    ),
// PR 4606
    'record_type_display_notes' => array(
        'Accounts' => 'Conta',
        'Contacts' => 'Contacto',
        'Opportunities' => 'Oportunidade',
        'Campaigns' => 'Campaña',
        'Tasks' => 'Tarefa',
        'Emails' => 'Emails',

        'Bugs' => 'Incidencia',
        'Project' => 'Proxecto',
        'ProjectTask' => 'Tarefa de Proxecto',
        'Prospects' => 'Público Obxectivo',
        'Cases' => 'Caso',
        'Leads' => 'Cliente Potencial',

        'Meetings' => 'Reunión',
        'Calls' => 'Chamada',

        'AOS_Contracts' => 'Contrato',
        'AOS_Invoices' => 'Factura',
        'AOS_Quotes' => 'Presuposto',
        'AOS_Products' => 'Produto',
    ),

    'parent_type_display' => array(
        'Accounts' => 'Conta',
        'Contacts' => 'Contacto',
        'Tasks' => 'Tarefa',
        'Opportunities' => 'Oportunidade',

        'Bugs' => 'Incidencia',
        'Cases' => 'Caso',
        'Leads' => 'Cliente Potencial',

        'Project' => 'Proxecto',
        'ProjectTask' => 'Tarefa de Proxecto',

        'Prospects' => 'Público Obxectivo',
        
        'AOS_Contracts' => 'Contrato',
        'AOS_Invoices' => 'Factura',
        'AOS_Quotes' => 'Presuposto',
        'AOS_Products' => 'Produto', 

    ),
    'parent_line_items' => array(
        'AOS_Quotes' => 'Presupostos',
        'AOS_Invoices' => 'Facturas',
        'AOS_Contracts' => 'Contratos',
    ),
    'issue_priority_default_key' => 'Media',
    'issue_priority_dom' => array(
        'Urgent' => 'Urxente',
        'High' => 'Alta',
        'Medium' => 'Media',
        'Low' => 'Baixa',
    ),
    'issue_resolution_default_key' => '',
    'issue_resolution_dom' => array(
        '' => '',
        'Accepted' => 'Aceptado',
        'Duplicate' => 'Duplicado',
        'Closed' => 'Cerrado',
        'Out of Date' => 'Caducado',
        'Invalid' => 'Non Válido',
    ),

    'issue_status_default_key' => 'Novo',
    'issue_status_dom' => array(
        'New' => 'Novo',
        'Assigned' => 'Asignado',
        'Closed' => 'Cerrado',
        'Pending' => 'Pendente',
        'Rejected' => 'Rexeitado',
    ),

    'bug_priority_default_key' => 'Media',
    'bug_priority_dom' => array(
        'Urgent' => 'Urxente',
        'High' => 'Alta',
        'Medium' => 'Media',
        'Low' => 'Baixa',
    ),
    'bug_resolution_default_key' => '',
    'bug_resolution_dom' => array(
        '' => '',
        'Accepted' => 'Aceptado',
        'Duplicate' => 'Duplicado',
        'Fixed' => 'Corrixido',
        'Out of Date' => 'Caducado',
        'Invalid' => 'Non Válido',
        'Later' => 'Posposto',
    ),
    'bug_status_default_key' => 'Novo',
    'bug_status_dom' => array(
        'New' => 'Novo',
        'Assigned' => 'Asignado',
        'Closed' => 'Cerrado',
        'Pending' => 'Pendente',
        'Rejected' => 'Rexeitado',
    ),
    'bug_type_default_key' => 'Incidencia',
    'bug_type_dom' => array(
        'Defect' => 'Defecto',
        'Feature' => 'Característica',
    ),
    'case_type_dom' => array(
        'Administration' => 'Administración',
        'Product' => 'Produto',
        'User' => 'Usuario',
    ),

    'source_default_key' => '',
    'source_dom' => array(
        '' => '',
        'Internal' => 'Interno',
        'Forum' => 'Foro',
        'Web' => 'Web',
        'InboundEmail' => 'Correo Entrante',
    ),

    'product_category_default_key' => '',
    'product_category_dom' => array(
        '' => '',
        'Accounts' => 'Contas',
        'Activities' => 'Actividades',
        'Bugs' => 'Incidencias',
        'Calendar' => 'Calendario',
        'Calls' => 'Chamadas',
        'Campaigns' => 'Campañas',
        'Cases' => 'Casos',
        'Contacts' => 'Contactos',
        'Currencies' => 'Moedas',
        'Dashboard' => 'Cadro de Mando',
        'Documents' => 'Documentos',
        'Emails' => 'Correos',
        'Feeds' => 'Fontes RSS',
        'Forecasts' => 'Previsións',
        'Help' => 'Axuda',
        'Home' => 'Inicio',
        'Leads' => 'Clientes Potenciais',
        'Meetings' => 'Reunións',
        'Notes' => 'Notas',
        'Opportunities' => 'Oportunidades',
        'Outlook Plugin' => 'Plugin de Outlook',
        'Projects' => 'Proxectos',
        'Quotes' => 'Presupostos',
        'Releases' => 'Lanzamentos',
        'RSS' => 'RSS',
        'Studio' => 'Estudio',
        'Upgrade' => 'Actualización',
        'Users' => 'Usuarios',
    ),
    /*Added entries 'Queued' and 'Sending' for 4.0 release..*/
    'campaign_status_dom' => array(
        '' => '',
        'Planning' => 'Planificación',
        'Active' => 'Activo',
        'Inactive' => 'Inactivo',
        'Complete' => 'Completa',
        //'In Queue' => 'In Queue',
        //'Sending' => 'Sending',
    ),
    'campaign_type_dom' => array(
        '' => '',
        'Telesales' => 'Televenta',
        'Mail' => 'Correo',
        'Email' => 'Email',
        'Print' => 'Imprenta',
        'Web' => 'Web',
        'Radio' => 'Radio',
        'Television' => 'Televisión',
        'NewsLetter' => 'Boletín de Noticias',
    ),

    'newsletter_frequency_dom' => array(
        '' => '',
        'Weekly' => 'Semanal',
        'Monthly' => 'Mensual',
        'Quarterly' => 'Trimestral',
        'Annually' => 'Anual',
    ),

    'notifymail_sendtype' => array(
        'SMTP' => 'SMTP',
    ),
    'dom_cal_month_long' => array(
        '0' => '',
        '1' => 'Xaneiro',
        '2' => 'Febrero',
        '3' => 'Marzo',
        '4' => 'Abril',
        '5' => 'Maio',
        '6' => 'Xuño',
        '7' => 'Xullo',
        '8' => 'Agosto',
        '9' => 'Setembro',
        '10' => 'Outubro',
        '11' => 'Novembro',
        '12' => 'Decembro',
    ),
    'dom_cal_month_short' => array(
        '0' => '',
        '1' => 'Xan',
        '2' => 'Feb',
        '3' => 'Mar',
        '4' => 'Abr',
        '5' => 'Mai',
        '6' => 'Xun',
        '7' => 'Xul',
        '8' => 'Ago',
        '9' => 'Sep',
        '10' => 'Out',
        '11' => 'Nov',
        '12' => 'Dec',
    ),
    'dom_cal_day_long' => array(
        '0' => '',
        '1' => 'Domingo',
        '2' => 'Luns',
        '3' => 'Martes',
        '4' => 'Mércores',
        '5' => 'Xoves',
        '6' => 'Venres',
        '7' => 'Sábado',
    ),
    'dom_cal_day_short' => array(
        '0' => '',
        '1' => 'Dom',
        '2' => 'Lun',
        '3' => 'Mar',
        '4' => 'Mér',
        '5' => 'Xov',
        '6' => 'Ven',
        '7' => 'Sáb',
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
        'group' => 'Grupo',
        'bounce' => 'Rebote',
    ],

    'dom_inbound_email_auth_types' => [
        'basic' => 'Autenticación Básica',
        'oauth' => 'OAuth',
    ],

    'dom_external_oauth_connection_types' => [
        'personal' => 'Personal',
        'group' => 'Grupo',
    ],

    'dom_external_oauth_provider_types' => [
        'personal' => 'Personal',
        'group' => 'Grupo',
    ],

    'dom_outbound_email_account_types' => [
        'user' => 'Personal',
        'group' => 'Grupo',
        'system' => 'Sistema',
        'system-override' => 'Anulación del Sistema',
    ],

    'dom_inbound_email_account_status' => [
        'Active' => 'Activo',
        'Inactive' => 'Inactivo',
    ],

    'dom_email_body_filtering_option' => [
      'multi' => 'Búsqueda de Múltiples Palabras',
      'single' => 'Búsqueda de Una Palabra',
    ],

    'dom_email_types' => array(
        'out' => 'Enviado',
        'archived' => 'Arquivado',
        'draft' => 'Borrador',
        'inbound' => 'Entrante',
        'campaign' => 'Campaña',
    ),
    'dom_email_status' => array(
        'archived' => 'Arquivado',
        'closed' => 'Cerrado',
        'draft' => 'Borrador',
        'read' => 'Lido',
        'replied' => 'Respondido',
        'sent' => 'Enviado',
        'send_error' => 'Erro de Envío',
        'unread' => 'No lido',
    ),
    'dom_email_archived_status' => array(
        'archived' => 'Arquivado',
    ),

    'dom_email_server_type' => array(
        '' => '--Ningún--',
        'imap' => 'IMAP',
    ),
    'dom_mailbox_type' => array(/*''           => '--None Specified--',*/
        'pick' => '--Ningún--',
        'createcase' => 'Novo Caso',
        'bounce' => 'Xestión de Rebotes',
    ),
    'dom_email_distribution' => array(
        '' => '--Ningún--',
        'direct' => 'Asignación Directa',
        'roundRobin' => 'Round-Robin',
        'leastBusy' => 'Menos-Ocupado',
    ),
    'dom_email_errors' => array(
        1 => 'Seleccione só un usuario cando asigne directamente elementos.',
        2 => 'Debes asignar soamente artigos seleccionados cando estos se asignan de forma directa.',
    ),
    'dom_email_bool' => array(
        'bool_true' => 'Sí',
        'bool_false' => 'Non',
    ),
    'dom_int_bool' => array(
        1 => 'Sí',
        0 => 'Non',
    ),
    'dom_switch_bool' => array(
        'on' => 'Sí',
        'off' => 'Non',
        '' => 'Non',
    ),

    'dom_email_link_type' => array(
        'sugar' => 'Cliente de correo de SuiteCRM',
        'mailto' => 'Cliente de correo externo',
    ),

    'dom_editor_type' => array(
        'none' => 'HTML directo',
        'tinymce' => 'TinyMCE Editor',
        'mozaik' => 'Mozaik editor',
    ),

    'dom_email_editor_option' => array(
        '' => 'Formato de correo por defecto',
        'html' => 'Correo HTML',
        'plain' => 'Correo con texto plano',
    ),

    'schedulers_times_dom' => array(
        'not run' => 'Hora de Execución Pasada, non Executado',
        'ready' => 'Listo',
        'in progress' => 'En Progreso',
        'failed' => 'Fallado',
        'completed' => 'Completado',
        'no curl' => 'Non executado: cURL non está dispoñible',
    ),

    'scheduler_status_dom' => array(
        'Active' => 'Activo',
        'Inactive' => 'Inactivo',
    ),

    'scheduler_period_dom' => array(
        'min' => 'Minutos',
        'hour' => 'Horas',
    ),
    'document_category_dom' => array(
        '' => '',
        'Marketing' => 'Marketing',
        'Knowledege Base' => 'Base de Coñecemento',
        'Sales' => 'Vendas',
    ),

    'email_category_dom' => array(
        '' => '',
        'Archived' => 'Arquivado',
        // TODO: add more categories here...
    ),

    'document_subcategory_dom' => array(
        '' => '',
        'Marketing Collateral' => 'Impresos de Marketing',
        'Product Brochures' => 'Folletos de Produto',
        'FAQ' => 'FAQ',
    ),

    'document_status_dom' => array(
        'Active' => 'Activo',
        'Draft' => 'Borrador',
        'FAQ' => 'FAQ',
        'Expired' => 'Caducado',
        'Under Review' => 'En Revisión',
        'Pending' => 'Pendente',
    ),
    'document_template_type_dom' => array(
        '' => '',
        'mailmerge' => 'Combinar correspondencia',
        'eula' => 'CLUF',
        'nda' => 'ANR',
        'license' => 'Contrato de Licenza',
    ),
    'dom_meeting_accept_options' => array(
        'accept' => 'Aceptar',
        'decline' => 'Rexeitar',
        'tentative' => 'Tentativa',
    ),
    'dom_meeting_accept_status' => array(
        'accept' => 'Aceptado',
        'decline' => 'Rexeitado',
        'tentative' => 'Tentativa',
        'none' => 'Ningún',
    ),
    'duration_intervals' => array(
        '0' => '00',
        '15' => '15',
        '30' => '30',
        '45' => '45',
    ),
    'repeat_type_dom' => array(
        '' => 'Ningún',
        'Daily' => 'Diario',
        'Weekly' => 'Semanal',
        'Monthly' => 'Mensual',
        'Yearly' => 'Anual',
    ),

    'repeat_intervals' => array(
        '' => '',
        'Daily' => 'Diario',
        'Weekly' => 'Semanal',
        'Monthly' => 'Mensual',
        'Yearly' => 'Anual',
    ),

    'duration_dom' => array(
        '' => 'Ningunha',
        '900' => '15 minutos',
        '1800' => '30 minutos',
        '2700' => '45 minutos',
        '3600' => '1 hora',
        '5400' => '1.5 horas',
        '7200' => '2 horas',
        '10800' => '3 horas',
        '21600' => '6 horas',
        '86400' => '1 día',
        '172800' => '2 días',
        '259200' => '3 días',
        '604800' => '1 semana',
    ),


//prospect list type dom
    'prospect_list_type_dom' => array(
        'default' => 'Por Defecto',
        'seed' => 'Cabeza de Serie',
        'exempt_domain' => 'Lista de Exclusión - Por Dominio',
        'exempt_address' => 'Lista de Exclusión - Por Enderezo de Email',
        'exempt' => 'Lista de Exclusión - Por Id',
        'test' => 'Proba',
    ),

    'email_settings_num_dom' => array(
        '10' => '10',
        '20' => '20',
        '50' => '50',
    ),
    'email_marketing_status_dom' => array(
        '' => '',
        'active' => 'Activo',
        'inactive' => 'Inactivo',
    ),

    'campainglog_activity_type_dom' => array(
        '' => '',
        'targeted' => 'Mensaxe enviado',
        'send erro' => 'Mensaxe non enviado (outras causas)',
        'invalid email' => 'Mensaxe non enviado (enderezo non válido)',
        'link' => 'Enlace clicado',
        'viewed' => 'Mensaxe visto',
        'removed' => 'Baixa',
        'lead' => 'Cliente Potenciai creado',
        'contact' => 'Contacto creado',
        'blocked' => 'Destinatario excluido por enderezo ou dominio',
        'Survey' => 'Resposta á enquisa',
    ),

    'campainglog_target_type_dom' => array(
        'Contacts' => 'Contactos',
        'Users' => 'Usuarios',
        'Prospects' => 'Público Obxectivo',
        'Leads' => 'Clientes Potenciais',
        'Accounts' => 'Contas',
    ),
    'merge_operators_dom' => array(
        'like' => 'Contén',
        'exact' => 'Exactamente',
        'start' => 'Comeza con',
    ),

    'custom_fields_importable_dom' => array(
        'true' => 'Si',
        'false' => 'Non',
        'required' => 'Requirido',
    ),

    'custom_fields_merge_dup_dom' => array(
        0 => 'Deshabilitado',
        1 => 'Habilitado',
        2 => 'En filtro',
        3 => 'Filtro seleccionado por defecto',
        4 => 'Só filtro',
    ),

    'projects_priority_options' => array(
        'high' => 'Alta',
        'medium' => 'Media',
        'low' => 'Baixa',
    ),

    'projects_status_options' => array(
        'notstarted' => 'Non Iniciado',
        'inprogress' => 'En Progreso',
        'completed' => 'Completado',
    ),
    // strings to pass to Flash charts
    'chart_strings' => array(
        'expandlegend' => 'Expandir Lenda',
        'collapselegend' => 'Contraer Lenda',
        'clickfordrilldown' => 'Clic para Profundizar',
        'detailview' => 'Máis Detalles...',
        'piechart' => 'Gráfico Circular',
        'groupchart' => 'Gráfico Agrupado',
        'stackedchart' => 'Gráfico Apilado',
        'barchart' => 'Gráfico de Barras',
        'horizontalbarchart' => 'Gráfico de Barras Horizontal',
        'linechart' => 'Gráfico de Liñas',
        'noData' => 'Datos non dispoñibles',
        'print' => 'Imprimir',
        'pieWedgeName' => 'seccións',
    ),
    'release_status_dom' => array(
        'Active' => 'Activo',
        'Inactive' => 'Inactivo',
    ),
    'email_settings_for_ssl' => array(
        '0' => '',
        '1' => 'SSL',
        '2' => 'TLS',
    ),
    'import_enclosure_options' => array(
        '\'' => 'Comilla simple (&#39;)',
        '"' => 'Comillas dobles (&#34;)',
        '' => 'Ningún',
        'other' => 'Outro:',
    ),
    'import_delimeter_options' => array(
        ',' => ',',
        ';' => ';',
        '\t' => '\t',
        '.' => '.',
        ':' => ':',
        '|' => '|',
        'other' => 'Outro:',
    ),
    'link_target_dom' => array(
        '_blank' => 'Nova Ventá',
        '_self' => 'Mesma Ventá',
    ),
    'dashlet_auto_refresh_options' => array(
        '-1' => 'Non actualizar automaticamente',
        '30' => 'Cada 30 segundos',
        '60' => 'Cada minuto',
        '180' => 'Cada 3 minutos',
        '300' => 'Cada 5 minutos',
        '600' => 'Cada 10 minutos',
    ),
    'dashlet_auto_refresh_options_admin' => array(
        '-1' => 'Nunca',
        '30' => 'Cada 30 segundos',
        '60' => 'Cada minuto',
        '180' => 'Cada 3 minutos',
        '300' => 'Cada 5 minutos',
        '600' => 'Cada 10 minutos',
    ),
    'date_range_search_dom' => array(
        '=' => 'Igual a',
        'not_equal' => 'Distinto de',
        'greater_than' => 'Despois de',
        'less_than' => 'Antes de',
        'last_7_days' => 'Últimos 7 días',
        'next_7_days' => 'Próximos 7 días',
        'last_30_days' => 'Últimos 30 días',
        'next_30_days' => 'Próximos 30 días',
        'last_month' => 'Último mes',
        'this_month' => 'Este mes',
        'next_month' => 'Próximo mes',
        'last_year' => 'Último ano',
        'this_year' => 'Este ano',
        'next_year' => 'Próximo ano',
        'between' => 'Está entre',
    ),
    'numeric_range_search_dom' => array(
        '=' => 'Igual a',
        'not_equal' => 'Distinto de',
        'greater_than' => 'Maior que',
        'greater_than_equals' => 'Maior ou Igual que',
        'less_than' => 'Menor que',
        'less_than_equals' => 'Menor ou Igual a',
        'between' => 'Está entre',
    ),
    'lead_conv_activity_opt' => array(
        'copy' => 'Copiar',
        'move' => 'Mover',
        'donothing' => 'Non facer nada',
    ),
    // PR 6009
    'inboundmail_assign_replies_to_admin' => array(
        'donothing' => 'Non facer nada',
        'repliedtoowner' => 'Respondeu ao propietario do correo electrónico',
        'recordowner' => 'Propietario do rexistro asociado',
    ),
);

$app_strings = array(
    'LBL_SEARCH_REAULTS_TITLE' => 'Resultados',
    'ERR_SEARCH_INVALID_QUERY' => 'Produciuse un erro ao realizar a busca. A sintaxe da súa consulta podería non ser válida.',
    'ERR_SEARCH_NO_RESULTS' => 'Non hai resultados para a súa busca. Inténteo de novo con outros criterios.',
    'LBL_SEARCH_PERFORMED_IN' => 'Busca realizada',
    'LBL_EMAIL_CODE' => 'Código de correo electrónico:',
    'LBL_SEND' => 'Enviar',
    'LBL_LOGOUT' => 'Saír',
    'LBL_TOUR_NEXT' => 'Seguinte',
    'LBL_TOUR_SKIP' => 'Saltar',
    'LBL_TOUR_BACK' => 'Atrás',
    'LBL_TOUR_TAKE_TOUR' => 'Visita guiada',
    'LBL_MOREDETAIL' => 'Máis detalles' /*for 508 compliance fix*/,
    'LBL_EDIT_INLINE' => 'Editar en liña' /*for 508 compliance fix*/,
    'LBL_VIEW_INLINE' => 'Ver' /*for 508 compliance fix*/,
    'LBL_BASIC_SEARCH' => 'Filtro' /*for 508 compliance fix*/,
    'LBL_Blank' => ' ' /*for 508 compliance fix*/,
    'LBL_ID_FF_ADD' => 'Engadir' /*for 508 compliance fix*/,
    'LBL_ID_FF_ADD_EMAIL' => 'Engadir enderezo de correo electrónico' /*for 508 compliance fix*/,
    'LBL_HIDE_SHOW' => 'Ocultar/Mostrar' /*for 508 compliance fix*/,
    'LBL_DELETE_INLINE' => 'Eliminar' /*for 508 compliance fix*/,
    'LBL_ID_FF_CLEAR' => 'Limpar' /*for 508 compliance fix*/,
    'LBL_ID_FF_VCARD' => 'vCard' /*for 508 compliance fix*/,
    'LBL_ID_FF_REMOVE' => 'Quitar' /*for 508 compliance fix*/,
    'LBL_ID_FF_REMOVE_EMAIL' => 'Eliminar enderezo de correo electrónico' /*for 508 compliance fix*/,
    'LBL_ID_FF_OPT_OUT' => 'Rehusar',
    'LBL_ID_FF_OPT_IN' => 'Autorizar',
    'LBL_ID_FF_INVALID' => 'Facer Inválido',
    'LBL_ADD' => 'Engadir' /*for 508 compliance fix*/,
    'LBL_COMPANY_LOGO' => 'Logo compañia' /*for 508 compliance fix*/,
    'LBL_CONNECTORS_POPUPS' => 'Conectores Popups',
    'LBL_CLOSEINLINE' => 'Cerrado',
    'LBL_VIEWINLINE' => 'Ver',
    'LBL_INFOINLINE' => 'Información',
    'LBL_PRINT' => 'Imprimir',
    'LBL_HELP' => 'Axuda',
    'LBL_ID_FF_SELECT' => 'Seleccionar',
    'DEFAULT' => 'Básico', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_SORT' => 'Ordear',
    'LBL_EMAIL_SMTP_SSL_OR_TLS' => '¿Habilitar SMTP sobre SSL ou TLS?',
    'LBL_NO_ACTION' => 'Non hai ningunha acción para o nome: %s',
    'LBL_NO_SHORTCUT_MENU' => 'Non hai accións dispoñibles.',
    'LBL_NO_DATA' => 'Sen Datos',

    'LBL_ERROR_UNDEFINED_BEHAVIOR' => 'Produciuse un erro inesperado.', //PR 3669
    'LBL_ERROR_UNHANDLED_VALUE' => 'Un valor non se manipulou correctamente o que impide que un proceso continúe.', //PR 3669
    'LBL_ERROR_UNUSABLE_VALUE' => 'Encontrouse un valor inutilizable que impide que un proceso continúe.', //PR 3669
    'LBL_ERROR_INVALID_TYPE' => 'O valor introducido non é do tipo esperado.', //PR 3669

    'LBL_ROUTING_FLAGGED' => 'conxunto de marcas de seguimento',
    'LBL_NOTIFICATIONS' => 'Notificacións',

    'LBL_ROUTING_TO' => 'a',
    'LBL_ROUTING_TO_ADDRESS' => 'a a enderezo',
    'LBL_ROUTING_WITH_TEMPLATE' => 'coa plantilla',

    'NTC_OVERWRITE_ADDRESS_PHONE_CONFIRM' => 'Os campos Teléfono e Enderezo do seu formulario xa teñen valor asignado. Para sobrescribir ditos valores co teléfono/enderezo da Conta que seleccionou, faga clic en "Aceptar". Para manter os valores actuais, faga clic en "Cancelar".',
    'LBL_DROP_HERE' => '[Soltar Aquí]',
    'LBL_EMAIL_ACCOUNTS_GMAIL_DEFAULTS' => 'Establecer configuración para Gmail&amp;#153;',
    'LBL_EMAIL_ACCOUNTS_NAME' => 'Nome',
    'LBL_EMAIL_ACCOUNTS_OUTBOUND' => 'Propiedades do Servidor de Correo Saínte',
    'LBL_EMAIL_ACCOUNTS_SMTPPASS' => 'Contrasinal SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPPORT' => 'Porto SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPSERVER' => 'Servidor SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPUSER' => 'Nome de usuario SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPDEFAULT' => 'Por Defecto',
    'LBL_EMAIL_WARNING_MISSING_USER_CREDS' => 'Aviso: Falta o nome de usuario e o contrasinal para a conta de correo saínte.',
    'LBL_EMAIL_ACCOUNTS_SUBTITLE' => 'Configurar Contas de Correo para ver correos entrantes das súas contas de correo.',
    'LBL_EMAIL_ACCOUNTS_OUTBOUND_SUBTITLE' => 'Proporcionar información do servidor de correo SMTP a utilizar para o correo saínte en Contas de Correo.',

    'LBL_EMAIL_ADDRESS_BOOK_ADD' => 'Feito',
    'LBL_EMAIL_ADDRESS_BOOK_CLEAR' => 'Borrar',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_TO' => 'Para:',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_CC' => 'Cc:',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_BCC' => 'Cco:',
    'LBL_EMAIL_ADDRESS_BOOK_ADRRESS_TYPE' => 'Para/Cc/Cco',
    'LBL_EMAIL_ADDRESS_BOOK_EMAIL_ADDR' => 'Enderezo de Email',
    'LBL_EMAIL_ADDRESS_BOOK_FILTER' => 'Filtro',
    'LBL_EMAIL_ADDRESS_BOOK_NAME' => 'Nome',
    'LBL_EMAIL_ADDRESS_BOOK_NOT_FOUND' => 'Non se encontrou ningunha enderezo',
    'LBL_EMAIL_ADDRESS_BOOK_SAVE_AND_ADD' => 'Gardar e Agregar á Libreta de Enderezos',
    'LBL_EMAIL_ADDRESS_BOOK_SELECT_TITLE' => 'Seleccionar Destinatarios de Correo',
    'LBL_EMAIL_ADDRESS_BOOK_TITLE' => 'Libreta de Enderezos',
    'LBL_EMAIL_REPORTS_TITLE' => 'Informes',
    'LBL_EMAIL_REMOVE_SMTP_WARNING' => '¡Aviso! a conta de correo saínte que está intentando eliminar está asociada a unha conta de correo entrante existente.  ¿Está seguro de que quere continuar?',
    'LBL_EMAIL_ADDRESSES' => 'Email',
    'LBL_EMAIL_ADDRESS_PRIMARY' => 'Enderezo de Email',
    'LBL_EMAIL_ADDRESS_OPT_IN' => 'Confirmou que o seu enderezo de correo foi autorizado a enviar: ',
    'LBL_EMAIL_ADDRESS_OPT_IN_ERR' => 'Non foi posible confirmar a enderezo de correo',
    'LBL_EMAIL_ARCHIVE_TO_SUITE' => 'Importar a SuiteCRM',
    'LBL_EMAIL_ASSIGNMENT' => 'Asignación',
    'LBL_EMAIL_ATTACH_FILE_TO_EMAIL' => 'Adxuntar',
    'LBL_EMAIL_ATTACHMENT' => 'Adxuntar',
    'LBL_EMAIL_ATTACHMENTS' => 'Desde o Equipo Local',
    'LBL_EMAIL_ATTACHMENTS2' => 'Desde Documentos SuiteCRM',
    'LBL_EMAIL_ATTACHMENTS3' => 'Adxuntos de Plantilla',
    'LBL_EMAIL_ATTACHMENTS_FILE' => 'Arquivo',
    'LBL_EMAIL_ATTACHMENTS_DOCUMENT' => 'Documento',
    'LBL_EMAIL_BCC' => 'CCO',
    'LBL_EMAIL_CANCEL' => 'Cancelar',
    'LBL_EMAIL_CC' => 'Cc',
    'LBL_EMAIL_CHARSET' => 'Xogo de Caracteres',
    'LBL_EMAIL_CHECK' => 'Comprobar Correo',
    'LBL_EMAIL_CHECKING_NEW' => 'Comprobando Correo Novo',
    'LBL_EMAIL_CHECKING_DESC' => 'Un momento, por favor... <br><br>Se é a primeira comprobación para esta conta de correo, pode tardar un pouco.',
    'LBL_EMAIL_CLOSE' => 'Cerrar',
    'LBL_EMAIL_COFFEE_BREAK' => 'Comprobando Correo Novo. <br><br>As contas de correo con gran volume poden tardar unha cantidade considerable de tempo.',

    'LBL_EMAIL_COMPOSE' => 'Correo',
    'LBL_EMAIL_COMPOSE_ERR_NO_RECIPIENTS' => 'Por favor, introduza os destinatarios deste correo.',
    'LBL_EMAIL_COMPOSE_NO_BODY' => 'O corpo deste mensaxe está baleiro.  ¿Enviar de todas formas?',
    'LBL_EMAIL_COMPOSE_NO_SUBJECT' => 'Esta mensaxe non ten asunto.  ¿Enviar de todas formas?',
    'LBL_EMAIL_COMPOSE_NO_SUBJECT_LITERAL' => '(sen asunto)',
    'LBL_EMAIL_COMPOSE_INVALID_ADDRESS' => 'Por favor, introduza un enderezo de correo válida para os campos Para, CC e CCO',

    'LBL_EMAIL_CONFIRM_CLOSE' => '¿Descartar este correo?',
    'LBL_EMAIL_CONFIRM_DELETE_SIGNATURE' => '¿Está seguro de que desexa eliminar esta sinatura?',

    'LBL_EMAIL_SENT_SUCCESS' => 'Correo electrónico enviado',

    'LBL_EMAIL_CREATE_NEW' => '--Crear Ao Gardar--',
    'LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS' => 'Múltiple',
    'LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS_EMPTY' => 'Baleiro',
    'LBL_EMAIL_DATE_SENT_BY_SENDER' => 'Data de Envío por Remitente',
    'LBL_EMAIL_DATE_TODAY' => 'Hoxe',
    'LBL_EMAIL_DELETE' => 'Eliminar',
    'LBL_EMAIL_DELETE_CONFIRM' => '¿Eliminar mensaxes seleccionados?',
    'LBL_EMAIL_DELETE_SUCCESS' => 'Email eliminado satisfactoriamente.',
    'LBL_EMAIL_DELETING_MESSAGE' => 'Eliminando Mensaxe',
    'LBL_EMAIL_DETAILS' => 'Detalles',

    'LBL_EMAIL_EDIT_CONTACT_WARN' => 'Só se utilizará o Enderezo principal de cada Contacto.',

    'LBL_EMAIL_EMPTYING_TRASH' => 'Vaciando Papeleira',
    'LBL_EMAIL_DELETING_OUTBOUND' => 'Eliminando servidor saínte',
    'LBL_EMAIL_CLEARING_CACHE_FILES' => 'Limpando arquivos da caché',
    'LBL_EMAIL_EMPTY_MSG' => 'Non hai mensaxes para mostrar.',
    'LBL_EMAIL_EMPTY_ADDR_MSG' => 'Non hai enderezos de correo electrónico para mostrar.',

    'LBL_EMAIL_ERROR_ADD_GROUP_FOLDER' => 'O nome de carpeta debe ser único e non baleiro. Por favor, inténteo de novo.',
    'LBL_EMAIL_ERROR_DELETE_GROUP_FOLDER' => 'Non pode borrarse a carpeta. Ou a carpeta ou os seus fillos teñen correos ou unha bandexa de correo asociada.',
    'LBL_EMAIL_ERROR_CANNOT_FIND_NODE' => 'Non se puido determinar a carpeta pretendida a partir do contexto. Inténteo de novo.',
    'LBL_EMAIL_ERROR_CHECK_IE_SETTINGS' => 'Por favor, comprobe a súa configuración.',
    'LBL_EMAIL_ERROR_DESC' => 'Detectáronse erros:',
    'LBL_EMAIL_DELETE_ERROR_DESC' => 'Non ten acceso a esta área. Contacte co administrador do sitio para obter acceso.',
    'LBL_EMAIL_ERROR_DUPE_FOLDER_NAME' => 'Os nomes de carpetas SuiteCRM deben ser únicos.',
    'LBL_EMAIL_ERROR_EMPTY' => 'Por favor, introduza algún criterio de busca.',
    'LBL_EMAIL_ERROR_GENERAL_TITLE' => 'Ocurreu un erro',
    'LBL_EMAIL_ERROR_MESSAGE_DELETED' => 'Mensaxe eliminada do servidor',
    'LBL_EMAIL_ERROR_IMAP_MESSAGE_DELETED' => 'A mensaxe eliminouse no servidor ou foi movido a outra carpeta',
    'LBL_EMAIL_ERROR_MAILSERVERCONNECTION' => 'A conexión co servidor de correo fallou. Por favor, contacte co seu Administrador',
    'LBL_EMAIL_ERROR_MOVE' => 'De momento non está soportado o mover correo entre servidores e/o contas de correo.',
    'LBL_EMAIL_ERROR_MOVE_TITLE' => 'Erro ao Mover',
    'LBL_EMAIL_ERROR_NAME' => 'Requírese un nome.',
    'LBL_EMAIL_ERROR_FROM_ADDRESS' => 'Requíresea Enderezo do Remitente. Por favor, introduza un enderezo de correo válida.',
    'LBL_EMAIL_ERROR_NO_FILE' => 'Por favor, proporcione un arquivo.',
    'LBL_EMAIL_ERROR_SERVER' => 'Requírese un enderezo de servidor de correo.',
    'LBL_EMAIL_ERROR_SAVE_ACCOUNT' => 'A conta de correo pode non haber sido gardada.',
    'LBL_EMAIL_ERROR_TIMEOUT' => 'Ha ocurrido un erro na comunicación co servidor de correo.',
    'LBL_EMAIL_ERROR_USER' => 'Requíreseun nome de inicio de sesión.',
    'LBL_EMAIL_ERROR_PORT' => 'Requíreseun porto do servidor de correo.',
    'LBL_EMAIL_ERROR_PROTOCOL' => 'Requíreseun protocolo non servidor.',
    'LBL_EMAIL_ERROR_MONITORED_FOLDER' => 'Requírese unha Carpeta Monitorizada.',
    'LBL_EMAIL_ERROR_TRASH_FOLDER' => 'Requírese unha Carpeta de Papeleira.',
    'LBL_EMAIL_ERROR_VIEW_RAW_SOURCE' => 'Esta información non está dispoñible',
    'LBL_EMAIL_ERROR_NO_OUTBOUND' => 'Non se especificou un servidor de correo saínte.',
    'LBL_EMAIL_ERROR_SENDING' => 'Erro ao enviar o correo electrónico. Póñase en contacto co seu administrador para obter axuda.',
    'LBL_EMAIL_FOLDERS' => SugarThemeRegistry::current()->getImage('icon_email_folder', 'align=absmiddle border=0', null, null, '.gif', '') . 'Carpetas',
    'LBL_EMAIL_FOLDERS_SHORT' => SugarThemeRegistry::current()->getImage('icon_email_folder', 'align=absmiddle border=0', null, null, '.gif', ''),
    'LBL_EMAIL_FOLDERS_ADD' => 'Agregar',
    'LBL_EMAIL_FOLDERS_ADD_DIALOG_TITLE' => 'Agregar Nova Carpeta',
    'LBL_EMAIL_FOLDERS_RENAME_DIALOG_TITLE' => 'Renomear Carpeta',
    'LBL_EMAIL_FOLDERS_ADD_NEW_FOLDER' => 'Gardar',
    'LBL_EMAIL_FOLDERS_ADD_THIS_TO' => 'Agregar esta carpeta a',
    'LBL_EMAIL_FOLDERS_CHANGE_HOME' => 'Esta carpeta non pode ser cambiada',
    'LBL_EMAIL_FOLDERS_DELETE_CONFIRM' => '¿Está seguro de que quere eliminar esta carpeta?\nEste proceso non pode ser volto atrás.\nA eliminación de carpetas aplicarase en cascada a todas as carpetas contidas.',
    'LBL_EMAIL_FOLDERS_NEW_FOLDER' => 'Nome da Nova Carpeta',
    'LBL_EMAIL_FOLDERS_NO_VALID_NODE' => 'Por favor, seleccione unha carpeta antes de realizar esta acción.',
    'LBL_EMAIL_FOLDERS_TITLE' => 'Administración de Carpetas',

    'LBL_EMAIL_FORWARD' => 'Reenviar',
    'LBL_EMAIL_DELIMITER' => '::;::',
    'LBL_EMAIL_DOWNLOAD_STATUS' => '[[count]] de [[total]] emails descargados',
    'LBL_EMAIL_FROM' => 'De',
    'LBL_EMAIL_GROUP' => 'grupo',
    'LBL_EMAIL_UPPER_CASE_GROUP' => 'Grupo',
    'LBL_EMAIL_HOME_FOLDER' => 'Inicio',
    'LBL_EMAIL_IE_DELETE' => 'Eliminando Conta de Correo',
    'LBL_EMAIL_IE_DELETE_SIGNATURE' => 'Eliminando sinatura',
    'LBL_EMAIL_IE_DELETE_CONFIRM' => '¿Está seguro de que desexa eliminar esta conta de correo?',
    'LBL_EMAIL_IE_DELETE_SUCCESSFUL' => 'Borrado satisfactorio.',
    'LBL_EMAIL_IE_SAVE' => 'Gardando Información de Conta de Correo',
    'LBL_EMAIL_IMPORTING_EMAIL' => 'Importando Email',
    'LBL_EMAIL_IMPORT_EMAIL' => 'Importar en SuiteCRM',
    'LBL_EMAIL_IMPORT_SETTINGS' => 'Configuración de Importación',
    'LBL_EMAIL_INVALID' => 'Non Válido',
    'LBL_EMAIL_LOADING' => 'Cargando...',
    'LBL_EMAIL_MARK' => 'Marcar',
    'LBL_EMAIL_MARK_FLAGGED' => 'Como Etiquetado',
    'LBL_EMAIL_MARK_READ' => 'Como Lido',
    'LBL_EMAIL_MARK_UNFLAGGED' => 'Como non Etiquetado',
    'LBL_EMAIL_MARK_UNREAD' => 'Como non Lido',
    'LBL_EMAIL_ASSIGN_TO' => 'Asignar a',

    'LBL_EMAIL_MENU_ADD_FOLDER' => 'Crear Carpeta',
    'LBL_EMAIL_MENU_COMPOSE' => 'Redactar para',
    'LBL_EMAIL_MENU_DELETE_FOLDER' => 'Eliminar Carpeta',
    'LBL_EMAIL_MENU_EMPTY_TRASH' => 'Baleirar Papeleira',
    'LBL_EMAIL_MENU_SYNCHRONIZE' => 'Sincronizar',
    'LBL_EMAIL_MENU_CLEAR_CACHE' => 'Limpar arquivos de caché',
    'LBL_EMAIL_MENU_REMOVE' => 'Quitar',
    'LBL_EMAIL_MENU_RENAME_FOLDER' => 'Renomear Carpeta',
    'LBL_EMAIL_MENU_RENAMING_FOLDER' => 'Renombrando Carpeta',
    'LBL_EMAIL_MENU_MAKE_SELECTION' => 'Por favor, realice unha selección antes de intentar esta operación.',

    'LBL_EMAIL_MENU_HELP_ADD_FOLDER' => 'Crear unha Carpeta (remota ou en SuiteCRM)',
    'LBL_EMAIL_MENU_HELP_DELETE_FOLDER' => 'Eliminar unha Carpeta (remota ou en SuiteCRM)',
    'LBL_EMAIL_MENU_HELP_EMPTY_TRASH' => 'Baleira todas as carpetas de Papeleira das súas contas de correo',
    'LBL_EMAIL_MENU_HELP_MARK_READ' => 'Marcar estos emails como lidos',
    'LBL_EMAIL_MENU_HELP_MARK_UNFLAGGED' => 'Marcar estos emails non etiquetados',
    'LBL_EMAIL_MENU_HELP_RENAME_FOLDER' => 'Renomear unha Carpeta (remota ou en SuiteCRM)',

    'LBL_EMAIL_MESSAGES' => 'mensaxes',

    'LBL_EMAIL_ML_NAME' => 'Nome de Lista',
    'LBL_EMAIL_ML_ADDRESSES_1' => 'Lista de Enderezos Seleccionada',
    'LBL_EMAIL_ML_ADDRESSES_2' => 'Lista de Enderezos Dispoñibles',

    'LBL_EMAIL_MULTISELECT' => '<b>Ctrl-Clic</b> para seleccionar múltiples<br />(os usuarios de Mac poden usar <b>CMD-Clic</b>)',

    'LBL_EMAIL_NO' => 'Non',
    'LBL_EMAIL_NOT_SENT' => 'O sistema non pode procesar a súa petición. Por favor, póñase en contacto co administrador do sistema.',

    'LBL_EMAIL_OK' => 'Aceptar',
    'LBL_EMAIL_ONE_MOMENT' => 'Un momento, por favor...',
    'LBL_EMAIL_OPEN_ALL' => 'Abrir Múltiples Mensaxes',
    'LBL_EMAIL_OPTIONS' => 'Opcións',
    'LBL_EMAIL_QUICK_COMPOSE' => 'Redacción Rápida',
    'LBL_EMAIL_OPT_OUT' => 'Rehusado',
    'LBL_EMAIL_OPT_IN' => 'Autorizado',
    'LBL_EMAIL_OPT_IN_AND_INVALID' => 'Autorizado e Inválido',
    'LBL_EMAIL_OPT_OUT_AND_INVALID' => 'Rehusado e invalido',
    'LBL_EMAIL_PERFORMING_TASK' => 'Realizando Tarefa',
    'LBL_EMAIL_PRIMARY' => 'Principal',
    'LBL_EMAIL_PRINT' => 'Imprimir',

    'LBL_EMAIL_QC_BUGS' => 'Incidencia',
    'LBL_EMAIL_QC_CASES' => 'Caso',
    'LBL_EMAIL_QC_LEADS' => 'Cliente Potencial',
    'LBL_EMAIL_QC_CONTACTS' => 'Contacto',
    'LBL_EMAIL_QC_TASKS' => 'Tarefa',
    'LBL_EMAIL_QC_OPPORTUNITIES' => 'Oportunidade',
    'LBL_EMAIL_QUICK_CREATE' => 'Creación Rápida',

    'LBL_EMAIL_REBUILDING_FOLDERS' => 'Reconstruíndo Carpetas',
    'LBL_EMAIL_RELATE_TO' => 'Relacionar con',
    'LBL_EMAIL_VIEW_RELATIONSHIPS' => 'Ver Relacións',
    'LBL_EMAIL_RECORD' => 'Rexistro de Email',
    'LBL_EMAIL_REMOVE' => 'Quitar',
    'LBL_EMAIL_REPLY' => 'Responder',
    'LBL_EMAIL_REPLY_ALL' => 'Responder a Todos',
    'LBL_EMAIL_REPLY_TO' => 'Responder a',
    'LBL_EMAIL_RETRIEVING_MESSAGE' => 'Recuperando Mensaxe',
    'LBL_EMAIL_RETRIEVING_RECORD' => 'Recuperando Rexistro de Email',
    'LBL_EMAIL_SELECT_ONE_RECORD' => 'Por favor, seleccione un único rexistro de email',
    'LBL_EMAIL_RETURN_TO_VIEW' => '¿Volver a Módulo Anterior?',
    'LBL_EMAIL_REVERT' => 'Revertir',
    'LBL_EMAIL_RELATE_EMAIL' => 'Relacionar Email',

    'LBL_EMAIL_RULES_TITLE' => 'Administración de Regras',

    'LBL_EMAIL_SAVE' => 'Gardar',
    'LBL_EMAIL_SAVE_AND_REPLY' => 'Gardar e Responder',
    'LBL_EMAIL_SAVE_DRAFT' => 'Gardar Borrador',
    'LBL_EMAIL_DRAFT_SAVED' => 'O borrador foi gardado',

    'LBL_EMAIL_SEARCH' => SugarThemeRegistry::current()->getImage('Search', 'align=absmiddle border=0', null, null,    '.gif', ''),
    'LBL_EMAIL_SEARCH_SHORT' => SugarThemeRegistry::current()->getImage('Search', 'align=absmiddle border=0', null,        null, '.gif', ''),
    'LBL_EMAIL_SEARCH_DATE_FROM' => 'Data Desde',
    'LBL_EMAIL_SEARCH_DATE_UNTIL' => 'Data Ata',
    'LBL_EMAIL_SEARCH_NO_RESULTS' => 'Non hai resultados para os seus criterios de busca.',
    'LBL_EMAIL_SEARCH_RESULTS_TITLE' => 'Resultados da Busca',

    'LBL_EMAIL_SELECT' => 'Seleccionar',

    'LBL_EMAIL_SEND' => 'Enviar',
    'LBL_EMAIL_SENDING_EMAIL' => 'Enviando Email',

    'LBL_EMAIL_SETTINGS' => 'Configuración',
    'LBL_EMAIL_SETTINGS_ACCOUNTS' => 'Contas de Correo',
    'LBL_EMAIL_SETTINGS_ADD_ACCOUNT' => 'Limpar Formulario',
    'LBL_EMAIL_SETTINGS_CHECK_INTERVAL' => 'Comprobar Correo Novo',
    'LBL_EMAIL_SETTINGS_FROM_ADDR' => 'Enderezo de Remitente',
    'LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR' => 'Enderezo para Notificación de Proba:',
    'LBL_EMAIL_SETTINGS_FROM_NAME' => 'Nome do Remitente',
    'LBL_EMAIL_SETTINGS_REPLY_TO_ADDR' => 'Enderezo de Responder a',
    'LBL_EMAIL_SETTINGS_FULL_SYNC' => 'Sincronizar Todas as Contas de Correo',
    'LBL_EMAIL_TEST_NOTIFICATION_SENT' => 'Enviouse un correo electrónico ao enderezo utilizando a configuración de correo saínte proporcionada. Por favor, comprobe se recibiu o correo para verificar que a configuración é correcta.',
    'LBL_EMAIL_TEST_SEE_FULL_SMTP_LOG' => 'Ver rexistro de SMTP completo',
    'LBL_EMAIL_SETTINGS_FULL_SYNC_WARN' => '¿Realizar unha sincronización completa?\nPara contas de correo grandes, pode durar varios minutos.',
    'LBL_EMAIL_SUBSCRIPTION_FOLDER_HELP' => 'Faga clic na Tecla Shift ou na tecla Ctrl para seleccionar carpetas múltiples.',
    'LBL_EMAIL_SETTINGS_GENERAL' => 'Xeral',
    'LBL_EMAIL_SETTINGS_GROUP_FOLDERS_CREATE' => 'Crear Carpetas de Grupo',

    'LBL_EMAIL_SETTINGS_GROUP_FOLDERS_EDIT' => 'Editar Carpetas de Grupo',

    'LBL_EMAIL_SETTINGS_NAME' => 'Nome de Conta de Correo',
    'LBL_EMAIL_SETTINGS_REQUIRE_REFRESH' => 'Seleccione o número de correos por páxina na Bandexa de Entrada. Estas opcións poden requirir dun refresco de páxina para ser activadas.',
    'LBL_EMAIL_SETTINGS_RETRIEVING_ACCOUNT' => 'Recuperando Email de Conta',
    'LBL_EMAIL_SETTINGS_SAVED' => 'Os axustes foron grabados.',
    'LBL_EMAIL_SETTINGS_SEND_EMAIL_AS' => 'Enviar Só Correos con Texto Plano',
    'LBL_EMAIL_SETTINGS_SHOW_NUM_IN_LIST' => 'Emails por Páxina',
    'LBL_EMAIL_SETTINGS_TITLE_LAYOUT' => 'Configuración Visual',
    'LBL_EMAIL_SETTINGS_TITLE_PREFERENCES' => 'Preferencias',
    'LBL_EMAIL_SETTINGS_USER_FOLDERS' => 'Carpetas de Usuario Dispoñibles',
    'LBL_EMAIL_ERROR_PREPEND' => 'Ocurreu un erro co correo electrónico:',
    'LBL_EMAIL_INVALID_PERSONAL_OUTBOUND' => 'O servidor de correo saínte seleccionado para a conta de correo que está utilizando non é válido.  Comprobe a configuración ou seleccione un servidor de correo distinto para a conta.',
    'LBL_EMAIL_INVALID_SYSTEM_OUTBOUND' => 'Non se configurou un servidor de correo saínte para o envío de correos. Por favor, configure ou seleccione un servidor de correo saínte para a conta de correo que está utilizando en Configuración >> Conta de Correo.',
    'LBL_DEFAULT_EMAIL_SIGNATURES' => 'Sinatura predeterminada',
    'LBL_EMAIL_SIGNATURES' => 'Sinatuas',
    'LBL_SMTPTYPE_GMAIL' => 'Gmail',
    'LBL_SMTPTYPE_YAHOO' => 'Correo Yahoo',
    'LBL_SMTPTYPE_EXCHANGE' => 'Microsoft Exchange',
    'LBL_SMTPTYPE_OTHER' => 'Outro:',
    'LBL_EMAIL_SPACER_MAIL_SERVER' => '[ Carpetas Remotas ]',
    'LBL_EMAIL_SPACER_LOCAL_FOLDER' => '[ Carpetas de SuiteCRM ]',
    'LBL_EMAIL_SUBJECT' => 'Asunto',
    'LBL_EMAIL_SUCCESS' => 'Éxito',
    'LBL_EMAIL_SUITE_FOLDER' => 'Carpeta de SuiteCRM',
    'LBL_EMAIL_TEMPLATE_EDIT_PLAIN_TEXT' => 'O corpo da plantilla de correo está baleiro',
    'LBL_EMAIL_TEMPLATES' => 'Plantillas',
    'LBL_EMAIL_TO' => 'Para',
    'LBL_EMAIL_VIEW' => 'Ver',
    'LBL_EMAIL_VIEW_HEADERS' => 'Mostrar Cabeceras',
    'LBL_EMAIL_VIEW_RAW' => 'Mostrar Código Fonte do Email',
    'LBL_EMAIL_VIEW_UNSUPPORTED' => 'Esta característica non está soportada cando se usa con POP3.',
    'LBL_DEFAULT_LINK_TEXT' => 'Texto de enlace por defecto.',
    'LBL_EMAIL_YES' => 'Si',
    'LBL_EMAIL_TEST_OUTBOUND_SETTINGS' => 'Enviar Correo de Proba',
    'LBL_EMAIL_TEST_OUTBOUND_SETTINGS_SENT' => 'Correo de Proba Enviado',
    'LBL_EMAIL_MESSAGE_NO' => 'Mensaxe Nº', // Counter. Message number xx
    'LBL_EMAIL_IMPORT_SUCCESS' => 'Importación Existosa',
    'LBL_EMAIL_IMPORT_FAIL' => 'Importación Fallida debido a que a mensaxe xa foi importada ou eliminada do servidor',

    'LBL_LINK_NONE' => 'Ningún',
    'LBL_LINK_ALL' => 'Todos',
    'LBL_LINK_RECORDS' => 'Rexistros',
    'LBL_LINK_SELECT' => 'Seleccionar',
    'LBL_LINK_ACTIONS' => 'Accións', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CLOSE_ACTIVITY_HEADER' => 'Confirmar',
    'LBL_CLOSE_ACTIVITY_CONFIRM' => '¿Desexa cerrar este #module#?',
    'LBL_INVALID_FILE_EXTENSION' => 'Extensión de arquivo invalida',

    'ERR_AJAX_LOAD' => 'Produciuse un erro:',
    'ERR_AJAX_LOAD_FAILURE' => 'Produciuse un erro ao procesar a súa petición, por favor inténteo de novo máis tarde.',
    'ERR_AJAX_LOAD_FOOTER' => 'Se persiste o erro, por favor solicite ao administrador que deshabilite Ajax para este módulo',
    'ERR_DECIMAL_SEP_EQ_THOUSANDS_SEP' => 'Non pode utilizarse o mesmo carácter como separador decimal que o utilizado como separador de miles.\n\n  Por favor, cambie os valores.',
    'ERR_DELETE_RECORD' => 'Debe especificar un número de rexistro para eliminar o contacto.',
    'ERR_EXPORT_DISABLED' => 'Exportación deshabilitada.',
    'ERR_EXPORT_TYPE' => 'Erro exportando',
    'ERR_INVALID_EMAIL_ADDRESS' => 'non é un enderezo de correo válida.',
    'ERR_INVALID_FILE_REFERENCE' => 'Referencia a arquivo non válida',
    'ERR_NO_HEADER_ID' => 'Esta funcionalidade non está dispoñible con este tema.',
    'ERR_NOT_ADMIN' => 'Acceso non autorizado á administración.',
    'ERR_MISSING_REQUIRED_FIELDS' => 'Falta campo requirido:',
    'ERR_INVALID_REQUIRED_FIELDS' => 'Campo requirido non válido:',
    'ERR_INVALID_VALUE' => 'Valor non válido:',
    'ERR_NO_SUCH_FILE' => 'O arquivo non existe no sistema',
    'ERR_FILE_EMPTY' => 'O arquivo está baleiro', // PR 6672
    'ERR_NO_SINGLE_QUOTE' => 'Non se pode usar comillas simples para ',
    'ERR_NOTHING_SELECTED' => 'Por favor, realice unha selección antes de proceder.',
    'ERR_SELF_REPORTING' => 'Un usuario non pode ser informador de si mesmo.',
    'ERR_SQS_NO_MATCH_FIELD' => 'Non se encontrou coincidencias para o campo:',
    'ERR_SQS_NO_MATCH' => 'Sen coincidencias',
    'ERR_ADDRESS_KEY_NOT_SPECIFIED' => 'Por favor, especifique o índice &amp;#39;clave&amp;#39; no atributo displayParams para a definición de Meta-Datos',
    'ERR_EXISTING_PORTAL_USERNAME' => 'Erro: o Nome de Portal xa foi asignado a outro contacto.',
    'ERR_COMPATIBLE_PRECISION_VALUE' => 'O valor do campo non é compatible co tipo de precisión',
    'ERR_EXTERNAL_API_SAVE_FAIL' => 'Produciuse un erro ao tratar de salvar na conta externa.',
    'ERR_NO_DB' => 'Non se puido realizar unha conexión á base de datos. Por favor, consulte SuiteCRM erro.log para máis detalles (0).',
    'ERR_DB_FAIL' => 'Erro de base de datos. Por favor, consulte SuiteCRM erro .log para máis detalles.',
    'ERR_DB_VERSION' => 'Arquivos de SuiteCRM {0} só se poden utilizar cunha base de datos de SuiteCRM {1}.',

    'LBL_ACCOUNT' => 'Conta',
    'LBL_ACCOUNTS' => 'Contas',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_ACCUMULATED_HISTORY_BUTTON_KEY' => 'H',
    'LBL_ACCUMULATED_HISTORY_BUTTON_LABEL' => 'Ver Resumo',
    'LBL_ACCUMULATED_HISTORY_BUTTON_TITLE' => 'Ver Resumo',
    'LBL_ADD_BUTTON' => 'Agregar',
    'LBL_ADD_DOCUMENT' => 'Agregar Documento',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_KEY' => 'L',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL' => 'Agregar A Lista de Público Obxectivo',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL_ACCOUNTS_CONTACTS' => 'Engadir contactos á lista de destino',
    'LBL_ADDITIONAL_DETAILS_CLOSE_TITLE' => 'Clic para Cerrar',
    'LBL_ADDITIONAL_DETAILS' => 'Detalles Adicionais',
    'LBL_ADMIN' => 'Administrador',
    'LBL_ALT_HOT_KEY' => '',
    'LBL_ARCHIVE' => 'Arquivo',
    'LBL_ASSIGNED_TO_USER' => 'Asignado a Usuario',
    'LBL_ASSIGNED_TO' => 'Asignado a:',
    'LBL_BACK' => 'Atrás',
    'LBL_BILLING_ADDRESS' => 'Enderezo de Facturación',
    'LBL_QUICK_CREATE' => 'Crear ',
    'LBL_BROWSER_TITLE' => 'SuiteCRM - CRM de Fontes Abertas',
    'LBL_BUGS' => 'Incidencias',
    'LBL_BY' => 'por',
    'LBL_CALLS' => 'Chamadas',
    'LBL_CAMPAIGNS_SEND_QUEUED' => 'Enviar Emails de Campaña Encolados',
    'LBL_SUBMIT_BUTTON_LABEL' => 'Enviar',
    'LBL_CASE' => 'Caso',
    'LBL_CASES' => 'Casos',
    'LBL_CHANGE_PASSWORD' => 'Cambiar contrasinal',
    'LBL_CHARSET' => 'UTF-8',
    'LBL_CHECKALL' => 'Marcar Todos',
    'LBL_CITY' => 'Cidade',
    'LBL_CLEAR_BUTTON_LABEL' => 'Limpar',
    'LBL_CLEAR_BUTTON_TITLE' => 'Limpar',
    'LBL_CLEARALL' => 'Desmarcar Todos',
    'LBL_CLOSE_BUTTON_TITLE' => 'Cerrar', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_LABEL' => 'Cerrar e Crear Novo', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_TITLE' => 'Cerrar e Crear Novo', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_KEY' => 'C',
    'LBL_OPEN_ITEMS' => 'Elementos Abertos:',
    'LBL_COMPOSE_EMAIL_BUTTON_KEY' => 'L',
    'LBL_COMPOSE_EMAIL_BUTTON_LABEL' => 'Redactar Correo',
    'LBL_COMPOSE_EMAIL_BUTTON_TITLE' => 'Redactar Correo',
    'LBL_SEARCH_DROPDOWN_YES' => 'Sí',
    'LBL_SEARCH_DROPDOWN_NO' => 'Non',
    'LBL_CONTACT_LIST' => 'Lista de Contactos',
    'LBL_CONTACT' => 'Contacto',
    'LBL_CONTACTS' => 'Contactos',
    'LBL_CONTRACT' => 'Contrato',
    'LBL_CONTRACTS' => 'Contratos',
    'LBL_COUNTRY' => 'País:',
    'LBL_CREATE_BUTTON_LABEL' => 'Crear', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CREATED_BY_USER' => 'Creado polo Usuario',
    'LBL_CREATED_USER' => 'Creado polo Usuario',
    'LBL_CREATED' => 'Creado por',
    'LBL_CURRENT_USER_FILTER' => 'Os Meus Elementos:',
    'LBL_CURRENCY' => 'Moeda:',
    'LBL_DOCUMENTS' => 'Documentos',
    'LBL_DATE_ENTERED' => 'Data de Creación:',
    'LBL_DATE_MODIFIED' => 'Data de Modificación:',
    'LBL_EDIT_BUTTON' => 'Editar',
    // STIC-Custom 20240214 JBL - QuickEdit view
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/93
    'LBL_QUICKEDIT_BUTTON' => '↙ Editar',
    // END STIC-Custom
    'LBL_DUPLICATE_BUTTON' => 'Duplicar',
    'LBL_DELETE_BUTTON' => 'Eliminar',
    'LBL_DELETE' => 'Eliminar',
    'LBL_DELETED' => 'Eliminado',
    'LBL_DIRECT_REPORTS' => 'Informa a',
    'LBL_DONE_BUTTON_LABEL' => 'Feito',
    'LBL_DONE_BUTTON_TITLE' => 'Feito',
    'LBL_FAVORITES' => 'Favoritos',
    'LBL_VCARD' => 'vCard',
    'LBL_EMPTY_VCARD' => 'Por favor, seleccione un arquivo vCard',
    'LBL_EMPTY_REQUIRED_VCARD' => 'A vCard non ten todos os campos requiridos para este módulo. Por favor consulte suitecrm.log para máis detalles.',
    'LBL_VCARD_ERROR_FILESIZE' => 'O arquivo subido excede o límite de tamaño, o cal se especificou no formulario HTML.',
    'LBL_VCARD_ERROR_DEFAULT' => 'Houbo un erro subindo o arquivo vCard. Por favor consulte suitecrm.log para máis detalles.',
    'LBL_IMPORT_VCARD' => 'Importar vCard:',
    'LBL_IMPORT_VCARD_BUTTON_LABEL' => 'Importar vCard',
    'LBL_IMPORT_VCARD_BUTTON_TITLE' => 'Importar vCard',
    'LBL_VIEW_BUTTON' => 'Ver',
    'LBL_EMAIL_PDF_BUTTON_LABEL' => 'Enviar como PDF',
    'LBL_EMAIL_PDF_BUTTON_TITLE' => 'Enviar como PDF',
    'LBL_EMAILS' => 'Correos',
    'LBL_EMPLOYEES' => 'Empregados',
    'LBL_ENTER_DATE' => 'Introducir Data',
    'LBL_EXPORT' => 'Exportar',
    'LBL_FAVORITES_FILTER' => 'Os Meus Favoritos:',
    'LBL_GO_BUTTON_LABEL' => 'Adiante',
    'LBL_HIDE' => 'Ocultar',
    'LBL_ID' => 'ID',
    'LBL_IMPORT' => 'Importar',
    'LBL_IMPORT_STARTED' => 'Importación iniciada:',
    'LBL_LAST_VIEWED' => 'Recentes',
    'LBL_LEADS' => 'Clientes Potenciais',
    'LBL_LESS' => 'menos',
    'LBL_CAMPAIGN' => 'Campaña:',
    'LBL_CAMPAIGNS' => 'Campañas',
    'LBL_CAMPAIGNLOG' => 'Rexistro de Campañas',
    'LBL_CAMPAIGN_CONTACT' => 'Campañas',
    'LBL_CAMPAIGN_ID' => 'campaign_id',
    'LBL_CAMPAIGN_NONE' => 'Ningún',
    'LBL_THEME' => 'Tema:',
    'LBL_FOUND_IN_RELEASE' => 'Encontrado en Versión',
    'LBL_FIXED_IN_RELEASE' => 'Corrixido en Versión',
    'LBL_LIST_ACCOUNT_NAME' => 'Nome de Conta',
    'LBL_LIST_ASSIGNED_USER' => 'Usuario',
    'LBL_LIST_CONTACT_NAME' => 'Nome Contacto',
    'LBL_LIST_CONTACT_ROLE' => 'Rol Contacto',
    'LBL_LIST_DATE_ENTERED' => 'Data de Creación',
    'LBL_LIST_EMAIL' => 'Correo',
    'LBL_LIST_NAME' => 'Nome',
    'LBL_LIST_OF' => 'de',
    'LBL_LIST_PHONE' => 'Teléfono',
    'LBL_LIST_RELATED_TO' => 'Relacionado con',
    'LBL_LIST_USER_NAME' => 'Nome de Usuario',
    'LBL_LISTVIEW_NO_SELECTED' => 'Por favor, seleccione polo menos 1 rexistro para proceder.',
    'LBL_LISTVIEW_TWO_REQUIRED' => 'Por favor, seleccione polo menos 2 rexistros para proceder.',
    'LBL_LISTVIEW_OPTION_SELECTED' => 'Rexistros Seleccionados',
    'LBL_LISTVIEW_SELECTED_OBJECTS' => 'Seleccionados: ',

    'LBL_LOCALE_NAME_EXAMPLE_FIRST' => 'Juan',
    'LBL_LOCALE_NAME_EXAMPLE_LAST' => 'Pérez',
    'LBL_LOCALE_NAME_EXAMPLE_SALUTATION' => 'Sr.',
    'LBL_LOCALE_NAME_EXAMPLE_TITLE' => 'Mago do Código Fonte',
    'LBL_CANCEL' => 'Cancelar',
    'LBL_VERIFY' => 'Verificar',
    'LBL_RESEND' => 'Reenviar',
    'LBL_PROFILE' => 'Perfil',
    'LBL_MAILMERGE' => 'Combinar Correspondencia',
    'LBL_MASS_UPDATE' => 'Actualización Masiva',
    'LBL_NO_MASS_UPDATE_FIELDS_AVAILABLE' => 'Non hai campos dispoñibles para a operación de actualización masiva.',
    // STIC-Custom - 20220704 - JCH - Duplicate & Mass Update
    // STIC#776
    'LBL_MASS_DUPLICATE_UPDATE' => 'Duplicado e Actualización Masiva',
    'LBL_MASS_DUPLICATE_REMOVE_NAME' => 'Valeirar o Nome dos novos rexistros para que poida ser reconstruido automáticamente',
    'LBL_MASS_DUPLICATE_UPDATE_CONFIRMATION_NUM' => '¿Está seguro de que desexa duplicar e actualizar o(os) ',
    'LBL_MASS_DUPLICATE_UPDATE_BTN' => 'Duplicar e Actualizar',
    // END STIC
    'LBL_OPT_OUT_FLAG_PRIMARY' => 'Rehusar para Email Principal',
    'LBL_OPT_IN_FLAG_PRIMARY' => 'Adherir con e-mail principal',
    'LBL_MEETINGS' => 'Reunións',
    'LBL_MEETING_GO_BACK' => 'Volver á reunión',
    'LBL_MEMBERS' => 'Membros',
    'LBL_MEMBER_OF' => 'Membro de',
    'LBL_MODIFIED_BY_USER' => 'Modificado polo Usuario',
    'LBL_MODIFIED_USER' => 'Modificado polo Usuario',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_NAME' => 'Modificado por Nome',
    'LBL_MORE' => 'Máis',
    'LBL_MY_ACCOUNT' => 'A Miña Configuración',
    'LBL_NAME' => 'Nome',
    'LBL_NEW_BUTTON_KEY' => 'N',
    'LBL_NEW_BUTTON_LABEL' => 'Novo',
    'LBL_NEW_BUTTON_TITLE' => 'Novo',
    'LBL_NEXT_BUTTON_LABEL' => 'Seguinte',
    'LBL_NONE' => '-ningún-',
    'LBL_NOTES' => 'Notas',
    'LBL_OPPORTUNITIES' => 'Oportunidades',
    'LBL_OPPORTUNITY_NAME' => 'Nome da oportunidade',
    'LBL_OPPORTUNITY' => 'Oportunidade',
    'LBL_OR' => 'O',
    'LBL_PANEL_OVERVIEW' => 'Visión Global', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PANEL_ASSIGNMENT' => 'Outro', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PANEL_ADVANCED' => 'Máis Información', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PARENT_TYPE' => 'Tipo de Pai',
    'LBL_PERCENTAGE_SYMBOL' => '%',
    'LBL_POSTAL_CODE' => 'Código Postal:',
    'LBL_PRIMARY_ADDRESS_CITY' => 'Cidade de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_COUNTRY' => 'País de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_POSTALCODE' => 'CP de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_STATE' => 'Estado/Provincia de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_STREET_2' => 'Rúa de enderezo principal 2',
    'LBL_PRIMARY_ADDRESS_STREET_3' => 'Rúa de enderezo principal 3',
    'LBL_PRIMARY_ADDRESS_STREET' => 'Rúa de enderezo principal:',
    'LBL_PRIMARY_ADDRESS' => 'Enderezo principal:',

    'LBL_PROSPECTS' => 'Prospectos',
    'LBL_PRODUCTS' => 'Produtos',
    'LBL_PROJECT_TASKS' => 'Tarefas de Proxecto',
    'LBL_PROJECTS' => 'Proxectos',
    'LBL_QUOTES' => 'Presupostos',

    'LBL_RELATED' => 'Relacionado',
    'LBL_RELATED_RECORDS' => 'Rexistros Relacionados',
    'LBL_REMOVE' => 'Quitar',
    'LBL_REPORTS_TO' => 'Informa a',
    'LBL_REQUIRED_SYMBOL' => '*',
    'LBL_REQUIRED_TITLE' => 'Indica que é un campo requirido',
    'LBL_EMAIL_DONE_BUTTON_LABEL' => 'Feito',
    'LBL_FULL_FORM_BUTTON_KEY' => 'F',
    'LBL_FULL_FORM_BUTTON_LABEL' => 'Formulario Completo',
    'LBL_FULL_FORM_BUTTON_TITLE' => 'Formulario Completo',
    'LBL_SAVE_NEW_BUTTON_LABEL' => 'Gardar e Crear Novo',
    'LBL_SAVE_NEW_BUTTON_TITLE' => 'Gardar e Crear Novo',
    'LBL_SAVE_OBJECT' => 'Gardar {0}',
    'LBL_SEARCH_BUTTON_KEY' => 'Q',
    'LBL_SEARCH_BUTTON_LABEL' => 'Busca',
    'LBL_SEARCH_BUTTON_TITLE' => 'Busca',
    'LBL_FILTER' => 'Filtro',
    'LBL_SEARCH' => 'Busca',
    'LBL_SEARCH_ALT' => '',
    'LBL_SEARCH_MORE' => 'máis',
    'LBL_UPLOAD_IMAGE_FILE_INVALID' => 'Formato de arquivo non válido, só é posible subir arquivos con imaxes.',
    'LBL_SELECT_BUTTON_KEY' => 'T',
    'LBL_SELECT_BUTTON_LABEL' => 'Seleccionar',
    'LBL_SELECT_BUTTON_TITLE' => 'Seleccionar',
    'LBL_BROWSE_DOCUMENTS_BUTTON_LABEL' => 'Explorar Documentos',
    'LBL_BROWSE_DOCUMENTS_BUTTON_TITLE' => 'Explorar Documentos',
    'LBL_SELECT_CONTACT_BUTTON_KEY' => 'T',
    'LBL_SELECT_CONTACT_BUTTON_LABEL' => 'Seleccionar Contacto',
    'LBL_SELECT_CONTACT_BUTTON_TITLE' => 'Seleccionar Contacto',
    'LBL_SELECT_REPORTS_BUTTON_LABEL' => 'Seleccionar desde Informes',
    'LBL_SELECT_REPORTS_BUTTON_TITLE' => 'Seleccionar Informes',
    'LBL_SELECT_USER_BUTTON_KEY' => 'U',
    'LBL_SELECT_USER_BUTTON_LABEL' => 'Seleccionar Usuario',
    'LBL_SELECT_USER_BUTTON_TITLE' => 'Seleccionar Usuario',
    // Clear buttons take up too many keys, lets default the relate and collection ones to be empty
    'LBL_ACCESSKEY_CLEAR_RELATE_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_RELATE_TITLE' => 'Borrar selección',
    'LBL_ACCESSKEY_CLEAR_RELATE_LABEL' => 'Borrar selección',
    'LBL_ACCESSKEY_CLEAR_COLLECTION_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_COLLECTION_TITLE' => 'Borrar selección',
    'LBL_ACCESSKEY_CLEAR_COLLECTION_LABEL' => 'Borrar selección',
    'LBL_ACCESSKEY_SELECT_FILE_KEY' => 'F',
    'LBL_ACCESSKEY_SELECT_FILE_TITLE' => 'Seleccionar Arquivo',
    'LBL_ACCESSKEY_SELECT_FILE_LABEL' => 'Seleccionar Arquivo',
    'LBL_ACCESSKEY_CLEAR_FILE_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_FILE_TITLE' => 'Limpar arquivo',
    'LBL_ACCESSKEY_CLEAR_FILE_LABEL' => 'Limpar arquivo',

    'LBL_ACCESSKEY_SELECT_USERS_KEY' => 'U',
    'LBL_ACCESSKEY_SELECT_USERS_TITLE' => 'Seleccionar usuario',
    'LBL_ACCESSKEY_SELECT_USERS_LABEL' => 'Seleccionar usuario',
    'LBL_ACCESSKEY_CLEAR_USERS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_USERS_TITLE' => 'Limpar usuario',
    'LBL_ACCESSKEY_CLEAR_USERS_LABEL' => 'Limpar usuairo',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_KEY' => 'A',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_TITLE' => 'Seleccionar Conta',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_LABEL' => 'Seleccionar Conta',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_TITLE' => 'Limpar Conta',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_LABEL' => 'Limpar Conta',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_KEY' => 'M',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_TITLE' => 'Seleccionar campaña',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_LABEL' => 'Seleccionar campaña',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_TITLE' => 'Limpar campaña',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_LABEL' => 'Limpar campaña',
    'LBL_ACCESSKEY_SELECT_CONTACTS_KEY' => 'C',
    'LBL_ACCESSKEY_SELECT_CONTACTS_TITLE' => 'Seleccionar Contacto',
    'LBL_ACCESSKEY_SELECT_CONTACTS_LABEL' => 'Seleccionar Contacto',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_TITLE' => 'Limpar contacto',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_LABEL' => 'Limpar contacto',
    'LBL_ACCESSKEY_SELECT_TEAMSET_KEY' => 'Z',
    'LBL_ACCESSKEY_SELECT_TEAMSET_TITLE' => 'Seleccionar equipo',
    'LBL_ACCESSKEY_SELECT_TEAMSET_LABEL' => 'Seleccionar equipo',
    'LBL_ACCESSKEY_CLEAR_TEAMS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_TEAMS_TITLE' => 'Limpar equipo',
    'LBL_ACCESSKEY_CLEAR_TEAMS_LABEL' => 'Limpar equipo',
    'LBL_SERVER_RESPONSE_RESOURCES' => 'Recursos usados para construir esta páxina (consultas, arquivos)',
    'LBL_SERVER_RESPONSE_TIME_SECONDS' => 'segundos.',
    'LBL_SERVER_RESPONSE_TIME' => 'Tempo de resposta do servidor:',
    'LBL_SERVER_MEMORY_BYTES' => 'bytes',
    'LBL_SERVER_MEMORY_USAGE' => 'Uso da memoria do servidor: {0} ({1})',
    'LBL_SERVER_MEMORY_LOG_MESSAGE' => 'Uso: - modulo: {0} - acción: {1}',
    'LBL_SERVER_PEAK_MEMORY_USAGE' => 'Uso da memoria máxima do servidor: {0} ({1})',
    'LBL_SHIPPING_ADDRESS' => 'Enderezo de Envío',
    'LBL_SHOW' => 'Mostrar',
    'LBL_STATE' => 'Estado:', //Used for Case State, situation, condition
    'LBL_STATUS_UPDATED' => '¡O seu estado para este evento foi actualizado!',
    'LBL_STATUS' => 'Estado:',
    'LBL_STREET' => 'Rúa',
    'LBL_SUBJECT' => 'Asunto',

    'LBL_INBOUNDEMAIL_ID' => 'ID de Correo Entrante',

    'LBL_SCENARIO_SALES' => 'Vendas',
    'LBL_SCENARIO_MARKETING' => 'Marketing',
    'LBL_SCENARIO_FINANCE' => 'Finanzas',
    'LBL_SCENARIO_SERVICE' => 'Servizo',
    'LBL_SCENARIO_PROJECT' => 'Administración de proxectos',

    'LBL_SCENARIO_SALES_DESCRIPTION' => 'Este escenario facilita a administración dos ítemes de venda',
    'LBL_SCENARIO_MAKETING_DESCRIPTION' => 'Este escenario facilita a xestión dos ítems de marketing',
    'LBL_SCENARIO_FINANCE_DESCRIPTION' => 'Esta situación facilita a xestión dos elementos relacionados coas finanzas',
    'LBL_SCENARIO_SERVICE_DESCRIPTION' => 'Este escenario facilita a xestión dos ítems relacionados con servizos',
    'LBL_SCENARIO_PROJECT_DESCRIPTION' => 'Este escenario facilita a administración dos ítems relacionados con proxectos',

    'LBL_SYNC' => 'Sincronizar',
    'LBL_TABGROUP_ALL' => 'Todo',
    'LBL_TABGROUP_ACTIVITIES' => 'Actividades',
    'LBL_TABGROUP_COLLABORATION' => 'Colaboración',
    'LBL_TABGROUP_MARKETING' => 'Marketing',
    'LBL_TABGROUP_OTHER' => 'Outro',
    'LBL_TABGROUP_SALES' => 'Vendas',
    'LBL_TABGROUP_SUPPORT' => 'Soporte',
    'LBL_TASKS' => 'Tarefas',
    'LBL_THOUSANDS_SYMBOL' => 'K',
    'LBL_TRACK_EMAIL_BUTTON_LABEL' => 'Arquivar Correo',
    'LBL_TRACK_EMAIL_BUTTON_TITLE' => 'Arquivar Correo',
    'LBL_UNDELETE_BUTTON_LABEL' => 'Restaurar',
    'LBL_UNDELETE_BUTTON_TITLE' => 'Restaurar',
    'LBL_UNDELETE_BUTTON' => 'Restaurar',
    'LBL_UNDELETE' => 'Restaurar',
    'LBL_UNSYNC' => 'Desincronizar',
    'LBL_UPDATE' => 'Actualizar',
    'LBL_USER_LIST' => 'Lista de Usuarios',
    'LBL_USERS' => 'Usuarios',
    'LBL_VERIFY_EMAIL_ADDRESS' => 'Comprobando a entrada de correo actual...',
    'LBL_VERIFY_PORTAL_NAME' => 'Comprobando o nome de portal actual...',
    'LBL_VIEW_IMAGE' => 'ver',

    'LNK_ABOUT' => 'Acerca de',
    'LNK_ADVANCED_FILTER' => 'Filtro avanzado',
    'LNK_BASIC_FILTER' => 'Filtro rápido',
    'LBL_ADVANCED_SEARCH' => 'Filtro avanzado',
    'LBL_QUICK_FILTER' => 'Filtro rápido',
    'LNK_SEARCH_NONFTS_VIEW_ALL' => 'Mostrar Todo',
    'LNK_CLOSE' => 'Peche',
    'LBL_MODIFY_CURRENT_FILTER' => 'Modificar filtro actual',
    'LNK_SAVED_VIEWS' => 'Opcións de Deseño',
    'LNK_DELETE' => 'Eliminar',
    'LNK_EDIT' => 'Editar',
    'LNK_GET_LATEST' => 'Obter última',
    'LNK_GET_LATEST_TOOLTIP' => 'Reemplazar con última versión',
    'LNK_HELP' => 'Axuda',
    'LNK_CREATE' => 'Crear',
    'LNK_LIST_END' => 'Fin',
    'LNK_LIST_NEXT' => 'Seguinte',
    'LNK_LIST_PREVIOUS' => 'Anterior',
    'LNK_LIST_RETURN' => 'Volver a lista',
    'LNK_LIST_START' => 'Inicio',
    'LNK_LOAD_SIGNED' => 'Asinar',
    'LNK_LOAD_SIGNED_TOOLTIP' => 'Reemplazar con documento asinado',
    'LNK_PRINT' => 'Imprimir',
    'LNK_BACKTOTOP' => 'Volver ao parte superior',
    'LNK_REMOVE' => 'Quitar',
    'LNK_RESUME' => 'Continuar',
    'LNK_VIEW_CHANGE_LOG' => 'Ver Rexistro de Cambios',

    'NTC_CLICK_BACK' => 'Por favor, presione o botón anterior do navegador e corrixa o erro.',
    'NTC_DATE_FORMAT' => '(aaaa-mm-dd)',
    'NTC_DELETE_CONFIRMATION_MULTIPLE' => '¿Está seguro de que desexa eliminar os rexistros seleccionados?',
    'NTC_TEMPLATE_IS_USED' => 'A plantilla estase utilizando en polo menos un rexistro de marketing por email. ¿Está seguro de que desexa eliminala?',
    'NTC_TEMPLATES_IS_USED' => 'As seguintes plantillas se utilizan nos rexistros de marketing por correo electrónico. ¿Seguro que queres eliminalos?' . PHP_EOL,
    'NTC_DELETE_CONFIRMATION' => '¿Está seguro de que desexa eliminar esta rexistro?',
    'NTC_DELETE_CONFIRMATION_NUM' => '¿Está seguro de que desexa eliminar o (os) ',
    'NTC_UPDATE_CONFIRMATION_NUM' => '¿Está seguro de que desexa actualizar o (os) ',
    'NTC_DELETE_SELECTED_RECORDS' => ' rexistro(s) seleccionado(s)?',
    'NTC_LOGIN_MESSAGE' => 'Por favor, introduza o seu nome de usuario e contrasinal.',
    'NTC_NO_ITEMS_DISPLAY' => 'ningún',
    'NTC_REMOVE_CONFIRMATION' => '¿Está seguro de que desexa quitar esta relación?',
    'NTC_REQUIRED' => 'Indica un campo requirido',
    'NTC_TIME_FORMAT' => '(24:00)',
    'NTC_WELCOME' => 'Benvido',
    'NTC_YEAR_FORMAT' => '(aaaa)',
    'WARN_UNSAVED_CHANGES' => 'Está a punto de abandonar este rexistro sen gardar os cambios que realizou. ¿Está seguro de que desexa saír deste rexistro?',
    'ERROR_NO_RECORD' => 'Erro ao recuperar rexistro.  Este rexistro pode ter sido eliminado ou pode que non estea autorizado para velo.',
    'WARN_BROWSER_VERSION_WARNING' => '<p><b>Aviso: </b>O seu navegador ou a versión do seu navegador non é compatible.</p><p>Recoméndanse as seguintes versións de navegadores:</p><ul><li>Internet Explorer 9</li><li>Mozilla Firefox 14, 15 </li><li>Safari 6</li><li>Google Chrome 22 (or latest version)</li></ul>',
    'WARN_BROWSER_IE_COMPATIBILITY_MODE_WARNING' => '<b>Advertencia:</b> O seu navegador está en modo compatibilidade IE o cal non é soportado.',
    'ERROR_TYPE_NOT_VALID' => 'Erro. Este tipo non é válido.',
    'ERROR_NO_BEAN' => 'Fallou a obtención do bean',
    'LBL_DUP_MERGE' => 'Buscar Duplicados',
    'LBL_MANAGE_SUBSCRIPTIONS' => 'Administrar Suscripcións',
    'LBL_MANAGE_SUBSCRIPTIONS_FOR' => 'Administrar Suscripcións a',
    // Ajax status strings
    'LBL_LOADING' => 'Cargando ...',
    'LBL_SEARCHING' => 'Buscando...',
    'LBL_SAVING_LAYOUT' => 'Gardando Deseño ...',
    'LBL_SAVED_LAYOUT' => 'O deseño foi gardado.',
    'LBL_SAVED' => 'Gardado',
    'LBL_SAVING' => 'Gardando',
    'LBL_DISPLAY_COLUMNS' => 'Mostrar Columnas',
    'LBL_HIDE_COLUMNS' => 'Ocultar Columnas',
    'LBL_SEARCH_CRITERIA' => 'Criterios de busca',
    'LBL_SAVED_VIEWS' => 'Vistas gardadas',
    'LBL_PROCESSING_REQUEST' => 'Procesando...',
    'LBL_REQUEST_PROCESSED' => 'Feito',
    'LBL_AJAX_FAILURE' => 'Fallo de Ajax',
    'LBL_MERGE_DUPLICATES' => 'Combinar',
    'LBL_SAVED_FILTER_SHORTCUT' => 'Os Meus filtros',
    'LBL_SEARCH_POPULATE_ONLY' => 'Realizar unha busca utilizando o formulario de busca anterior',
    'LBL_DETAILVIEW' => 'Vista de Detalle',
    'LBL_LISTVIEW' => 'Vista de Lista',
    'LBL_EDITVIEW' => 'Vista de Edición',
    'LBL_BILLING_STREET' => 'Rúa:',
    'LBL_SHIPPING_STREET' => 'Rúa:',
    'LBL_SEARCHFORM' => 'Formulario de Busca',
    'LBL_SAVED_SEARCH_ERROR' => 'Por favor, introduza un nome para esta vista.',
    'LBL_DISPLAY_LOG' => 'Mostrar Traza',
    'ERROR_JS_ALERT_SYSTEM_CLASS' => 'Sistema',
    'ERROR_JS_ALERT_TIMEOUT_TITLE' => 'Peche da Sesión',
    'ERROR_JS_ALERT_TIMEOUT_MSG_1' => 'A súa sesión va a expirar en 2 minutos. Por favor, garde su traballo.',
    'ERROR_JS_ALERT_TIMEOUT_MSG_2' => 'A súa sesión ha expirado.',
    'MSG_JS_ALERT_MTG_REMINDER_AGENDA' => "Axenda:",
    'MSG_JS_ALERT_MTG_REMINDER_MEETING' => 'Reunión',
    'MSG_JS_ALERT_MTG_REMINDER_CALL' => 'Chamada',
    'MSG_JS_ALERT_MTG_REMINDER_TIME' => 'Hora:',
    'MSG_JS_ALERT_MTG_REMINDER_LOC' => 'Lugar:',
    'MSG_JS_ALERT_MTG_REMINDER_DESC' => 'Descrición:',
    'MSG_JS_ALERT_MTG_REMINDER_STATUS' => 'Estado:',
    'MSG_JS_ALERT_MTG_REMINDER_RELATED_TO' => 'Relacionado A: ',
    'MSG_JS_ALERT_MTG_REMINDER_CALL_MSG' => "\nFaga clic en Aceptar para acceder a esta chamada ou faga clic en Cancelar para cerrar esta mensaxe.",
    'MSG_JS_ALERT_MTG_REMINDER_MEETING_MSG' => "\nFaga clic en Aceptar para ver esta reunión ou en Cancelar para omitir esta mensaxe.",
    'MSG_JS_ALERT_MTG_REMINDER_NO_EVENT_NAME' => 'Evento',
    'MSG_JS_ALERT_MTG_REMINDER_NO_DESCRIPTION' => 'Evento non establecido.',
    'MSG_JS_ALERT_MTG_REMINDER_NO_LOCATION' => 'Localización non establecida.',
    'MSG_JS_ALERT_MTG_REMINDER_NO_START_DATE' => 'A data de inicio non está definida.',
    'MSG_LIST_VIEW_NO_RESULTS_BASIC' => 'Non se encontraron resultados.',
    'MSG_LIST_VIEW_NO_RESULTS_CHANGE_CRITERIA' => 'Non se encontraron resultados... Volve a intentar cambiando o teu criterio de busca',
    'MSG_LIST_VIEW_NO_RESULTS' => 'Non se encontrou resultados para <item1>',
    'MSG_LIST_VIEW_NO_RESULTS_SUBMSG' => 'Crear <item1> como un novo <item2>',
    'MSG_LIST_VIEW_CHANGE_SEARCH' => 'ou cambia o teu criterio de busca',
    'MSG_EMPTY_LIST_VIEW_NO_RESULTS' => 'Actualmente non tes rexistros gardados. <item2> ou <item3> agora un.',

    'LBL_CLICK_HERE' => 'Faga clic aquí',
    // contextMenu strings
    'LBL_ADD_TO_FAVORITES' => 'Agregar aos Meus Favoritos',
    'LBL_CREATE_CONTACT' => 'Novo Contacto',
    'LBL_CREATE_CASE' => 'Novo Caso',
    'LBL_CREATE_NOTE' => 'Nova Nota',
    'LBL_CREATE_OPPORTUNITY' => 'Nova Oportunidade',
    'LBL_SCHEDULE_CALL' => 'Rexistrar Chamada',
    'LBL_SCHEDULE_MEETING' => 'Programar Reunión',
    'LBL_CREATE_TASK' => 'Nova Tarefa',
    //web to lead
    'LBL_GENERATE_WEB_TO_LEAD_FORM' => 'Xerar Formulario',
    'LBL_SAVE_WEB_TO_LEAD_FORM' => 'Gardar Formulario',
    'LBL_AVAILABLE_FIELDS' => 'Campos Dispoñibles',
    'LBL_FIRST_FORM_COLUMN' => 'Primeira columna do Formulario',
    'LBL_SECOND_FORM_COLUMN' => 'Segunda columna do formulario',
    'LBL_ASSIGNED_TO_REQUIRED' => 'Falta campo obrigatorio: Asignado a',
    'LBL_RELATED_CAMPAIGN_REQUIRED' => 'Falta campo obrigatorio: Campaña relacionada',
    'LBL_TYPE_OF_PERSON_FOR_FORM' => 'Formulario web para crear ',
    'LBL_TYPE_OF_PERSON_FOR_FORM_DESC' => 'O envío deste formulario creará ',

    'LBL_ADD_ALL_LEAD_FIELDS' => 'Agregar Todos os Campos',
    'LBL_RESET_ALL_LEAD_FIELDS' => 'Restablecer todos os campos',
    'LBL_REMOVE_ALL_LEAD_FIELDS' => 'Quitar Todos os Campos',
    'LBL_NEXT_BTN' => 'Seguinte',
    'LBL_ONLY_IMAGE_ATTACHMENT' => 'Só pode incluirse un adxunto de tipo imaxe',
    'LBL_TRAINING' => 'Foro de Soporte',
    'ERR_MSSQL_DB_CONTEXT' => 'Cambiado o contexto de base de datos a',
    'ERR_MSSQL_WARNING' => 'Aviso:',

    //Meta-Data framework
    'ERR_CANNOT_CREATE_METADATA_FILE' => 'Erro: non existe o arquivo [[file]]. Non se puido crear porque o arquivo co HTML correspondente non foi encontrado.',
    'ERR_CANNOT_FIND_MODULE' => 'Erro: o módulo [module] non existe.',
    'LBL_ALT_ADDRESS' => 'Outra Enderezo:',
    'ERR_SMARTY_UNEQUAL_RELATED_FIELD_PARAMETERS' => 'Erro: Hai un número de argumentos desigual para os elementos &amp;#39;key&amp;#39; e &amp;#39;copy&amp;#39; non array displayParams.',

    /* MySugar Framework (for Home and Dashboard) */
    'LBL_DASHLET_CONFIGURE_GENERAL' => 'Xeral',
    'LBL_DASHLET_CONFIGURE_FILTERS' => 'Filtros',
    'LBL_DASHLET_CONFIGURE_MY_ITEMS_ONLY' => 'Só Os Meus Elementos',
    'LBL_DASHLET_CONFIGURE_TITLE' => 'Título',
    'LBL_DASHLET_CONFIGURE_DISPLAY_ROWS' => 'Mostrar Filas',

    // MySugar status strings
    'LBL_MAX_DASHLETS_REACHED' => 'Alcanzaou o máximo número de dashlets establecido polo seu administrador. Por favor, quite un SuiteCRM Dashlet para poder agregar máis.',
    'LBL_ADDING_DASHLET' => 'Agregando SuiteCRM Dashlet ...',
    'LBL_ADDED_DASHLET' => 'SuiteCRM Dashlet Agregado',
    'LBL_REMOVE_DASHLET_CONFIRM' => '¿Está seguro de que desexa quitar o SuiteCRM Dashlet?',
    'LBL_REMOVING_DASHLET' => 'Quitando SuiteCRM Dashlet ...',
    'LBL_REMOVED_DASHLET' => 'SuiteCRM Dashlet Quitado',
    'LBL_MAX_DASHLET_COLUMNS' => "Se pueden mostrar un máximo de %s columnas.",
    'LBL_RETRIEVING_XML_DATA' => "Hubo un problema al recuperar los datos XML:\n",

    // MySugar Menu Options

    'LBL_LOADING_PAGE' => 'Cargando páxina, espere por favor...',

    'LBL_RELOAD_PAGE' => 'Por favor, <a href="javascript: window.location.reload()">recargue a ventá</a> para usar este SuiteCRM Dashlet.',
    'LBL_ADD_DASHLETS' => 'Agregar Dashlets',
    'LBL_CLOSE_DASHLETS' => 'Cerrar',
    'LBL_OPTIONS' => 'Opcións',
    'LBL_1_COLUMN' => '1 Columna',
    'LBL_2_COLUMN' => '2 Columnas',
    'LBL_3_COLUMN' => '3 Columnas',
    'LBL_PAGE_NAME' => 'Nome de páxina',

    'LBL_SEARCH_RESULTS' => 'Resultados de Busca',
    'LBL_SEARCH_MODULES' => 'Módulos',
    'LBL_SEARCH_TOOLS' => 'Ferramentas',
    'LBL_SEARCH_HELP_TITLE' => 'Consellos de Busca',
    /* End MySugar Framework strings */

    'LBL_NO_IMAGE' => 'Sen Imaxe',

    //adding a label for address copy from left
    'LBL_COPY_ADDRESS_FROM_LEFT' => 'Copiar enderezo da esquerda:',
    'LBL_SAVE_AND_CONTINUE' => 'Gardar e Continuar',

    'LBL_SEARCH_HELP_TEXT' => '<p><br /><strong>Controis de Selección Múltiple</strong></p><ul><li>Click nun valor para seleccionar un atributo.</li><li>Ctrl-click&nbsp;para&nbsp;seleccionar múltiples atributos. Usuarios de Mac usar CMD-click.</li><li>Para seleccionar todos os valores entre dous atributos,&nbsp; click no primeiro valor&nbsp;y logo shift-click no último valor.</li></ul><p><strong>Busca avanzada & Opcións de Deseño</strong><br><br>Ao usar a <b>Busca Avanzada & Opcións de Deseño</b>, vostede pode gardar un conxunto de parámetros de busca e/ou unha Vista de Lista personalizada co fin de obter rápidamente os resultados de busca e presentación en futuras oportunidades. Todas as buscas gardadas aparecen na lista de Buscas Gardadas, identificadas polo seu nome, na que a última busca cargada aparece en primeiro lugar.<br><br>Para personalizar a Vista de Lista, utilice as cajas Esconder Columnas e Mostrar Columnas que permiten seleccionar os campos que se mostrarán no resultado da busca. Por exemplo, vostede pode mostrar ou esconder no resultado da busca detalles tales como o nome do rexistro, o usuario asignado ou o equipo asignado. Para agregar unha columna á Vista de Lista, seleccione o campo correspondente da lista Esconder Columnas e use a frecha cara a esquerda para movelo á lista Mostrar Columnas. Para eliminar unha columna da Vista de Lista, selecciónea na lista Mostrar Columnas e use a frecha cara a dereita para movela á lista Esconder Columnas.<br><br>Se vostede garda as opcións de deseño, poderá cargalas en calquera momento para ver os resultados da súa busca de maneira personalizada.<br><br>Para gardar e actualizar unha busca e/ou un deseño:<ol><li>Ingrese un nome para o resultado da busca no campo <b>Gardar busca como</b> e faga click en <b>Gardar</b>. O nome dado agora móstrase na lista de Buscas gardadas, adxacente ao botón <b>Limpar</b>. </li><li>Para ver unha busca gardada, selecciónea da lista de Buscas gardadas. Os resultados da busca son mostrados na Vista de Lista.</li><li>Para actualizar as propiedades dunha busca gardada, selecciónea da lista, seleccione o novo criterio de busca e/ou a nova opción de deseño na área Busca Avanzada e logo faga click en <b>Actualizar</b> ao lado de <b>Modificar busca actual</b>.</li><li>Para eliminar unha busca gardada, selecciónea na lista Buscas Gardadas e logo faga click en <b>Eliminar</b> ao lado de <b>Modificar busca actual</b>, e logo faga click en <b>OK</b> para confirmar a eliminación.</li></ol><p><strong>Tips</strong><br><br>Pode utilizar o signo % como comodín para realizar unha busca máis ampla.Por exemplo, en vez de buscar resultados iguais a "Manzás" vostede podería cambiar a súa busca a "Manzás%" o que de dará como resultado todos os rexistros que empezan coa palabra Manzás pero tamén outras que poderían estar seguidas por outros carcateres.</p>',

    //resource management
    'ERR_QUERY_LIMIT' => 'Erro: Límite de $limit consultas alcanzado no módulo $module.',
    'ERROR_NOTIFY_OVERRIDE' => 'Erro: ResourceObserver->notify() necesita ser reemplazado.',

    //tracker labels
    'ERR_MONITOR_FILE_MISSING' => 'Erro: non se pode crear monitor porque o arquivo de metadatos está baleiro ou o arquivo non existe.',
    'ERR_MONITOR_NOT_CONFIGURED' => 'Erro: non hai monitor configurado para o nome solicitado',
    'ERR_UNDEFINED_METRIC' => 'Erro: non se pode establecer o valor de métrica definido',
    'ERR_STORE_FILE_MISSING' => 'Erro: non se pode encontrar o arquivo da aplicación da tenda',

    'LBL_MONITOR_ID' => 'Id do Monitor',
    'LBL_USER_ID' => 'Id do Usuario',
    'LBL_MODULE' => 'Módulo',
    'LBL_ITEM_ID' => 'Id do Elemento',
    'LBL_ITEM_SUMMARY' => 'Resumo do Elemento',
    'LBL_ACTION' => 'Acción',
    'LBL_SESSION_ID' => 'Id da Sesión',
    'LBL_BREADCRUMBSTACK_CREATED' => 'BreadCrumbStack creado polo usuario id {0}',
    'LBL_VISIBLE' => 'Dato visible',
    'LBL_DATE_LAST_ACTION' => 'Data',

    //jc:#12287 - For javascript validation messages
    'MSG_IS_NOT_BEFORE' => 'non antes de',
    'MSG_IS_MORE_THAN' => 'é máis que',
    'MSG_IS_LESS_THAN' => 'é menor que',
    'MSG_SHOULD_BE' => 'debe ser',
    'MSG_OR_GREATER' => 'ou máis',

    'LBL_LIST' => 'Lista',
    'LBL_CREATE_BUG' => 'Crear incidencia',

    'LBL_OBJECT_IMAGE' => 'imaxe obxecto',
    //jchi #12300
    'LBL_MASSUPDATE_DATE' => 'Seleccionar data',

    'LBL_VALUE' => 'valor',
    'LBL_VALIDATE_RANGE' => 'non está dentro do rango válido',
    'LBL_CHOOSE_START_AND_END_DATES' => 'Por favor seleccione un rango de data inicial e un rango de data final',
    'LBL_CHOOSE_START_AND_END_ENTRIES' => 'Por favor seleccione un rango de entrada de inicio e de finalización',

    //jchi #  20776
    'LBL_DROPDOWN_LIST_ALL' => 'Todos',

    //Connector
    'ERR_CONNECTOR_FILL_BEANS_SIZE_MISMATCH' => 'Erro: a cantidade do Array do parámetro bean non coincide coa cantidade do Array do resultado.',
    'ERR_MISSING_MAPPING_ENTRY_FORM_MODULE' => 'Erro: Falta o módulo de entrada de asignación.',
    'ERROR_UNABLE_TO_RETRIEVE_DATA' => 'Erro: non se pode recuperar datos de {0} conector. Actualmente, o servizo pode ser inaccesible ou os axustes de configuración poden non ser válidas. Mensaxe de erro do conector: ({1}).',

    // fastcgi checks
    'LBL_FASTCGI_LOGGING' => 'Para uns resultados óptimos ao utilizar o sapi IIS/FastCGI, estableza fastcgi.logging a 0 no seu arquivo php.ini.',

    //Collection Field
    'LBL_COLLECTION_NAME' => 'Nome',
    'LBL_COLLECTION_PRIMARY' => 'Principal',
    'ERROR_MISSING_COLLECTION_SELECTION' => 'Campo obrigatorio baleiro',

    //MB -Fixed Bug #32812 -Max
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_DESCRIPTION' => 'Descrición',

    'LBL_YESTERDAY' => 'onte',
    'LBL_NOW' => 'agora',
    'LBL_TODAY' => 'hoxe',
    'LBL_TOMORROW' => 'mañá',
    'LBL_NEXT_WEEK' => 'a semana que ven',
    'LBL_NEXT_MONDAY' => 'próximo luns',
    'LBL_NEXT_FRIDAY' => 'próximo venres',
    'LBL_TWO_WEEKS' => 'dúas semanas',
    'LBL_NEXT_MONTH' => 'o mes que ven',
    'LBL_FIRST_DAY_OF_NEXT_MONTH' => 'primeiro día do próximo mes',
    'LBL_THREE_MONTHS' => 'tres meses',
    'LBL_SIXMONTHS' => 'seis meses',
    'LBL_NEXT_YEAR' => 'próximo ano',

    //Datetimecombo fields
    'LBL_HOURS' => 'Horas',
    'LBL_MINUTES' => 'Minutos',
    'LBL_MERIDIEM' => 'Meridiano',
    'LBL_DATE' => 'Data',
    'LBL_DASHLET_CONFIGURE_AUTOREFRESH' => 'Actualización automática',

    'LBL_DURATION_DAY' => 'día',
    'LBL_DURATION_HOUR' => 'hora',
    'LBL_DURATION_MINUTE' => 'minuto',
    'LBL_DURATION_DAYS' => 'días',
    'LBL_DURATION_HOURS' => 'Duración (Horas)',
    'LBL_DURATION_MINUTES' => 'Duración (Minutos)',

    //Calendar widget labels
    'LBL_CHOOSE_MONTH' => 'Escoller mes',
    'LBL_ENTER_YEAR' => 'Poñer ano',
    'LBL_ENTER_VALID_YEAR' => 'Por favor, poñer un ano valido',

    //File write erro label
    'ERR_FILE_WRITE' => 'Erro: non se puido escribir o arquivo {0}. Por favor, revise o sistema e os permisos do servidor web.',
    'ERR_FILE_NOT_FOUND' => 'Erro: non se pode cargar o arquivo {0}. Por favor, comprobe os permisos do sistema e do servidor web.',

    'LBL_AND' => 'e',

    // File fields
    'LBL_SEARCH_EXTERNAL_API' => 'Arquivo de fonte externa',
    'LBL_EXTERNAL_SECURITY_LEVEL' => 'Seguridade',

    //IMPORT SAMPLE TEXT
    'LBL_IMPORT_SAMPLE_FILE_TEXT' => '"Este é un arquivo de importación de mostra que é un exemplo dos contidos que se espera dun arquivo que está listo para a importación." "O arquivo é un delimitado por comas .csv, usando comillas como o calificador de campo." "A fila de encabezado é a fila de arriba a maioría no arquivo que contén as etiquetas de campo como se fose a ver na aplicación." "Estas etiquetas utilízanse para o mapeo dos datos no arquivo dos campos da aplicación." "Notas: os nomes de base de datos tamén poderían ser utilizados na cabeceira. Isto é útil cando vostede está usando phpMyAdmin ou calquera outra ferramenta de bases de datos para proporcionar unha lista de exportación de datos a importar." "A orde das columnas non é crítico, o proceso de importación coincide cos datos nos campos apropiados baseados ​na fila de cabeceira". "Para utilizar este arquivo como plantilla, faga o seguinte:" "1. Quitar as filas da mostra dos datos" "2. Retire o texto de axuda que vostede está lendo agora mesmo" "3. dea entrada dos seus propios datos nas filas correspondentes e columnas" " 4. Garde o arquivo nunha ubicación coñecida do seu sistema " " 5. Faga clic na opción Importar no menú Accións na aplicación e escoller o arquivo a subir "',
    //define labels to be used for overriding local values during import/export

    'LBL_NOTIFICATIONS_NONE' => 'Non hai notificacións actuais',
    'LBL_ALT_SORT_DESC' => 'Ordeado descendente',
    'LBL_ALT_SORT_ASC' => 'Ordeado ascendente',
    'LBL_ALT_SORT' => 'Ordear',
    'LBL_ALT_SHOW_OPTIONS' => 'Mostrar opcións',
    'LBL_ALT_HIDE_OPTIONS' => 'Ocultar opcións',
    'LBL_ALT_MOVE_COLUMN_LEFT' => 'Mover selección á lista da esquerda',
    'LBL_ALT_MOVE_COLUMN_RIGHT' => 'Mover selección á lista da dereita',
    'LBL_ALT_MOVE_COLUMN_UP' => 'Mover selección cara arriba no orde da lista',
    'LBL_ALT_MOVE_COLUMN_DOWN' => 'Mover selección cara abaixo no orde da lista',
    'LBL_ALT_INFO' => 'Información',
    'MSG_DUPLICATE' => 'O rexistro {0} que está a punto de crear pode ser un duplicado dun rexistro {0} que xa existe. {1} rexistros que conteñen nomes similares enuméranse a continuación.<br />Faga clic en Crear {1} para continuar a creación deste novo {0}, ou seleccionar un arquivo {0} enuméranse a continuación.',
    'MSG_SHOW_DUPLICATES' => 'O rexistro {0} que está a punto de crear pode ser un duplicado dun rexistro {0} que xa existe. {1} rexistros que conteñen nomes similares enuméranse a continuación. Faga clic en Gardar para continuar coa creación deste novo {0}, ou faga clic en Cancelar para volver ao módulo sen necesidade de crear {0}.',
    'LBL_EMAIL_TITLE' => 'Email',
    'LBL_EMAIL_OPT_TITLE' => 'Email rehusado',
    'LBL_EMAIL_INV_TITLE' => 'email invalido',
    'LBL_EMAIL_PRIM_TITLE' => 'Designar como enderezo de correo electrónico principal',
    'LBL_SELECT_ALL_TITLE' => 'Seleccionar todo',
    'LBL_SELECT_THIS_ROW_TITLE' => 'Seleccionar esta fila',

    //for upload errors
    'UPLOAD_ERROR_TEXT' => 'ERRO: Houbo un erro durante a subida. Código de erro: {0} - {1}',
    'UPLOAD_ERROR_TEXT_SIZEINFO' => 'ERRO: Houbo un erro durante a subida. Código de erro: {0} - {1}. o upload_maxsize é {2}',
    'UPLOAD_ERROR_HOME_TEXT' => 'ERRO: Produciuse un erro durante a subida, por favor póñase en contacto cun administrador para obter axuda.',
    'UPLOAD_MAXIMUM_EXCEEDED' => 'O tamaño da ({0} bytes) Superou o máximo permitido: {1} bytes',
    'UPLOAD_REQUEST_ERROR' => 'Ocurreu un erro. Por favor actualice a súa páxina e volva a intentalo.',

    //508 used Access Keys
    'LBL_EDIT_BUTTON_KEY' => 'E',
    'LBL_EDIT_BUTTON_LABEL' => 'Editar',
    'LBL_EDIT_BUTTON_TITLE' => 'Editar',
    'LBL_DUPLICATE_BUTTON_KEY' => 'U',
    'LBL_DUPLICATE_BUTTON_LABEL' => 'Duplicar',
    'LBL_DUPLICATE_BUTTON_TITLE' => 'Duplicar',
    'LBL_DELETE_BUTTON_KEY' => 'D',
    'LBL_DELETE_BUTTON_LABEL' => 'Eliminar',
    'LBL_DELETE_BUTTON_TITLE' => 'Eliminar',
    'LBL_BULK_ACTION_BUTTON_LABEL' => 'Acción masiva', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_BULK_ACTION_BUTTON_LABEL_MOBILE' => 'Acción', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_SAVE_BUTTON_KEY' => 'S',
    'LBL_SAVE_BUTTON_LABEL' => 'Gardar',
    'LBL_SAVE_BUTTON_TITLE' => 'Gardar',
    'LBL_CANCEL_BUTTON_KEY' => 'X',
    'LBL_CANCEL_BUTTON_LABEL' => 'Cancelar',
    'LBL_CANCEL_BUTTON_TITLE' => 'Cancelar',
    'LBL_FIRST_INPUT_EDIT_VIEW_KEY' => '7',
    'LBL_ADV_SEARCH_LNK_KEY' => '8',
    'LBL_FIRST_INPUT_SEARCH_KEY' => '9',

    'ERR_CONNECTOR_NOT_ARRAY' => 'conector serie en {0} definido incorrectamente ou está baleiro e non se podían usar.',
    'ERR_SUHOSIN' => 'O fluxo de subida está bloqueado por Suhosin, engade un "upload" en suhosin.executor.include.whitelist (Ver suitecrm.log para máis información)',
    'ERR_BAD_RESPONSE_FROM_SERVER' => 'Resposta incorrecta do servidor',
    'LBL_ACCOUNT_PRODUCT_QUOTE_LINK' => 'Presuposto',
    'LBL_ACCOUNT_PRODUCT_SALE_PRICE' => 'Prezo',
    'LBL_EMAIL_CHECK_INTERVAL_DOM' => array(
        '-1' => 'Manualmente',
        '5' => 'Cada 5 minutos',
        '15' => 'Cada 15 minutos',
        '30' => 'Cada 30 minutos',
        '60' => 'Cada hora',
    ),

    'ERR_A_REMINDER_IS_EMPTY_OR_INCORRECT' => 'Un recordatorio é baleiro ou incorrecto.',
    'ERR_REMINDER_IS_NOT_SET_POPUP_OR_EMAIL' => 'Recordatorio non está axustado para un popup ou correo electrónico.',
    'ERR_NO_INVITEES_FOR_REMINDER' => 'Non hai invitados para recordatorio.',
    'LBL_DELETE_REMINDER_CONFIRM' => 'Recordatorio non inclúe invitados, ¿desexa eliminar o recordatorio?',
    'LBL_DELETE_REMINDER' => 'Eliminar Recordatorio',
    'LBL_OK' => 'Ok',

    'LBL_COLUMNS_FILTER_HEADER_TITLE' => 'Escoller columnas',
    'LBL_COLUMN_CHOOSER' => 'Selector de columna',
    'LBL_SAVE_CHANGES_BUTTON_TITLE' => 'Gardar Cambios',
    'LBL_DISPLAYED' => 'Mostrado',
    'LBL_HIDDEN' => 'Oculto',
    'ERR_EMPTY_COLUMNS_LIST' => 'Polo menos un dos elementos é necesario',

    'LBL_FILTER_HEADER_TITLE' => 'Filtro',

    'LBL_CATEGORY' => 'Categoría',
    'LBL_LIST_CATEGORY' => 'Categoría',
    'ERR_FACTOR_TPL_INVALID' => 'A mensaxe de verificación de factor non é válido, ponte en contacto co teu administrador.',
    'LBL_SUBTHEMES' => 'Estilo',
    'LBL_SUBTHEME_OPTIONS_DAWN' => 'Amencer',
    'LBL_SUBTHEME_OPTIONS_DAY' => 'Día',
    'LBL_SUBTHEME_OPTIONS_DUSK' => 'Crepúsculo',
    'LBL_SUBTHEME_OPTIONS_NIGHT' => 'Noite',
    'LBL_SUBTHEME_OPTIONS_NOON' => 'Mediodía', 

    'LBL_CONFIRM_DISREGARD_DRAFT_TITLE' => 'Descartar o borrador',
    'LBL_CONFIRM_DISREGARD_DRAFT_BODY' => 'Esta operación eliminará esta mensaxe, ¿desexa continuar?',
    'LBL_CONFIRM_DISREGARD_EMAIL_TITLE' => 'Saír do cadro de diálogo compoñer',
    'LBL_CONFIRM_DISREGARD_EMAIL_BODY' => 'Ao saír do diálogo de redacción perderase toda a información ingresada, ¿desexa continuar?',
    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE' => 'Aplicar unha plantilla de mensaxe',
    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_BODY' => 'Esta operación borrará o campo corpo da mensaxe, ¿quere continuar?',

    'LBL_CONFIRM_OPT_IN_TITLE' => 'Adhesión confirmada',
    'LBL_OPT_IN_TITLE' => 'Autorizar',
    'LBL_CONFIRM_OPT_IN_DATE' => 'Data de confirmación de adhesión',
    'LBL_CONFIRM_OPT_IN_SENT_DATE' => 'Data de envío de confirmación de autorización',
    'LBL_CONFIRM_OPT_IN_FAIL_DATE' => 'Data de falla na confirmación de autorización',
    'LBL_CONFIRM_OPT_IN_TOKEN' => 'Token de Confirmación de Subscrición',
    'ERR_OPT_IN_TPL_NOT_SET' => 'O modelo de email para autorización non está configurado. Por favor, configúreo nas configuracións de e-mail.',
    'ERR_OPT_IN_RELATION_INCORRECT' => 'Para autorizar é necesario que o e-mail estea relacionado cunha Conta/Contacto/Cliente Potencial/Público Obxectivo',

    'LBL_SECURITYGROUP_NONINHERITABLE' => 'Grupo non herdable',
    'LBL_PRIMARY_GROUP' => "Grupo Principal",

    // footer
    'LBL_SUITE_TOP' => 'Volver á parte superior',
    'LBL_SUITE_SUPERCHARGED' => 'Sobrealimentado por SuiteCRM',
    'LBL_SUITE_POWERED_BY' => 'Desenvolvido por SugarCRM',
    'LBL_SUITE_DESC1' => 'SuiteCRM foi escrito e ensamblado por <a href="https://salesagility.com"> SalesAgility</a>. O programa suminístrase TAL CAL É, sen garantía. Baixo licenza AGPLv3.',
    'LBL_SUITE_DESC2' => 'Este programa é software libre; pode redistribuilo e/ou modificalo baixo os termos da GNU Affero General Public License versión 3 publicada pola Free Software Foundation, incluíndo o permiso adicional na cabeceira do código fonte.',
    'LBL_SUITE_DESC3' => 'SuiteCRM é unha marca rexistrada de SalesAgility Ltd. Todos os nomes doutras empresas e produtos poden ser marcas rexistradas das respectivas empresas coas que se asocian.',
    'LBL_GENERATE_PASSWORD_BUTTON_TITLE' => 'Restablecer Contrasinal',
    'LBL_SEND_CONFIRM_OPT_IN_EMAIL' => 'Enviar e-mail de confirmación de autorización',
    'LBL_CONFIRM_OPT_IN_ONLY_FOR_PERSON' => 'Envio de e-mail de confirmación de autorización só para Contas/Contactos/Clientes Potenciais/Público Obxectivo',
    'LBL_CONFIRM_OPT_IN_IS_DISABLED' => 'O envío de email de confirmación da autorización está desactivado. Actíveo en Configuracións de Email ou contacte co seu administrador.',
    'LBL_CONTACT_HAS_NO_PRIMARY_EMAIL' => 'O envío de e-mail de confirmación de autorización non é posible porque o contacto non posúe un enderezo primario de e-mail rexistrado',
    'LBL_CONFIRM_EMAIL_SENDING_FAILED' => 'Envío de e-mail de confirmación fallado',
    'LBL_CONFIRM_EMAIL_SENT' => 'E-mail de confirmación de autorización enviado con éxito',
);

$app_list_strings['moduleList']['Library'] = 'Biblioteca';
$app_list_strings['moduleList']['EmailAddresses'] = 'Enderezo de Email';
$app_list_strings['project_priority_default'] = 'Media';
$app_list_strings['project_priority_options'] = array(
    'High' => 'Alta',
    'Medium' => 'Media',
    'Low' => 'Baixa',
);

//GDPR lawful basis options
$app_list_strings['lawful_basis_dom'] = array(
    '' => '',
    'consent' => 'Consentimento',
    'contract' => 'Contrato',
    'legal_obligation' => 'Obligación legal',
    'protection_of_interest' => 'Protección do interese',
    'public_interest' => 'Interese público',
    'legitimate_interest' => 'Interese lexítimo',
    'withdrawn' => 'Retirado',
);
//End GDPR lawful basis options

//GDPR lawful basis source options
$app_list_strings['lawful_basis_source_dom'] = array(
    '' => '',
    'website' => 'Sitio Web',
    'phone' => 'Teléfono',
    'given_to_user' => 'Dado ao Usuario',
    'email' => 'Email',
    'third_party' => 'Terceiro',
);
//End GDPR lawful basis source options

$app_list_strings['moduleList']['KBDocuments'] = 'Base de Coñecemento';

$app_list_strings['countries_dom'] = array(
    '' => '',
    'ABU DHABI' => 'ABU DHABI',
    'ADEN' => 'ADEN',
    'AFGHANISTAN' => 'Afganistán',
    'ALBANIA' => 'Albania',
    'ALGERIA' => 'Argelia',
    'AMERICAN SAMOA' => 'SAMOA AMERICANA',
    'ANDORRA' => 'ANDORRA',
    'ANGOLA' => 'ANGOLA',
    'ANTARCTICA' => 'ANTÁRTIDA',
    'ANTIGUA' => 'ANTIGUA',
    'ARXENTINA' => 'Arxentina',
    'ARMENIA' => 'Armenia',
    'ARUBA' => 'ARUBA',
    'AUSTRALIA' => 'Australia',
    'AUSTRIA' => 'Austria',
    'AZERBAIJAN' => 'Azerbaiyán',
    'BAHAMAS' => 'Bahamas',
    'BAHRAIN' => 'Bahrein',
    'BANGLADESH' => 'Bangladesh',
    'BARBADOS' => 'Barbados',
    'BELARUS' => 'Bielorrusia',
    'BELGIUM' => 'Bélgica',
    'BELIZE' => 'Belice',
    'BENIN' => 'Benim',
    'BERMUDA' => 'Bermudas',
    'BHUTAN' => 'Bután',
    'BOLIVIA' => 'Bolivia',
    'BOSNIA' => 'Bosnia',
    'BOTSWANA' => 'Botswana',
    'BOUVET ISLAND' => 'Isla Bouvet',
    'BRAZIL' => 'Brasil',
    'BRITISH ANTARCTICA TERRITORY' => 'Territorio británico na Antártida',
    'BRITISH INDIAN OCEAN TERRITORY' => 'Territorio británico en océano Índico',
    'BRITISH VIRGIN ISLANDS' => 'Illas Vírgenes Británicas',
    'BRITISH WEST INDIES' => 'Indias Occidentales Británicas',
    'BRUNEI' => 'Brunei',
    'BULGARIA' => 'Bulgaria',
    'BURKINA FASO' => 'Burkina Faso',
    'BURUNDI' => 'Burundi',
    'CAMBODIA' => 'Camboya',
    'CAMEROON' => 'Camerún',
    'CANADA' => 'Canadá',
    'CANAL ZONE' => 'Zona do Canal',
    'CANARY ISLAND' => 'ILLAS CANARIAS',
    'CAPE VERDI ISLANDS' => 'Cabo Verde',
    'CAYMAN ISLANDS' => 'ILLAS CAIMAN',
    'CHAD' => 'Chad',
    'CHANNEL ISLAND UK' => 'Illas do Canal Británicas',
    'CHILE' => 'Chile',
    'CHINA' => 'China',
    'CHRISTMAS ISLAND' => 'Isla de Navidad',
    'COCOS (KEELING) ISLAND' => 'COCOS (KEELING) ISLAND',
    'COLOMBIA' => 'COLOMBIA',
    'COMORO ISLANDS' => 'COMORO ISLANDS',
    'CONGO' => 'CONGO',
    'CONGO KINSHASA' => 'CONGO KINSHASA',
    'COOK ISLANDS' => 'ILLAS COOK',
    'COSTA RICA' => 'COSTA RICA',
    'CROATIA' => 'CROACIA',
    'CUBA' => 'CUBA',
    'CURACAO' => 'CURACAO',
    'CYPRUS' => 'CHIPRE',
    'CZECH REPUBLIC' => 'REPÚBLICA CHECA',
    'DAHOMEY' => 'DAHOMEY',
    'DENMARK' => 'DINAMARCA',
    'DJIBOUTI' => 'YIBUTI',
    'DOMINICA' => 'DOMINICA',
    'DOMINICAN REPUBLIC' => 'REPÚBLICA DOMINICANA',
    'DUBAI' => 'DUBAI',
    'ECUADOR' => 'Ecuador',
    'EGYPT' => 'EXIPTO',
    'O SALVADOR' => 'O Salvador',
    'EQUATORIAL GUINEA' => 'GUINEA ECUATORIAL',
    'ESTONIA' => 'Estonia',
    'ETHIOPIA' => 'ETIOPÍA',
    'FAEROE ISLANDS' => 'ILLAS FEROE',
    'FALKLAND ISLANDS' => 'AS ILLAS MALVINAS',
    'FIJI' => 'FIJI',
    'FINLAND' => 'FINLANDIA',
    'FRANCE' => 'FRANCIA',
    'FRENCH GUIANA' => 'GUAYANA FRANCESA',
    'FRENCH POLYNESIA' => 'POLINESIA FRANCESA',
    'GABON' => 'GABÓN',
    'GAMBIA' => 'GAMBIA',
    'GEORGIA' => 'Georgia',
    'GERMANY' => 'ALEMANIA',
    'GHANA' => 'GHANA',
    'GIBRALTAR' => 'GIBRALTAR',
    'GREECE' => 'GRECIA',
    'GREENLAND' => 'GROENLANDIA',
    'GUADELOUPE' => 'GUADALUPE',
    'GUAM' => 'GUAM',
    'GUATEMALA' => 'Guatemala',
    'GUINEA' => 'GUINEA',
    'GUYANA' => 'GUYANA',
    'HAITI' => 'HAITI',
    'HONDURAS' => 'HONDURAS',
    'HONG KONG' => 'HONG KONG',
    'HUNGARY' => 'HUNGRÍA',
    'ICELAND' => 'ISLANDIA',
    'IFNI' => 'IFNI',
    'INDIA' => 'INDIA',
    'INDONESIA' => 'INDONESIA',
    'IRAN' => 'IRAN',
    'IRAQ' => 'IRAQ',
    'IRELAND' => 'IRLANDA',
    'ISRAEL' => 'ISRAEL',
    'ITALY' => 'ITALIA',
    'IVORY COAST' => 'COSTA DE MARFIL',
    'JAMAICA' => 'JAMAICA',
    'JAPAN' => 'JAPON',
    'JORDAN' => 'JORDANIA',
    'KAZAKHSTAN' => 'KAZAJSTÁN',
    'KENYA' => 'KENIA',
    'KOREA' => 'KOREA',
    'KOREA, SOUTH' => 'COREA DO SUR',
    'KUWAIT' => 'KUWAIT',
    'KYRGYZSTAN' => 'KIRGUISTÁN',
    'LAOS' => 'LAOS',
    'LATVIA' => 'LETONIA',
    'LEBANON' => 'LÍBANO',
    'LEEWARD ISLANDS' => 'ILLAS DE SOTAVENTO',
    'LESOTHO' => 'LESOTHO',
    'LIBYA' => 'LIBIA',
    'LIECHTENSTEIN' => 'LIECHTENSTEIN',
    'LITHUANIA' => 'LITUANIA',
    'LUXEMBOURG' => 'LUXEMBURGO',
    'MACAO' => 'MACAO',
    'MACEDONIA' => 'MACEDONIA',
    'MADAGASCAR' => 'MADAGASCAR',
    'MALAWI' => 'MALAWI',
    'MALAYSIA' => 'MALAYSIA',
    'MALDIVES' => 'MALDIVES',
    'MALI' => 'MALI',
    'MALTA' => 'MALTA',
    'MARTINIQUE' => 'MARTINIQUE',
    'MAURITANIA' => 'MAURITANIA',
    'MAURITIUS' => 'MAURITIUS',
    'MELANESIA' => 'MELANESIA',
    'MEXICO' => 'MÉXICO',
    'MOLDOVIA' => 'MOLDOVIA',
    'MONACO' => 'MONACO',
    'MONGOLIA' => 'MONGOLIA',
    'MOROCCO' => 'MARRUECOS',
    'MOZAMBIQUE' => 'MOZAMBIQUE',
    'MYANAMAR' => 'MYANAMAR',
    'NAMIBIA' => 'NAMIBIA',
    'NEPAL' => 'NEPAL',
    'NETHERLANDS' => 'PAÍSES BAJOS',
    'NETHERLANDS ANTILLES' => 'ANTILLAS HOLANDESAS',
    'NETHERLANDS ANTILLES NEUTRAL ZONE' => 'ANTILLAS HOLANDESAS NEUTRAL ZONE',
    'NEW CALADONIA' => 'NOVA CALADONIA',
    'NEW HEBRIDES' => 'NEW HEBRIDES',
    'NEW ZEALAND' => 'NOVA ZELANDA',
    'NICARAGUA' => 'NICARAGUA',
    'NIGER' => 'NIGER',
    'NIGERIA' => 'NIGERIA',
    'NORFOLK ISLAND' => 'ISLA NORFOLK',
    'NORWAY' => 'NORUEGA',
    'OMAN' => 'OMAN',
    'OTHER' => 'OTHER',
    'PACIFIC ISLAND' => 'ISLA DO PACIFICO',
    'PAKISTAN' => 'PAKISTAN',
    'PANAMA' => 'PANAMA',
    'PAPUA NEW GUINEA' => 'PAPUA NOVA GUINEA',
    'PARAGUAY' => 'PARAGUAY',
    'PERU' => 'PERU',
    'PHILIPPINES' => 'FILIPINAS',
    'POLAND' => 'POLONIA',
    'PORTUGAL' => 'PORTUGAL',
    'PORTUGUESE TIMOR' => 'TIMOR ORIENTAL',
    'PORTO RICO' => 'PORTO RICO',
    'QATAR' => 'QATAR',
    'REPUBLIC OF BELARUS' => 'REPÚBLICA DE BIELORRUSIA',
    'REPUBLIC OF SOUTH AFRICA' => 'REPÚBLICA DE SUDÁFRICA',
    'REUNION' => 'REUNION',
    'ROMANIA' => 'RUMANIA',
    'RUSSIA' => 'RUSIA',
    'RWANDA' => 'RUANDA',
    'RYUKYU ISLANDS' => 'RYUKYU ISLANDS',
    'SABAH' => 'SABAH',
    'SAN MARINO' => 'SAN MARINO',
    'SAUDI ARABIA' => 'ARABIA SAUDITA',
    'SENEGAL' => 'SENEGAL',
    'SERBIA' => 'SERBIA',
    'SEYCHELLES' => 'SEYCHELLES',
    'SIERRA LEONE' => 'SIERRA LEONE',
    'SINGAPORE' => 'SINGAPORE',
    'SLOVAKIA' => 'SLOVAKIA',
    'SLOVENIA' => 'SLOVENIA',
    'SOMALILIAND' => 'SOMALILIAND',
    'SOUTH AFRICA' => 'SUDÁFRICA',
    'SOUTH YEMEN' => 'SOUTH YEMEN',
    'SPAIN' => 'ESPAÑA',
    'SPANISH SAHARA' => 'SAHARA ESPAÑOL',
    'SRI LANKA' => 'SRI LANKA',
    'ST. KITTS AND NEVIS' => 'ST. KITTS AND NEVIS',
    'ST. LUCIA' => 'ST. LUCIA',
    'SUDAN' => 'SUDAN',
    'SURINAM' => 'SURINAM',
    'SW AFRICA' => 'SW AFRICA',
    'SWAZILAND' => 'SWAZILAND',
    'SWEDEN' => 'SUECIA',
    'SWITZERLAND' => 'SUIZA',
    'SYRIA' => 'SIRIA',
    'TAIWAN' => 'TAIWAN',
    'TAJIKISTAN' => 'TAJIKISTAN',
    'TANZANIA' => 'TANZANIA',
    'THAILAND' => 'THAILAND',
    'TONGA' => 'TONGA',
    'TRINIDAD' => 'TRINIDAD',
    'TUNISIA' => 'TUNISIA',
    'TURKEY' => 'TURKEY',
    'UGANDA' => 'UGANDA',
    'UKRAINE' => 'UCRANIA',
    'UNITED ARAB EMIRATES' => 'EMIRATOS ÁRABES UNIDOS',
    'UNITED KINGDOM' => 'REINO UNIDO',
    'URUGUAY' => 'URUGUAY',
    'US PACIFIC ISLAND' => 'EE.UU. ISLA DO PACIFICO',
    'US VIRGIN ISLANDS' => 'ILLAS VÍRGENES DE EE.UU.',
    'USA' => 'EE.UU.',
    'UZBEKISTAN' => 'UZBEKISTÁN',
    'VANUATU' => 'VANUATU',
    'VATICAN CITY' => 'CIDADE DO VATICANO',
    'VENEZUELA' => 'VENEZUELA',
    'VIETNAM' => 'VIETNAM',
    'WAKE ISLAND' => 'WAKE ISLAND',
    'WEST INDIES' => 'ANTILLAS',
    'WESTERN SAHARA' => 'SAHARA OCCIDENTAL',
    'YEMEN' => 'YEMEN',
    'ZAIRE' => 'ZAIRE',
    'ZAMBIA' => 'ZAMBIA',
    'ZIMBABWE' => 'ZIMBABWE',
);

$app_list_strings['charset_dom'] = array(
    'BIG-5' => 'BIG-5 (Taiwan e Hong Kong)',
    /*'CP866'     => 'CP866', // ms-dos Cyrillic */
    /*'CP949'     => 'CP949 (Microsoft Korean)', */
    'CP1251' => 'CP1251 (Cirílico de MS)',
    'CP1252' => 'CP1252 (Europa Occidental e EEUU de Ms)',
    'EUC-CN' => 'EUC-CN (Chino Simplificado GB2312)',
    'EUC-JP' => 'EUC-JP (Xaponés Unix)',
    'EUC-KR' => 'EUC-KR (Coreano)',
    'EUC-TW' => 'EUC-TW (Taiwanés)',
    'ISO-2022-JP' => 'ISO-2022-JP (Xaponés)',
    'ISO-2022-KR' => 'ISO-2022-KR (Coreano)',
    'ISO-8859-1' => 'ISO-8859-1 (Europa Occidental e EEUU)',
    'ISO-8859-2' => 'ISO-8859-2 (Centroeuropa e Europa do Este)',
    'ISO-8859-3' => 'ISO-8859-3 (Latín 3)',
    'ISO-8859-4' => 'ISO-8859-4 (Latín 4)',
    'ISO-8859-5' => 'ISO-8859-5 (Cirílico)',
    'ISO-8859-6' => 'ISO-8859-6 (Árabe)',
    'ISO-8859-7' => 'ISO-8859-7 (Grego)',
    'ISO-8859-8' => 'ISO-8859-8 (Hebreo)',
    'ISO-8859-9' => 'ISO-8859-9 (Latín 5)',
    'ISO-8859-10' => 'ISO-8859-10 (Latín 6)',
    'ISO-8859-13' => 'ISO-8859-13 (Latín 7)',
    'ISO-8859-14' => 'ISO-8859-14 (Latín 8)',
    'ISO-8859-15' => 'ISO-8859-15 (Latín 9)',
    'KOI8-R' => 'KOI8-R (Cirílico Ruso)',
    'KOI8-U' => 'KOI8-U (Cirílico Ucraniano)',
    'SJIS' => 'SJIS (Xaponés de MS)',
    'UTF-8' => 'UTF-8',
);

$app_list_strings['timezone_dom'] = array(

    'Africa/Algiers' => 'Africa/Algiers',
    'Africa/Luanda' => 'Africa/Luanda',
    'Africa/Porto-Novo' => 'Africa/Porto-Novo',
    'Africa/Gaborone' => 'Africa/Gaborone',
    'Africa/Ouagadougou' => 'Africa/Ouagadougou',
    'Africa/Bujumbura' => 'Africa/Bujumbura',
    'Africa/Douala' => 'Africa/Douala',
    'Atlantic/Cape_Verde' => 'Atlantic/Cape_Verde',
    'Africa/Bangui' => 'Africa/Bangui',
    'Africa/Ndjamena' => 'Africa/Ndjamena',
    'Indian/Comoro' => 'Indian/Comoro',
    'Africa/Kinshasa' => 'Africa/Kinshasa',
    'Africa/Lubumbashi' => 'Africa/Lubumbashi',
    'Africa/Brazzaville' => 'Africa/Brazzaville',
    'Africa/Abidjan' => 'Africa/Abidjan',
    'Africa/Djibouti' => 'Africa/Djibouti',
    'Africa/Cairo' => 'Africa/Cairo',
    'Africa/Malabo' => 'Africa/Malabo',
    'Africa/Asmera' => 'Africa/Asmera',
    'Africa/Addis_Ababa' => 'Africa/Addis_Ababa',
    'Africa/Libreville' => 'Africa/Libreville',
    'Africa/Banjul' => 'Africa/Banjul',
    'Africa/Accra' => 'Africa/Accra',
    'Africa/Conakry' => 'Africa/Conakry',
    'Africa/Bissau' => 'Africa/Bissau',
    'Africa/Nairobi' => 'Africa/Nairobi',
    'Africa/Maseru' => 'Africa/Maseru',
    'Africa/Monrovia' => 'Africa/Monrovia',
    'Africa/Tripoli' => 'Africa/Tripoli',
    'Indian/Antananarivo' => 'Indian/Antananarivo',
    'Africa/Blantyre' => 'Africa/Blantyre',
    'Africa/Bamako' => 'Africa/Bamako',
    'Africa/Nouakchott' => 'Africa/Nouakchott',
    'Indian/Mauritius' => 'Indian/Mauritius',
    'Indian/Mayotte' => 'Indian/Mayotte',
    'Africa/Casablanca' => 'Africa/Casablanca',
    'Africa/El_Aaiun' => 'Africa/El_Aaiun',
    'Africa/Maputo' => 'Africa/Maputo',
    'Africa/Windhoek' => 'Africa/Windhoek',
    'Africa/Niamey' => 'Africa/Niamey',
    'Africa/Lagos' => 'Africa/Lagos',
    'Indian/Reunion' => 'Indian/Reunion',
    'Africa/Kigali' => 'Africa/Kigali',
    'Atlantic/St_Helena' => 'Atlantic/St_Helena',
    'Africa/Sao_Tome' => 'Africa/Sao_Tome',
    'Africa/Dakar' => 'Africa/Dakar',
    'Indian/Mahe' => 'Indian/Mahe',
    'Africa/Freetown' => 'Africa/Freetown',
    'Africa/Mogadishu' => 'Africa/Mogadishu',
    'Africa/Johannesburg' => 'Africa/Johannesburg',
    'Africa/Khartoum' => 'Africa/Khartoum',
    'Africa/Mbabane' => 'Africa/Mbabane',
    'Africa/Dar_es_Salaam' => 'Africa/Dar_es_Salaam',
    'Africa/Lome' => 'Africa/Lome',
    'Africa/Tunis' => 'Africa/Tunis',
    'Africa/Kampala' => 'Africa/Kampala',
    'Africa/Lusaka' => 'Africa/Lusaka',
    'Africa/Harare' => 'Africa/Harare',
    'Antarctica/Casey' => 'Antarctica/Casey',
    'Antarctica/Davis' => 'Antarctica/Davis',
    'Antarctica/Mawson' => 'Antarctica/Mawson',
    'Indian/Kerguelen' => 'Indian/Kerguelen',
    'Antarctica/DumontDUrville' => 'Antarctica/DumontDUrville',
    'Antarctica/Syowa' => 'Antarctica/Syowa',
    'Antarctica/Vostok' => 'Antarctica/Vostok',
    'Antarctica/Rothera' => 'Antarctica/Rothera',
    'Antarctica/Palmer' => 'Antarctica/Palmer',
    'Antarctica/McMurdo' => 'Antarctica/McMurdo',
    'Asia/Kabul' => 'Asia/Kabul',
    'Asia/Yerevan' => 'Asia/Yerevan',
    'Asia/Baku' => 'Asia/Baku',
    'Asia/Bahrain' => 'Asia/Bahrain',
    'Asia/Dhaka' => 'Asia/Dhaka',
    'Asia/Thimphu' => 'Asia/Thimphu',
    'Indian/Chagos' => 'Indian/Chagos',
    'Asia/Brunei' => 'Asia/Brunei',
    'Asia/Rangoon' => 'Asia/Rangoon',
    'Asia/Phnom_Penh' => 'Asia/Phnom_Penh',
    'Asia/Beijing' => 'Asia/Beijing',
    'Asia/Harbin' => 'Asia/Harbin',
    'Asia/Shanghai' => 'Asia/Shanghai',
    'Asia/Chongqing' => 'Asia/Chongqing',
    'Asia/Urumqi' => 'Asia/Urumqi',
    'Asia/Kashgar' => 'Asia/Kashgar',
    'Asia/Hong_Kong' => 'Asia/Hong_Kong',
    'Asia/Taipei' => 'Asia/Taipei',
    'Asia/Macau' => 'Asia/Macau',
    'Asia/Nicosia' => 'Asia/Nicosia',
    'Asia/Tbilisi' => 'Asia/Tbilisi',
    'Asia/Dili' => 'Asia/Dili',
    'Asia/Calcutta' => 'Asia/Calcutta',
    'Asia/Jakarta' => 'Asia/Jakarta',
    'Asia/Pontianak' => 'Asia/Pontianak',
    'Asia/Makassar' => 'Asia/Makassar',
    'Asia/Jayapura' => 'Asia/Jayapura',
    'Asia/Tehran' => 'Asia/Tehran',
    'Asia/Baghdad' => 'Asia/Baghdad',
    'Asia/Jerusalem' => 'Asia/Jerusalem',
    'Asia/Tokyo' => 'Asia/Tokyo',
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
    'Asia/Beirut' => 'Asia/Beirut',
    'Asia/Kuala_Lumpur' => 'Asia/Kuala_Lumpur',
    'Asia/Kuching' => 'Asia/Kuching',
    'Indian/Maldives' => 'Indian/Maldives',
    'Asia/Hovd' => 'Asia/Hovd',
    'Asia/Ulaanbaatar' => 'Asia/Ulaanbaatar',
    'Asia/Choibalsan' => 'Asia/Choibalsan',
    'Asia/Katmandu' => 'Asia/Katmandu',
    'Asia/Muscat' => 'Asia/Muscat',
    'Asia/Karachi' => 'Asia/Karachi',
    'Asia/Gaza' => 'Asia/Gaza',
    'Asia/Manila' => 'Asia/Manila',
    'Asia/Qatar' => 'Asia/Qatar',
    'Asia/Riyadh' => 'Asia/Riyadh',
    'Asia/Singapore' => 'Asia/Singapore',
    'Asia/Colombo' => 'Asia/Colombo',
    'Asia/Damascus' => 'Asia/Damascus',
    'Asia/Dushanbe' => 'Asia/Dushanbe',
    'Asia/Bangkok' => 'Asia/Bangkok',
    'Asia/Ashgabat' => 'Asia/Ashgabat',
    'Asia/Dubai' => 'Asia/Dubai',
    'Asia/Samarkand' => 'Asia/Samarkand',
    'Asia/Tashkent' => 'Asia/Tashkent',
    'Asia/Saigon' => 'Asia/Saigon',
    'Asia/Aden' => 'Asia/Aden',
    'Australia/Darwin' => 'Australia/Darwin',
    'Australia/Perth' => 'Australia/Perth',
    'Australia/Brisbane' => 'Australia/Brisbane',
    'Australia/Lindeman' => 'Australia/Lindeman',
    'Australia/Adelaide' => 'Australia/Adelaide',
    'Australia/Hobart' => 'Australia/Hobart',
    'Australia/Currie' => 'Australia/Currie',
    'Australia/Melbourne' => 'Australia/Melbourne',
    'Australia/Sydney' => 'Australia/Sydney',
    'Australia/Broken_Hill' => 'Australia/Broken_Hill',
    'Indian/Christmas' => 'Indian/Christmas',
    'Pacific/Rarotonga' => 'Pacific/Rarotonga',
    'Indian/Cocos' => 'Indian/Cocos',
    'Pacific/Fiji' => 'Pacific/Fiji',
    'Pacific/Gambier' => 'Pacific/Gambier',
    'Pacific/Marquesas' => 'Pacific/Marquesas',
    'Pacific/Tahiti' => 'Pacific/Tahiti',
    'Pacific/Guam' => 'Pacific/Guam',
    'Pacific/Tarawa' => 'Pacific/Tarawa',
    'Pacific/Enderbury' => 'Pacific/Enderbury',
    'Pacific/Kiritimati' => 'Pacific/Kiritimati',
    'Pacific/Saipan' => 'Pacific/Saipan',
    'Pacific/Majuro' => 'Pacific/Majuro',
    'Pacific/Kwajalein' => 'Pacific/Kwajalein',
    'Pacific/Truk' => 'Pacific/Truk',
    'Pacific/Pohnpei' => 'Pacífico/Pohnpei',
    'Pacific/Kosrae' => 'Pacific/Kosrae',
    'Pacific/Nauru' => 'Pacific/Nauru',
    'Pacific/Noumea' => 'Pacific/Noumea',
    'Pacific/Auckland' => 'Pacific/Auckland',
    'Pacific/Chatham' => 'Pacific/Chatham',
    'Pacific/Niue' => 'Pacific/Niue',
    'Pacific/Norfolk' => 'Pacific/Norfolk',
    'Pacific/Palau' => 'Pacific/Palau',
    'Pacific/Port_Moresby' => 'Pacific/Port_Moresby',
    'Pacific/Pitcairn' => 'Pacific/Pitcairn',
    'Pacific/Pago_Pago' => 'Pacific/Pago_Pago',
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
    'Europe/London' => 'Europe/London',
    'Europe/Dublin' => 'Europe/Dublin',
    'WET' => 'WET',
    'CET' => 'CET',
    'MET' => 'MET',
    'EET' => 'EET',
    'Europe/Tirane' => 'Europe/Tirane',
    'Europe/Andorra' => 'Europe/Andorra',
    'Europe/Vienna' => 'Europe/Vienna',
    'Europe/Minsk' => 'Europe/Minsk',
    'Europe/Brussels' => 'Europe/Brussels',
    'Europe/Sofia' => 'Europe/Sofia',
    'Europe/Prague' => 'Europe/Prague',
    'Europe/Copenhagen' => 'Europe/Copenhagen',
    'Atlantic/Faeroe' => 'Atlantic/Faeroe',
    'America/Danmarkshavn' => 'America/Danmarkshavn',
    'America/Scoresbysund' => 'America/Scoresbysund',
    'America/Godthab' => 'America/Godthab',
    'America/Thule' => 'America/Thule',
    'Europe/Tallinn' => 'Europe/Tallinn',
    'Europe/Helsinki' => 'Europe/Helsinki',
    'Europe/Paris' => 'Europe/Paris',
    'Europe/Berlin' => 'Europe/Berlin',
    'Europe/Gibraltar' => 'Europe/Gibraltar',
    'Europe/Athens' => 'Europe/Athens',
    'Europe/Budapest' => 'Europe/Budapest',
    'Atlantic/Reykjavik' => 'Atlantic/Reykjavik',
    'Europe/Rome' => 'Europe/Rome',
    'Europe/Riga' => 'Europe/Riga',
    'Europe/Vaduz' => 'Europe/Vaduz',
    'Europe/Vilnius' => 'Europe/Vilnius',
    'Europe/Luxembourg' => 'Europe/Luxembourg',
    'Europe/Malta' => 'Europe/Malta',
    'Europe/Chisinau' => 'Europe/Chisinau',
    'Europe/Monaco' => 'Europe/Monaco',
    'Europe/Amsterdam' => 'Europe/Amsterdam',
    'Europe/Oslo' => 'Europe/Oslo',
    'Europe/Warsaw' => 'Europe/Warsaw',
    'Europe/Lisbon' => 'Europe/Lisbon',
    'Atlantic/Azores' => 'Atlantic/Azores',
    'Atlantic/Madeira' => 'Atlantic/Madeira',
    'Europe/Bucharest' => 'Europe/Bucharest',
    'Europe/Kaliningrad' => 'Europe/Kaliningrad',
    'Europe/Moscow' => 'Europe/Moscow',
    'Europe/Samara' => 'Europe/Samara',
    'Asia/Yekaterinburg' => 'Asia/Yekaterinburg',
    'Asia/Omsk' => 'Asia/Omsk',
    'Asia/Novosibirsk' => 'Asia/Novosibirsk',
    'Asia/Krasnoyarsk' => 'Asia/Krasnoyarsk',
    'Asia/Irkutsk' => 'Asia/Irkutsk',
    'Asia/Yakutsk' => 'Asia/Yakutsk',
    'Asia/Vladivostok' => 'Asia/Vladivostok',
    'Asia/Sakhalin' => 'Asia/Sakhalin',
    'Asia/Magadan' => 'Asia/Magadan',
    'Asia/Kamchatka' => 'Asia/Kamchatka',
    'Asia/Anadyr' => 'Asia/Anadyr',
    'Europe/Belgrade' => 'Europe/Belgrade',
    'Europe/Madrid' => 'Europe/Madrid',
    'Africa/Ceuta' => 'Africa/Ceuta',
    'Atlantic/Canary' => 'Atlantic/Canary',
    'Europe/Stockholm' => 'Europe/Stockholm',
    'Europe/Zurich' => 'Europe/Zurich',
    'Europe/Istanbul' => 'Europe/Istanbul',
    'Europe/Kiev' => 'Europe/Kiev',
    'Europe/Uzhgorod' => 'Europe/Uzhgorod',
    'Europe/Zaporozhye' => 'Europe/Zaporozhye',
    'Europe/Simferopol' => 'Europe/Simferopol',
    'America/New_York' => 'America/New_York',
    'America/Chicago' => 'America/Chicago',
    'America/North_Dakota/Center' => 'America/North_Dakota/Center',
    'America/Denver' => 'America/Denver',
    'America/Los_Angeles' => 'America/Los_Angeles',
    'America/Juneau' => 'America/Juneau',
    'America/Yakutat' => 'America/Yakutat',
    'America/Anchorage' => 'America/Anchorage',
    'America/Nome' => 'America/Nome',
    'America/Adak' => 'America/Adak',
    'Pacific/Honolulu' => 'Pacific/Honolulu',
    'America/Phoenix' => 'America/Phoenix',
    'America/Boise' => 'America/Boise',
    'America/Indiana/Indianapolis' => 'America/Indiana/Indianapolis',
    'America/Indiana/Marengo' => 'America/Indiana/Marengo',
    'America/Indiana/Knox' => 'America/Indiana/Knox',
    'America/Indiana/Vevay' => 'America/Indiana/Vevay',
    'America/Kentucky/Louisville' => 'America/Kentucky/Louisville',
    'America/Kentucky/Monticello' => 'America/Kentucky/Monticello',
    'America/Detroit' => 'America/Detroit',
    'America/Menominee' => 'America/Menominee',
    'America/St_Johns' => 'America/St_Johns',
    'America/Goose_Bay' => 'America/Goose_Bay',
    'America/Halifax' => 'America/Halifax',
    'America/Glace_Bay' => 'America/Glace_Bay',
    'America/Montreal' => 'America/Montreal',
    'America/Toronto' => 'America/Toronto',
    'America/Thunder_Bay' => 'America/Thunder_Bay',
    'America/Nipigon' => 'America/Nipigon',
    'America/Rainy_River' => 'America/Rainy_River',
    'America/Winnipeg' => 'America/Winnipeg',
    'America/Regina' => 'America/Regina',
    'America/Swift_Current' => 'America/Swift_Current',
    'America/Edmonton' => 'America/Edmonton',
    'America/Vancouver' => 'America/Vancouver',
    'America/Dawson_Creek' => 'America/Dawson_Creek',
    'America/Pangnirtung' => 'America/Pangnirtung',
    'America/Iqaluit' => 'America/Iqaluit',
    'America/Coral_Harbour' => 'America/Coral_Harbour',
    'America/Rankin_Inlet' => 'America/Rankin_Inlet',
    'America/Cambridge_Bay' => 'America/Cambridge_Bay',
    'America/Yellowknife' => 'America/Yellowknife',
    'America/Inuvik' => 'America/Inuvik',
    'America/Whitehorse' => 'America/Whitehorse',
    'America/Dawson' => 'America/Dawson',
    'America/Cancun' => 'America/Cancun',
    'America/Merida' => 'America/Merida',
    'America/Monterrey' => 'America/Monterrey',
    'America/Mexico_City' => 'America/Mexico_City',
    'America/Chihuahua' => 'America/Chihuahua',
    'America/Hermosillo' => 'America/Hermosillo',
    'America/Mazatlan' => 'America/Mazatlan',
    'America/Tijuana' => 'America/Tijuana',
    'America/Anguilla' => 'America/Anguilla',
    'America/Antigua' => 'America/Antigua',
    'America/Nassau' => 'America/Nassau',
    'America/Barbados' => 'America/Barbados',
    'America/Belize' => 'America/Belize',
    'Atlantic/Bermuda' => 'Atlantic/Bermuda',
    'America/Cayman' => 'America/Cayman',
    'America/Costa_Rica' => 'America/Costa_Rica',
    'America/Havana' => 'America/Havana',
    'America/Dominica' => 'America/Dominica',
    'America/Santo_Domingo' => 'America/Santo_Domingo',
    'America/El_Salvador' => 'America/El_Salvador',
    'America/Grenada' => 'America/Grenada',
    'America/Guadeloupe' => 'America/Guadeloupe',
    'America/Guatemala' => 'America/Guatemala',
    'America/Port-au-Prince' => 'America/Port-au-Prince',
    'America/Tegucigalpa' => 'America/Tegucigalpa',
    'America/Jamaica' => 'America/Jamaica',
    'America/Martinique' => 'America/Martinique',
    'America/Montserrat' => 'America/Montserrat',
    'America/Managua' => 'America/Managua',
    'America/Panama' => 'America/Panama',
    'America/Puerto_Rico' => 'America/Puerto_Rico',
    'America/St_Kitts' => 'America/St_Kitts',
    'America/St_Lucia' => 'America/St_Lucia',
    'America/Miquelon' => 'America/Miquelon',
    'America/St_Vincent' => 'America/St_Vincent',
    'America/Grand_Turk' => 'America/Grand_Turk',
    'America/Tortola' => 'America/Tortola',
    'America/St_Thomas' => 'America/St_Thomas',
    'America/Arxentina/Buenos_Aires' => 'America/Arxentina/Buenos_Aires',
    'America/Arxentina/Cordoba' => 'America/Arxentina/Cordoba',
    'America/Arxentina/Tucuman' => 'America/Arxentina/Tucuman',
    'America/Arxentina/La_Rioja' => 'America/Arxentina/La_Rioja',
    'America/Arxentina/San_Juan' => 'America/Arxentina/San_Juan',
    'America/Arxentina/Jujuy' => 'America/Arxentina/Jujuy',
    'America/Arxentina/Catamarca' => 'America/Arxentina/Catamarca',
    'America/Arxentina/Mendoza' => 'America/Arxentina/Mendoza',
    'America/Arxentina/Rio_Gallegos' => 'America/Arxentina/Rio_Gallegos',
    'America/Arxentina/Ushuaia' => 'America/Arxentina/Ushuaia',
    'America/Aruba' => 'America/Aruba',
    'America/La_Paz' => 'America/La_Paz',
    'America/Noronha' => 'America/Noronha',
    'America/Belem' => 'America/Belem',
    'America/Fortaleza' => 'America/Fortaleza',
    'America/Recife' => 'America/Recife',
    'America/Araguaina' => 'America/Araguaina',
    'America/Maceio' => 'America/Maceio',
    'America/Bahia' => 'America/Bahia',
    'America/Sao_Paulo' => 'America/Sao_Paulo',
    'America/Campo_Grande' => 'America/Campo_Grande',
    'America/Cuiaba' => 'America/Cuiaba',
    'America/Porto_Velho' => 'America/Porto_Velho',
    'America/Boa_Vista' => 'America/Boa_Vista',
    'America/Manaus' => 'America/Manaus',
    'America/Eirunepe' => 'America/Eirunepe',
    'America/Rio_Branco' => 'America/Rio_Branco',
    'America/Santiago' => 'America/Santiago',
    'Pacific/Easter' => 'Pacific/Easter',
    'America/Bogota' => 'America/Bogota',
    'America/Curacao' => 'America/Curacao',
    'America/Guayaquil' => 'America/Guayaquil',
    'Pacific/Galapagos' => 'Pacific/Galapagos',
    'Atlantic/Stanley' => 'Atlantic/Stanley',
    'America/Cayenne' => 'America/Cayenne',
    'America/Guyana' => 'America/Guyana',
    'America/Asuncion' => 'America/Asuncion',
    'America/Lima' => 'America/Lima',
    'Atlantic/South_Georgia' => 'Atlantic/South_Georgia',
    'America/Paramaribo' => 'America/Paramaribo',
    'America/Port_of_Spain' => 'America/Port_of_Spain',
    'America/Montevideo' => 'America/Montevideo',
    'America/Caracas' => 'America/Caracas',
);

$app_list_strings['eapm_list'] = array(
    'Sugar' => 'SuiteCRM',
    'WebEx' => 'WebEx',
    'GoToMeeting' => 'GoToMeeting',
    'IBMSmartCloud' => 'IBM SmartCloud',
    'Google' => 'Google',
    'Box' => 'Box.net',
    'Facebook' => 'Facebook',
    'Twitter' => 'Twitter',
);
$app_list_strings['eapm_list_import'] = array(
    'Google' => 'Contactos de Google',
);
$app_list_strings['eapm_list_documents'] = array(
    'Google' => 'Documentos de Google',
);
$app_list_strings['token_status'] = array(
    1 => 'Solicitude',
    2 => 'Acceso',
    3 => 'Invalido',
);
// PR 5464
$app_list_strings ['emailTemplates_type_list'] = array(
    '' => '',
    'campaign' => 'Campaña',
    'email' => 'Email',
    'event' => 'Evento',
);

$app_list_strings ['emailTemplates_type_list_campaigns'] = array(
    '' => '',
    'campaign' => 'Campaña',
);

$app_list_strings ['emailTemplates_type_list_no_workflow'] = array(
    '' => '',
    'campaign' => 'Campaña',
    'email' => 'Email',
    'event' => 'Evento',
    'system' => 'Sistema',
);

// knowledge base
$app_list_strings['moduleList']['AOK_KnowledgeBase'] = 'Base de Coñecemento'; // Shows in the ALL menu entries
$app_list_strings['moduleList']['AOK_Knowledge_Base_Categories'] = 'KB - Categorías'; // Shows in the ALL menu entries
$app_list_strings['aok_status_list']['Draft'] = 'Borrador';
$app_list_strings['aok_status_list']['Expired'] = 'Expirado';
$app_list_strings['aok_status_list']['In_Review'] = 'En revisión';
//$app_list_strings['aok_status_list']['Published'] = 'Published';
$app_list_strings['aok_status_list']['published_private'] = 'Particular';
$app_list_strings['aok_status_list']['published_public'] = 'Público';

$app_list_strings['moduleList']['FP_events'] = 'Eventos';
$app_list_strings['moduleList']['FP_Event_Locations'] = 'Ubicacións';

//events
$app_list_strings['fp_event_invite_status_dom']['Invited'] = 'Invitados';
$app_list_strings['fp_event_invite_status_dom']['Not Invited'] = 'Non Invitados';
$app_list_strings['fp_event_invite_status_dom']['Attended'] = 'Asistentes';
$app_list_strings['fp_event_invite_status_dom']['Not Attended'] = 'Non Asistentes';
$app_list_strings['fp_event_status_dom']['Accepted'] = 'Aceptado';
$app_list_strings['fp_event_status_dom']['Declined'] = 'Rexeitado';
$app_list_strings['fp_event_status_dom']['No Response'] = 'Sen resposta';

$app_strings['LBL_STATUS_EVENT'] = 'Estado de Invitación';
$app_strings['LBL_ACCEPT_STATUS'] = 'Aceptar estato';
$app_strings['LBL_LISTVIEW_OPTION_CURRENT'] = 'Seleccionar Páxina Actual';
$app_strings['LBL_LISTVIEW_OPTION_ENTIRE'] = 'Seleccionar Todo';
$app_strings['LBL_LISTVIEW_NONE'] = 'Quitar Selección';

//aod
$app_list_strings['moduleList']['AOD_IndexEvent'] = 'Evento índice';
$app_list_strings['moduleList']['AOD_Index'] = 'Índice';

$app_list_strings['moduleList']['AOP_Case_Events'] = 'Eventos de Casos';
$app_list_strings['moduleList']['AOP_Case_Updates'] = 'Actualizacións de Casos';
$app_strings['LBL_AOP_EMAIL_REPLY_DELIMITER'] = '========== Por favor responda por riba desta liña ==========';


//aop PR 5426
$app_list_strings['moduleList']['JAccount'] = 'Conta Joomla';

$app_list_strings['case_state_default_key'] = 'Open';
$app_list_strings['case_state_dom'] =
    array(
        'Open' => 'Aberto',
        'Closed' => 'Cerrado',
    );
$app_list_strings['case_status_default_key'] = 'Open_New';
$app_list_strings['case_status_dom'] =
    array(
        'Open_New' => 'Novo',
        'Open_Assigned' => 'Asignado',
        'Closed_Closed' => 'Cerrado',
        'Open_Pending Input' => 'Pendente de Información',
        'Closed_Rejected' => 'Rexeitado',
        'Closed_Duplicate' => 'Duplicado',
    );
$app_list_strings['contact_portal_user_type_dom'] =
    array(
        'Single' => 'Usuario individual',
        'Account' => 'Conta de usuario',
    );
$app_list_strings['dom_email_distribution_for_auto_create'] = array(
    'AOPDefault' => 'Usa o AOP predeterminado',
    'singleUser' => 'Usuario individual',
    'roundRobin' => 'Quenda Rotatoria',
    'leastBusy' => 'Menos-Ocupado',
    'random' => 'Aleatorio',
);

//aor
$app_list_strings['moduleList']['AOR_Reports'] = 'Informes';
$app_list_strings['moduleList']['AOR_Conditions'] = 'Condicións de Reportes';
$app_list_strings['moduleList']['AOR_Charts'] = 'Gráficos de Informe';
$app_list_strings['moduleList']['AOR_Fields'] = 'Campos de Reportes';
$app_list_strings['moduleList']['AOR_Scheduled_Reports'] = 'Informes programados';
$app_list_strings['aor_operator_list']['Equal_To'] = 'Igual a';
$app_list_strings['aor_operator_list']['Not_Equal_To'] = 'Non igual a';
$app_list_strings['aor_operator_list']['Greater_Than'] = 'Maior que';
$app_list_strings['aor_operator_list']['Less_Than'] = 'Menor que';
$app_list_strings['aor_operator_list']['Greater_Than_or_Equal_To'] = 'Maior ou igual a';
$app_list_strings['aor_operator_list']['Less_Than_or_Equal_To'] = 'Menor ou igual a';
$app_list_strings['aor_operator_list']['Contains'] = 'Contén';
$app_list_strings['aor_operator_list']['Not_Contains'] = 'Non contén';
$app_list_strings['aor_operator_list']['Starts_With'] = 'Comeza con';
$app_list_strings['aor_operator_list']['Ends_With'] = 'Finaliza con';
$app_list_strings['aor_format_options'][''] = '';
$app_list_strings['aor_format_options']['Y-m-d'] = 'A-m-d';
$app_list_strings['aor_format_options']['m-d-Y'] = 'm-d-A';
$app_list_strings['aor_format_options']['d-m-Y'] = 'd-m-A';
$app_list_strings['aor_format_options']['Y/m/d'] = 'A/m/d';
$app_list_strings['aor_format_options']['m/d/Y'] = 'm/d/A';
$app_list_strings['aor_format_options']['d/m/Y'] = 'd/m/A';
$app_list_strings['aor_format_options']['Y.m.d'] = 'A.m.d';
$app_list_strings['aor_format_options']['m.d.Y'] = 'm.d.A';
$app_list_strings['aor_format_options']['d.m.Y'] = 'd.m.A';
$app_list_strings['aor_format_options']['Ymd'] = 'Amd';
$app_list_strings['aor_format_options']['Y-m'] = 'A-d';
$app_list_strings['aor_format_options']['Y'] = 'A';
$app_list_strings['aor_condition_operator_list']['And'] = 'e';
$app_list_strings['aor_condition_operator_list']['OR'] = 'ou';
$app_list_strings['aor_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aor_condition_type_list']['Field'] = 'Campo';
$app_list_strings['aor_condition_type_list']['Date'] = 'Data';
$app_list_strings['aor_condition_type_list']['Multi'] = 'Multiple';
$app_list_strings['aor_condition_type_list']['Period'] = 'Periodo';
$app_list_strings['aor_condition_type_list']['CurrentUserID'] = 'Usuario actual';
$app_list_strings['aor_date_type_list'][''] = '';
$app_list_strings['aor_date_type_list']['minute'] = 'Minutos';
$app_list_strings['aor_date_type_list']['hour'] = 'Horas';
$app_list_strings['aor_date_type_list']['day'] = 'Días';
$app_list_strings['aor_date_type_list']['week'] = 'Semanas';
$app_list_strings['aor_date_type_list']['month'] = 'Meses';
$app_list_strings['aor_date_type_list']['business_hours'] = 'Horas Laborais';
$app_list_strings['aor_date_options']['now'] = 'Agora';
$app_list_strings['aor_date_options']['field'] = 'Este Campo';
$app_list_strings['aor_date_operator']['now'] = '';
$app_list_strings['aor_date_operator']['plus'] = '+';
$app_list_strings['aor_date_operator']['minus'] = '-';
$app_list_strings['aor_sort_operator'][''] = '';
$app_list_strings['aor_sort_operator']['ASC'] = 'Ascendente';
$app_list_strings['aor_sort_operator']['DESC'] = 'Descendente';
$app_list_strings['aor_function_list'][''] = '';
$app_list_strings['aor_function_list']['COUNT'] = 'Total';
$app_list_strings['aor_function_list']['MIN'] = 'Minimo';
$app_list_strings['aor_function_list']['MAX'] = 'Maximo';
$app_list_strings['aor_function_list']['SUM'] = 'Suma';
$app_list_strings['aor_function_list']['AVG'] = 'Promedio';
$app_list_strings['aor_total_options'][''] = '';
$app_list_strings['aor_total_options']['COUNT'] = 'Total';
$app_list_strings['aor_total_options']['SUM'] = 'Suma';
$app_list_strings['aor_total_options']['AVG'] = 'Promedio';
$app_list_strings['aor_chart_types']['bar'] = 'Gráfico de barras';
$app_list_strings['aor_chart_types']['line'] = 'Gráfico de liñas';
$app_list_strings['aor_chart_types']['pé'] = 'Gráfico de sectores';
$app_list_strings['aor_chart_types']['radar'] = 'Gráfico radial';
$app_list_strings['aor_chart_types']['stacked_bar'] = 'Barra apilada';
$app_list_strings['aor_chart_types']['grouped_bar'] = 'Barra agrupada';
$app_list_strings['aor_scheduled_report_schedule_types']['monthly'] = 'Mensual';
$app_list_strings['aor_scheduled_report_schedule_types']['weekly'] = 'Semanal';
$app_list_strings['aor_scheduled_report_schedule_types']['daily'] = 'Diario';
$app_list_strings['aor_scheduled_reports_status_dom']['active'] = 'Activo';
$app_list_strings['aor_scheduled_reports_status_dom']['inactive'] = 'Inactivo';
$app_list_strings['aor_email_type_list']['Email Address'] = 'Email';
$app_list_strings['aor_email_type_list']['Specify User'] = 'Usuario';
$app_list_strings['aor_email_type_list']['Users'] = 'Usuarios';
$app_list_strings['aor_assign_options']['all'] = 'Todos os usuarios';
$app_list_strings['aor_assign_options']['role'] = 'Todos os usuarios en Role';
$app_list_strings['aor_assign_options']['security_group'] = 'Todos os usuarios no Grupo de Seguridade';
$app_list_strings['date_time_period_list']['today'] = 'Hoxe';
$app_list_strings['date_time_period_list']['yesterday'] = 'Onte';
$app_list_strings['date_time_period_list']['this_week'] = 'Esta semana';
$app_list_strings['date_time_period_list']['last_week'] = 'Última Semana';
$app_list_strings['date_time_period_list']['last_month'] = 'Último mes';
$app_list_strings['date_time_period_list']['this_month'] = 'Este mes';
$app_list_strings['date_time_period_list']['this_quarter'] = 'Este Trimestre';
$app_list_strings['date_time_period_list']['last_quarter'] = 'Úlimo Trimestre';
$app_list_strings['date_time_period_list']['this_year'] = 'Este ano';
$app_list_strings['date_time_period_list']['last_year'] = 'O ano pasado';
$app_strings['LBL_CRON_ON_THE_MONTHDAY'] = 'Nel';
$app_strings['LBL_CRON_ON_THE_WEEKDAY'] = 'el';
$app_strings['LBL_CRON_AT'] = 'á(s)';
$app_strings['LBL_CRON_RAW'] = 'Avanzado';
$app_strings['LBL_CRON_MIN'] = 'Mín';
$app_strings['LBL_CRON_HOUR'] = 'Hora';
$app_strings['LBL_CRON_DAY'] = 'Día';
$app_strings['LBL_CRON_MONTH'] = 'Mes';
$app_strings['LBL_CRON_DOW'] = 'DOW';
$app_strings['LBL_CRON_DAILY'] = 'Diario';
$app_strings['LBL_CRON_WEEKLY'] = 'Semanal';
$app_strings['LBL_CRON_MONTHLY'] = 'Mensual';

//aos
$app_list_strings['moduleList']['AOS_Contracts'] = 'Contratos';
$app_list_strings['moduleList']['AOS_Invoices'] = 'Facturas';
$app_list_strings['moduleList']['AOS_PDF_Templates'] = 'Plantillas PDF';
$app_list_strings['moduleList']['AOS_Product_Categories'] = 'Categorías de Produtos';
$app_list_strings['moduleList']['AOS_Products'] = 'Produtos';
$app_list_strings['moduleList']['AOS_Products_Quotes'] = 'Liñas de Presuposto';
$app_list_strings['moduleList']['AOS_Line_Item_Groups'] = 'Grupos';
$app_list_strings['moduleList']['AOS_Quotes'] = 'Presupostos';
$app_list_strings['aos_quotes_type_dom'][''] = '';
$app_list_strings['aos_quotes_type_dom']['Analyst'] = 'Analista';
$app_list_strings['aos_quotes_type_dom']['Competitor'] = 'Competidor';
$app_list_strings['aos_quotes_type_dom']['Customer'] = 'Cliente';
$app_list_strings['aos_quotes_type_dom']['Integrator'] = 'Integrador';
$app_list_strings['aos_quotes_type_dom']['Investor'] = 'Inversor';
$app_list_strings['aos_quotes_type_dom']['Partner'] = 'Socio';
$app_list_strings['aos_quotes_type_dom']['Press'] = 'Prensa';
$app_list_strings['aos_quotes_type_dom']['Prospect'] = 'Prospecto';
$app_list_strings['aos_quotes_type_dom']['Reseller'] = 'Revendedor';
$app_list_strings['aos_quotes_type_dom']['Other'] = 'Outro';
$app_list_strings['template_ddown_c_list'][''] = '';
$app_list_strings['quote_stage_dom']['Draft'] = 'Borrador';
$app_list_strings['quote_stage_dom']['Negotiation'] = 'Negociación';
$app_list_strings['quote_stage_dom']['Delivered'] = 'Enviado';
$app_list_strings['quote_stage_dom']['On Hold'] = 'En Espera';
$app_list_strings['quote_stage_dom']['Confirmed'] = 'Confirmado';
$app_list_strings['quote_stage_dom']['Closed Accepted'] = 'Cerrado Aceptado';
$app_list_strings['quote_stage_dom']['Closed Lost'] = 'Perdido';
$app_list_strings['quote_stage_dom']['Closed Dead'] = 'Cerrado Morto';
$app_list_strings['quote_term_dom']['Net 15'] = 'Red 15';
$app_list_strings['quote_term_dom']['Net 30'] = 'Red 30';
$app_list_strings['quote_term_dom'][''] = '';
$app_list_strings['approval_status_dom']['Approved'] = 'Aprobado';
$app_list_strings['approval_status_dom']['Not Approved'] = 'Non Aprobado';
$app_list_strings['approval_status_dom'][''] = '';
$app_list_strings['vat_list']['0.0'] = '0%';
$app_list_strings['vat_list']['5.0'] = '5%';
$app_list_strings['vat_list']['7.5'] = '7.5%';
$app_list_strings['vat_list']['17.5'] = '17.5%';
$app_list_strings['vat_list']['20.0'] = '20%';
$app_list_strings['discount_list']['Percentage'] = 'Porcentaxe';
$app_list_strings['discount_list']['Amount'] = 'Cantidade';
$app_list_strings['aos_invoices_type_dom'][''] = '';
$app_list_strings['aos_invoices_type_dom']['Analyst'] = 'Analista';
$app_list_strings['aos_invoices_type_dom']['Competitor'] = 'Competidor';
$app_list_strings['aos_invoices_type_dom']['Customer'] = 'Cliente';
$app_list_strings['aos_invoices_type_dom']['Integrator'] = 'Integrador';
$app_list_strings['aos_invoices_type_dom']['Investor'] = 'Inversor';
$app_list_strings['aos_invoices_type_dom']['Partner'] = 'Socio';
$app_list_strings['aos_invoices_type_dom']['Press'] = 'Prensa';
$app_list_strings['aos_invoices_type_dom']['Prospect'] = 'Prospecto';
$app_list_strings['aos_invoices_type_dom']['Reseller'] = 'Revendedor';
$app_list_strings['aos_invoices_type_dom']['Other'] = 'Outro';
$app_list_strings['invoice_status_dom']['Paid'] = 'Pagado';
$app_list_strings['invoice_status_dom']['Unpaid'] = 'Non Pagado';
$app_list_strings['invoice_status_dom']['Cancelled'] = 'Cancelado';
$app_list_strings['invoice_status_dom'][''] = '';
$app_list_strings['quote_invoice_status_dom']['Not Invoiced'] = 'Non Facturado';
$app_list_strings['quote_invoice_status_dom']['Invoiced'] = 'Facturado';
$app_list_strings['product_code_dom']['XXXX'] = 'XXXX';
$app_list_strings['product_code_dom']['YYYY'] = 'YYYY';
$app_list_strings['product_category_dom']['Laptops'] = 'Laptops';
$app_list_strings['product_category_dom']['Desktops'] = 'Desktops';
$app_list_strings['product_category_dom'][''] = '';
$app_list_strings['product_type_dom']['Good'] = 'Ben';
$app_list_strings['product_type_dom']['Service'] = 'Servizo';
$app_list_strings['product_quote_parent_type_dom']['AOS_Quotes'] = 'Presupostos';
$app_list_strings['product_quote_parent_type_dom']['AOS_Invoices'] = 'Facturas';
$app_list_strings['product_quote_parent_type_dom']['AOS_Contracts'] = 'Contratos';
// STIC-Custom 20220124 MHP - Delete the values of the pdf_template_type_dom  
// STIC#564            
// $app_list_strings['pdf_template_type_dom']['AOS_Quotes'] = 'Presupostos';
// $app_list_strings['pdf_template_type_dom']['AOS_Invoices'] = 'Facturas';
// $app_list_strings['pdf_template_type_dom']['AOS_Contracts'] = 'Contratos';
// $app_list_strings['pdf_template_type_dom']['Accounts'] = 'Contas';
// $app_list_strings['pdf_template_type_dom']['Contacts'] = 'Contactos';
// $app_list_strings['pdf_template_type_dom']['Leads'] = 'Clientes Potenciais';
// END STIC-Custom
$app_list_strings['pdf_template_sample_dom'][''] = '';
$app_list_strings['contract_status_list']['Not Started'] = 'Non Iniciado';
$app_list_strings['contract_status_list']['In Progress'] = 'En Progreso';
$app_list_strings['contract_status_list']['Signed'] = 'Asinado';
$app_list_strings['contract_type_list']['Type'] = 'Tipo';
$app_strings['LBL_PRINT_AS_PDF'] = 'Xerar documento PDF';
$app_strings['LBL_SELECT_TEMPLATE'] = 'Por favor seleccione un formato';
$app_strings['LBL_NO_TEMPLATE'] = 'ERRO\nNon se encontraron formatos.\nPor favor vaia ao módulo de Formatos PDF e cree un';

//aow PR 5775
$app_list_strings['moduleList']['AOW_WorkFlow'] = 'Fluxo de traballo';
$app_list_strings['moduleList']['AOW_Conditions'] = 'Condicións de Fluxo de Traballo';
$app_list_strings['moduleList']['AOW_Processed'] = 'Auditoría de Procesos';
$app_list_strings['moduleList']['AOW_Actions'] = 'Accións de Fluxo de Traballo';
$app_list_strings['aow_status_list']['Active'] = 'Activo';
$app_list_strings['aow_status_list']['Inactive'] = 'Inactivo';
$app_list_strings['aow_operator_list']['Equal_To'] = 'Igual a';
$app_list_strings['aow_operator_list']['Not_Equal_To'] = 'Non igual a';
$app_list_strings['aow_operator_list']['Greater_Than'] = 'Maior que';
$app_list_strings['aow_operator_list']['Less_Than'] = 'Menor que';
$app_list_strings['aow_operator_list']['Greater_Than_or_Equal_To'] = 'Maior ou igual a';
$app_list_strings['aow_operator_list']['Less_Than_or_Equal_To'] = 'Menor ou igual a';
$app_list_strings['aow_operator_list']['Contains'] = 'Contén';
$app_list_strings['aow_operator_list']['Not_Contains'] = 'Non contén';
$app_list_strings['aow_operator_list']['Starts_With'] = 'Comeza con';
$app_list_strings['aow_operator_list']['Ends_With'] = 'Finaliza con';
$app_list_strings['aow_operator_list']['is_null'] = 'É Nulo';
$app_list_strings['aow_operator_list']['is_not_null'] = 'É non Nulo';
$app_list_strings['aow_operator_list']['Anniversary'] = 'Aniversario';
$app_list_strings['aow_process_status_list']['Complete'] = 'Completa';
$app_list_strings['aow_process_status_list']['Running'] = 'Executando';
$app_list_strings['aow_process_status_list']['Pending'] = 'Pendente';
$app_list_strings['aow_process_status_list']['Failed'] = 'Fallado';
$app_list_strings['aow_condition_operator_list']['And'] = 'E';
$app_list_strings['aow_condition_operator_list']['OR'] = 'Ou';
$app_list_strings['aow_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aow_condition_type_list']['Field'] = 'Campo';
$app_list_strings['aow_condition_type_list']['Any_Change'] = 'Calquera cambio';
$app_list_strings['aow_condition_type_list']['SecurityGroup'] = 'En SecurityGroup';
$app_list_strings['aow_condition_type_list']['Date'] = 'Data';
$app_list_strings['aow_condition_type_list']['Multi'] = 'Un de';
$app_list_strings['aow_action_type_list']['Value'] = 'Valor';
$app_list_strings['aow_action_type_list']['Field'] = 'Campo';
$app_list_strings['aow_action_type_list']['Date'] = 'Data';
$app_list_strings['aow_action_type_list']['Round_Robin'] = 'Round Robin';
$app_list_strings['aow_action_type_list']['Least_Busy'] = 'Menos Ocupado';
$app_list_strings['aow_action_type_list']['Random'] = 'Aleatorio';
$app_list_strings['aow_rel_action_type_list']['Value'] = 'Valor';
$app_list_strings['aow_rel_action_type_list']['Field'] = 'Campo';
$app_list_strings['aow_date_type_list'][''] = '';
$app_list_strings['aow_date_type_list']['minute'] = 'Minutos';
$app_list_strings['aow_date_type_list']['hour'] = 'Horas';
$app_list_strings['aow_date_type_list']['day'] = 'Días';
$app_list_strings['aow_date_type_list']['week'] = 'Semanas';
$app_list_strings['aow_date_type_list']['month'] = 'Meses';
$app_list_strings['aow_date_type_list']['business_hours'] = 'Horarios';
$app_list_strings['aow_date_options']['now'] = 'Agora';
$app_list_strings['aow_date_options']['today'] = 'Hoxe';
$app_list_strings['aow_date_options']['field'] = 'Este Campo';
$app_list_strings['aow_date_operator']['now'] = '';
$app_list_strings['aow_date_operator']['plus'] = '+';
$app_list_strings['aow_date_operator']['minus'] = '-';
$app_list_strings['aow_assign_options']['all'] = 'Todos os usuarios';
$app_list_strings['aow_assign_options']['role'] = 'Todos os usuarios en Role';
$app_list_strings['aow_assign_options']['security_group'] = 'Todos os usuarios no Grupo de Seguridade';
$app_list_strings['aow_email_type_list']['Email Address'] = 'Email';
$app_list_strings['aow_email_type_list']['Record Email'] = 'Email do Rexistro';
$app_list_strings['aow_email_type_list']['Related Field'] = 'Campo Relacionado';
$app_list_strings['aow_email_type_list']['Specify User'] = 'Usuario';
$app_list_strings['aow_email_type_list']['Users'] = 'Usuarios';
$app_list_strings['aow_email_to_list']['to'] = 'Para';
$app_list_strings['aow_email_to_list']['cc'] = 'CC';
$app_list_strings['aow_email_to_list']['bcc'] = 'CCO';
$app_list_strings['aow_run_on_list']['All_Records'] = 'Todos os rexistros';
$app_list_strings['aow_run_on_list']['New_Records'] = 'Novos rexistros';
$app_list_strings['aow_run_on_list']['Modified_Records'] = 'Rexistros modificados';
$app_list_strings['aow_run_when_list']['Always'] = 'Sempre';
$app_list_strings['aow_run_when_list']['On_Save'] = 'Só ao gardar';
$app_list_strings['aow_run_when_list']['In_Scheduler'] = 'Só no Planificador';

//gant
$app_list_strings['moduleList']['AM_ProjectTemplates'] = 'Proxectos - Plantillas';
$app_list_strings['moduleList']['AM_TaskTemplates'] = 'Plantillas de tarefas de proxecto';
$app_list_strings['relationship_type_list']['FS'] = 'Finalizar para iniciar';
$app_list_strings['relationship_type_list']['SS'] = 'Iniciar para iniciar';
$app_list_strings['duration_unit_dom']['Days'] = 'Días';
$app_list_strings['duration_unit_dom']['Hours'] = 'Horas';
$app_strings['LBL_GANTT_BUTTON_LABEL'] = 'Ver Gantt';
$app_strings['LBL_DETAIL_BUTTON_LABEL'] = 'Ver Detalle';
$app_strings['LBL_CREATE_PROJECT'] = 'Crear Proxecto';

//gmaps
$app_strings['LBL_MAP'] = 'Mapa';

$app_strings['LBL_JJWG_MAPS_LNG'] = 'Lonxitude';
$app_strings['LBL_JJWG_MAPS_LAT'] = 'Latitude';
$app_strings['LBL_JJWG_MAPS_GEOCODE_STATUS'] = 'Estado de Xeocodificación';
$app_strings['LBL_JJWG_MAPS_ADDRESS'] = 'Enderezo';

$app_list_strings['moduleList']['jjwg_Maps'] = 'Mapas';
$app_list_strings['moduleList']['jjwg_Markers'] = 'Mapas - marcadores';
$app_list_strings['moduleList']['jjwg_Areas'] = 'Mapas - Áreas';
$app_list_strings['moduleList']['jjwg_Address_Cache'] = 'Mapas - Caché de Enderezos';

$app_list_strings['moduleList']['jjwp_Partners'] = 'Socios JJWP';

$app_list_strings['map_unit_type_list']['mi'] = 'Millas';
$app_list_strings['map_unit_type_list']['km'] = 'Quilómetros';

$app_list_strings['map_module_type_list']['Accounts'] = 'Contas';
$app_list_strings['map_module_type_list']['Contacts'] = 'Contactos';
$app_list_strings['map_module_type_list']['Cases'] = 'Casos';
$app_list_strings['map_module_type_list']['Leads'] = 'Clientes Potenciais';
$app_list_strings['map_module_type_list']['Meetings'] = 'Reunións';
$app_list_strings['map_module_type_list']['Opportunities'] = 'Oportunidades';
$app_list_strings['map_module_type_list']['Project'] = 'Proxectos';
$app_list_strings['map_module_type_list']['Prospects'] = 'Público Obxectivo';

$app_list_strings['map_relate_type_list']['Accounts'] = 'Conta';
$app_list_strings['map_relate_type_list']['Contacts'] = 'Contacto';
$app_list_strings['map_relate_type_list']['Cases'] = 'Caso';
$app_list_strings['map_relate_type_list']['Leads'] = 'Cliente Potencial';
$app_list_strings['map_relate_type_list']['Meetings'] = 'Reunión';
$app_list_strings['map_relate_type_list']['Opportunities'] = 'Oportunidade';
$app_list_strings['map_relate_type_list']['Project'] = 'Proxecto';
$app_list_strings['map_relate_type_list']['Prospects'] = 'Público Obxectivo';

$app_list_strings['marker_image_list']['accident'] = 'Accidente';
$app_list_strings['marker_image_list']['administration'] = 'Administración';
$app_list_strings['marker_image_list']['agriculture'] = 'Agricultura';
$app_list_strings['marker_image_list']['aircraft_small'] = 'Aviación pequena';
$app_list_strings['marker_image_list']['airplane_tourism'] = 'Avion Turismo';
$app_list_strings['marker_image_list']['airport'] = 'Aeroporto';
$app_list_strings['marker_image_list']['amphitheater'] = 'Anfiteatro';
$app_list_strings['marker_image_list']['apartment'] = 'Departamento';
$app_list_strings['marker_image_list']['aquarium'] = 'Acuario';
$app_list_strings['marker_image_list']['arch'] = 'Arco';
$app_list_strings['marker_image_list']['atm'] = 'Atm';
$app_list_strings['marker_image_list']['audio'] = 'Audio';
$app_list_strings['marker_image_list']['bank'] = 'Banco';
$app_list_strings['marker_image_list']['bank_euro'] = 'Banco Euro';
$app_list_strings['marker_image_list']['bank_pound'] = 'Banco Libra';
$app_list_strings['marker_image_list']['bar'] = 'Barra';
$app_list_strings['marker_image_list']['beach'] = 'Praia';
$app_list_strings['marker_image_list']['beautiful'] = 'Beleza';
$app_list_strings['marker_image_list']['bicycle_parking'] = 'Estacionamento de Bicicletas';
$app_list_strings['marker_image_list']['big_city'] = 'Cidade Grande';
$app_list_strings['marker_image_list']['bridge'] = 'Ponte';
$app_list_strings['marker_image_list']['bridge_modern'] = 'Ponte Moderno';
$app_list_strings['marker_image_list']['bus'] = 'Bus';
$app_list_strings['marker_image_list']['cable_car'] = 'Cable carril';
$app_list_strings['marker_image_list']['car'] = 'Automóbil';
$app_list_strings['marker_image_list']['car_rental'] = 'Aluguer de Automóbiles';
$app_list_strings['marker_image_list']['carrepair'] = 'Reparación de Automóbiles';
$app_list_strings['marker_image_list']['castle'] = 'Castillo';
$app_list_strings['marker_image_list']['cathedral'] = 'Catedral';
$app_list_strings['marker_image_list']['chapel'] = 'Capilla';
$app_list_strings['marker_image_list']['church'] = 'Igrexa';
$app_list_strings['marker_image_list']['city_square'] = 'Area Central';
$app_list_strings['marker_image_list']['cluster'] = 'Clúster';
$app_list_strings['marker_image_list']['cluster_2'] = 'Clúster 2';
$app_list_strings['marker_image_list']['cluster_3'] = 'Clúster 3';
$app_list_strings['marker_image_list']['cluster_4'] = 'Clúster 4';
$app_list_strings['marker_image_list']['cluster_5'] = 'Clúster 5';
$app_list_strings['marker_image_list']['coffee'] = 'Café';
$app_list_strings['marker_image_list']['community_centre'] = 'Centro Comunitario';
$app_list_strings['marker_image_list']['company'] = 'Compañía';
$app_list_strings['marker_image_list']['conference'] = 'Conferencia';
$app_list_strings['marker_image_list']['construction'] = 'Construción';
$app_list_strings['marker_image_list']['convenience'] = 'Conveniencia';
$app_list_strings['marker_image_list']['court'] = 'Xulgado';
$app_list_strings['marker_image_list']['cruise'] = 'Cruceiro';
$app_list_strings['marker_image_list']['currency_exchange'] = 'Cambio de Moeda';
$app_list_strings['marker_image_list']['customs'] = 'Aduana';
$app_list_strings['marker_image_list']['cycling'] = 'Ciclismo';
$app_list_strings['marker_image_list']['dam'] = 'Represa';
$app_list_strings['marker_image_list']['dentist'] = 'Dentista';
$app_list_strings['marker_image_list']['deptartment_store'] = 'Tenda por Departamentos';
$app_list_strings['marker_image_list']['disability'] = 'Discapacidade';
$app_list_strings['marker_image_list']['disabled_parking'] = 'Estacionamento p/Discapacitados';
$app_list_strings['marker_image_list']['doctor'] = 'Doctor';
$app_list_strings['marker_image_list']['dog_leash'] = 'Correa p/Perros';
$app_list_strings['marker_image_list']['down'] = 'Abaixo';
$app_list_strings['marker_image_list']['down_left'] = 'Abaixo Esquerda';
$app_list_strings['marker_image_list']['down_right'] = 'Abaixo Dereita';
$app_list_strings['marker_image_list']['down_then_left'] = 'Abaixo logo á esquerda';
$app_list_strings['marker_image_list']['down_then_right'] = 'Abaixo logo á dereita';
$app_list_strings['marker_image_list']['drugs'] = 'Drogas';
$app_list_strings['marker_image_list']['elevator'] = 'Elevador';
$app_list_strings['marker_image_list']['embassy'] = 'Embaixada';
$app_list_strings['marker_image_list']['expert'] = 'Experto';
$app_list_strings['marker_image_list']['factory'] = 'Fábrica';
$app_list_strings['marker_image_list']['falling_rocks'] = 'Zona de Derrumbes';
$app_list_strings['marker_image_list']['fast_food'] = 'Comida Rápida';
$app_list_strings['marker_image_list']['festival'] = 'Festival';
$app_list_strings['marker_image_list']['fjord'] = 'Fiordo';
$app_list_strings['marker_image_list']['forest'] = 'Bosque';
$app_list_strings['marker_image_list']['fountain'] = 'Fonte';
$app_list_strings['marker_image_list']['friday'] = 'Venres';
$app_list_strings['marker_image_list']['garden'] = 'Xardín';
$app_list_strings['marker_image_list']['gas_station'] = 'Bomba de Combustible';
$app_list_strings['marker_image_list']['geyser'] = 'Géiser';
$app_list_strings['marker_image_list']['gifts'] = 'Regalos';
$app_list_strings['marker_image_list']['gourmet'] = 'Gourmet';
$app_list_strings['marker_image_list']['grocery'] = 'Almacén';
$app_list_strings['marker_image_list']['hairsalon'] = 'Estilista';
$app_list_strings['marker_image_list']['helicopter'] = 'Helicóptero';
$app_list_strings['marker_image_list']['highway'] = 'Autopista';
$app_list_strings['marker_image_list']['historical_quarter'] = 'Casco Histórico';
$app_list_strings['marker_image_list']['home'] = 'Inicio';
$app_list_strings['marker_image_list']['hospital'] = 'Hospital';
$app_list_strings['marker_image_list']['hostel'] = 'Hostal';
$app_list_strings['marker_image_list']['hotel'] = 'Hotel';
$app_list_strings['marker_image_list']['hotel_1_star'] = 'Hotel 1 Estrela';
$app_list_strings['marker_image_list']['hotel_2_stars'] = 'Hotel 2 Estrelas';
$app_list_strings['marker_image_list']['hotel_3_stars'] = 'Hotel 3 Estrelas';
$app_list_strings['marker_image_list']['hotel_4_stars'] = 'Hotel 4 Estrelas';
$app_list_strings['marker_image_list']['hotel_5_stars'] = 'Hotel 5 Estrelas';
$app_list_strings['marker_image_list']['info'] = 'Información';
$app_list_strings['marker_image_list']['justice'] = 'Xulgado';
$app_list_strings['marker_image_list']['lake'] = 'Lago';
$app_list_strings['marker_image_list']['laundromat'] = 'Lavandeiría';
$app_list_strings['marker_image_list']['left'] = 'Esquerda';
$app_list_strings['marker_image_list']['left_then_down'] = 'Esquerda Logo Abaixo';
$app_list_strings['marker_image_list']['left_then_up'] = 'Esquerda Logo Arriba';
$app_list_strings['marker_image_list']['library'] = 'Biblioteca';
$app_list_strings['marker_image_list']['lighthouse'] = 'Iluminación';
$app_list_strings['marker_image_list']['liquor'] = 'Expendio de Bebidas Alcoholicas';
$app_list_strings['marker_image_list']['lock'] = 'Candado';
$app_list_strings['marker_image_list']['main_road'] = 'Camiño Principal';
$app_list_strings['marker_image_list']['massage'] = 'Masaxes';
$app_list_strings['marker_image_list']['mobile_phone_tower'] = 'Antena de Telefonía Móbil';
$app_list_strings['marker_image_list']['modern_tower'] = 'Torre Moderna';
$app_list_strings['marker_image_list']['monastery'] = 'Monasterio';
$app_list_strings['marker_image_list']['monday'] = 'Luns';
$app_list_strings['marker_image_list']['monument'] = 'Monumento';
$app_list_strings['marker_image_list']['mosque'] = 'Mezquita';
$app_list_strings['marker_image_list']['motorcycle'] = 'Motocicleta';
$app_list_strings['marker_image_list']['museum'] = 'Museo';
$app_list_strings['marker_image_list']['music_live'] = 'Música en Vivo';
$app_list_strings['marker_image_list']['oil_pump_jack'] = 'Gato da bomba de aceite';
$app_list_strings['marker_image_list']['pagoda'] = 'Pagoda';
$app_list_strings['marker_image_list']['palace'] = 'Palacio';
$app_list_strings['marker_image_list']['panoramic'] = 'Vista Panorámica';
$app_list_strings['marker_image_list']['park'] = 'Parque';
$app_list_strings['marker_image_list']['park_and_ride'] = 'Parque e Camiata';
$app_list_strings['marker_image_list']['parking'] = 'Estacionamento';
$app_list_strings['marker_image_list']['photo'] = 'Foto';
$app_list_strings['marker_image_list']['picnic'] = 'Pícnic';
$app_list_strings['marker_image_list']['places_unvisited'] = 'Lugares non Visitados';
$app_list_strings['marker_image_list']['places_visited'] = 'Lugares Visitados';
$app_list_strings['marker_image_list']['playground'] = 'Praza';
$app_list_strings['marker_image_list']['police'] = 'Policía';
$app_list_strings['marker_image_list']['port'] = 'Porto';
$app_list_strings['marker_image_list']['postal'] = 'Postal';
$app_list_strings['marker_image_list']['power_line_pole'] = 'Poste de Liña Eléctrica';
$app_list_strings['marker_image_list']['power_plant'] = 'Planta de Enerxía';
$app_list_strings['marker_image_list']['power_substation'] = 'Subestación de Enerxía';
$app_list_strings['marker_image_list']['public_art'] = 'Arte Público';
$app_list_strings['marker_image_list']['rain'] = 'Chuvia';
$app_list_strings['marker_image_list']['real_estate'] = 'Inmobiliaria';
$app_list_strings['marker_image_list']['regroup'] = 'Reagrupamiento';
$app_list_strings['marker_image_list']['resort'] = 'Complexo';
$app_list_strings['marker_image_list']['restaurant'] = 'Restaurante';
$app_list_strings['marker_image_list']['restaurant_african'] = 'Restaurante Africana';
$app_list_strings['marker_image_list']['restaurant_barbecue'] = 'Restaurante Barbacoa';
$app_list_strings['marker_image_list']['restaurant_buffet'] = 'Restaurante de Bufé';
$app_list_strings['marker_image_list']['restaurant_chinese'] = 'Restaurante Chino';
$app_list_strings['marker_image_list']['restaurant_fish'] = 'Restaurante Pescado';
$app_list_strings['marker_image_list']['restaurant_fish_chips'] = 'Restaurante Chips de Pescado';
$app_list_strings['marker_image_list']['restaurant_gourmet'] = 'Restaurante Gourmet';
$app_list_strings['marker_image_list']['restaurant_greek'] = 'Restaurante Grego';
$app_list_strings['marker_image_list']['restaurant_indian'] = 'Restaurante Hindú';
$app_list_strings['marker_image_list']['restaurant_italian'] = 'Restaurante Italiano';
$app_list_strings['marker_image_list']['restaurant_japanese'] = 'Restaurante Xaponés';
$app_list_strings['marker_image_list']['restaurant_kebab'] = 'Restaurante Brochette';
$app_list_strings['marker_image_list']['restaurant_korean'] = 'Restaurante Coreano';
$app_list_strings['marker_image_list']['restaurant_mediterranean'] = 'Restaurante Mediterráneo';
$app_list_strings['marker_image_list']['restaurant_mexican'] = 'Restaurante Mexicano';
$app_list_strings['marker_image_list']['restaurant_romantic'] = 'Restaurante Romántico';
$app_list_strings['marker_image_list']['restaurant_thai'] = 'Restaurante Thai';
$app_list_strings['marker_image_list']['restaurant_turkish'] = 'Restaurante Turco';
$app_list_strings['marker_image_list']['right'] = 'Dereita';
$app_list_strings['marker_image_list']['right_then_down'] = 'Dereita Logo Abaixo';
$app_list_strings['marker_image_list']['right_then_up'] = 'Dereita Logo Arriba';
$app_list_strings['marker_image_list']['saturday'] = 'Sábado';
$app_list_strings['marker_image_list']['school'] = 'Escola';
$app_list_strings['marker_image_list']['shopping_mall'] = 'Mall';
$app_list_strings['marker_image_list']['shore'] = 'Apuntalamento';
$app_list_strings['marker_image_list']['sight'] = 'Vista';
$app_list_strings['marker_image_list']['small_city'] = 'Pequena Cidade';
$app_list_strings['marker_image_list']['snow'] = 'Neve';
$app_list_strings['marker_image_list']['spaceport'] = 'Porto Espacial';
$app_list_strings['marker_image_list']['speed_100'] = 'Velocidade 100';
$app_list_strings['marker_image_list']['speed_110'] = 'Velocidade 110';
$app_list_strings['marker_image_list']['speed_120'] = 'Velocidade 120';
$app_list_strings['marker_image_list']['speed_130'] = 'Velocidade 130';
$app_list_strings['marker_image_list']['speed_20'] = 'Velocidade 20';
$app_list_strings['marker_image_list']['speed_30'] = 'Velocidade 30';
$app_list_strings['marker_image_list']['speed_40'] = 'Velocidade 40';
$app_list_strings['marker_image_list']['speed_50'] = 'Velocidade 50';
$app_list_strings['marker_image_list']['speed_60'] = 'Velocidade 60';
$app_list_strings['marker_image_list']['speed_70'] = 'Velocidade 70';
$app_list_strings['marker_image_list']['speed_80'] = 'Velocidade 80';
$app_list_strings['marker_image_list']['speed_90'] = 'Velocidade 90';
$app_list_strings['marker_image_list']['speed_hump'] = 'Velocidade Hump';
$app_list_strings['marker_image_list']['stadium'] = 'Estadio';
$app_list_strings['marker_image_list']['statue'] = 'Estatua';
$app_list_strings['marker_image_list']['steam_train'] = 'Tren a Vapor';
$app_list_strings['marker_image_list']['stop'] = 'Parar';
$app_list_strings['marker_image_list']['stoplight'] = 'Semáforo';
$app_list_strings['marker_image_list']['subway'] = 'Subterráneo';
$app_list_strings['marker_image_list']['sun'] = 'Dom';
$app_list_strings['marker_image_list']['sunday'] = 'Domingo';
$app_list_strings['marker_image_list']['supermarket'] = 'Super Mercado';
$app_list_strings['marker_image_list']['synagogue'] = 'Sinagoga';
$app_list_strings['marker_image_list']['tapas'] = 'Tapas';
$app_list_strings['marker_image_list']['taxi'] = 'Taxi';
$app_list_strings['marker_image_list']['taxiway'] = 'Vía p/Taxis';
$app_list_strings['marker_image_list']['teahouse'] = 'Casa de Té';
$app_list_strings['marker_image_list']['telephone'] = 'Teléfono';
$app_list_strings['marker_image_list']['temple_hindu'] = 'Templo Hindú';
$app_list_strings['marker_image_list']['terrace'] = 'Terraza';
$app_list_strings['marker_image_list']['text'] = 'Texto';
$app_list_strings['marker_image_list']['theater'] = 'Teatro';
$app_list_strings['marker_image_list']['theme_park'] = 'Parque Temático';
$app_list_strings['marker_image_list']['thursday'] = 'Xoves';
$app_list_strings['marker_image_list']['toilets'] = 'Aseos';
$app_list_strings['marker_image_list']['toll_station'] = 'Peaxe';
$app_list_strings['marker_image_list']['tower'] = 'Torre';
$app_list_strings['marker_image_list']['traffic_enforcement_camera'] = 'Control de Velocidade';
$app_list_strings['marker_image_list']['train'] = 'Tren';
$app_list_strings['marker_image_list']['tram'] = 'Tranvía';
$app_list_strings['marker_image_list']['truck'] = 'Camión';
$app_list_strings['marker_image_list']['tuesday'] = 'Martes';
$app_list_strings['marker_image_list']['tunnel'] = 'Tunel';
$app_list_strings['marker_image_list']['turn_left'] = 'Xiro á Esquerda';
$app_list_strings['marker_image_list']['turn_right'] = 'Xiro á Dereita';
$app_list_strings['marker_image_list']['university'] = 'Universidade';
$app_list_strings['marker_image_list']['up'] = 'Arriba';
$app_list_strings['marker_image_list']['up_left'] = 'Arriba Esquerda';
$app_list_strings['marker_image_list']['up_right'] = 'Arriba Dereita';
$app_list_strings['marker_image_list']['up_then_left'] = 'Arriba Logo Esquerda';
$app_list_strings['marker_image_list']['up_then_right'] = 'Arriba Logo Dereita';
$app_list_strings['marker_image_list']['vespa'] = 'Vespa';
$app_list_strings['marker_image_list']['video'] = 'Video';
$app_list_strings['marker_image_list']['villa'] = 'Villa';
$app_list_strings['marker_image_list']['water'] = 'Auga';
$app_list_strings['marker_image_list']['waterfall'] = 'Cascada';
$app_list_strings['marker_image_list']['watermill'] = 'Molino de Auga';
$app_list_strings['marker_image_list']['waterpark'] = 'Parque Acuático';
$app_list_strings['marker_image_list']['watertower'] = 'Torre de Auga';
$app_list_strings['marker_image_list']['wednesday'] = 'Mércores';
$app_list_strings['marker_image_list']['wifi'] = 'WiFi';
$app_list_strings['marker_image_list']['wind_turbine'] = 'Turbina de Vento';
$app_list_strings['marker_image_list']['windmill'] = 'Molino de Vento';
$app_list_strings['marker_image_list']['winery'] = 'Lagar';
$app_list_strings['marker_image_list']['work_office'] = 'Oficina';
$app_list_strings['marker_image_list']['world_heritage_site'] = 'Patrimonio da Humanidade';
$app_list_strings['marker_image_list']['zoo'] = 'Zoo';

//Reschedule
$app_list_strings['call_reschedule_dom'][''] = '';
$app_list_strings['call_reschedule_dom']['Out of Office'] = 'Fóra da Oficina';
$app_list_strings['call_reschedule_dom']['In a Meeting'] = 'É unha reunion';

$app_strings['LBL_RESCHEDULE_LABEL'] = 'Replanificacións';
$app_strings['LBL_RESCHEDULE_TITLE'] = 'Por favor ingrese os datos da Replanificaci&oacute;n';
$app_strings['LBL_RESCHEDULE_DATE'] = 'Data';
$app_strings['LBL_RESCHEDULE_REASON'] = 'Raz&oacute;n:';
$app_strings['LBL_RESCHEDULE_ERROR1'] = 'Por favor seleccione unha data v&aacute;lida';
$app_strings['LBL_RESCHEDULE_ERROR2'] = 'Por favor seleccione unha raz&oacute;n';

$app_strings['LBL_RESCHEDULE_PANEL'] = 'Replanificacións';
$app_strings['LBL_RESCHEDULE_HISTORY'] = 'Historial de Intentos de Chamada';
$app_strings['LBL_RESCHEDULE_COUNT'] = 'Intentos de Chamada';

//SecurityGroups
$app_list_strings['moduleList']['SecurityGroups'] = 'Xestión de Seguridade';
$app_strings['LBL_SECURITYGROUP'] = 'Grupo de seguridade';

$app_list_strings['moduleList']['OutboundEmailAccounts'] = 'Contas de correo electrónico saínte';

//social
$app_strings['FACEBOOK_USER_C'] = 'Facebook';
$app_strings['TWITTER_USER_C'] = 'Twitter';
$app_strings['LBL_PANEL_SOCIAL_FEED'] = 'Detalles da actividade Social';

$app_strings['LBL_SUBPANEL_FILTER_LABEL'] = 'Filtro';

$app_strings['LBL_COLLECTION_TYPE'] = 'Tipo';

$app_strings['LBL_ADD_TAB'] = 'Engadir pestana';
$app_strings['LBL_EDIT_TAB'] = 'Editar Pestanas';
$app_strings['LBL_SUITE_DASHBOARD'] = 'Cadro de Mando SuiteCRM'; //Can be translated in all caps. This string will be used by SuiteP template menu actions
$app_strings['LBL_ENTER_DASHBOARD_NAME'] = 'Introduza o nome do Dashboard:';
$app_strings['LBL_NUMBER_OF_COLUMNS'] = 'Número de columnas:';
$app_strings['LBL_DELETE_DASHBOARD1'] = '¿Seguro que desexa eliminar';
$app_strings['LBL_DELETE_DASHBOARD2'] = 'tablero?';
$app_strings['LBL_ADD_DASHBOARD_PAGE'] = 'Agregar unha páxina do Dashboard';
$app_strings['LBL_DELETE_DASHBOARD_PAGE'] = 'Eliminar páxina actual do Dashboard';
$app_strings['LBL_RENAME_DASHBOARD_PAGE'] = 'Cambiar o nome de páxina do Dashboard';
$app_strings['LBL_SUITE_DASHBOARD_ACTIONS'] = 'Accións'; //Can be translated in all caps. This string will be used by SuiteP template menu actions

$app_list_strings['collection_temp_list'] = array(
    'Tasks' => 'Tarefas',
    'Meetings' => 'Reunións',
    'Calls' => 'Chamadas',
    'Notes' => 'Notas',
    'Emails' => 'Correos'
);

$app_list_strings['moduleList']['TemplateEditor'] = 'Editor de Segmento de Plantilla';
$app_strings['LBL_CONFIRM_CANCEL_INLINE_EDITING'] = "Vostede fixo clic afora sen gardar. Faga clic en aceptar se desexa PERDER os seus cambios, ou cancelar se desexa seguir editando";
$app_strings['LBL_LOADING_ERROR_INLINE_EDITING'] = "Houbo un erro ao cargar o campo. A sesión pode ter expirado. Inicia sesión novamente para solucionar este problema";

//SuiteSpots
$app_list_strings['spots_areas'] = array(
    'getSalesSpotsData' => 'Vendas',
    'getAccountsSpotsData' => 'Contas',
    'getLeadsSpotsData' => 'Clientes Potenciais',
    'getServiceSpotsData' => 'Servizo',
    'getMarketingSpotsData' => 'Marketing',
    'getMarketingActivitySpotsData' => 'Actividade de marketing',
    'getActivitiesSpotsData' => 'Actividades',
    'getQuotesSpotsData' => 'Presupostos'
);

$app_list_strings['moduleList']['Spots'] = 'Análise dinámico';

$app_list_strings['moduleList']['AOBH_BusinessHours'] = 'Horarios';
$app_list_strings['business_hours_list']['0'] = '00:00';
$app_list_strings['business_hours_list']['1'] = '1:00';
$app_list_strings['business_hours_list']['2'] = '2:00';
$app_list_strings['business_hours_list']['3'] = '3:00';
$app_list_strings['business_hours_list']['4'] = '4:00';
$app_list_strings['business_hours_list']['5'] = '5:00';
$app_list_strings['business_hours_list']['6'] = '6:00';
$app_list_strings['business_hours_list']['7'] = '7:00';
$app_list_strings['business_hours_list']['8'] = '8:00';
$app_list_strings['business_hours_list']['9'] = '9:00';
$app_list_strings['business_hours_list']['10'] = '10:00';
$app_list_strings['business_hours_list']['11'] = '11:00';
$app_list_strings['business_hours_list']['12'] = '12:00';
$app_list_strings['business_hours_list']['13'] = '13:00';
$app_list_strings['business_hours_list']['14'] = '14:00';
$app_list_strings['business_hours_list']['15'] = '15:00';
$app_list_strings['business_hours_list']['16'] = '16:00';
$app_list_strings['business_hours_list']['17'] = '17:00';
$app_list_strings['business_hours_list']['18'] = '18:00';
$app_list_strings['business_hours_list']['19'] = '19:00';
$app_list_strings['business_hours_list']['20'] = '20:00';
$app_list_strings['business_hours_list']['21'] = '21:00';
$app_list_strings['business_hours_list']['22'] = '22:00';
$app_list_strings['business_hours_list']['23'] = '23:00';
$app_list_strings['day_list']['Monday'] = 'Luns';
$app_list_strings['day_list']['Tuesday'] = 'Martes';
$app_list_strings['day_list']['Wednesday'] = 'Mércores';
$app_list_strings['day_list']['Thursday'] = 'Xoves';
$app_list_strings['day_list']['Friday'] = 'Venres';
$app_list_strings['day_list']['Saturday'] = 'Sábado';
$app_list_strings['day_list']['Sunday'] = 'Domingo';
$app_list_strings['pdf_page_size_dom']['A4'] = 'A4';
$app_list_strings['pdf_page_size_dom']['Letter'] = 'Carta';
$app_list_strings['pdf_page_size_dom']['Legal'] = 'Legal';
$app_list_strings['pdf_orientation_dom']['Portrait'] = 'Vertical';
$app_list_strings['pdf_orientation_dom']['Landscape'] = 'Horizontal';
$app_list_strings['run_when_dom']['When True'] = 'Evaluar ao gardar'; // PR 6143
$app_list_strings['run_when_dom']['Once True'] = 'Perpetuo - (O campo debe ser auditado)';
$app_list_strings['sa_status_list']['Complete'] = 'Completa';
$app_list_strings['sa_status_list']['In_Review'] = 'En Revisión';
$app_list_strings['sa_status_list']['Issue_Resolution'] = 'Resolución de problemas';
$app_list_strings['sa_status_list']['Pending_Apttus_Submission'] = 'Envío de Apttus pendente';
$app_list_strings['sharedGroupRule']['none'] = 'Sen acceso';
$app_list_strings['sharedGroupRule']['view'] = 'Só lectura';
$app_list_strings['sharedGroupRule']['view_edit'] = 'Ver e editar';
$app_list_strings['sharedGroupRule']['view_edit_delete'] = 'Ver, editar e borrar';
$app_list_strings['moduleList']['SharedSecurityRulesFields'] = 'Campos de regras de seguridade compartidos';
$app_list_strings['moduleList']['SharedSecurityRules'] = 'Regras de seguridade compartidas';
$app_list_strings['moduleList']['SharedSecurityRulesActions'] = 'Accións de regras de seguridade compartidas';
$app_list_strings['shared_email_type_list'][''] = '';
$app_list_strings['shared_email_type_list']['Specify User'] = 'Usuario';
$app_list_strings['shared_email_type_list']['Users'] = 'Usuarios';
$app_list_strings['aow_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aow_condition_type_list']['Field'] = 'Campo';
$app_list_strings['aow_condition_type_list']['Any_Change'] = 'Calquera cambio';
$app_list_strings['aow_condition_type_list']['SecurityGroup'] = 'En SecurityGroup';
$app_list_strings['aow_condition_type_list']['currentUser'] = 'Usuario logueado como';
$app_list_strings['aow_condition_type_list']['Date'] = 'Data';
$app_list_strings['aow_condition_type_list']['Multi'] = 'Multiple';


$app_list_strings['moduleList']['SurveyResponses'] = 'Respostas á enquisa';
$app_list_strings['moduleList']['Surveys'] = 'Enquisas';
$app_list_strings['moduleList']['SurveyQuestionResponses'] = 'Respostas de preguntas de enquisa';
$app_list_strings['moduleList']['SurveyQuestions'] = 'Preguntas da enquisa';
$app_list_strings['moduleList']['SurveyQuestionOptions'] = 'Opcións de preguntas de enquisa';
$app_list_strings['survey_status_list']['Draft'] = 'Borrador';
$app_list_strings['survey_status_list']['Public'] = 'Público';
$app_list_strings['survey_status_list']['Closed'] = 'Cerrado';
$app_list_strings['surveys_question_type']['Text'] = 'Texto';
$app_list_strings['surveys_question_type']['Textbox'] = 'Cadro de texto';
$app_list_strings['surveys_question_type']['Checkbox'] = 'Casilla de Verificación';
$app_list_strings['surveys_question_type']['Radio'] = 'Radio';
$app_list_strings['surveys_question_type']['Dropdown'] = 'Despregable';
$app_list_strings['surveys_question_type']['Multiselect'] = 'Selección múltiple';
$app_list_strings['surveys_question_type']['Matrix'] = 'Matriz';
$app_list_strings['surveys_question_type']['DateTime'] = 'Data e hora';
$app_list_strings['surveys_question_type']['Date'] = 'Data';
$app_list_strings['surveys_question_type']['Scale'] = 'Escala';
$app_list_strings['surveys_question_type']['Rating'] = 'Cualificación';
$app_list_strings['surveys_matrix_options'][0] = 'Satisfeito';
$app_list_strings['surveys_matrix_options'][1] = 'Nin satisfeito nin insatisfeito';
$app_list_strings['surveys_matrix_options'][2] = 'Insatisfeito';

$app_strings['LBL_OPT_IN_PENDING_EMAIL_NOT_SENT'] = 'Autorización pendente. Confirmación non enviada';
$app_strings['LBL_OPT_IN_PENDING_EMAIL_FAILED'] = 'Envío de e-mail de confirmación fallado';
$app_strings['LBL_OPT_IN_PENDING_EMAIL_SENT'] = 'Autorización pendente. Confirmación xa enviada';
$app_strings['LBL_OPT_IN'] = 'Adherido';
$app_strings['LBL_OPT_IN_CONFIRMED'] = 'Adhesión confirmada';
$app_strings['LBL_OPT_IN_OPT_OUT'] = 'Rehusado';
$app_strings['LBL_OPT_IN_INVALID'] = 'Non Válido';

/** @see SugarEmailAddress */
$app_list_strings['email_settings_opt_in_dom'] = array(
    'not-opt-in' => 'Deshabilitado',
    'opt-in' => 'Autorizar',
    'confirmed-opt-in' => 'Adhesión confirmada'
);

$app_list_strings['email_confirmed_opt_in_dom'] = array(
    'not-opt-in' => 'Non autorizado',
    'opt-in' => 'Autorizar',
    'confirmed-opt-in' => 'Adhesión confirmada'
);

$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL'] = 'O e-mail de confirmación de autorización foi agregado á cola de mensaxes para %s enderezo(es). ';
$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_NOT_OPT_IN'] = 'Non se pode enviar e-mail a %s correo(s) porque o(s) enderezo(s) non está(n) autorizado(s) a recibir mensaxes.';
$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_MISSING_EMAIL_ADDRESS_ID'] = '%s enderezo de correo electrónico non ten un id válido. ';

$app_strings['ERR_TWO_FACTOR_FAILED'] = 'Fallou a Autenticación de dous factores';
$app_strings['ERR_TWO_FACTOR_CODE_SENT'] = 'Enviouse código de Autenticación de dous factores.';
$app_strings['ERR_TWO_FACTOR_CODE_FAILED'] = 'O envío do código de autenticación en dous factores fallou.';
$app_strings['LBL_THANKS_FOR_SUBMITTING'] = '¡Grazas por contarnos as súas experiencias!';

$app_strings['ERR_IP_CHANGE'] = 'Finalizamos a súa sesión debido a un cambio significativo no seu enderezo IP';
$app_strings['ERR_RETURN'] = 'Volver ao inicio';


$app_list_strings['oauth2_grant_type_dom'] = array(
    'password' => 'Outorgar Contrasinal',
    'client_credentials' => 'Credenciais do cliente',
    'implicit' => 'Implícito',
    'authorization_code' => 'Código de autorización'
);

$app_list_strings['oauth2_duration_units'] = [
    'minute' => 'minutos',
    'hour' => 'horas',
    'day' => 'días',
    'week' => 'semanas',
    'month' => 'meses',
];

$app_list_strings['search_controllers'] = [
    'Search' => 'Busca (nova)',
    'UnifiedSearch' => 'Busca global unificada (herdada)'
];


$app_strings['LBL_DEFAULT_API_ERROR_TITLE'] = 'Erro en API JSON';
$app_strings['LBL_DEFAULT_API_ERROR_DETAIL'] = 'Erro en API JSON.';
$app_strings['LBL_API_EXCEPTION_DETAIL'] = 'Versión de API: 8';
$app_strings['LBL_BAD_REQUEST_EXCEPTION_DETAIL'] = 'Por favor, asegúrese de reencher todos os campos requiridos';
$app_strings['LBL_EMPTY_BODY_EXCEPTION_DETAIL'] = 'Json API espera que o corpo da solicitude sexa JSON';
$app_strings['LBL_INVALID_JSON_API_REQUEST_EXCEPTION_DETAIL'] = 'Non se pode validar a solicitude de carga útil Json Api';
$app_strings['LBL_INVALID_JSON_API_RESPONSE_EXCEPTION_DETAIL'] = 'Non se pode validar a resposta de carga útil Json Api';
$app_strings['LBL_MODULE_NOT_FOUND_EXCEPTION_DETAIL'] = 'Json API non pode encontrar recursos';
$app_strings['LBL_NOT_ACCEPTABLE_EXCEPTION_DETAIL'] = 'Json API expects the "Aceptar" header to be application/vnd.api+json';
$app_strings['LBL_UNSUPPORTED_MEDIA_TYPE_EXCEPTION_DETAIL'] = 'Json API expects the "Content-Type" header to be application/vnd.api+json';

$app_strings['MSG_BROWSER_NOTIFICATIONS_ENABLED'] = 'As notificacións de escritorio están agora habilitadas para este navegador web.';
$app_strings['MSG_BROWSER_NOTIFICATIONS_DISABLED'] = 'As notificacións de escritorio están desactivadas para este navegador web. Utilice as preferencias do seu navegador para habilitalas outra vez.';
$app_strings['MSG_BROWSER_NOTIFICATIONS_UNSUPPORTED'] = 'Este navegador non é compatible coas notificacións de escritorio.';

$app_strings['LBL_GOOGLE_SYNC_ERR'] = 'Erro SuiteCRM Google Sync';
$app_strings['LBL_THERE_WAS_AN_ERR'] = 'Houbo un erro: ';
$app_strings['LBL_CLICK_HERE'] = 'Faga clic aquí';
$app_strings['LBL_TO_CONTINUE'] = ' para continuar.';

$app_strings['IMAP_HANDLER_ERROR'] = 'ERRO: {erro}; usouse a clave: "{key}".';
$app_strings['IMAP_HANDLER_SUCCESS'] = 'OK: configuración de proba cambiada a "{key}"';
$app_strings['IMAP_HANDLER_ERROR_INVALID_REQUEST'] = 'Petición non válida, use o valor "{var}".';
$app_strings['IMAP_HANDLER_ERROR_UNKNOWN_BY_KEY'] = 'Produciuse un erro descoñecido, a clave "{key}" non foi gardada.';
$app_strings['IMAP_HANDLER_ERROR_NO_TEST_SET'] = 'Non existen as configuracións de proba.';
$app_strings['IMAP_HANDLER_ERROR_NO_KEY'] = 'Clave non encontrada.';
$app_strings['IMAP_HANDLER_ERROR_KEY_SAVE'] = 'Erro ao gardar a clave.';
$app_strings['IMAP_HANDLER_ERROR_UNKNOWN'] = 'Erro descoñecido';
$app_strings['LBL_SEARCH_TITLE']                   = 'Busca';
$app_strings['LBL_SEARCH_TEXT_FIELD_TITLE_ATTR']   = 'Criterios de busca';
$app_strings['LBL_SEARCH_SUBMIT_FIELD_TITLE_ATTR'] = 'Busca';
$app_strings['LBL_SEARCH_SUBMIT_FIELD_VALUE']      = 'Busca';
$app_strings['LBL_SEARCH_QUERY']                   = 'Consulta: ';
$app_strings['LBL_SEARCH_RESULTS_PER_PAGE']        = 'Resultados por páxina: ';
$app_strings['LBL_SEARCH_ENGINE']                  = 'Buscador: ';
$app_strings['LBL_SEARCH_TOTAL'] = 'Resultado(s) total(es): ';
$app_strings['LBL_SEARCH_PREV'] = 'Anterior';
$app_strings['LBL_SEARCH_NEXT'] = 'Seguinte';
$app_strings['LBL_SEARCH_PAGE'] = 'Páxina ';
$app_strings['LBL_SEARCH_OF'] = ' de '; // Usage: Page 1 of 5
$app_strings['LBL_USE_ADVANCED_SEARCH'] = 'Utilizar a búsqueda avanzada';
$app_strings['LBL_USE_BASIC_SEARCH'] = 'Utilizar a búsqueda básica';

// PDF Engines
$app_strings['LBL_LEGACY_MPDF_ENGINE'] = 'Motor MPDF obsoleto';
$app_strings['LBL_TCPDF_ENGINE'] = 'Motor TCPDF';

$app_strings['ERR_INVALID_FILE_NAME'] = 'O nome do ficheiro non é válido:';
$app_strings['LBL_LOGGER_VALID_FILENAME_CHARACTERS'] = 'Só pode conter caracteres alfanuméricos, o punto (.), o guion (-) ou o guion baixo (_).';
$app_strings['LBL_LOGGER_INVALID_FILENAME'] = 'O nome do ficheiro de importación non é válido';
