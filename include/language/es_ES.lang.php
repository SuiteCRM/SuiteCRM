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
    'language_pack_name' => 'Español (España) - es_ES',
    'moduleList' => array(
        'Home' => 'Inicio',
        'ResourceCalendar' => 'Calendario de recursos',
        'Contacts' => 'Contactos',
        'Accounts' => 'Cuentas',
        'Alerts' => 'Alertas',
        'Opportunities' => 'Oportunidades',
        'Cases' => 'Casos',
        'Notes' => 'Notas',
        'Calls' => 'Llamadas',
        'TemplateSectionLine' => 'Línea de sección de plantilla',
        'Calls_Reschedule' => 'Reprogramación de llamadas',
        'Emails' => 'Correos',
        'EAPM' => 'EAPM',
        'Meetings' => 'Reuniones',
        'Tasks' => 'Tareas',
        'Calendar' => 'Calendario',
        'Leads' => 'Clientes Potenciales',
        'Currencies' => 'Monedas',
        'Activities' => 'Actividades',
        'Bugs' => 'Incidencias',
        'Feeds' => 'RSS',
        'iFrames' => 'Mis Sitios',
        'TimePeriods' => 'Períodos de Tiempo',
        'ContractTypes' => 'Tipos de Contrato',
        'Schedulers' => 'Planificadores',
        'Project' => 'Proyectos',
        'ProjectTask' => 'Tareas de Proyecto',
        'Campaigns' => 'Campañas',
        'CampaignLog' => 'Registro de Campañas',
        'Documents' => 'Documentos',
        'DocumentRevisions' => 'Versiones de documento',
        'Connectors' => 'Conectores',
        'Roles' => 'Roles',
        'Notifications' => 'Notificaciones',
        'Sync' => 'Sincronizar',
        'Users' => 'Usuarios',
        'Employees' => 'Empleados',
        'Administration' => 'Administración',
        'ACLRoles' => 'Roles',
        'InboundEmail' => 'Correo Entrante',
        'Releases' => 'Lanzamientos',
        'Prospects' => 'Público Objetivo',
        'Queues' => 'Colas',
        'EmailMarketing' => 'Marketing por Email',
        'EmailTemplates' => 'Correo electrónico - Plantillas',
        'ProspectLists' => 'Público Objetivo - Listas',
        'SavedSearch' => 'Búsquedas Guardadas',
        'UpgradeWizard' => 'Asistente de Actualizaciones',
        'Trackers' => 'Monitorización',
        'TrackerSessions' => 'Monitorización de Sesiones',
        'TrackerQueries' => 'Consultas de Monitorización',
        'FAQ' => 'FAQ',
        'Newsletters' => 'Boletines de Noticias',
        'SugarFeed' => 'SuiteCRM alimentación',
        'SugarFavorites' => 'Favoritos',

        'OAuthKeys' => 'Claves del Consumidor OAuth',
        'OAuthTokens' => 'Tokens OAuth',
        'OAuth2Clients' => 'Clientes de OAuth',
        'OAuth2Tokens' => 'Tokens OAuth',
    ),

    'moduleListSingular' => array(
        'Home' => 'Inicio',
        'Dashboard' => 'Cuadro de Mando',
        'Contacts' => 'Contacto',
        'Accounts' => 'Cuenta',
        'Opportunities' => 'Oportunidad',
        'Cases' => 'Caso',
        'Notes' => 'Nota',
        'Calls' => 'Llamada',
        'Emails' => 'Email',
        'EmailTemplates' => 'Plantilla de Email',
        'Meetings' => 'Reunión',
        'Tasks' => 'Tarea',
        'Calendar' => 'Calendario',
        'Leads' => 'Cliente Potencial',
        'Activities' => 'Actividades',
        'Bugs' => 'Incidencia',
        'KBDocuments' => 'Base de Conocimiento',
        'Feeds' => 'RSS',
        'iFrames' => 'Mis Sitios',
        'TimePeriods' => 'Período de Tiempo',
        'Project' => 'Proyecto',
        'ProjectTask' => 'Tarea de Proyecto',
        'Prospects' => 'Público Objetivo',
        'Campaigns' => 'Campaña',
        'Documents' => 'Documentos',
        'Sync' => 'Sincronización',
        'Users' => 'Usuarios',
        'SugarFavorites' => 'Favoritos',

    ),

    'checkbox_dom' => array(
        '' => '',
        '1' => 'Si',
        '2' => 'No',
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
        'Other' => 'Otro',
    ),
    //e.g. en espanol 'Apparel'=>'Ropa',
    'industry_dom' => array(
        '' => '',
        'Apparel' => 'Textil',
        'Banking' => 'Banca',
        'Biotechnology' => 'Biotecnología',
        'Chemicals' => 'Química',
        'Communications' => 'Comunicaciones',
        'Construction' => 'Construcción',
        'Consulting' => 'Consultoría',
        'Education' => 'Educación',
        'Electronics' => 'Electronica',
        'Energy' => 'Energía',
        'Engineering' => 'Ingeniería',
        'Entertainment' => 'Entretenimiento',
        'Environmental' => 'Medio ambiente',
        'Finance' => 'Finanzas',
        'Government' => 'Gobierno',
        'Healthcare' => 'Sanidad',
        'Hospitality' => 'Caridad',
        'Insurance' => 'Seguros',
        'Machinery' => 'Maquinaria',
        'Manufacturing' => 'Fabricación',
        'Media' => 'Medios de comunicación',
        'Not For Profit' => 'Sin ánimo de lucro',
        'Recreation' => 'Ocio',
        'Retail' => 'Minoristas',
        'Shipping' => 'Envíos',
        'Technology' => 'Tecnología',
        'Telecommunications' => 'Telecomunicaciones',
        'Transportation' => 'Transporte',
        'Utilities' => 'Servicios públicos',
        'Other' => 'Otro',
    ),
    'lead_source_default_key' => 'Self Generated',
    'lead_source_dom' => array(
        '' => '',
        'Cold Call' => 'Llamada en Frío',
        'Existing Customer' => 'Cliente Existente',
        'Self Generated' => 'Auto Generado',
        'Employee' => 'Empleado',
        'Partner' => 'Socio',
        'Public Relations' => 'Relaciones Públicas',
        'Direct Mail' => 'Correo Directo',
        'Conference' => 'Conferencia',
        'Trade Show' => 'Exposición',
        'Web Site' => 'Sitio Web',
        'Word of mouth' => 'Recomendación',
        'Email' => 'Email',
        'Campaign' => 'Campaña',
        'Other' => 'Otro',
    ),
    'language_dom' => array(
        'af' => 'Africano',
        'ar-EG' => 'Árabe (Egipto)',
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
        'el' => 'Griego',
        'en-GB' => 'Inglés, Reino Unido',
        'en-US' => 'Inglés, Estados Unidos',
        'es-ES' => 'Español',
        'es-MX' => 'Español, México',
        'es-PY' => 'Español, Paraguay',
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
        'ja' => 'Japonés',
        'ka' => 'Georgiano',
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
        'other' => 'Otro',
    ),
    'opportunity_type_dom' => array(
        '' => '',
        'Existing Business' => 'Negocios Existentes',
        'New Business' => 'Nuevos Negocios',
    ),
    'roi_type_dom' => array(
        'Revenue' => 'Ingresos',
        'Investment' => 'Inversión',
        'Expected_Revenue' => 'Ingresos Esperados',
        'Budget' => 'Presupuesto',

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
        'Executive Sponsor' => 'Patrocinador Ejecutivo',
        'Influencer' => 'Influenciador',
        'Other' => 'Otro',
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
        'Qualification' => 'Calificación',
        'Needs Analysis' => 'Necesita Análisis',
        'Value Proposition' => 'Propuesta de Valor',
        'Id. Decision Makers' => 'Identificar a los tomadores de decisión',
        'Perception Analysis' => 'Análisis de Percepción',
        'Proposal/Price Quote' => 'Propuesta/Presupuesto',
        'Negotiation/Review' => 'Negociación/Revisión',
        'Closed Won' => 'Ganado',
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
        'Call' => 'Llamada',
        'Meeting' => 'Reunión',
        'Task' => 'Tarea',
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
        'Low' => 'Baja',
    ),
    'task_status_default' => 'No Iniciado',
    'task_status_dom' => array(
        'Not Started' => 'No Iniciada',
        'In Progress' => 'En Progreso',
        'Completed' => 'Completada',
        'Pending Input' => 'Pendiente de Información',
        'Deferred' => 'Aplazada',
    ),
    'meeting_status_default' => 'Planned',
    'meeting_status_dom' => array(
        'Planned' => 'Planificada',
        'Held' => 'Realizada',
        'Not Held' => 'No Realizada',
    ),
    'extapi_meeting_password' => array(
        'WebEx' => 'WebEx',
    ),
    'meeting_type_dom' => array(
        'Other' => 'Otro',
        'Sugar' => 'SuiteCRM',
    ),
    'call_status_default' => 'Planificada',
    'call_status_dom' => array(
        'Planned' => 'Planificada',
        'Held' => 'Realizada',
        'Not Held' => 'No Realizada',
    ),
    'call_direction_default' => 'Outbound',
    'call_direction_dom' => array(
        'Inbound' => 'Entrante',
        'Outbound' => 'Saliente',
    ),
    'lead_status_dom' => array(
        '' => '',
        'New' => 'Nuevo',
        'Assigned' => 'Asignado',
        'In Process' => 'En Proceso',
        'Converted' => 'Convertido',
        'Recycled' => 'Reciclado',
        'Dead' => 'Muerto',
    ),
    'case_priority_default_key' => 'P2',
    'case_priority_dom' => array(
        'P1' => 'Alta',
        'P2' => 'Media',
        'P3' => 'Baja',
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
        'Low' => 'Baja',
    ),
    'project_task_priority_default' => 'Media',

    'project_task_status_options' => array(
        'Not Started' => 'No Iniciada',
        'In Progress' => 'En Progreso',
        'Completed' => 'Completada',
        'Pending Input' => 'Pendiente de Información',
        'Deferred' => 'Retrasada',
    ),
    'project_task_utilization_options' => array(
        '0' => 'ninguno',
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
        '' => '--Ninguno--',
        'active' => 'Activo',
        'inactive' => 'Inactivo',
    ),

    // Note:  do not translate record_type_default_key
    //        it is the key for the default record_type_module value
    'record_type_default_key' => 'Cuentas',
    'record_type_display' => array(
        '' => '',
        'Accounts' => 'Cuenta',
        'Opportunities' => 'Oportunidades',
        'Cases' => 'Casos',
        'Leads' => 'Clientes Potenciales',
        'Contacts' => 'Contactos', // cn (11/22/2005) added to support Emails

        'Bugs' => 'Incidencia',
        'Project' => 'Proyectos',

        'Prospects' => 'Público Objetivo',
        'ProjectTask' => 'Tareas de Proyecto',

        'Tasks' => 'Tareas',

        'AOS_Contracts' => 'Contrato',
        'AOS_Invoices' => 'Factura',
        'AOS_Quotes' => 'Presupuesto',
        'AOS_Products' => 'Producto',

    ),
// PR 4606
    'record_type_display_notes' => array(
        'Accounts' => 'Cuenta',
        'Contacts' => 'Contacto',
        'Opportunities' => 'Oportunidad',
        'Campaigns' => 'Campaña',
        'Tasks' => 'Tarea',
        'Emails' => 'Emails',

        'Bugs' => 'Incidencia',
        'Project' => 'Proyecto',
        'ProjectTask' => 'Tarea de Proyecto',
        'Prospects' => 'Público Objetivo',
        'Cases' => 'Caso',
        'Leads' => 'Cliente Potencial',

        'Meetings' => 'Reunión',
        'Calls' => 'Llamada',

        'AOS_Contracts' => 'Contrato',
        'AOS_Invoices' => 'Factura',
        'AOS_Quotes' => 'Presupuesto',
        'AOS_Products' => 'Producto',
    ),

    'parent_type_display' => array(
        'Accounts' => 'Cuenta',
        'Contacts' => 'Contacto',
        'Tasks' => 'Tarea',
        'Opportunities' => 'Oportunidad',

        'Bugs' => 'Incidencia',
        'Cases' => 'Caso',
        'Leads' => 'Cliente Potencial',

        'Project' => 'Proyecto',
        'ProjectTask' => 'Tarea de Proyecto',

        'Prospects' => 'Público Objetivo',
        
        'AOS_Contracts' => 'Contrato',
        'AOS_Invoices' => 'Factura',
        'AOS_Quotes' => 'Presupuesto',
        'AOS_Products' => 'Producto', 

    ),
    'parent_line_items' => array(
        'AOS_Quotes' => 'Presupuestos',
        'AOS_Invoices' => 'Facturas',
        'AOS_Contracts' => 'Contratos',
    ),
    'issue_priority_default_key' => 'Media',
    'issue_priority_dom' => array(
        'Urgent' => 'Urgente',
        'High' => 'Alta',
        'Medium' => 'Media',
        'Low' => 'Baja',
    ),
    'issue_resolution_default_key' => '',
    'issue_resolution_dom' => array(
        '' => '',
        'Accepted' => 'Aceptado',
        'Duplicate' => 'Duplicado',
        'Closed' => 'Cerrado',
        'Out of Date' => 'Caducado',
        'Invalid' => 'No Válido',
    ),

    'issue_status_default_key' => 'Nuevo',
    'issue_status_dom' => array(
        'New' => 'Nuevo',
        'Assigned' => 'Asignado',
        'Closed' => 'Cerrado',
        'Pending' => 'Pendiente',
        'Rejected' => 'Rechazado',
    ),

    'bug_priority_default_key' => 'Media',
    'bug_priority_dom' => array(
        'Urgent' => 'Urgente',
        'High' => 'Alta',
        'Medium' => 'Media',
        'Low' => 'Baja',
    ),
    'bug_resolution_default_key' => '',
    'bug_resolution_dom' => array(
        '' => '',
        'Accepted' => 'Aceptado',
        'Duplicate' => 'Duplicado',
        'Fixed' => 'Corregido',
        'Out of Date' => 'Caducado',
        'Invalid' => 'No Válido',
        'Later' => 'Pospuesto',
    ),
    'bug_status_default_key' => 'Nuevo',
    'bug_status_dom' => array(
        'New' => 'Nuevo',
        'Assigned' => 'Asignado',
        'Closed' => 'Cerrado',
        'Pending' => 'Pendiente',
        'Rejected' => 'Rechazado',
    ),
    'bug_type_default_key' => 'Incidencia',
    'bug_type_dom' => array(
        'Defect' => 'Defecto',
        'Feature' => 'Característica',
    ),
    'case_type_dom' => array(
        'Administration' => 'Administración',
        'Product' => 'Producto',
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
        'Accounts' => 'Cuentas',
        'Activities' => 'Actividades',
        'Bugs' => 'Incidencias',
        'Calendar' => 'Calendario',
        'Calls' => 'Llamadas',
        'Campaigns' => 'Campañas',
        'Cases' => 'Casos',
        'Contacts' => 'Contactos',
        'Currencies' => 'Monedas',
        'Dashboard' => 'Cuadro de Mando',
        'Documents' => 'Documentos',
        'Emails' => 'Correos',
        'Feeds' => 'Fuentes RSS',
        'Forecasts' => 'Previsiones',
        'Help' => 'Ayuda',
        'Home' => 'Inicio',
        'Leads' => 'Clientes Potenciales',
        'Meetings' => 'Reuniones',
        'Notes' => 'Notas',
        'Opportunities' => 'Oportunidades',
        'Outlook Plugin' => 'Plugin de Outlook',
        'Projects' => 'Proyectos',
        'Quotes' => 'Presupuestos',
        'Releases' => 'Lanzamientos',
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
        '1' => 'Enero',
        '2' => 'Febrero',
        '3' => 'Marzo',
        '4' => 'Abril',
        '5' => 'Mayo',
        '6' => 'Junio',
        '7' => 'Julio',
        '8' => 'Agosto',
        '9' => 'Septiembre',
        '10' => 'Octubre',
        '11' => 'Noviembre',
        '12' => 'Diciembre',
    ),
    'dom_cal_month_short' => array(
        '0' => '',
        '1' => 'Ene',
        '2' => 'Feb',
        '3' => 'Mar',
        '4' => 'Abr',
        '5' => 'May',
        '6' => 'Jun',
        '7' => 'Jul',
        '8' => 'Ago',
        '9' => 'Sep',
        '10' => 'Oct',
        '11' => 'Nov',
        '12' => 'Dic',
    ),
    'dom_cal_day_long' => array(
        '0' => '',
        '1' => 'Domingo',
        '2' => 'Lunes',
        '3' => 'Martes',
        '4' => 'Miércoles',
        '5' => 'Jueves',
        '6' => 'Viernes',
        '7' => 'Sábado',
    ),
    'dom_cal_day_short' => array(
        '0' => '',
        '1' => 'Dom',
        '2' => 'Lun',
        '3' => 'Mar',
        '4' => 'Mié',
        '5' => 'Jue',
        '6' => 'Vie',
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
        'archived' => 'Archivado',
        'draft' => 'Borrador',
        'inbound' => 'Entrante',
        'campaign' => 'Campaña',
    ),
    'dom_email_status' => array(
        'archived' => 'Archivado',
        'closed' => 'Cerrado',
        'draft' => 'Borrador',
        'read' => 'Leído',
        'replied' => 'Respondido',
        'sent' => 'Enviado',
        'send_error' => 'Error de Envío',
        'unread' => 'No leído',
    ),
    'dom_email_archived_status' => array(
        'archived' => 'Archivado',
    ),

    'dom_email_server_type' => array(
        '' => '--Ninguno--',
        'imap' => 'IMAP',
    ),
    'dom_mailbox_type' => array(/*''           => '--None Specified--',*/
        'pick' => '--Ninguno--',
        'createcase' => 'Nuevo Caso',
        'bounce' => 'Gestión de Rebotes',
    ),
    'dom_email_distribution' => array(
        '' => '--Ninguno--',
        'direct' => 'Asignación Directa',
        'roundRobin' => 'Round-Robin',
        'leastBusy' => 'Menos-Ocupado',
    ),
    'dom_email_errors' => array(
        1 => 'Seleccione sólo un usuario cuando asigne directamente elementos.',
        2 => 'Debes asignar solamente artículos seleccionados cuando estos se asignan de forma directa.',
    ),
    'dom_email_bool' => array(
        'bool_true' => 'Sí',
        'bool_false' => 'No',
    ),
    'dom_int_bool' => array(
        1 => 'Sí',
        0 => 'No',
    ),
    'dom_switch_bool' => array(
        'on' => 'Sí',
        'off' => 'No',
        '' => 'No',
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
        'not run' => 'Hora de Ejecución Pasada, No Ejecutado',
        'ready' => 'Listo',
        'in progress' => 'En Progreso',
        'failed' => 'Fallado',
        'completed' => 'Completado',
        'no curl' => 'No ejecutado: cURL no está disponible',
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
        'Knowledege Base' => 'Base de Conocimiento',
        'Sales' => 'Ventas',
    ),

    'email_category_dom' => array(
        '' => '',
        'Archived' => 'Archivado',
        // TODO: add more categories here...
    ),

    'document_subcategory_dom' => array(
        '' => '',
        'Marketing Collateral' => 'Impresos de Marketing',
        'Product Brochures' => 'Folletos de Producto',
        'FAQ' => 'FAQ',
    ),

    'document_status_dom' => array(
        'Active' => 'Activo',
        'Draft' => 'Borrador',
        'FAQ' => 'FAQ',
        'Expired' => 'Caducado',
        'Under Review' => 'En Revisión',
        'Pending' => 'Pendiente',
    ),
    'document_template_type_dom' => array(
        '' => '',
        'mailmerge' => 'Combinar correspondencia',
        'eula' => 'CLUF',
        'nda' => 'ANR',
        'license' => 'Contrato de Licencia',
    ),
    'dom_meeting_accept_options' => array(
        'accept' => 'Aceptar',
        'decline' => 'Rechazar',
        'tentative' => 'Tentativa',
    ),
    'dom_meeting_accept_status' => array(
        'accept' => 'Aceptado',
        'decline' => 'Rechazado',
        'tentative' => 'Tentativa',
        'none' => 'Ninguno',
    ),
    'duration_intervals' => array(
        '0' => '00',
        '15' => '15',
        '30' => '30',
        '45' => '45',
    ),
    'repeat_type_dom' => array(
        '' => 'Ninguno',
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
        '' => 'Ninguna',
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
        'exempt_address' => 'Lista de Exclusión - Por Dirección de Email',
        'exempt' => 'Lista de Exclusión - Por Id',
        'test' => 'Prueba',
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
        'targeted' => 'Mensaje enviado',
        'send error' => 'Mensaje no enviado (otras causas)',
        'invalid email' => 'Mensaje no enviado (dirección no válida)',
        'link' => 'Enlace clicado',
        'viewed' => 'Mensaje visto',
        'removed' => 'Baja',
        'lead' => 'Cliente potencial creado',
        'contact' => 'Contacto creado',
        'blocked' => 'Destinatario excluido por dirección o dominio',
        'Survey' => 'Respuesta a encuesta',
    ),

    'campainglog_target_type_dom' => array(
        'Contacts' => 'Contactos',
        'Users' => 'Usuarios',
        'Prospects' => 'Público Objetivo',
        'Leads' => 'Clientes Potenciales',
        'Accounts' => 'Cuentas',
    ),
    'merge_operators_dom' => array(
        'like' => 'Contiene',
        'exact' => 'Exactamente',
        'start' => 'Comienza con',
    ),

    'custom_fields_importable_dom' => array(
        'true' => 'Sí',
        'false' => 'No',
        'required' => 'Requerido',
    ),

    'custom_fields_merge_dup_dom' => array(
        0 => 'Deshabilitado',
        1 => 'Habilitado',
        2 => 'En filtro',
        3 => 'Filtro seleccionado por defecto',
        4 => 'Sólo filtro',
    ),

    'projects_priority_options' => array(
        'high' => 'Alta',
        'medium' => 'Media',
        'low' => 'Baja',
    ),

    'projects_status_options' => array(
        'notstarted' => 'No Iniciado',
        'inprogress' => 'En Progreso',
        'completed' => 'Completado',
    ),
    // strings to pass to Flash charts
    'chart_strings' => array(
        'expandlegend' => 'Expandir Leyenda',
        'collapselegend' => 'Contraer Leyenda',
        'clickfordrilldown' => 'Clic para Profundizar',
        'detailview' => 'Más Detalles...',
        'piechart' => 'Gráfico Circular',
        'groupchart' => 'Gráfico Agrupado',
        'stackedchart' => 'Gráfico Apilado',
        'barchart' => 'Gráfico de Barras',
        'horizontalbarchart' => 'Gráfico de Barras Horizontal',
        'linechart' => 'Gráfico de Líneas',
        'noData' => 'Datos no disponibles',
        'print' => 'Imprimir',
        'pieWedgeName' => 'secciones',
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
        '' => 'Ninguno',
        'other' => 'Otro:',
    ),
    'import_delimeter_options' => array(
        ',' => ',',
        ';' => ';',
        '\t' => '\t',
        '.' => '.',
        ':' => ':',
        '|' => '|',
        'other' => 'Otro:',
    ),
    'link_target_dom' => array(
        '_blank' => 'Nueva Ventana',
        '_self' => 'Misma Ventana',
    ),
    'dashlet_auto_refresh_options' => array(
        '-1' => 'No actualizar automáticamente',
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
        'greater_than' => 'Después de',
        'less_than' => 'Antes de',
        'last_7_days' => 'Últimos 7 días',
        'next_7_days' => 'Próximos 7 días',
        'last_30_days' => 'Últimos 30 días',
        'next_30_days' => 'Próximos 30 días',
        'last_month' => 'Último mes',
        'this_month' => 'Este mes',
        'next_month' => 'Próximo mes',
        'last_year' => 'Último año',
        'this_year' => 'Este año',
        'next_year' => 'Próximo año',
        'between' => 'Está entre',
    ),
    'numeric_range_search_dom' => array(
        '=' => 'Igual a',
        'not_equal' => 'Distinto de',
        'greater_than' => 'Mayor que',
        'greater_than_equals' => 'Mayor o Igual que',
        'less_than' => 'Menor que',
        'less_than_equals' => 'Menor o Igual a',
        'between' => 'Está entre',
    ),
    'lead_conv_activity_opt' => array(
        'copy' => 'Copiar',
        'move' => 'Mover',
        'donothing' => 'No hacer nada',
    ),
    // PR 6009
    'inboundmail_assign_replies_to_admin' => array(
        'donothing' => 'No hacer nada',
        'repliedtoowner' => 'Respondió al propietario del correo electrónico',
        'recordowner' => 'Propietario del registro asociado',
    ),
);

$app_strings = array(
    'LBL_SEARCH_REAULTS_TITLE' => 'Resultados',
    'ERR_SEARCH_INVALID_QUERY' => 'Se ha producido un error al realizar la búsqueda. La sintaxis de su consulta podría no ser válida.',
    'ERR_SEARCH_NO_RESULTS' => 'No hay resultados para su búsqueda. Inténtelo de nuevo con otros criterios.',
    'LBL_SEARCH_PERFORMED_IN' => 'Búsqueda realizada',
    'LBL_EMAIL_CODE' => 'Código de correo electrónico:',
    'LBL_SEND' => 'Enviar',
    'LBL_LOGOUT' => 'Salir',
    'LBL_TOUR_NEXT' => 'Siguiente',
    'LBL_TOUR_SKIP' => 'Saltar',
    'LBL_TOUR_BACK' => 'Atrás',
    'LBL_TOUR_TAKE_TOUR' => 'Visita guiada',
    'LBL_MOREDETAIL' => 'Más detalles' /*for 508 compliance fix*/,
    'LBL_EDIT_INLINE' => 'Editar en línea' /*for 508 compliance fix*/,
    'LBL_VIEW_INLINE' => 'Ver' /*for 508 compliance fix*/,
    'LBL_BASIC_SEARCH' => 'Filtro' /*for 508 compliance fix*/,
    'LBL_Blank' => ' ' /*for 508 compliance fix*/,
    'LBL_ID_FF_ADD' => 'Añadir' /*for 508 compliance fix*/,
    'LBL_ID_FF_ADD_EMAIL' => 'Añadir dirección de correo electrónico' /*for 508 compliance fix*/,
    'LBL_HIDE_SHOW' => 'Ocultar/Mostrar' /*for 508 compliance fix*/,
    'LBL_DELETE_INLINE' => 'Eliminar' /*for 508 compliance fix*/,
    'LBL_ID_FF_CLEAR' => 'Limpiar' /*for 508 compliance fix*/,
    'LBL_ID_FF_VCARD' => 'vCard' /*for 508 compliance fix*/,
    'LBL_ID_FF_REMOVE' => 'Quitar' /*for 508 compliance fix*/,
    'LBL_ID_FF_REMOVE_EMAIL' => 'Eliminar dirección de correo electrónico' /*for 508 compliance fix*/,
    'LBL_ID_FF_OPT_OUT' => 'Rehusar',
    'LBL_ID_FF_OPT_IN' => 'Autorizar',
    'LBL_ID_FF_INVALID' => 'Hacer Inválido',
    'LBL_ADD' => 'Añadir' /*for 508 compliance fix*/,
    'LBL_COMPANY_LOGO' => 'Logo compañia' /*for 508 compliance fix*/,
    'LBL_CONNECTORS_POPUPS' => 'Conectores Popups',
    'LBL_CLOSEINLINE' => 'Cerrado',
    'LBL_VIEWINLINE' => 'Ver',
    'LBL_INFOINLINE' => 'Información',
    'LBL_PRINT' => 'Imprimir',
    'LBL_HELP' => 'Ayuda',
    'LBL_ID_FF_SELECT' => 'Seleccionar',
    'DEFAULT' => 'Básico', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_SORT' => 'Ordenar',
    'LBL_EMAIL_SMTP_SSL_OR_TLS' => '¿Habilitar SMTP sobre SSL o TLS?',
    'LBL_NO_ACTION' => 'No hay ninguna acción para el nombre: %s',
    'LBL_NO_SHORTCUT_MENU' => 'No hay acciones disponibles.',
    'LBL_NO_DATA' => 'Sin Datos',

    'LBL_ERROR_UNDEFINED_BEHAVIOR' => 'Se ha producido un error inesperado.', //PR 3669
    'LBL_ERROR_UNHANDLED_VALUE' => 'Un valor no se ha manipulado correctamente lo que impide que un proceso continúe.', //PR 3669
    'LBL_ERROR_UNUSABLE_VALUE' => 'Se encontró un valor inutilizable que impide que un proceso continúe.', //PR 3669
    'LBL_ERROR_INVALID_TYPE' => 'El valor introducido no es del tipo esperado.', //PR 3669

    'LBL_ROUTING_FLAGGED' => 'conjunto de marcas de seguimiento',
    'LBL_NOTIFICATIONS' => 'Notificaciones',

    'LBL_ROUTING_TO' => 'a',
    'LBL_ROUTING_TO_ADDRESS' => 'a la dirección',
    'LBL_ROUTING_WITH_TEMPLATE' => 'con la plantilla',

    'NTC_OVERWRITE_ADDRESS_PHONE_CONFIRM' => 'Los campos Teléfono y Dirección de su formulario ya tienen valor asignado. Para sobrescribir dichos valores con el teléfono/dirección de la Cuenta que ha seleccionado, haga clic en "Aceptar". Para mantener los valores actuales, haga clic en "Cancelar".',
    'LBL_DROP_HERE' => '[Soltar Aquí]',
    'LBL_EMAIL_ACCOUNTS_GMAIL_DEFAULTS' => 'Establecer configuración para Gmail&amp;#153;',
    'LBL_EMAIL_ACCOUNTS_NAME' => 'Nombre',
    'LBL_EMAIL_ACCOUNTS_OUTBOUND' => 'Propiedades del Servidor de Correo Saliente',
    'LBL_EMAIL_ACCOUNTS_SMTPPASS' => 'Contraseña SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPPORT' => 'Puerto SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPSERVER' => 'Servidor SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPUSER' => 'Nombre de usuario SMTP',
    'LBL_EMAIL_ACCOUNTS_SMTPDEFAULT' => 'Por Defecto',
    'LBL_EMAIL_WARNING_MISSING_USER_CREDS' => 'Aviso: Falta el nombre de usuario y la contraseña para la cuenta de correo saliente.',
    'LBL_EMAIL_ACCOUNTS_SUBTITLE' => 'Configurar Cuentas de Correo para ver correos entrantes de sus cuentas de correo.',
    'LBL_EMAIL_ACCOUNTS_OUTBOUND_SUBTITLE' => 'Proporcionar información del servidor de correo SMTP a utilizar para el correo saliente en Cuentas de Correo.',

    'LBL_EMAIL_ADDRESS_BOOK_ADD' => 'Hecho',
    'LBL_EMAIL_ADDRESS_BOOK_CLEAR' => 'Borrar',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_TO' => 'Para:',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_CC' => 'Cc:',
    'LBL_EMAIL_ADDRESS_BOOK_ADD_BCC' => 'Cco:',
    'LBL_EMAIL_ADDRESS_BOOK_ADRRESS_TYPE' => 'Para/Cc/Cco',
    'LBL_EMAIL_ADDRESS_BOOK_EMAIL_ADDR' => 'Dirección de Email',
    'LBL_EMAIL_ADDRESS_BOOK_FILTER' => 'Filtro',
    'LBL_EMAIL_ADDRESS_BOOK_NAME' => 'Nombre',
    'LBL_EMAIL_ADDRESS_BOOK_NOT_FOUND' => 'No se han encontrado ninguna dirección',
    'LBL_EMAIL_ADDRESS_BOOK_SAVE_AND_ADD' => 'Guardar y Agregar a la Libreta de Direcciones',
    'LBL_EMAIL_ADDRESS_BOOK_SELECT_TITLE' => 'Seleccionar Destinatarios de Correo',
    'LBL_EMAIL_ADDRESS_BOOK_TITLE' => 'Libreta de Direcciones',
    'LBL_EMAIL_REPORTS_TITLE' => 'Informes',
    'LBL_EMAIL_REMOVE_SMTP_WARNING' => '¡Aviso! La cuenta de correo saliente que está intentando eliminar está asociada a una cuenta de correo entrante existente.  ¿Está seguro de que quiere continuar?',
    'LBL_EMAIL_ADDRESSES' => 'Email',
    'LBL_EMAIL_ADDRESS_PRIMARY' => 'Dirección de Email',
    'LBL_EMAIL_ADDRESS_OPT_IN' => 'Ha confirmado que su dirección de correo ha sido autorizada a enviar: ',
    'LBL_EMAIL_ADDRESS_OPT_IN_ERR' => 'No fue posible confirmar la dirección de correo',
    'LBL_EMAIL_ARCHIVE_TO_SUITE' => 'Importar a SuiteCRM',
    'LBL_EMAIL_ASSIGNMENT' => 'Asignación',
    'LBL_EMAIL_ATTACH_FILE_TO_EMAIL' => 'Adjuntar',
    'LBL_EMAIL_ATTACHMENT' => 'Adjuntar',
    'LBL_EMAIL_ATTACHMENTS' => 'Desde el Equipo Local',
    'LBL_EMAIL_ATTACHMENTS2' => 'Desde Documentos SuiteCRM',
    'LBL_EMAIL_ATTACHMENTS3' => 'Adjuntos de Plantilla',
    'LBL_EMAIL_ATTACHMENTS_FILE' => 'Archivo',
    'LBL_EMAIL_ATTACHMENTS_DOCUMENT' => 'Documento',
    'LBL_EMAIL_BCC' => 'CCO',
    'LBL_EMAIL_CANCEL' => 'Cancelar',
    'LBL_EMAIL_CC' => 'Cc',
    'LBL_EMAIL_CHARSET' => 'Juego de Caracteres',
    'LBL_EMAIL_CHECK' => 'Comprobar Correo',
    'LBL_EMAIL_CHECKING_NEW' => 'Comprobando Correo Nuevo',
    'LBL_EMAIL_CHECKING_DESC' => 'Un momento, por favor... <br><br>Si es la primera comprobación para esta cuenta de correo, puede tardar un poco.',
    'LBL_EMAIL_CLOSE' => 'Cerrar',
    'LBL_EMAIL_COFFEE_BREAK' => 'Comprobando Correo Nuevo. <br><br>Las cuentas de correo con gran volumen pueden tardar una cantidad considerable de tiempo.',

    'LBL_EMAIL_COMPOSE' => 'Correo',
    'LBL_EMAIL_COMPOSE_ERR_NO_RECIPIENTS' => 'Por favor, introduzca los destinatarios de este correo.',
    'LBL_EMAIL_COMPOSE_NO_BODY' => 'El cuerpo de este mensaje está vacío.  ¿Enviar de todas formas?',
    'LBL_EMAIL_COMPOSE_NO_SUBJECT' => 'Este mensaje no tiene asunto.  ¿Enviar de todas formas?',
    'LBL_EMAIL_COMPOSE_NO_SUBJECT_LITERAL' => '(sin asunto)',
    'LBL_EMAIL_COMPOSE_INVALID_ADDRESS' => 'Por favor, introduzca una dirección de correo válida para los campos Para, CC y CCO',

    'LBL_EMAIL_CONFIRM_CLOSE' => '¿Descartar este correo?',
    'LBL_EMAIL_CONFIRM_DELETE_SIGNATURE' => '¿Está seguro de que desea eliminar esta firma?',

    'LBL_EMAIL_SENT_SUCCESS' => 'Correo electrónico enviado',

    'LBL_EMAIL_CREATE_NEW' => '--Crear Al Guardar--',
    'LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS' => 'Múltiple',
    'LBL_EMAIL_MULT_GROUP_FOLDER_ACCOUNTS_EMPTY' => 'Vacío',
    'LBL_EMAIL_DATE_SENT_BY_SENDER' => 'Fecha de Envío por Remitente',
    'LBL_EMAIL_DATE_TODAY' => 'Hoy',
    'LBL_EMAIL_DELETE' => 'Eliminar',
    'LBL_EMAIL_DELETE_CONFIRM' => '¿Eliminar mensajes seleccionados?',
    'LBL_EMAIL_DELETE_SUCCESS' => 'Email eliminado satisfactoriamente.',
    'LBL_EMAIL_DELETING_MESSAGE' => 'Eliminando Mensaje',
    'LBL_EMAIL_DETAILS' => 'Detalles',

    'LBL_EMAIL_EDIT_CONTACT_WARN' => 'Sólo se utilizará la Dirección principal de cada Contacto.',

    'LBL_EMAIL_EMPTYING_TRASH' => 'Vaciando Papelera',
    'LBL_EMAIL_DELETING_OUTBOUND' => 'Eliminando servidor saliente',
    'LBL_EMAIL_CLEARING_CACHE_FILES' => 'Limpiando archivos de la caché',
    'LBL_EMAIL_EMPTY_MSG' => 'No hay mensajes para mostrar.',
    'LBL_EMAIL_EMPTY_ADDR_MSG' => 'No hay direcciones de correo electrónico para mostrar.',

    'LBL_EMAIL_ERROR_ADD_GROUP_FOLDER' => 'El nombre de carpeta debe ser único y no vacío. Por favor, inténtelo de nuevo.',
    'LBL_EMAIL_ERROR_DELETE_GROUP_FOLDER' => 'No puede borrarse la carpeta. O la carpeta o sus hijos tienen correos o una bandeja de correo asociada.',
    'LBL_EMAIL_ERROR_CANNOT_FIND_NODE' => 'No se ha podido determinar la carpeta pretendida a partir del contexto. Inténtelo de nuevo.',
    'LBL_EMAIL_ERROR_CHECK_IE_SETTINGS' => 'Por favor, compruebe su configuración.',
    'LBL_EMAIL_ERROR_DESC' => 'Se han detectado errores:',
    'LBL_EMAIL_DELETE_ERROR_DESC' => 'No tiene acceso a este área. Contacte con el administrador del sitio para obtener acceso.',
    'LBL_EMAIL_ERROR_DUPE_FOLDER_NAME' => 'Los nombres de carpetas SuiteCRM deben ser únicos.',
    'LBL_EMAIL_ERROR_EMPTY' => 'Por favor, introduzca algún criterio de búsqueda.',
    'LBL_EMAIL_ERROR_GENERAL_TITLE' => 'Ha ocurrido un error',
    'LBL_EMAIL_ERROR_MESSAGE_DELETED' => 'Mensaje eliminado del servidor',
    'LBL_EMAIL_ERROR_IMAP_MESSAGE_DELETED' => 'O el mensaje se ha eliminado en el servidor o ha sido movido a otra carpeta',
    'LBL_EMAIL_ERROR_MAILSERVERCONNECTION' => 'La conexión con el servidor de correo ha fallado. Por favor, contacte con su Administrador',
    'LBL_EMAIL_ERROR_MOVE' => 'De momento no está soportado el mover correo entre servidores y/o cuentas de correo.',
    'LBL_EMAIL_ERROR_MOVE_TITLE' => 'Error al Mover',
    'LBL_EMAIL_ERROR_NAME' => 'Se requiere un nombre.',
    'LBL_EMAIL_ERROR_FROM_ADDRESS' => 'Se requiere la Dirección del Remitente. Por favor, introduzca una dirección de correo válida.',
    'LBL_EMAIL_ERROR_NO_FILE' => 'Por favor, proporcione un archivo.',
    'LBL_EMAIL_ERROR_SERVER' => 'Se requiere una dirección de servidor de correo.',
    'LBL_EMAIL_ERROR_SAVE_ACCOUNT' => 'La cuenta de correo puede no haber sido guardada.',
    'LBL_EMAIL_ERROR_TIMEOUT' => 'Ha ocurrido un error en la comunicación con el servidor de correo.',
    'LBL_EMAIL_ERROR_USER' => 'Se requiere un nombre de inicio de sesión.',
    'LBL_EMAIL_ERROR_PORT' => 'Se requiere un puerto del servidor de correo.',
    'LBL_EMAIL_ERROR_PROTOCOL' => 'Se requiere un protocolo en el servidor.',
    'LBL_EMAIL_ERROR_MONITORED_FOLDER' => 'Se requiere una Carpeta Monitorizada.',
    'LBL_EMAIL_ERROR_TRASH_FOLDER' => 'Se requiere una Carpeta de Papelera.',
    'LBL_EMAIL_ERROR_VIEW_RAW_SOURCE' => 'Esta información no setá disponible',
    'LBL_EMAIL_ERROR_NO_OUTBOUND' => 'No se ha especificado un servidor de correo saliente.',
    'LBL_EMAIL_ERROR_SENDING' => 'Error al enviar el correo electrónico. Póngase en contacto con su administrador para obtener ayuda.',
    'LBL_EMAIL_FOLDERS' => SugarThemeRegistry::current()->getImage('icon_email_folder', 'align=absmiddle border=0', null, null, '.gif', '') . 'Carpetas',
    'LBL_EMAIL_FOLDERS_SHORT' => SugarThemeRegistry::current()->getImage('icon_email_folder', 'align=absmiddle border=0', null, null, '.gif', ''),
    'LBL_EMAIL_FOLDERS_ADD' => 'Agregar',
    'LBL_EMAIL_FOLDERS_ADD_DIALOG_TITLE' => 'Agregar Nueva Carpeta',
    'LBL_EMAIL_FOLDERS_RENAME_DIALOG_TITLE' => 'Renombrar Carpeta',
    'LBL_EMAIL_FOLDERS_ADD_NEW_FOLDER' => 'Guardar',
    'LBL_EMAIL_FOLDERS_ADD_THIS_TO' => 'Agregar esta carpeta a',
    'LBL_EMAIL_FOLDERS_CHANGE_HOME' => 'Esta carpeta no puede ser cambiada',
    'LBL_EMAIL_FOLDERS_DELETE_CONFIRM' => '¿Está seguro de que quiere eliminar esta carpeta?\nEste proceso no puede ser vuelto atrás.\nLa eliminación de carpetas se aplicará en cascada a todas las carpetas contenidas.',
    'LBL_EMAIL_FOLDERS_NEW_FOLDER' => 'Nombre de la Nueva Carpeta',
    'LBL_EMAIL_FOLDERS_NO_VALID_NODE' => 'Por favor, seleccione una carpeta antes de realizar esta acción.',
    'LBL_EMAIL_FOLDERS_TITLE' => 'Administración de Carpetas',

    'LBL_EMAIL_FORWARD' => 'Reenviar',
    'LBL_EMAIL_DELIMITER' => '::;::',
    'LBL_EMAIL_DOWNLOAD_STATUS' => '[[count]] de [[total]] emails descargados',
    'LBL_EMAIL_FROM' => 'De',
    'LBL_EMAIL_GROUP' => 'grupo',
    'LBL_EMAIL_UPPER_CASE_GROUP' => 'Grupo',
    'LBL_EMAIL_HOME_FOLDER' => 'Inicio',
    'LBL_EMAIL_IE_DELETE' => 'Eliminando Cuenta de Correo',
    'LBL_EMAIL_IE_DELETE_SIGNATURE' => 'Eliminando firma',
    'LBL_EMAIL_IE_DELETE_CONFIRM' => '¿Está seguro de que desea eliminar esta cuenta de correo?',
    'LBL_EMAIL_IE_DELETE_SUCCESSFUL' => 'Borrado satisfactorio.',
    'LBL_EMAIL_IE_SAVE' => 'Guardando Información de Cuenta de Correo',
    'LBL_EMAIL_IMPORTING_EMAIL' => 'Importando Email',
    'LBL_EMAIL_IMPORT_EMAIL' => 'Importar en SuiteCRM',
    'LBL_EMAIL_IMPORT_SETTINGS' => 'Configuración de Importación',
    'LBL_EMAIL_INVALID' => 'No válido',
    'LBL_EMAIL_LOADING' => 'Cargando...',
    'LBL_EMAIL_MARK' => 'Marcar',
    'LBL_EMAIL_MARK_FLAGGED' => 'Como Etiquetado',
    'LBL_EMAIL_MARK_READ' => 'Como Leído',
    'LBL_EMAIL_MARK_UNFLAGGED' => 'Como No Etiquetado',
    'LBL_EMAIL_MARK_UNREAD' => 'Como no Ledído',
    'LBL_EMAIL_ASSIGN_TO' => 'Asignar a',

    'LBL_EMAIL_MENU_ADD_FOLDER' => 'Crear Carpeta',
    'LBL_EMAIL_MENU_COMPOSE' => 'Redactar para',
    'LBL_EMAIL_MENU_DELETE_FOLDER' => 'Eliminar Carpeta',
    'LBL_EMAIL_MENU_EMPTY_TRASH' => 'Vaciar Papelera',
    'LBL_EMAIL_MENU_SYNCHRONIZE' => 'Sincronizar',
    'LBL_EMAIL_MENU_CLEAR_CACHE' => 'Limpiar archivos de caché',
    'LBL_EMAIL_MENU_REMOVE' => 'Quitar',
    'LBL_EMAIL_MENU_RENAME_FOLDER' => 'Renombrar Carpeta',
    'LBL_EMAIL_MENU_RENAMING_FOLDER' => 'Renombrando Carpeta',
    'LBL_EMAIL_MENU_MAKE_SELECTION' => 'Por favor, realice una selección antes de intentar esta operación.',

    'LBL_EMAIL_MENU_HELP_ADD_FOLDER' => 'Crear una Carpeta (remota o en SuiteCRM)',
    'LBL_EMAIL_MENU_HELP_DELETE_FOLDER' => 'Eliminar una Carpeta (remota o en SuiteCRM)',
    'LBL_EMAIL_MENU_HELP_EMPTY_TRASH' => 'Vacía todas las carpetas de Papelera de sus cuentas de correo',
    'LBL_EMAIL_MENU_HELP_MARK_READ' => 'Marcar estos emails como leídos',
    'LBL_EMAIL_MENU_HELP_MARK_UNFLAGGED' => 'Marcar estos emails no etiquetados',
    'LBL_EMAIL_MENU_HELP_RENAME_FOLDER' => 'Renombrar una Carpeta (remota o en SuiteCRM)',

    'LBL_EMAIL_MESSAGES' => 'mensajes',

    'LBL_EMAIL_ML_NAME' => 'Nombre de Lista',
    'LBL_EMAIL_ML_ADDRESSES_1' => 'Lista de Direcciones Seleccionada',
    'LBL_EMAIL_ML_ADDRESSES_2' => 'Lista de Direcciones Disponibles',

    'LBL_EMAIL_MULTISELECT' => '<b>Ctrl-Clic</b> para seleccionar múltiples<br />(los usuarios de Mac pueden usar <b>CMD-Clic</b>)',

    'LBL_EMAIL_NO' => 'No',
    'LBL_EMAIL_NOT_SENT' => 'El sistema no puede procesar su petición. Por favor, póngase en contacto con el administrador del sistema.',

    'LBL_EMAIL_OK' => 'Aceptar',
    'LBL_EMAIL_ONE_MOMENT' => 'Un momento, por favor...',
    'LBL_EMAIL_OPEN_ALL' => 'Abrir Múltiples Mensajes',
    'LBL_EMAIL_OPTIONS' => 'Opciones',
    'LBL_EMAIL_QUICK_COMPOSE' => 'Redacción Rápida',
    'LBL_EMAIL_OPT_OUT' => 'Rehusado',
    'LBL_EMAIL_OPT_IN' => 'Autorizado',
    'LBL_EMAIL_OPT_IN_AND_INVALID' => 'Autorizado e Inválido',
    'LBL_EMAIL_OPT_OUT_AND_INVALID' => 'Rehusado e invalido',
    'LBL_EMAIL_PERFORMING_TASK' => 'Realizando Tarea',
    'LBL_EMAIL_PRIMARY' => 'Principal',
    'LBL_EMAIL_PRINT' => 'Imprimir',

    'LBL_EMAIL_QC_BUGS' => 'Incidencia',
    'LBL_EMAIL_QC_CASES' => 'Caso',
    'LBL_EMAIL_QC_LEADS' => 'Cliente Potencial',
    'LBL_EMAIL_QC_CONTACTS' => 'Contacto',
    'LBL_EMAIL_QC_TASKS' => 'Tarea',
    'LBL_EMAIL_QC_OPPORTUNITIES' => 'Oportunidad',
    'LBL_EMAIL_QUICK_CREATE' => 'Creación Rápida',

    'LBL_EMAIL_REBUILDING_FOLDERS' => 'Reconstruyendo Carpetas',
    'LBL_EMAIL_RELATE_TO' => 'Relacionar con',
    'LBL_EMAIL_VIEW_RELATIONSHIPS' => 'Ver Relaciones',
    'LBL_EMAIL_RECORD' => 'Registro de Email',
    'LBL_EMAIL_REMOVE' => 'Quitar',
    'LBL_EMAIL_REPLY' => 'Responder',
    'LBL_EMAIL_REPLY_ALL' => 'Responder a Todos',
    'LBL_EMAIL_REPLY_TO' => 'Responder a',
    'LBL_EMAIL_RETRIEVING_MESSAGE' => 'Recuperando Mensaje',
    'LBL_EMAIL_RETRIEVING_RECORD' => 'Recuperando Registro de Email',
    'LBL_EMAIL_SELECT_ONE_RECORD' => 'Por favor, seleccione un único registro de email',
    'LBL_EMAIL_RETURN_TO_VIEW' => '¿Volver a Módulo Anterior?',
    'LBL_EMAIL_REVERT' => 'Revertir',
    'LBL_EMAIL_RELATE_EMAIL' => 'Relacionar Email',

    'LBL_EMAIL_RULES_TITLE' => 'Administración de Reglas',

    'LBL_EMAIL_SAVE' => 'Guardar',
    'LBL_EMAIL_SAVE_AND_REPLY' => 'Guardar y Responder',
    'LBL_EMAIL_SAVE_DRAFT' => 'Guardar Borrador',
    'LBL_EMAIL_DRAFT_SAVED' => 'El borrador ha sido guardado',

    'LBL_EMAIL_SEARCH' => SugarThemeRegistry::current()->getImage('Search', 'align=absmiddle border=0', null, null,    '.gif', ''),
    'LBL_EMAIL_SEARCH_SHORT' => SugarThemeRegistry::current()->getImage('Search', 'align=absmiddle border=0', null,        null, '.gif', ''),
    'LBL_EMAIL_SEARCH_DATE_FROM' => 'Fecha Desde',
    'LBL_EMAIL_SEARCH_DATE_UNTIL' => 'Fecha Hasta',
    'LBL_EMAIL_SEARCH_NO_RESULTS' => 'No hay resultados para sus criterios de búsqueda.',
    'LBL_EMAIL_SEARCH_RESULTS_TITLE' => 'Resultados de la Búsqueda',

    'LBL_EMAIL_SELECT' => 'Seleccionar',

    'LBL_EMAIL_SEND' => 'Enviar',
    'LBL_EMAIL_SENDING_EMAIL' => 'Enviando Email',

    'LBL_EMAIL_SETTINGS' => 'Configuración',
    'LBL_EMAIL_SETTINGS_ACCOUNTS' => 'Cuentas de Correo',
    'LBL_EMAIL_SETTINGS_ADD_ACCOUNT' => 'Limpiar Formulario',
    'LBL_EMAIL_SETTINGS_CHECK_INTERVAL' => 'Comprobar Correo Nuevo',
    'LBL_EMAIL_SETTINGS_FROM_ADDR' => 'Dirección de Remitente',
    'LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR' => 'Dirección para Notificación de Prueba:',
    'LBL_EMAIL_SETTINGS_FROM_NAME' => 'Nombre del Remitente',
    'LBL_EMAIL_SETTINGS_REPLY_TO_ADDR' => 'Dirección de Responder a',
    'LBL_EMAIL_SETTINGS_FULL_SYNC' => 'Sincronizar Todas las Cuentas de Correo',
    'LBL_EMAIL_TEST_NOTIFICATION_SENT' => 'Se ha enviado un correo electrónico a la dirección  utilizando la configuración de correo saliente proporcionada. Por favor, compruebe si ha recibido el correo para verificar que la configuración es correcta.',
    'LBL_EMAIL_TEST_SEE_FULL_SMTP_LOG' => 'Ver registro de SMTP completo',
    'LBL_EMAIL_SETTINGS_FULL_SYNC_WARN' => '¿Realizar una sincronización completa?\nPara cuentas de correo grandes, puede durar varios minutos.',
    'LBL_EMAIL_SUBSCRIPTION_FOLDER_HELP' => 'Haga clic en la Tecla Shift o en la tecla Ctrl para seleccionar carpetas múltiples.',
    'LBL_EMAIL_SETTINGS_GENERAL' => 'General',
    'LBL_EMAIL_SETTINGS_GROUP_FOLDERS_CREATE' => 'Crear Carpetas de Grupo',

    'LBL_EMAIL_SETTINGS_GROUP_FOLDERS_EDIT' => 'Editar Carpetas de Grupo',

    'LBL_EMAIL_SETTINGS_NAME' => 'Nombre de Cuenta de Correo',
    'LBL_EMAIL_SETTINGS_REQUIRE_REFRESH' => 'Seleccione el número de correos por página en la Bandeja de Entrada. Estas opciones pueden requerir de un refresco de página para ser activadas.',
    'LBL_EMAIL_SETTINGS_RETRIEVING_ACCOUNT' => 'Recuperando Email de Cuenta',
    'LBL_EMAIL_SETTINGS_SAVED' => 'Los ajustes han sido grabados.',
    'LBL_EMAIL_SETTINGS_SEND_EMAIL_AS' => 'Enviar Sólo Correos con Texto Plano',
    'LBL_EMAIL_SETTINGS_SHOW_NUM_IN_LIST' => 'Emails por Página',
    'LBL_EMAIL_SETTINGS_TITLE_LAYOUT' => 'Configuración Visual',
    'LBL_EMAIL_SETTINGS_TITLE_PREFERENCES' => 'Preferencias',
    'LBL_EMAIL_SETTINGS_USER_FOLDERS' => 'Carpetas de Usuario Disponibles',
    'LBL_EMAIL_ERROR_PREPEND' => 'Ha ocurrido un error con el correo electrónico:',
    'LBL_EMAIL_INVALID_PERSONAL_OUTBOUND' => 'El servidor de correo saliente seleccionado para la cuenta de correo que está utilizando no es válido.  Compruebe la configuración o seleccione un servidor de correo distinto para la cuenta.',
    'LBL_EMAIL_INVALID_SYSTEM_OUTBOUND' => 'No se ha configurado un servidor de correo saliente para el envío de correos. Por favor, configure o seleccione un servidor de correo saliente para la cuenta de correo que está utilizando en Configuración >> Cuenta de Correo.',
    'LBL_DEFAULT_EMAIL_SIGNATURES' => 'Firma predeterminada',
    'LBL_EMAIL_SIGNATURES' => 'Firmas',
    'LBL_SMTPTYPE_GMAIL' => 'Gmail',
    'LBL_SMTPTYPE_YAHOO' => 'Correo Yahoo',
    'LBL_SMTPTYPE_EXCHANGE' => 'Microsoft Exchange',
    'LBL_SMTPTYPE_OTHER' => 'Otro:',
    'LBL_EMAIL_SPACER_MAIL_SERVER' => '[ Carpetas Remotas ]',
    'LBL_EMAIL_SPACER_LOCAL_FOLDER' => '[ Carpetas de SuiteCRM ]',
    'LBL_EMAIL_SUBJECT' => 'Asunto',
    'LBL_EMAIL_SUCCESS' => 'Éxito',
    'LBL_EMAIL_SUITE_FOLDER' => 'Carpeta de SuiteCRM',
    'LBL_EMAIL_TEMPLATE_EDIT_PLAIN_TEXT' => 'El cuerpo de la plantilla de correo está vacío',
    'LBL_EMAIL_TEMPLATES' => 'Plantillas',
    'LBL_EMAIL_TO' => 'Para',
    'LBL_EMAIL_VIEW' => 'Ver',
    'LBL_EMAIL_VIEW_HEADERS' => 'Mostrar Cabeceras',
    'LBL_EMAIL_VIEW_RAW' => 'Mostrar Código Fuente del Email',
    'LBL_EMAIL_VIEW_UNSUPPORTED' => 'Esta característica no está soportada cuando se usa con POP3.',
    'LBL_DEFAULT_LINK_TEXT' => 'Texto de enlace por defecto.',
    'LBL_EMAIL_YES' => 'Sí',
    'LBL_EMAIL_TEST_OUTBOUND_SETTINGS' => 'Enviar Correo de Prueba',
    'LBL_EMAIL_TEST_OUTBOUND_SETTINGS_SENT' => 'Correo de Prueba Enviado',
    'LBL_EMAIL_MESSAGE_NO' => 'Mensaje Nº', // Counter. Message number xx
    'LBL_EMAIL_IMPORT_SUCCESS' => 'Importación Existosa',
    'LBL_EMAIL_IMPORT_FAIL' => 'Importación Fallida debido a que el mensaje ya ha sido importado o eliminado del servidor',

    'LBL_LINK_NONE' => 'Ninguno',
    'LBL_LINK_ALL' => 'Todos',
    'LBL_LINK_RECORDS' => 'Registros',
    'LBL_LINK_SELECT' => 'Seleccionar',
    'LBL_LINK_ACTIONS' => 'Acciones', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CLOSE_ACTIVITY_HEADER' => 'Confirmar',
    'LBL_CLOSE_ACTIVITY_CONFIRM' => '¿Desea cerrar este #module#?',
    'LBL_INVALID_FILE_EXTENSION' => 'Extensión de archivo invalida',

    'ERR_AJAX_LOAD' => 'Se produjo un error:',
    'ERR_AJAX_LOAD_FAILURE' => 'Se ha producido un error al procesar su petición, por favor inténtelo de nuevo más tarde.',
    'ERR_AJAX_LOAD_FOOTER' => 'Si persiste el error, por favor solicite al administrador que deshabilite Ajax para este módulo',
    'ERR_DECIMAL_SEP_EQ_THOUSANDS_SEP' => 'No puede utilizarse el mismo carácter como separador decimal que el utilizado como separador de miles.\n\n  Por favor, cambie los valores.',
    'ERR_DELETE_RECORD' => 'Debe especificar un número de registro para eliminar el contacto.',
    'ERR_EXPORT_DISABLED' => 'Exportación deshabilitada.',
    'ERR_EXPORT_TYPE' => 'Error exportando',
    'ERR_INVALID_EMAIL_ADDRESS' => 'no es una dirección de correo válida.',
    'ERR_INVALID_FILE_REFERENCE' => 'Referencia a archivo no válida',
    'ERR_NO_HEADER_ID' => 'Esta funcionalidad no está disponible con este tema.',
    'ERR_NOT_ADMIN' => 'Acceso no autorizado a la administración.',
    'ERR_MISSING_REQUIRED_FIELDS' => 'Falta campo requerido:',
    'ERR_INVALID_REQUIRED_FIELDS' => 'Campo requerido no válido:',
    'ERR_INVALID_VALUE' => 'Valor no válido:',
    'ERR_NO_SUCH_FILE' => 'El archivo no existe en el sistema',
    'ERR_FILE_EMPTY' => 'El archivo está vacío', // PR 6672
    'ERR_NO_SINGLE_QUOTE' => 'No se puede usar comillas simples para ',
    'ERR_NOTHING_SELECTED' => 'Por favor, realice una selección antes de proceder.',
    'ERR_SELF_REPORTING' => 'Un usuario no puede ser informador de si mismo.',
    'ERR_SQS_NO_MATCH_FIELD' => 'No se han encontrado coincidencias para el campo:',
    'ERR_SQS_NO_MATCH' => 'Sin coincidencias',
    'ERR_ADDRESS_KEY_NOT_SPECIFIED' => 'Por favor, especifique el índice &amp;#39;clave&amp;#39; en el atributo displayParams para la definición de Meta-Datos',
    'ERR_EXISTING_PORTAL_USERNAME' => 'Error: El Nombre de Portal ya ha sido asignado a otro contacto.',
    'ERR_COMPATIBLE_PRECISION_VALUE' => 'El valor del campo no es compatible con el tipo de precisión',
    'ERR_EXTERNAL_API_SAVE_FAIL' => 'Se produjo un error al tratar de salvar en la cuenta externa.',
    'ERR_NO_DB' => 'No se ha podido realizar una conexión a la base de datos. Por favor, consulte SuiteCRM error.log para más detalles (0).',
    'ERR_DB_FAIL' => 'Error de base de datos. Por favor, consulte suitecrm.log para más detalles.',
    'ERR_DB_VERSION' => 'Archivos de SuiteCRM {0} sólo se puede utilizar con una base de datos de SuiteCRM {1}.',

    'LBL_ACCOUNT' => 'Cuenta',
    'LBL_ACCOUNTS' => 'Cuentas',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_ACCUMULATED_HISTORY_BUTTON_KEY' => 'H',
    'LBL_ACCUMULATED_HISTORY_BUTTON_LABEL' => 'Ver Resumen',
    'LBL_ACCUMULATED_HISTORY_BUTTON_TITLE' => 'Ver Resumen',
    'LBL_ADD_BUTTON' => 'Agregar',
    'LBL_ADD_DOCUMENT' => 'Agregar Documento',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_KEY' => 'L',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL' => 'Agregar A Lista de Público Objetivo',
    'LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL_ACCOUNTS_CONTACTS' => 'Añadir contactos a la lista de destino',
    'LBL_ADDITIONAL_DETAILS_CLOSE_TITLE' => 'Clic para Cerrar',
    'LBL_ADDITIONAL_DETAILS' => 'Detalles Adicionales',
    'LBL_ADMIN' => 'Administración',
    'LBL_ALT_HOT_KEY' => '',
    'LBL_ARCHIVE' => 'Archivo',
    'LBL_ASSIGNED_TO_USER' => 'Asignado a Usuario',
    'LBL_ASSIGNED_TO' => 'Asignado a:',
    'LBL_BACK' => 'Atrás',
    'LBL_BILLING_ADDRESS' => 'Dirección de Facturación',
    'LBL_QUICK_CREATE' => 'Crear ',
    'LBL_BROWSER_TITLE' => 'SuiteCRM - CRM de Fuentes Abiertas',
    'LBL_BUGS' => 'Incidencias',
    'LBL_BY' => 'por',
    'LBL_CALLS' => 'Llamadas',
    'LBL_CAMPAIGNS_SEND_QUEUED' => 'Enviar los mensajes de la cola de correo',
    'LBL_SUBMIT_BUTTON_LABEL' => 'Enviar',
    'LBL_CASE' => 'Caso',
    'LBL_CASES' => 'Casos',
    'LBL_CHANGE_PASSWORD' => 'Cambiar contraseña',
    'LBL_CHARSET' => 'UTF-8',
    'LBL_CHECKALL' => 'Marcar Todos',
    'LBL_CITY' => 'Ciudad',
    'LBL_CLEAR_BUTTON_LABEL' => 'Limpiar',
    'LBL_CLEAR_BUTTON_TITLE' => 'Limpiar',
    'LBL_CLEARALL' => 'Desmarcar Todos',
    'LBL_CLOSE_BUTTON_TITLE' => 'Cerrar', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_LABEL' => 'Cerrar y Crear Nuevo', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_TITLE' => 'Cerrar y Crear Nuevo', // As in closing a task
    'LBL_CLOSE_AND_CREATE_BUTTON_KEY' => 'C',
    'LBL_OPEN_ITEMS' => 'Elementos Abiertos:',
    'LBL_COMPOSE_EMAIL_BUTTON_KEY' => 'L',
    'LBL_COMPOSE_EMAIL_BUTTON_LABEL' => 'Redactar Correo',
    'LBL_COMPOSE_EMAIL_BUTTON_TITLE' => 'Redactar Correo',
    'LBL_SEARCH_DROPDOWN_YES' => 'Sí',
    'LBL_SEARCH_DROPDOWN_NO' => 'No',
    'LBL_CONTACT_LIST' => 'Lista de Contactos',
    'LBL_CONTACT' => 'Contacto',
    'LBL_CONTACTS' => 'Contactos',
    'LBL_CONTRACT' => 'Contrato',
    'LBL_CONTRACTS' => 'Contratos',
    'LBL_COUNTRY' => 'País:',
    'LBL_CREATE_BUTTON_LABEL' => 'Crear', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CREATED_BY_USER' => 'Creado por el Usuario',
    'LBL_CREATED_USER' => 'Creado por el Usuario',
    'LBL_CREATED' => 'Creado por',
    'LBL_CURRENT_USER_FILTER' => 'Mis Elementos:',
    'LBL_CURRENCY' => 'Moneda:',
    'LBL_DOCUMENTS' => 'Documentos',
    'LBL_DATE_ENTERED' => 'Fecha de Creación:',
    'LBL_DATE_MODIFIED' => 'Fecha de Modificación:',
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
    'LBL_DONE_BUTTON_LABEL' => 'Hecho',
    'LBL_DONE_BUTTON_TITLE' => 'Hecho',
    'LBL_FAVORITES' => 'Favoritos',
    'LBL_VCARD' => 'vCard',
    'LBL_EMPTY_VCARD' => 'Por favor, seleccione un archivo vCard',
    'LBL_EMPTY_REQUIRED_VCARD' => 'La vCard no tiene todos los campos requeridos para este módulo. Por favor consulte suitecrm.log para más detalles.',
    'LBL_VCARD_ERROR_FILESIZE' => 'El archivo subido excede el límite de tamaño, el cual se especificó en el formulario HTML.',
    'LBL_VCARD_ERROR_DEFAULT' => 'Hubo un error subiendo el archivo vCard. Por favor consulte suitecrm.log para más detalles.',
    'LBL_IMPORT_VCARD' => 'Importar vCard:',
    'LBL_IMPORT_VCARD_BUTTON_LABEL' => 'Importar vCard',
    'LBL_IMPORT_VCARD_BUTTON_TITLE' => 'Importar vCard',
    'LBL_VIEW_BUTTON' => 'Ver',
    'LBL_EMAIL_PDF_BUTTON_LABEL' => 'Enviar como PDF',
    'LBL_EMAIL_PDF_BUTTON_TITLE' => 'Enviar como PDF',
    'LBL_EMAILS' => 'Correos',
    'LBL_EMPLOYEES' => 'Empleados',
    'LBL_ENTER_DATE' => 'Introducir Fecha',
    'LBL_EXPORT' => 'Exportar',
    'LBL_FAVORITES_FILTER' => 'Mis Favoritos:',
    'LBL_GO_BUTTON_LABEL' => 'Adelante',
    'LBL_HIDE' => 'Ocultar',
    'LBL_ID' => 'ID',
    'LBL_IMPORT' => 'Importar',
    'LBL_IMPORT_STARTED' => 'Importación iniciada:',
    'LBL_LAST_VIEWED' => 'Recientes',
    'LBL_LEADS' => 'Clientes Potenciales',
    'LBL_LESS' => 'menos',
    'LBL_CAMPAIGN' => 'Campaña:',
    'LBL_CAMPAIGNS' => 'Campañas',
    'LBL_CAMPAIGNLOG' => 'Registro de Campañas',
    'LBL_CAMPAIGN_CONTACT' => 'Campañas',
    'LBL_CAMPAIGN_ID' => 'campaign_id',
    'LBL_CAMPAIGN_NONE' => 'Ninguno',
    'LBL_THEME' => 'Tema:',
    'LBL_FOUND_IN_RELEASE' => 'Encontrado en Versión',
    'LBL_FIXED_IN_RELEASE' => 'Corregido en Versión',
    'LBL_LIST_ACCOUNT_NAME' => 'Nombre de Cuenta',
    'LBL_LIST_ASSIGNED_USER' => 'Usuario',
    'LBL_LIST_CONTACT_NAME' => 'Nombre Contacto',
    'LBL_LIST_CONTACT_ROLE' => 'Rol Contacto',
    'LBL_LIST_DATE_ENTERED' => 'Fecha de Creación',
    'LBL_LIST_EMAIL' => 'Correo',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_LIST_OF' => 'de',
    'LBL_LIST_PHONE' => 'Teléfono',
    'LBL_LIST_RELATED_TO' => 'Relacionado con',
    'LBL_LIST_USER_NAME' => 'Nombre de Usuario',
    'LBL_LISTVIEW_NO_SELECTED' => 'Por favor, seleccione al menos 1 registro para proceder.',
    'LBL_LISTVIEW_TWO_REQUIRED' => 'Por favor, seleccione al menos 2 registros para proceder.',
    'LBL_LISTVIEW_OPTION_SELECTED' => 'Registros Seleccionados',
    'LBL_LISTVIEW_SELECTED_OBJECTS' => 'Seleccionados: ',

    'LBL_LOCALE_NAME_EXAMPLE_FIRST' => 'Juan',
    'LBL_LOCALE_NAME_EXAMPLE_LAST' => 'Pérez',
    'LBL_LOCALE_NAME_EXAMPLE_SALUTATION' => 'Sr.',
    'LBL_LOCALE_NAME_EXAMPLE_TITLE' => 'Mago del Código Fuente',
    'LBL_CANCEL' => 'Cancelar',
    'LBL_VERIFY' => 'Verificar',
    'LBL_RESEND' => 'Reenviar',
    'LBL_PROFILE' => 'Perfil',
    'LBL_MAILMERGE' => 'Combinar Correspondencia',
    'LBL_MASS_UPDATE' => 'Actualización Masiva',
    // STIC-Custom - 20220704 - JCH - Duplicate & Mass Update
    // STIC#776
    'LBL_MASS_DUPLICATE_UPDATE' => 'Duplicado y Actualización Masiva',
    'LBL_MASS_DUPLICATE_REMOVE_NAME' => 'Vaciar el Nombre de los nuevos registros para que pueda ser reconstruido automáticamente',
    'LBL_MASS_DUPLICATE_UPDATE_CONFIRMATION_NUM' => '¿Está seguro de que desea duplicar y actualizar el(los) ',
    'LBL_MASS_DUPLICATE_UPDATE_BTN' => 'Duplicar y Actualizar',
    // END STIC
    'LBL_NO_MASS_UPDATE_FIELDS_AVAILABLE' => 'No hay campos disponibles para la operación de actualización masiva.',
    'LBL_OPT_OUT_FLAG_PRIMARY' => 'Rehusar para Email Principal',
    'LBL_OPT_IN_FLAG_PRIMARY' => 'Adherir con e-mail principal',
    'LBL_MEETINGS' => 'Reuniones',
    'LBL_MEETING_GO_BACK' => 'Volver a la reunión',
    'LBL_MEMBERS' => 'Miembros',
    'LBL_MEMBER_OF' => 'Miembro de',
    'LBL_MODIFIED_BY_USER' => 'Modificado por el Usuario',
    'LBL_MODIFIED_USER' => 'Modificado por el Usuario',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_NAME' => 'Modificado por Nombre',
    'LBL_MORE' => 'Más',
    'LBL_MY_ACCOUNT' => 'Mi Configuración',
    'LBL_NAME' => 'Nombre',
    'LBL_NEW_BUTTON_KEY' => 'N',
    'LBL_NEW_BUTTON_LABEL' => 'Nuevo',
    'LBL_NEW_BUTTON_TITLE' => 'Nuevo',
    'LBL_NEXT_BUTTON_LABEL' => 'Siguiente',
    'LBL_NONE' => '-ninguno-',
    'LBL_NOTES' => 'Notas',
    'LBL_OPPORTUNITIES' => 'Oportunidades',
    'LBL_OPPORTUNITY_NAME' => 'Nombre de la oportunidad',
    'LBL_OPPORTUNITY' => 'Oportunidad',
    'LBL_OR' => 'O',
    'LBL_PANEL_OVERVIEW' => 'Visión Global', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PANEL_ASSIGNMENT' => 'Otro', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PANEL_ADVANCED' => 'Más Información', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_PARENT_TYPE' => 'Tipo de Padre',
    'LBL_PERCENTAGE_SYMBOL' => '%',
    'LBL_POSTAL_CODE' => 'Código Postal:',
    'LBL_PRIMARY_ADDRESS_CITY' => 'Ciudad de dirección principal:',
    'LBL_PRIMARY_ADDRESS_COUNTRY' => 'País de dirección principal:',
    'LBL_PRIMARY_ADDRESS_POSTALCODE' => 'CP de dirección principal:',
    'LBL_PRIMARY_ADDRESS_STATE' => 'Estado/Provincia de dirección principal:',
    'LBL_PRIMARY_ADDRESS_STREET_2' => 'Calle de dirección principal 2',
    'LBL_PRIMARY_ADDRESS_STREET_3' => 'Calle de dirección principal 3',
    'LBL_PRIMARY_ADDRESS_STREET' => 'Calle de dirección principal:',
    'LBL_PRIMARY_ADDRESS' => 'Dirección principal:',

    'LBL_PROSPECTS' => 'Prospectos',
    'LBL_PRODUCTS' => 'Productos',
    'LBL_PROJECT_TASKS' => 'Tareas de Proyecto',
    'LBL_PROJECTS' => 'Proyectos',
    'LBL_QUOTES' => 'Presupuestos',

    'LBL_RELATED' => 'Relacionado',
    'LBL_RELATED_RECORDS' => 'Registros Relacionados',
    'LBL_REMOVE' => 'Quitar',
    'LBL_REPORTS_TO' => 'Informa a',
    'LBL_REQUIRED_SYMBOL' => '*',
    'LBL_REQUIRED_TITLE' => 'Indica que es un campo requerido',
    'LBL_EMAIL_DONE_BUTTON_LABEL' => 'Hecho',
    'LBL_FULL_FORM_BUTTON_KEY' => 'F',
    'LBL_FULL_FORM_BUTTON_LABEL' => 'Formulario Completo',
    'LBL_FULL_FORM_BUTTON_TITLE' => 'Formulario Completo',
    'LBL_SAVE_NEW_BUTTON_LABEL' => 'Guardar y Crear Nuevo',
    'LBL_SAVE_NEW_BUTTON_TITLE' => 'Guardar y Crear Nuevo',
    'LBL_SAVE_OBJECT' => 'Guardar {0}',
    'LBL_SEARCH_BUTTON_KEY' => 'Q',
    'LBL_SEARCH_BUTTON_LABEL' => 'Búsqueda',
    'LBL_SEARCH_BUTTON_TITLE' => 'Búsqueda',
    'LBL_FILTER' => 'Filtro',
    'LBL_SEARCH' => 'Búsqueda',
    'LBL_SEARCH_ALT' => '',
    'LBL_SEARCH_MORE' => 'más',
    'LBL_UPLOAD_IMAGE_FILE_INVALID' => 'Formato de archivo no válido, sólo es posible subir archivos con imágenes.',
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
    'LBL_ACCESSKEY_SELECT_FILE_TITLE' => 'Seleccionar Archivo',
    'LBL_ACCESSKEY_SELECT_FILE_LABEL' => 'Seleccionar Archivo',
    'LBL_ACCESSKEY_CLEAR_FILE_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_FILE_TITLE' => 'Limpiar archivo',
    'LBL_ACCESSKEY_CLEAR_FILE_LABEL' => 'Limpiar archivo',

    'LBL_ACCESSKEY_SELECT_USERS_KEY' => 'U',
    'LBL_ACCESSKEY_SELECT_USERS_TITLE' => 'Seleccionar usuario',
    'LBL_ACCESSKEY_SELECT_USERS_LABEL' => 'Seleccionar usuario',
    'LBL_ACCESSKEY_CLEAR_USERS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_USERS_TITLE' => 'Limpiar usuario',
    'LBL_ACCESSKEY_CLEAR_USERS_LABEL' => 'Limpiar usuairo',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_KEY' => 'A',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_TITLE' => 'Seleccionar Cuenta',
    'LBL_ACCESSKEY_SELECT_ACCOUNTS_LABEL' => 'Seleccionar Cuenta',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_TITLE' => 'Limpiar Cuenta',
    'LBL_ACCESSKEY_CLEAR_ACCOUNTS_LABEL' => 'Limpiar Cuenta',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_KEY' => 'M',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_TITLE' => 'Seleccionar campaña',
    'LBL_ACCESSKEY_SELECT_CAMPAIGNS_LABEL' => 'Seleccionar campaña',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_TITLE' => 'Limpiar campaña',
    'LBL_ACCESSKEY_CLEAR_CAMPAIGNS_LABEL' => 'Limpiar campaña',
    'LBL_ACCESSKEY_SELECT_CONTACTS_KEY' => 'C',
    'LBL_ACCESSKEY_SELECT_CONTACTS_TITLE' => 'Seleccionar Contacto',
    'LBL_ACCESSKEY_SELECT_CONTACTS_LABEL' => 'Seleccionar Contacto',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_TITLE' => 'Limpiar contacto',
    'LBL_ACCESSKEY_CLEAR_CONTACTS_LABEL' => 'Limpiar contacto',
    'LBL_ACCESSKEY_SELECT_TEAMSET_KEY' => 'Z',
    'LBL_ACCESSKEY_SELECT_TEAMSET_TITLE' => 'Seleccionar equipo',
    'LBL_ACCESSKEY_SELECT_TEAMSET_LABEL' => 'Seleccionar equipo',
    'LBL_ACCESSKEY_CLEAR_TEAMS_KEY' => ' ',
    'LBL_ACCESSKEY_CLEAR_TEAMS_TITLE' => 'Limpiar equipo',
    'LBL_ACCESSKEY_CLEAR_TEAMS_LABEL' => 'Limpiar equipo',
    'LBL_SERVER_RESPONSE_RESOURCES' => 'Recursos usados para construir esta página (consultas, archivos)',
    'LBL_SERVER_RESPONSE_TIME_SECONDS' => 'segundos.',
    'LBL_SERVER_RESPONSE_TIME' => 'Tiempo de respuesta del servidor:',
    'LBL_SERVER_MEMORY_BYTES' => 'bytes',
    'LBL_SERVER_MEMORY_USAGE' => 'Uso de la memoria del servidor: {0} ({1})',
    'LBL_SERVER_MEMORY_LOG_MESSAGE' => 'Uso: - modulo: {0} - acción: {1}',
    'LBL_SERVER_PEAK_MEMORY_USAGE' => 'Uso de la memoria máxima del servidor: {0} ({1})',
    'LBL_SHIPPING_ADDRESS' => 'Dirección de Envío',
    'LBL_SHOW' => 'Mostrar',
    'LBL_STATE' => 'Estado:', //Used for Case State, situation, condition
    'LBL_STATUS_UPDATED' => '¡Su estado para este evento ha sido actualizado!',
    'LBL_STATUS' => 'Estado:',
    'LBL_STREET' => 'Calle',
    'LBL_SUBJECT' => 'Asunto',

    'LBL_INBOUNDEMAIL_ID' => 'ID de Correo Entrante',

    'LBL_SCENARIO_SALES' => 'Ventas',
    'LBL_SCENARIO_MARKETING' => 'Marketing',
    'LBL_SCENARIO_FINANCE' => 'Finanzas',
    'LBL_SCENARIO_SERVICE' => 'Servicio',
    'LBL_SCENARIO_PROJECT' => 'Administración de proyectos',

    'LBL_SCENARIO_SALES_DESCRIPTION' => 'Este escenario facilita la administración de los ítemes de venta',
    'LBL_SCENARIO_MAKETING_DESCRIPTION' => 'Este escenario facilita la gestión de los ítems de marketing',
    'LBL_SCENARIO_FINANCE_DESCRIPTION' => 'Esta situación facilita la gestión de los elementos relacionados con las finanzas',
    'LBL_SCENARIO_SERVICE_DESCRIPTION' => 'Este escenario facilita la gestión de los ítems relacionados con servicios',
    'LBL_SCENARIO_PROJECT_DESCRIPTION' => 'Este escenario facilita la administración de los ítems relacionados con proyectos',

    'LBL_SYNC' => 'Sincronizar',
    'LBL_TABGROUP_ALL' => 'Todo',
    'LBL_TABGROUP_ACTIVITIES' => 'Actividades',
    'LBL_TABGROUP_COLLABORATION' => 'Colaboración',
    'LBL_TABGROUP_MARKETING' => 'Marketing',
    'LBL_TABGROUP_OTHER' => 'Otro',
    'LBL_TABGROUP_SALES' => 'Ventas',
    'LBL_TABGROUP_SUPPORT' => 'Soporte',
    'LBL_TASKS' => 'Tareas',
    'LBL_THOUSANDS_SYMBOL' => 'K',
    'LBL_TRACK_EMAIL_BUTTON_LABEL' => 'Archivar Correo',
    'LBL_TRACK_EMAIL_BUTTON_TITLE' => 'Archivar Correo',
    'LBL_UNDELETE_BUTTON_LABEL' => 'Restaurar',
    'LBL_UNDELETE_BUTTON_TITLE' => 'Restaurar',
    'LBL_UNDELETE_BUTTON' => 'Restaurar',
    'LBL_UNDELETE' => 'Restaurar',
    'LBL_UNSYNC' => 'Desincronizar',
    'LBL_UPDATE' => 'Actualizar',
    'LBL_USER_LIST' => 'Lista de Usuarios',
    'LBL_USERS' => 'Usuarios',
    'LBL_VERIFY_EMAIL_ADDRESS' => 'Comprobando la entrada de correo actual...',
    'LBL_VERIFY_PORTAL_NAME' => 'Comprobando el nombre de portal actual...',
    'LBL_VIEW_IMAGE' => 'ver',

    'LNK_ABOUT' => 'Acerca de',
    'LNK_ADVANCED_FILTER' => 'Filtro avanzado',
    'LNK_BASIC_FILTER' => 'Filtro rápido',
    'LBL_ADVANCED_SEARCH' => 'Filtro avanzado',
    'LBL_QUICK_FILTER' => 'Filtro rápido',
    'LNK_SEARCH_NONFTS_VIEW_ALL' => 'Mostrar Todo',
    'LNK_CLOSE' => 'Cierre',
    'LBL_MODIFY_CURRENT_FILTER' => 'Modificar filtro actual',
    'LNK_SAVED_VIEWS' => 'Opciones de Diseño',
    'LNK_DELETE' => 'Eliminar',
    'LNK_EDIT' => 'Editar',
    'LNK_GET_LATEST' => 'Obtener última',
    'LNK_GET_LATEST_TOOLTIP' => 'Reemplazar con última versión',
    'LNK_HELP' => 'Ayuda',
    'LNK_CREATE' => 'Crear',
    'LNK_LIST_END' => 'Fin',
    'LNK_LIST_NEXT' => 'Siguiente',
    'LNK_LIST_PREVIOUS' => 'Anterior',
    'LNK_LIST_RETURN' => 'Volver a lista',
    'LNK_LIST_START' => 'Inicio',
    'LNK_LOAD_SIGNED' => 'Firmar',
    'LNK_LOAD_SIGNED_TOOLTIP' => 'Reemplazar con documento firmado',
    'LNK_PRINT' => 'Imprimir',
    'LNK_BACKTOTOP' => 'Volver al parte superior',
    'LNK_REMOVE' => 'Quitar',
    'LNK_RESUME' => 'Continuar',
    'LNK_VIEW_CHANGE_LOG' => 'Ver Registro de Cambios',

    'NTC_CLICK_BACK' => 'Por favor, presione el botón anterior del navegador y corrija el error.',
    'NTC_DATE_FORMAT' => '(aaaa-mm-dd)',
    'NTC_DELETE_CONFIRMATION_MULTIPLE' => '¿Está seguro de que desea eliminar los registros seleccionados?',
    'NTC_TEMPLATE_IS_USED' => 'La plantilla se está utilizando en al menos un registro de marketing por email. ¿Está seguro de que desea eliminarla?',
    'NTC_TEMPLATES_IS_USED' => 'Las siguientes plantillas se utilizan en los registros de marketing por correo electrónico. ¿Seguro que quieres eliminarlos?' . PHP_EOL,
    'NTC_DELETE_CONFIRMATION' => '¿Está seguro de que desea eliminar esta registro?',
    'NTC_DELETE_CONFIRMATION_NUM' => '¿Está seguro de que desea eliminar el(los) ',
    'NTC_UPDATE_CONFIRMATION_NUM' => '¿Está seguro de que desea actualizar el(los) ',
    'NTC_DELETE_SELECTED_RECORDS' => ' registro(s) seleccionado(s)?',
    'NTC_LOGIN_MESSAGE' => 'Por favor, introduzca su nombre de usuario y contraseña.',
    'NTC_NO_ITEMS_DISPLAY' => 'ninguno',
    'NTC_REMOVE_CONFIRMATION' => '¿Está seguro de que desea quitar esta relación?',
    'NTC_REQUIRED' => 'Indica un campo requerido',
    'NTC_TIME_FORMAT' => '(24:00)',
    'NTC_WELCOME' => 'Bienvenido',
    'NTC_YEAR_FORMAT' => '(aaaa)',
    'WARN_UNSAVED_CHANGES' => 'Está a punto de abandonar este registro sin guardar los cambios que haya podido realizar. ¿Está seguro de que desea salir de este registro?',
    'ERROR_NO_RECORD' => 'Error al recuperar registro.  Este registro puede haber sido eliminado o puede que no esté autorizado para verlo.',
    'WARN_BROWSER_VERSION_WARNING' => '<p><b>Aviso: </b>Su navegador o la versión de su navegador no es compatible.</p><p>Se recomiendan las siguientes versiones de navegadores:</p><ul><li>Internet Explorer 9</li><li>Mozilla Firefox 14, 15 </li><li>Safari 6</li><li>Google Chrome 22 (or latest version)</li></ul>',
    'WARN_BROWSER_IE_COMPATIBILITY_MODE_WARNING' => '<b>Advertencia:</b> Su navegador está en modo compatibilidad IE el cual no es soportado.',
    'ERROR_TYPE_NOT_VALID' => 'Error. Este tipo no es válido.',
    'ERROR_NO_BEAN' => 'Falló la obtención del bean',
    'LBL_DUP_MERGE' => 'Buscar Duplicados',
    'LBL_MANAGE_SUBSCRIPTIONS' => 'Administrar Suscripciones',
    'LBL_MANAGE_SUBSCRIPTIONS_FOR' => 'Administrar Suscripciones a',
    // Ajax status strings
    'LBL_LOADING' => 'Cargando ...',
    'LBL_SEARCHING' => 'Buscando...',
    'LBL_SAVING_LAYOUT' => 'Guardando Diseño ...',
    'LBL_SAVED_LAYOUT' => 'El diseño ha sido guardado.',
    'LBL_SAVED' => 'Guardado',
    'LBL_SAVING' => 'Guardando',
    'LBL_DISPLAY_COLUMNS' => 'Mostrar Columnas',
    'LBL_HIDE_COLUMNS' => 'Ocultar Columnas',
    'LBL_SEARCH_CRITERIA' => 'Criterios de búsqueda',
    'LBL_SAVED_VIEWS' => 'Vistas guardadas',
    'LBL_PROCESSING_REQUEST' => 'Procesando...',
    'LBL_REQUEST_PROCESSED' => 'Hecho',
    'LBL_AJAX_FAILURE' => 'Fallo de Ajax',
    'LBL_MERGE_DUPLICATES' => 'Combinar',
    'LBL_SAVED_FILTER_SHORTCUT' => 'Mis filtros',
    'LBL_SEARCH_POPULATE_ONLY' => 'Realizar una búsqueda utilizando el formulario de búsqueda anterior',
    'LBL_DETAILVIEW' => 'Vista de Detalle',
    'LBL_LISTVIEW' => 'Vista de Lista',
    'LBL_EDITVIEW' => 'Vista de Edición',
    'LBL_BILLING_STREET' => 'Calle:',
    'LBL_SHIPPING_STREET' => 'Calle:',
    'LBL_SEARCHFORM' => 'Formulario de Búsqueda',
    'LBL_SAVED_SEARCH_ERROR' => 'Por favor, introduzca un nombre para esta vista.',
    'LBL_DISPLAY_LOG' => 'Mostrar Traza',
    'ERROR_JS_ALERT_SYSTEM_CLASS' => 'Sistema',
    'ERROR_JS_ALERT_TIMEOUT_TITLE' => 'Cierre de la Sesión',
    'ERROR_JS_ALERT_TIMEOUT_MSG_1' => 'Su sesión va a expirar en 2 minutos. Por favor, guarde su trabajo.',
    'ERROR_JS_ALERT_TIMEOUT_MSG_2' => 'Su sesión ha expirado.',
    'MSG_JS_ALERT_MTG_REMINDER_AGENDA' => "Agenda:",
    'MSG_JS_ALERT_MTG_REMINDER_MEETING' => 'Reunión',
    'MSG_JS_ALERT_MTG_REMINDER_CALL' => 'Llamada',
    'MSG_JS_ALERT_MTG_REMINDER_TIME' => 'Hora:',
    'MSG_JS_ALERT_MTG_REMINDER_LOC' => 'Lugar:',
    'MSG_JS_ALERT_MTG_REMINDER_DESC' => 'Descripción:',
    'MSG_JS_ALERT_MTG_REMINDER_STATUS' => 'Estado:',
    'MSG_JS_ALERT_MTG_REMINDER_RELATED_TO' => 'Relacionado A: ',
    'MSG_JS_ALERT_MTG_REMINDER_CALL_MSG' => "\nHaga clic en Aceptar para acceder a esta llamada o haga clic en Cancelar para cerrar este mensaje.",
    'MSG_JS_ALERT_MTG_REMINDER_MEETING_MSG' => "\nHaga clic en Aceptar para ver esta reunión o en Cancelar para omitir este mensaje.",
    'MSG_JS_ALERT_MTG_REMINDER_NO_EVENT_NAME' => 'Evento',
    'MSG_JS_ALERT_MTG_REMINDER_NO_DESCRIPTION' => 'Evento no establecido.',
    'MSG_JS_ALERT_MTG_REMINDER_NO_LOCATION' => 'Localización no establecida.',
    'MSG_JS_ALERT_MTG_REMINDER_NO_START_DATE' => 'La fecha de inicio no está definida.',
    'MSG_LIST_VIEW_NO_RESULTS_BASIC' => 'No se encontraron resultados.',
    'MSG_LIST_VIEW_NO_RESULTS_CHANGE_CRITERIA' => 'No se encontraron resultados... Vuelve a intentar cambiando tu criterio de búsqueda',
    'MSG_LIST_VIEW_NO_RESULTS' => 'No se han encontrado resultados para <item1>',
    'MSG_LIST_VIEW_NO_RESULTS_SUBMSG' => 'Crear <item1> como un nuevo <item2>',
    'MSG_LIST_VIEW_CHANGE_SEARCH' => 'o cambia tu criterio de búsqueda',
    'MSG_EMPTY_LIST_VIEW_NO_RESULTS' => 'Actualmente no tienes registros guardados. <item2> o <item3> ahora uno.',

    'LBL_CLICK_HERE' => 'Haga clic aquí',
    // contextMenu strings
    'LBL_ADD_TO_FAVORITES' => 'Agregar a Mis Favoritos',
    'LBL_CREATE_CONTACT' => 'Nuevo Contacto',
    'LBL_CREATE_CASE' => 'Nuevo Caso',
    'LBL_CREATE_NOTE' => 'Nueva Nota',
    'LBL_CREATE_OPPORTUNITY' => 'Nueva Oportunidad',
    'LBL_SCHEDULE_CALL' => 'Registrar Llamada',
    'LBL_SCHEDULE_MEETING' => 'Programar Reunión',
    'LBL_CREATE_TASK' => 'Nueva Tarea',
    //web to lead
    'LBL_GENERATE_WEB_TO_LEAD_FORM' => 'Generar Formulario',
    'LBL_SAVE_WEB_TO_LEAD_FORM' => 'Guardar Formulario',
    'LBL_AVAILABLE_FIELDS' => 'Campos Disponibles',
    'LBL_FIRST_FORM_COLUMN' => 'Primera columna del Formulario',
    'LBL_SECOND_FORM_COLUMN' => 'Segunda columna del formulario',
    'LBL_ASSIGNED_TO_REQUIRED' => 'Falta campo obligatorio: Asignado a',
    'LBL_RELATED_CAMPAIGN_REQUIRED' => 'Falta campo obligatorio: Campaña relacionada',
    'LBL_TYPE_OF_PERSON_FOR_FORM' => 'Formulario web para crear ',
    'LBL_TYPE_OF_PERSON_FOR_FORM_DESC' => 'El envío de este formulario creará ',

    'LBL_ADD_ALL_LEAD_FIELDS' => 'Agregar Todos los Campos',
    'LBL_RESET_ALL_LEAD_FIELDS' => 'Restablecer todos los campos',
    'LBL_REMOVE_ALL_LEAD_FIELDS' => 'Quitar Todos los Campos',
    'LBL_NEXT_BTN' => 'Siguiente',
    'LBL_ONLY_IMAGE_ATTACHMENT' => 'Sólo puede incluirse un adjunto de tipo imagen',
    'LBL_TRAINING' => 'Foro de Soporte',
    'ERR_MSSQL_DB_CONTEXT' => 'Cambiado el contexto de base de datos a',
    'ERR_MSSQL_WARNING' => 'Aviso:',

    //Meta-Data framework
    'ERR_CANNOT_CREATE_METADATA_FILE' => 'Error: No existe el archivo [[file]].  No se ha podido crear porque el archivo con el HTML correspondiente no ha sido encontrado.',
    'ERR_CANNOT_FIND_MODULE' => 'Error: El módulo [module] no existe.',
    'LBL_ALT_ADDRESS' => 'Otra Dirección:',
    'ERR_SMARTY_UNEQUAL_RELATED_FIELD_PARAMETERS' => 'Error: Hay un número de argumentos desigual para los elementos &amp;#39;key&amp;#39; y &amp;#39;copy&amp;#39; en el array displayParams.',

    /* MySugar Framework (for Home and Dashboard) */
    'LBL_DASHLET_CONFIGURE_GENERAL' => 'General',
    'LBL_DASHLET_CONFIGURE_FILTERS' => 'Filtros',
    'LBL_DASHLET_CONFIGURE_MY_ITEMS_ONLY' => 'Sólo Mis Elementos',
    'LBL_DASHLET_CONFIGURE_TITLE' => 'Título',
    'LBL_DASHLET_CONFIGURE_DISPLAY_ROWS' => 'Mostrar Filas',

    // MySugar status strings
    'LBL_MAX_DASHLETS_REACHED' => 'Ha alcanzado el máximo número de dashlets establecido por su administrador. Por favor, quite un SuiteCRM Dashlet para poder agregar más.',
    'LBL_ADDING_DASHLET' => 'Agregando SuiteCRM Dashlet ...',
    'LBL_ADDED_DASHLET' => 'SuiteCRM Dashlet Agregado',
    'LBL_REMOVE_DASHLET_CONFIRM' => '¿Está seguro de que desea quitar el SuiteCRM Dashlet?',
    'LBL_REMOVING_DASHLET' => 'Quitando SuiteCRM Dashlet ...',
    'LBL_REMOVED_DASHLET' => 'SuiteCRM Dashlet Quitado',
    'LBL_MAX_DASHLET_COLUMNS' => "Se pueden mostrar un máximo de %s columnas.",
    'LBL_RETRIEVING_XML_DATA' => "Hubo un problema al recuperar los datos XML:\n",

    // MySugar Menu Options

    'LBL_LOADING_PAGE' => 'Cargando página, espere por favor...',

    'LBL_RELOAD_PAGE' => 'Por favor, <a href="javascript: window.location.reload()">recargue la ventana</a> para usar este SuiteCRM Dashlet.',
    'LBL_ADD_DASHLETS' => 'Agregar Dashlets',
    'LBL_CLOSE_DASHLETS' => 'Cerrar',
    'LBL_OPTIONS' => 'Opciones',
    'LBL_1_COLUMN' => '1 Columna',
    'LBL_2_COLUMN' => '2 Columnas',
    'LBL_3_COLUMN' => '3 Columnas',
    'LBL_PAGE_NAME' => 'Nombre de página',

    'LBL_SEARCH_RESULTS' => 'Resultados de Búsqueda',
    'LBL_SEARCH_MODULES' => 'Módulos',
    'LBL_SEARCH_TOOLS' => 'Herramientas',
    'LBL_SEARCH_HELP_TITLE' => 'Consejos de Búsqueda',
    /* End MySugar Framework strings */

    'LBL_NO_IMAGE' => 'Sin Imagen',

    //adding a label for address copy from left
    'LBL_COPY_ADDRESS_FROM_LEFT' => 'Copiar dirección de la izquierda:',
    'LBL_SAVE_AND_CONTINUE' => 'Guardar y Continuar',

    'LBL_SEARCH_HELP_TEXT' => '<p><br /><strong>Controles de Selección Múltiple</strong></p><ul><li>Click en un valor para seleccionar un atributo.</li><li>Ctrl-click&nbsp;para&nbsp;seleccionar múltiples atributos. Usuarios de Mac usar CMD-click.</li><li>Para seleccionar todos los valores entre dos atributos,&nbsp; click en el primer valor&nbsp;y luego shift-click en el último valor.</li></ul><p><strong>Búsqueda avanzada & Opciones de Diseño</strong><br><br>Al usar la <b>Búsqueda Avanzada & Opciones de Diseño</b>, usted puede guardar un conjunto de parámetros de búsqueda y/o una Vista de Lista personalizada con el fin de obtener rápidamente los resultados de búsqueda y presentación en futuras oportunidades. Todas las búsquedas guardadas aparecen en la lista de Búsquedas Guardadas, identificadas por su nombre, en la que la última búsqueda cargada aparece en primer lugar.<br><br>Para personalizar la Vista de Lista, utilice las cajas Esconder Columnas y Mostrar Columnas que permiten seleccionar los campos que se mostrarán en el resultado de la búsqueda. Por ejemplo, usted puede mostrar o esconder en el resultado de la búsqueda detalles tales como el nombre del registro, el usuario asignado o el equipo asignado. Para agregar una columna a la Vista de Lista, seleccione el campo correspondiente de la lista Esconder Columnas y use la flecha hacia la izquierda para moverlo a la lista Mostrar Columnas. Para eliminar una columna de la Vista de Lista, selecciónela en la lista Mostrar Columnas y use la flecha hacia la derecha para moverla a la lista Esconder Columnas.<br><br>Si usted guarda las opciones de diseño, podrá cargarlas en cualquier momento para ver los resultados de su búsqueda de manera personalizada.<br><br>Para guardar y actualizar una búsqueda y/o un diseño:<ol><li>Ingrese un nombre para el resultado de la búsqueda en el campo <b>Guardar búsqueda como</b> y haga click en <b>Guardar</b>. El nombre dado ahora se muestra en la lista de Búsquedas guardadas, adyacente al botón <b>Limpiar</b>. </li><li>Para ver una búsqueda guardada, selecciónela de la lista de Búsquedas guardas. Los resultados de la búsqueda son mostrados en la Vista de Lista.</li><li>Para actualizar las propiedades de una búsqueda guardada, selecciónela de la lista, seleccione el nuevo criterio de búsqueda y/o la nueva opción de diseño en el área Búsqueda Avanzada y luego haga click en <b>Actualizar</b> al lado de <b>Modificar búsqueda actual</b>.</li><li>Para eliminar una búsqueda guardada, selecciónela en la lista Búsquedas Guardas y luego haga click en <b>Eliminar</b> al lado de <b>Modificar búsqueda actual</b>, y luego haga click en <b>OK</b> para confirmar la eliminación.</li></ol><p><strong>Tips</strong><br><br>Puede utilizar el signo % como comodín para realizar una búsqueda más amplia.Por ejemplo, en vez de buscar resultados iguales a "Manzanas" usted podría cambiar su búsqueda a "Manzanas%" lo que le dará como resultado todos los registros que empiezan con la palabra Manzanas pero también otras que podrían estar seguidos por otros carcateres.</p>',

    //resource management
    'ERR_QUERY_LIMIT' => 'Error: Límite de $limit consultas alcanzado en el módulo $module.',
    'ERROR_NOTIFY_OVERRIDE' => 'Error: ResourceObserver->notify() necesita ser reemplazado.',

    //tracker labels
    'ERR_MONITOR_FILE_MISSING' => 'Error: No se puede crear monitor porque el archivo de metadatos está vacío o el archivo no existe.',
    'ERR_MONITOR_NOT_CONFIGURED' => 'Error: No hay monitor configurado para el nombre solicitado',
    'ERR_UNDEFINED_METRIC' => 'Error: No se puede establecer el valor de métrica definido',
    'ERR_STORE_FILE_MISSING' => 'Error: No se puede encontrar el archivo de la aplicación de la tienda',

    'LBL_MONITOR_ID' => 'Id del Monitor',
    'LBL_USER_ID' => 'Id del Usuario',
    'LBL_MODULE' => 'Módulo',
    'LBL_ITEM_ID' => 'Id del Elemento',
    'LBL_ITEM_SUMMARY' => 'Resumen del Elemento',
    'LBL_ACTION' => 'Acción',
    'LBL_SESSION_ID' => 'Id de la Sesión',
    'LBL_BREADCRUMBSTACK_CREATED' => 'BreadCrumbStack creado por el usuario id {0}',
    'LBL_VISIBLE' => 'Dato visible',
    'LBL_DATE_LAST_ACTION' => 'Fecha',
    
    //jc:#12287 - For javascript validation messages
    'MSG_IS_NOT_BEFORE' => 'no antes de',
    'MSG_IS_MORE_THAN' => 'es más que',
    'MSG_IS_LESS_THAN' => 'es menor que',
    'MSG_SHOULD_BE' => 'debe ser',
    'MSG_OR_GREATER' => 'o más',

    'LBL_LIST' => 'Lista',
    'LBL_CREATE_BUG' => 'Crear incidencia',

    'LBL_OBJECT_IMAGE' => 'imagen objeto',
    //jchi #12300
    'LBL_MASSUPDATE_DATE' => 'Seleccionar fecha',

    'LBL_VALUE' => 'valor',
    'LBL_VALIDATE_RANGE' => 'no está dentro del rango válido',
    'LBL_CHOOSE_START_AND_END_DATES' => 'Por favor seleccione un rango de fecha inicial y un rango de fecha final',
    'LBL_CHOOSE_START_AND_END_ENTRIES' => 'Por favor seleccione un rango de entrada de inicio y de finalización',

    //jchi #  20776
    'LBL_DROPDOWN_LIST_ALL' => 'Todos',

    //Connector
    'ERR_CONNECTOR_FILL_BEANS_SIZE_MISMATCH' => 'Error: La cantidad del Array del parámetro bean no coincide con la cantidad del Array del resultado.',
    'ERR_MISSING_MAPPING_ENTRY_FORM_MODULE' => 'Error: Falta el módulo de entrada de asignación.',
    'ERROR_UNABLE_TO_RETRIEVE_DATA' => 'Error: No se puede recuperar datos de {0} conector. Actualmente, el servicio puede ser inaccesible o los ajustes de configuración pueden no ser válidas. Mensaje de error del conector: ({1}).',

    // fastcgi checks
    'LBL_FASTCGI_LOGGING' => 'Para unos resultados óptimos al utilizar el sapi IIS/FastCGI, establezca fastcgi.logging a 0 en su archivo php.ini.',

    //Collection Field
    'LBL_COLLECTION_NAME' => 'Nombre',
    'LBL_COLLECTION_PRIMARY' => 'Principal',
    'ERROR_MISSING_COLLECTION_SELECTION' => 'Campo obligatorio vacío',

    //MB -Fixed Bug #32812 -Max
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_DESCRIPTION' => 'Descripción',

    'LBL_YESTERDAY' => 'ayer',
    'LBL_NOW' => 'ahora',
    'LBL_TODAY' => 'hoy',
    'LBL_TOMORROW' => 'mañana',
    'LBL_NEXT_WEEK' => 'la semana que viene',
    'LBL_NEXT_MONDAY' => 'próximo lunes',
    'LBL_NEXT_FRIDAY' => 'próximo viernes',
    'LBL_TWO_WEEKS' => 'dos semanas',
    'LBL_NEXT_MONTH' => 'el mes que viene',
    'LBL_FIRST_DAY_OF_NEXT_MONTH' => 'primer día del próximo mes',
    'LBL_THREE_MONTHS' => 'tres meses',
    'LBL_SIXMONTHS' => 'seis meses',
    'LBL_NEXT_YEAR' => 'próximo año',

    //Datetimecombo fields
    'LBL_HOURS' => 'Horas',
    'LBL_MINUTES' => 'Minutos',
    'LBL_MERIDIEM' => 'Meridiano',
    'LBL_DATE' => 'Fecha',
    'LBL_DASHLET_CONFIGURE_AUTOREFRESH' => 'Actualización automática',

    'LBL_DURATION_DAY' => 'día',
    'LBL_DURATION_HOUR' => 'hora',
    'LBL_DURATION_MINUTE' => 'minuto',
    'LBL_DURATION_DAYS' => 'días',
    'LBL_DURATION_HOURS' => 'Duración (Horas)',
    'LBL_DURATION_MINUTES' => 'Duración (Minutos)',

    //Calendar widget labels
    'LBL_CHOOSE_MONTH' => 'Elegir mes',
    'LBL_ENTER_YEAR' => 'Poner año',
    'LBL_ENTER_VALID_YEAR' => 'Por favor, poner un año valido',

    //File write error label
    'ERR_FILE_WRITE' => 'Error: No se pudo escribir el archivo {0}. Por favor, revise el sistema y los permisos del servidor web.',
    'ERR_FILE_NOT_FOUND' => 'Error: No se puede cargar el archivo {0}. Por favor, compruebe los permisos del sistema y del servidor web.',

    'LBL_AND' => 'y',

    // File fields
    'LBL_SEARCH_EXTERNAL_API' => 'Archivo de fuente externa',
    'LBL_EXTERNAL_SECURITY_LEVEL' => 'Seguridad',

    //IMPORT SAMPLE TEXT
    'LBL_IMPORT_SAMPLE_FILE_TEXT' => '"Este es un archivo de importación de muestra que es un ejemplo de los contenidos que se espera de un archivo que está listo para la importación." "El archivo es uno delimitado por comas .csv, usando comillas como el calificador de campo." "La fila de encabezado es la fila de arriba la mayoría en el archivo que contiene las etiquetas de campo como si fuera a ver en la aplicación." "Estas etiquetas se utilizan para el mapeo de los datos en el archivo de los campos de la aplicación." "Notas: Los nombres de base de datos también podrían ser utilizados en la cabecera. Esto es útil cuando usted está usando phpMyAdmin o cualquier otra herramienta de bases de datos para proporcionar una lista de exportación de datos a importar." "El orden de las columnas no es crítico, el proceso de importación coincide con los datos en los campos apropiados basados ​​en la fila de cabecera". "Para utilizar este archivo como plantilla, haga lo siguiente:" "1. Quitar las filas de la muestra de los datos" "2. Retire el texto de ayuda que usted está leyendo ahora mismo" "3. de entrada de sus propios datos en las filas correspondientes y columnas" " 4. Guarde el archivo en una ubicación conocida de su sistema " " 5. Haga clic en la opción Importar en el menú Acciones en la aplicación y elegir el archivo a subir "',
    //define labels to be used for overriding local values during import/export

    'LBL_NOTIFICATIONS_NONE' => 'No hay notificaciones actuales',
    'LBL_ALT_SORT_DESC' => 'Ordenado descendente',
    'LBL_ALT_SORT_ASC' => 'Ordenado ascendente',
    'LBL_ALT_SORT' => 'Ordenar',
    'LBL_ALT_SHOW_OPTIONS' => 'Mostrar opciones',
    'LBL_ALT_HIDE_OPTIONS' => 'Ocultar opciones',
    'LBL_ALT_MOVE_COLUMN_LEFT' => 'Mover selección a la lista de la izquierda',
    'LBL_ALT_MOVE_COLUMN_RIGHT' => 'Mover selección a la lista de la derecha',
    'LBL_ALT_MOVE_COLUMN_UP' => 'Mover selección hacia arriba en el orden de la lista',
    'LBL_ALT_MOVE_COLUMN_DOWN' => 'Mover selección hacia abajo en el orden de la lista',
    'LBL_ALT_INFO' => 'Información',
    'MSG_DUPLICATE' => 'El registro {0} que está a punto de crear puede ser un duplicado de un registro {0} que ya existe. {1} registros que contienen nombres similares se enumeran a continuación.<br />Haga clic en Crear {1} para continuar la creación de este nuevo {0}, o seleccionar un archivo {0} se enumeran a continuación.',
    'MSG_SHOW_DUPLICATES' => 'El registro {0} que está a punto de crear puede ser un duplicado de un registro {0} que ya existe. {1} registros que contienen nombres similares se enumeran a continuación. Haga clic en Guardar para continuar con la creación de este nuevo {0}, o haga clic en Cancelar para volver al módulo sin necesidad de crear {0}.',
    'LBL_EMAIL_TITLE' => 'Email',
    'LBL_EMAIL_OPT_TITLE' => 'Email rehusado',
    'LBL_EMAIL_INV_TITLE' => 'email invalido',
    'LBL_EMAIL_PRIM_TITLE' => 'Designar como dirección de correo electrónico principal',
    'LBL_SELECT_ALL_TITLE' => 'Seleccionar todo',
    'LBL_SELECT_THIS_ROW_TITLE' => 'Seleccionar esta fila',

    //for upload errors
    'UPLOAD_ERROR_TEXT' => 'ERROR: Hubo un error durante la subida. Código de error: {0} - {1}',
    'UPLOAD_ERROR_TEXT_SIZEINFO' => 'ERROR: Hubo un error durante la subida. Código de error: {0} - {1}. El upload_maxsize es {2}',
    'UPLOAD_ERROR_HOME_TEXT' => 'ERROR: Se ha producido un error durante la subida, por favor póngase en contacto con un administrador para obtener ayuda.',
    'UPLOAD_MAXIMUM_EXCEEDED' => 'El tamaño de la ({0} bytes) Superó el máximo permitido: {1} bytes',
    'UPLOAD_REQUEST_ERROR' => 'Ocurrió un error. Por favor actualice su página y vuelva a intentarlo.',

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
    'LBL_SAVE_BUTTON_LABEL' => 'Guardar',
    'LBL_SAVE_BUTTON_TITLE' => 'Guardar',
    'LBL_CANCEL_BUTTON_KEY' => 'X',
    'LBL_CANCEL_BUTTON_LABEL' => 'Cancelar',
    'LBL_CANCEL_BUTTON_TITLE' => 'Cancelar',
    'LBL_FIRST_INPUT_EDIT_VIEW_KEY' => '7',
    'LBL_ADV_SEARCH_LNK_KEY' => '8',
    'LBL_FIRST_INPUT_SEARCH_KEY' => '9',

    'ERR_CONNECTOR_NOT_ARRAY' => 'conector serie en {0} ha definido incorrectamente o está vacío y no se podían usar.',
    'ERR_SUHOSIN' => 'El flujo de subida está bloqueado por Suhosin, añade un "upload" en suhosin.executor.include.whitelist (Ver suitecrm.log para más información)',
    'ERR_BAD_RESPONSE_FROM_SERVER' => 'Respuesta incorrecta del servidor',
    'LBL_ACCOUNT_PRODUCT_QUOTE_LINK' => 'Presupuesto',
    'LBL_ACCOUNT_PRODUCT_SALE_PRICE' => 'Precio',
    'LBL_EMAIL_CHECK_INTERVAL_DOM' => array(
        '-1' => 'Manualmente',
        '5' => 'Cada 5 minutos',
        '15' => 'Cada 15 minutos',
        '30' => 'Cada 30 minutos',
        '60' => 'Cada hora',
    ),

    'ERR_A_REMINDER_IS_EMPTY_OR_INCORRECT' => 'Un recordatorio es vacío o incorrecto.',
    'ERR_REMINDER_IS_NOT_SET_POPUP_OR_EMAIL' => 'Recordatorio no está ajustado para un popup o correo electrónico.',
    'ERR_NO_INVITEES_FOR_REMINDER' => 'No hay invitados para recordatorio.',
    'LBL_DELETE_REMINDER_CONFIRM' => 'Recordatorio no incluye invitados, ¿desea eliminar el recordatorio?',
    'LBL_DELETE_REMINDER' => 'Eliminar Recordatorio',
    'LBL_OK' => 'Ok',

    'LBL_COLUMNS_FILTER_HEADER_TITLE' => 'Elegir columnas',
    'LBL_COLUMN_CHOOSER' => 'Selector de columna',
    'LBL_SAVE_CHANGES_BUTTON_TITLE' => 'Guardar Cambios',
    'LBL_DISPLAYED' => 'Mostrado',
    'LBL_HIDDEN' => 'Oculto',
    'ERR_EMPTY_COLUMNS_LIST' => 'Al menos uno de los elementos es necesario',

    'LBL_FILTER_HEADER_TITLE' => 'Filtro',

    'LBL_CATEGORY' => 'Categoría',
    'LBL_LIST_CATEGORY' => 'Categoría',
    'ERR_FACTOR_TPL_INVALID' => 'El mensaje de verificación de factor no es válido, ponte en contacto con tu administrador.',
    'LBL_SUBTHEMES' => 'Estilo',
    'LBL_SUBTHEME_OPTIONS_DAWN' => 'Amanecer',
    'LBL_SUBTHEME_OPTIONS_DAY' => 'Día',
    'LBL_SUBTHEME_OPTIONS_DUSK' => 'Crepúsculo',
    'LBL_SUBTHEME_OPTIONS_NIGHT' => 'Noche',
    'LBL_SUBTHEME_OPTIONS_NOON' => 'Mediodía', 

    'LBL_CONFIRM_DISREGARD_DRAFT_TITLE' => 'Descartar el borrador',
    'LBL_CONFIRM_DISREGARD_DRAFT_BODY' => 'Esta operación eliminará este mensaje, ¿desea continuar?',
    'LBL_CONFIRM_DISREGARD_EMAIL_TITLE' => 'Salir del cuadro de diálogo componer',
    'LBL_CONFIRM_DISREGARD_EMAIL_BODY' => 'Al salir del diálogo de redacción se perderá toda la información ingresada, ¿desea continuar?',
    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE' => 'Aplicar una plantilla de mensaje',
    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_BODY' => 'Esta operación borrará el campo cuerpo del mensaje, ¿quiere continuar?',

    'LBL_CONFIRM_OPT_IN_TITLE' => 'Adhesión confirmada',
    'LBL_OPT_IN_TITLE' => 'Autorizar',
    'LBL_CONFIRM_OPT_IN_DATE' => 'Fecha de confirmación de adhesión',
    'LBL_CONFIRM_OPT_IN_SENT_DATE' => 'Fecha de envío de confirmación de autorización',
    'LBL_CONFIRM_OPT_IN_FAIL_DATE' => 'Fecha de falla en la confirmación de autorización',
    'LBL_CONFIRM_OPT_IN_TOKEN' => 'Token de Confirmación de Suscripción',
    'ERR_OPT_IN_TPL_NOT_SET' => 'El modelo de email para autorización no está configurado. Por favor, configúrelo en las configuraciones de e-mail.',
    'ERR_OPT_IN_RELATION_INCORRECT' => 'Para autorizar es necesario que el e-mail esté relacionado com una Cuenta/Contacto/Cliente Potencial/Público Objetivo',

    'LBL_SECURITYGROUP_NONINHERITABLE' => 'Grupo no heredable',
    'LBL_PRIMARY_GROUP' => "Grupo Principal",

    // footer
    'LBL_SUITE_TOP' => 'Volver al parte superior',
    'LBL_SUITE_SUPERCHARGED' => 'Sobrealimentado por SuiteCRM',
    'LBL_SUITE_POWERED_BY' => 'Desarrollado por SugarCRM',
    'LBL_SUITE_DESC1' => 'SuiteCRM ha sido escrito y ensamblado por <a href="https://salesagility.com"> SalesAgility</a>.  El programa se suministra TAL CUAL ES, sin garantía.  Bajo licencia AGPLv3.',
    'LBL_SUITE_DESC2' => 'Este programa es software libre; puede redistribuirlo y/o modificarlo bajo los términos de la GNU Affero General Public License versión 3 publicada por la Free Software Foundation, incluyendo el permiso adicional en la cabecera del código fuente.',
    'LBL_SUITE_DESC3' => 'SuiteCRM es una marca registrada de SalesAgility Ltd. Todos los nombres de otras empresas y productos pueden ser marcas registradas de las respectivas empresas con las que se asocian.',
    'LBL_GENERATE_PASSWORD_BUTTON_TITLE' => 'Restablecer Contraseña',
    'LBL_SEND_CONFIRM_OPT_IN_EMAIL' => 'Enviar e-mail de confirmación de autorización',
    'LBL_CONFIRM_OPT_IN_ONLY_FOR_PERSON' => 'Envio de e-mail de confirmación de autorización sólo para Cuentas/Contactos/Clientes Potenciales/Público Objetivo',
    'LBL_CONFIRM_OPT_IN_IS_DISABLED' => 'El envío de email de confirmación de la autorización está desactivado. Actívelo en Configuraciones de Email o contacte su administrador.',
    'LBL_CONTACT_HAS_NO_PRIMARY_EMAIL' => 'El envío de e-mail de confirmación de autorización no es posible porque el contacto no posee una dirección primario de e-mail registrada',
    'LBL_CONFIRM_EMAIL_SENDING_FAILED' => 'Envío de e-mail de confirmación fallado',
    'LBL_CONFIRM_EMAIL_SENT' => 'E-mail de confirmación de autorización enviado con éxito',
);

$app_list_strings['moduleList']['Library'] = 'Biblioteca';
$app_list_strings['moduleList']['EmailAddresses'] = 'Dirección de Email';
$app_list_strings['project_priority_default'] = 'Media';
$app_list_strings['project_priority_options'] = array(
    'High' => 'Alta',
    'Medium' => 'Media',
    'Low' => 'Baja',
);

//GDPR lawful basis options
$app_list_strings['lawful_basis_dom'] = array(
    '' => '',
    'consent' => 'Consentimiento',
    'contract' => 'Contrato',
    'legal_obligation' => 'Obligación legal',
    'protection_of_interest' => 'Protección del interés',
    'public_interest' => 'Interés público',
    'legitimate_interest' => 'Interés legítimo',
    'withdrawn' => 'Retirado',
);
//End GDPR lawful basis options

//GDPR lawful basis source options
$app_list_strings['lawful_basis_source_dom'] = array(
    '' => '',
    'website' => 'Sitio Web',
    'phone' => 'Teléfono',
    'given_to_user' => 'Dado al Usuario',
    'email' => 'Email',
    'third_party' => 'Tercero',
);
//End GDPR lawful basis source options

$app_list_strings['moduleList']['KBDocuments'] = 'Base de Conocimiento';

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
    'ARGENTINA' => 'Argentina',
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
    'BRITISH ANTARCTICA TERRITORY' => 'Territorio británico en la Antártida',
    'BRITISH INDIAN OCEAN TERRITORY' => 'Territorio británico en océano Índico',
    'BRITISH VIRGIN ISLANDS' => 'Islas Vírgenes Británicas',
    'BRITISH WEST INDIES' => 'Indias Occidentales Británicas',
    'BRUNEI' => 'Brunei',
    'BULGARIA' => 'Bulgaria',
    'BURKINA FASO' => 'Burkina Faso',
    'BURUNDI' => 'Burundi',
    'CAMBODIA' => 'Camboya',
    'CAMEROON' => 'Camerún',
    'CANADA' => 'Canadá',
    'CANAL ZONE' => 'Zona del Canal',
    'CANARY ISLAND' => 'ISLAS CANARIAS',
    'CAPE VERDI ISLANDS' => 'Cabo Verde',
    'CAYMAN ISLANDS' => 'ISLAS CAIMAN',
    'CHAD' => 'Chad',
    'CHANNEL ISLAND UK' => 'Islas del Canal Británicas',
    'CHILE' => 'Chile',
    'CHINA' => 'China',
    'CHRISTMAS ISLAND' => 'Isla de Navidad',
    'COCOS (KEELING) ISLAND' => 'COCOS (KEELING) ISLAND',
    'COLOMBIA' => 'COLOMBIA',
    'COMORO ISLANDS' => 'COMORO ISLANDS',
    'CONGO' => 'CONGO',
    'CONGO KINSHASA' => 'CONGO KINSHASA',
    'COOK ISLANDS' => 'ISLAS COOK',
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
    'EGYPT' => 'EGIPTO',
    'EL SALVADOR' => 'El Salvador',
    'EQUATORIAL GUINEA' => 'GUINEA ECUATORIAL',
    'ESTONIA' => 'Estonia',
    'ETHIOPIA' => 'ETIOPÍA',
    'FAEROE ISLANDS' => 'ISLAS FEROE',
    'FALKLAND ISLANDS' => 'LAS ISLAS MALVINAS',
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
    'KOREA, SOUTH' => 'COREA DEL SUR',
    'KUWAIT' => 'KUWAIT',
    'KYRGYZSTAN' => 'KIRGUISTÁN',
    'LAOS' => 'LAOS',
    'LATVIA' => 'LETONIA',
    'LEBANON' => 'LÍBANO',
    'LEEWARD ISLANDS' => 'ISLAS DE SOTAVENTO',
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
    'NEW CALADONIA' => 'NUEVA CALADONIA',
    'NEW HEBRIDES' => 'NEW HEBRIDES',
    'NEW ZEALAND' => 'NUEVA ZELANDA',
    'NICARAGUA' => 'NICARAGUA',
    'NIGER' => 'NIGER',
    'NIGERIA' => 'NIGERIA',
    'NORFOLK ISLAND' => 'ISLA NORFOLK',
    'NORWAY' => 'NORUEGA',
    'OMAN' => 'OMAN',
    'OTHER' => 'OTHER',
    'PACIFIC ISLAND' => 'ISLA DEL PACIFICO',
    'PAKISTAN' => 'PAKISTAN',
    'PANAMA' => 'PANAMA',
    'PAPUA NEW GUINEA' => 'PAPUA NUEVA GUINEA',
    'PARAGUAY' => 'PARAGUAY',
    'PERU' => 'PERU',
    'PHILIPPINES' => 'FILIPINAS',
    'POLAND' => 'POLONIA',
    'PORTUGAL' => 'PORTUGAL',
    'PORTUGUESE TIMOR' => 'TIMOR ORIENTAL',
    'PUERTO RICO' => 'PUERTO RICO',
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
    'US PACIFIC ISLAND' => 'EE.UU. ISLA DEL PACIFICO',
    'US VIRGIN ISLANDS' => 'ISLAS VÍRGENES DE EE.UU.',
    'USA' => 'EE.UU.',
    'UZBEKISTAN' => 'UZBEKISTÁN',
    'VANUATU' => 'VANUATU',
    'VATICAN CITY' => 'CIUDAD DEL VATICANO',
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
    'BIG-5' => 'BIG-5 (Taiwan y Hong Kong)',
    /*'CP866'     => 'CP866', // ms-dos Cyrillic */
    /*'CP949'     => 'CP949 (Microsoft Korean)', */
    'CP1251' => 'CP1251 (Cirílico de MS)',
    'CP1252' => 'CP1252 (Europa Occidental y EEUU de Ms)',
    'EUC-CN' => 'EUC-CN (Chino Simplificado GB2312)',
    'EUC-JP' => 'EUC-JP (Japonés Unix)',
    'EUC-KR' => 'EUC-KR (Coreano)',
    'EUC-TW' => 'EUC-TW (Taiwanés)',
    'ISO-2022-JP' => 'ISO-2022-JP (Japonés)',
    'ISO-2022-KR' => 'ISO-2022-KR (Coreano)',
    'ISO-8859-1' => 'ISO-8859-1 (Europa Occidental y EEUU)',
    'ISO-8859-2' => 'ISO-8859-2 (Centroeuropa y Europa del Este)',
    'ISO-8859-3' => 'ISO-8859-3 (Latín 3)',
    'ISO-8859-4' => 'ISO-8859-4 (Latín 4)',
    'ISO-8859-5' => 'ISO-8859-5 (Cirílico)',
    'ISO-8859-6' => 'ISO-8859-6 (Árabe)',
    'ISO-8859-7' => 'ISO-8859-7 (Griego)',
    'ISO-8859-8' => 'ISO-8859-8 (Hebreo)',
    'ISO-8859-9' => 'ISO-8859-9 (Latín 5)',
    'ISO-8859-10' => 'ISO-8859-10 (Latín 6)',
    'ISO-8859-13' => 'ISO-8859-13 (Latín 7)',
    'ISO-8859-14' => 'ISO-8859-14 (Latín 8)',
    'ISO-8859-15' => 'ISO-8859-15 (Latín 9)',
    'KOI8-R' => 'KOI8-R (Cirílico Ruso)',
    'KOI8-U' => 'KOI8-U (Cirílico Ucraniano)',
    'SJIS' => 'SJIS (Japonés de MS)',
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
    'America/Argentina/Buenos_Aires' => 'America/Argentina/Buenos_Aires',
    'America/Argentina/Cordoba' => 'America/Argentina/Cordoba',
    'America/Argentina/Tucuman' => 'America/Argentina/Tucuman',
    'America/Argentina/La_Rioja' => 'America/Argentina/La_Rioja',
    'America/Argentina/San_Juan' => 'America/Argentina/San_Juan',
    'America/Argentina/Jujuy' => 'America/Argentina/Jujuy',
    'America/Argentina/Catamarca' => 'America/Argentina/Catamarca',
    'America/Argentina/Mendoza' => 'America/Argentina/Mendoza',
    'America/Argentina/Rio_Gallegos' => 'America/Argentina/Rio_Gallegos',
    'America/Argentina/Ushuaia' => 'America/Argentina/Ushuaia',
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
    1 => 'Solicitud',
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
$app_list_strings['moduleList']['AOK_KnowledgeBase'] = 'Base de Conocimiento'; // Shows in the ALL menu entries
$app_list_strings['moduleList']['AOK_Knowledge_Base_Categories'] = 'KB - Categorías'; // Shows in the ALL menu entries
$app_list_strings['aok_status_list']['Draft'] = 'Borrador';
$app_list_strings['aok_status_list']['Expired'] = 'Expirado';
$app_list_strings['aok_status_list']['In_Review'] = 'En revisión';
//$app_list_strings['aok_status_list']['Published'] = 'Published';
$app_list_strings['aok_status_list']['published_private'] = 'Particular';
$app_list_strings['aok_status_list']['published_public'] = 'Público';

$app_list_strings['moduleList']['FP_events'] = 'Eventos';
$app_list_strings['moduleList']['FP_Event_Locations'] = 'Ubicaciones';

//events
$app_list_strings['fp_event_invite_status_dom']['Invited'] = 'Invitados';
$app_list_strings['fp_event_invite_status_dom']['Not Invited'] = 'No Invitados';
$app_list_strings['fp_event_invite_status_dom']['Attended'] = 'Asistentes';
$app_list_strings['fp_event_invite_status_dom']['Not Attended'] = 'No Asistentes';
$app_list_strings['fp_event_status_dom']['Accepted'] = 'Aceptado';
$app_list_strings['fp_event_status_dom']['Declined'] = 'Rechazado';
$app_list_strings['fp_event_status_dom']['No Response'] = 'Sin respuesta';

$app_strings['LBL_STATUS_EVENT'] = 'Estado de Invitación';
$app_strings['LBL_ACCEPT_STATUS'] = 'Aceptar estato';
$app_strings['LBL_LISTVIEW_OPTION_CURRENT'] = 'Seleccionar Página Actual';
$app_strings['LBL_LISTVIEW_OPTION_ENTIRE'] = 'Seleccionar Todo';
$app_strings['LBL_LISTVIEW_NONE'] = 'Quitar Selección';

//aod
$app_list_strings['moduleList']['AOD_IndexEvent'] = 'Evento índice';
$app_list_strings['moduleList']['AOD_Index'] = 'Índice';

$app_list_strings['moduleList']['AOP_Case_Events'] = 'Eventos de Casos';
$app_list_strings['moduleList']['AOP_Case_Updates'] = 'Actualizaciones de Casos';
$app_strings['LBL_AOP_EMAIL_REPLY_DELIMITER'] = '========== Por favor responda por encima de esta linea ==========';


//aop PR 5426
$app_list_strings['moduleList']['JAccount'] = 'Cuenta Joomla';

$app_list_strings['case_state_default_key'] = 'Open';
$app_list_strings['case_state_dom'] =
    array(
        'Open' => 'Abierto',
        'Closed' => 'Cerrado',
    );
$app_list_strings['case_status_default_key'] = 'Open_New';
$app_list_strings['case_status_dom'] =
    array(
        'Open_New' => 'Nuevo',
        'Open_Assigned' => 'Asignado',
        'Closed_Closed' => 'Cerrado',
        'Open_Pending Input' => 'Pendiente de Información',
        'Closed_Rejected' => 'Rechazado',
        'Closed_Duplicate' => 'Duplicado',
    );
$app_list_strings['contact_portal_user_type_dom'] =
    array(
        'Single' => 'Usuario individual',
        'Account' => 'Cuenta de usuario',
    );
$app_list_strings['dom_email_distribution_for_auto_create'] = array(
    'AOPDefault' => 'Usa el AOP predeterminado',
    'singleUser' => 'Usuario individual',
    'roundRobin' => 'Turno Rotatorio',
    'leastBusy' => 'Menos-Ocupado',
    'random' => 'Aleatorio',
);

//aor
$app_list_strings['moduleList']['AOR_Reports'] = 'Informes';
$app_list_strings['moduleList']['AOR_Conditions'] = 'Condiciones de Reportes';
$app_list_strings['moduleList']['AOR_Charts'] = 'Gráficos de Informe';
$app_list_strings['moduleList']['AOR_Fields'] = 'Campos de Reportes';
$app_list_strings['moduleList']['AOR_Scheduled_Reports'] = 'Informes programados';
$app_list_strings['aor_operator_list']['Equal_To'] = 'Igual a';
$app_list_strings['aor_operator_list']['Not_Equal_To'] = 'No igual a';
$app_list_strings['aor_operator_list']['Greater_Than'] = 'Mayor que';
$app_list_strings['aor_operator_list']['Less_Than'] = 'Menor que';
$app_list_strings['aor_operator_list']['Greater_Than_or_Equal_To'] = 'Mayor o igual a';
$app_list_strings['aor_operator_list']['Less_Than_or_Equal_To'] = 'Menor o igual a';
$app_list_strings['aor_operator_list']['Contains'] = 'Contiene';
$app_list_strings['aor_operator_list']['Not_Contains'] = 'No contiene';
$app_list_strings['aor_operator_list']['Starts_With'] = 'Comienza con';
$app_list_strings['aor_operator_list']['Ends_With'] = 'Finaliza con';
$app_list_strings['aor_format_options'][''] = '';
$app_list_strings['aor_format_options']['Y-m-d'] = 'A-m-d';
$app_list_strings['aor_format_options']['m-d-Y'] = 'm-d-Y';
$app_list_strings['aor_format_options']['d-m-Y'] = 'd-m-Y';
$app_list_strings['aor_format_options']['Y/m/d'] = 'Y/m/d';
$app_list_strings['aor_format_options']['m/d/Y'] = 'm/d/Y';
$app_list_strings['aor_format_options']['d/m/Y'] = 'd/m/A';
$app_list_strings['aor_format_options']['Y.m.d'] = 'Y.m.d';
$app_list_strings['aor_format_options']['m.d.Y'] = 'm.d.Y';
$app_list_strings['aor_format_options']['d.m.Y'] = 'd.m.Y';
$app_list_strings['aor_format_options']['Ymd'] = 'Amd';
$app_list_strings['aor_format_options']['Y-m'] = 'A-d';
$app_list_strings['aor_format_options']['Y'] = 'A';
$app_list_strings['aor_condition_operator_list']['And'] = 'y';
$app_list_strings['aor_condition_operator_list']['OR'] = 'O';
$app_list_strings['aor_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aor_condition_type_list']['Field'] = 'Campo';
$app_list_strings['aor_condition_type_list']['Date'] = 'Fecha';
$app_list_strings['aor_condition_type_list']['Multi'] = 'Multiple';
$app_list_strings['aor_condition_type_list']['Period'] = 'Periodo';
$app_list_strings['aor_condition_type_list']['CurrentUserID'] = 'Usuario actual';
$app_list_strings['aor_date_type_list'][''] = '';
$app_list_strings['aor_date_type_list']['minute'] = 'Minutos';
$app_list_strings['aor_date_type_list']['hour'] = 'Horas';
$app_list_strings['aor_date_type_list']['day'] = 'Días';
$app_list_strings['aor_date_type_list']['week'] = 'Semanas';
$app_list_strings['aor_date_type_list']['month'] = 'Meses';
$app_list_strings['aor_date_type_list']['business_hours'] = 'Horas Laborales';
$app_list_strings['aor_date_options']['now'] = 'Ahora';
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
$app_list_strings['aor_chart_types']['line'] = 'Gráfico de líneas';
$app_list_strings['aor_chart_types']['pie'] = 'Gráfico de sectores';
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
$app_list_strings['aor_assign_options']['all'] = 'Todos los usuarios';
$app_list_strings['aor_assign_options']['role'] = 'Todos los usuarios en Role';
$app_list_strings['aor_assign_options']['security_group'] = 'Todos los usuarios en el Grupo de Seguridad';
$app_list_strings['date_time_period_list']['today'] = 'Hoy';
$app_list_strings['date_time_period_list']['yesterday'] = 'Ayer';
$app_list_strings['date_time_period_list']['this_week'] = 'Esta semana';
$app_list_strings['date_time_period_list']['last_week'] = 'Última Semana';
$app_list_strings['date_time_period_list']['last_month'] = 'Último mes';
$app_list_strings['date_time_period_list']['this_month'] = 'Este mes';
$app_list_strings['date_time_period_list']['this_quarter'] = 'Este Trimestre';
$app_list_strings['date_time_period_list']['last_quarter'] = 'Úlimo Trimestre';
$app_list_strings['date_time_period_list']['this_year'] = 'Este año';
$app_list_strings['date_time_period_list']['last_year'] = 'El año pasado';
$app_strings['LBL_CRON_ON_THE_MONTHDAY'] = 'En el';
$app_strings['LBL_CRON_ON_THE_WEEKDAY'] = 'el';
$app_strings['LBL_CRON_AT'] = 'a la(s)';
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
$app_list_strings['moduleList']['AOS_Product_Categories'] = 'Categorías de Productos';
$app_list_strings['moduleList']['AOS_Products'] = 'Productos';
$app_list_strings['moduleList']['AOS_Products_Quotes'] = 'Líneas de Presupuesto';
$app_list_strings['moduleList']['AOS_Line_Item_Groups'] = 'Grupos';
$app_list_strings['moduleList']['AOS_Quotes'] = 'Presupuestos';
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
$app_list_strings['aos_quotes_type_dom']['Other'] = 'Otro';
$app_list_strings['template_ddown_c_list'][''] = '';
$app_list_strings['quote_stage_dom']['Draft'] = 'Borrador';
$app_list_strings['quote_stage_dom']['Negotiation'] = 'Negociación';
$app_list_strings['quote_stage_dom']['Delivered'] = 'Enviado';
$app_list_strings['quote_stage_dom']['On Hold'] = 'En Espera';
$app_list_strings['quote_stage_dom']['Confirmed'] = 'Confirmado';
$app_list_strings['quote_stage_dom']['Closed Accepted'] = 'Cerrado Aceptado';
$app_list_strings['quote_stage_dom']['Closed Lost'] = 'Perdido';
$app_list_strings['quote_stage_dom']['Closed Dead'] = 'Cerrado Muerto';
$app_list_strings['quote_term_dom']['Net 15'] = 'Red 15';
$app_list_strings['quote_term_dom']['Net 30'] = 'Red 30';
$app_list_strings['quote_term_dom'][''] = '';
$app_list_strings['approval_status_dom']['Approved'] = 'Aprobado';
$app_list_strings['approval_status_dom']['Not Approved'] = 'No Aprobado';
$app_list_strings['approval_status_dom'][''] = '';
$app_list_strings['vat_list']['0.0'] = '0%';
$app_list_strings['vat_list']['5.0'] = '5%';
$app_list_strings['vat_list']['7.5'] = '7.5%';
$app_list_strings['vat_list']['17.5'] = '17.5%';
$app_list_strings['vat_list']['20.0'] = '20%';
$app_list_strings['discount_list']['Percentage'] = 'Porcentaje';
$app_list_strings['discount_list']['Amount'] = 'Cantidad';
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
$app_list_strings['aos_invoices_type_dom']['Other'] = 'Otro';
$app_list_strings['invoice_status_dom']['Paid'] = 'Pagado';
$app_list_strings['invoice_status_dom']['Unpaid'] = 'No Pagado';
$app_list_strings['invoice_status_dom']['Cancelled'] = 'Cancelado';
$app_list_strings['invoice_status_dom'][''] = '';
$app_list_strings['quote_invoice_status_dom']['Not Invoiced'] = 'No Facturado';
$app_list_strings['quote_invoice_status_dom']['Invoiced'] = 'Facturado';
$app_list_strings['product_code_dom']['XXXX'] = 'XXXX';
$app_list_strings['product_code_dom']['YYYY'] = 'YYYY';
$app_list_strings['product_category_dom']['Laptops'] = 'Laptops';
$app_list_strings['product_category_dom']['Desktops'] = 'Desktops';
$app_list_strings['product_category_dom'][''] = '';
$app_list_strings['product_type_dom']['Good'] = 'Bien';
$app_list_strings['product_type_dom']['Service'] = 'Servicio';
$app_list_strings['product_quote_parent_type_dom']['AOS_Quotes'] = 'Presupuestos';
$app_list_strings['product_quote_parent_type_dom']['AOS_Invoices'] = 'Facturas';
$app_list_strings['product_quote_parent_type_dom']['AOS_Contracts'] = 'Contratos';
// STIC-Custom 20220124 MHP - Delete the values of the pdf_template_type_dom  
// STIC#564            
// $app_list_strings['pdf_template_type_dom']['AOS_Quotes'] = 'Presupuestos';
// $app_list_strings['pdf_template_type_dom']['AOS_Invoices'] = 'Facturas';
// $app_list_strings['pdf_template_type_dom']['AOS_Contracts'] = 'Contratos';
// $app_list_strings['pdf_template_type_dom']['Accounts'] = 'Cuentas';
// $app_list_strings['pdf_template_type_dom']['Contacts'] = 'Contactos';
// $app_list_strings['pdf_template_type_dom']['Leads'] = 'Clientes Potenciales';
// END STIC-Custom
$app_list_strings['pdf_template_sample_dom'][''] = '';
$app_list_strings['contract_status_list']['Not Started'] = 'No Iniciado';
$app_list_strings['contract_status_list']['In Progress'] = 'En Progreso';
$app_list_strings['contract_status_list']['Signed'] = 'Firmado';
$app_list_strings['contract_type_list']['Type'] = 'Tipo';
$app_strings['LBL_PRINT_AS_PDF'] = 'Generar documento PDF';
$app_strings['LBL_SELECT_TEMPLATE'] = 'Por favor seleccione un formato';
$app_strings['LBL_NO_TEMPLATE'] = 'ERROR\nNo se encontraron formatos.\nPor favor vaya al módulo de Formatos PDF y cree uno';

//aow PR 5775
$app_list_strings['moduleList']['AOW_WorkFlow'] = 'Flujo de trabajo';
$app_list_strings['moduleList']['AOW_Conditions'] = 'Condiciones de Flujo de Trabajo';
$app_list_strings['moduleList']['AOW_Processed'] = 'Auditoría de Procesos';
$app_list_strings['moduleList']['AOW_Actions'] = 'Acciones de Flujo de Trabajo';
$app_list_strings['aow_status_list']['Active'] = 'Activo';
$app_list_strings['aow_status_list']['Inactive'] = 'Inactivo';
$app_list_strings['aow_operator_list']['Equal_To'] = 'Igual a';
$app_list_strings['aow_operator_list']['Not_Equal_To'] = 'No igual a';
$app_list_strings['aow_operator_list']['Greater_Than'] = 'Mayor que';
$app_list_strings['aow_operator_list']['Less_Than'] = 'Menor que';
$app_list_strings['aow_operator_list']['Greater_Than_or_Equal_To'] = 'Mayor o igual que';
$app_list_strings['aow_operator_list']['Less_Than_or_Equal_To'] = 'Menor o igual que';
$app_list_strings['aow_operator_list']['Contains'] = 'Contiene';
$app_list_strings['aow_operator_list']['Not_Contains'] = 'No contiene';
$app_list_strings['aow_operator_list']['Starts_With'] = 'Comienza con';
$app_list_strings['aow_operator_list']['Ends_With'] = 'Finaliza con';
$app_list_strings['aow_operator_list']['is_null'] = 'Es nulo';
$app_list_strings['aow_operator_list']['is_not_null'] = 'No es nulo';
$app_list_strings['aow_operator_list']['Anniversary'] = 'Aniversario';
$app_list_strings['aow_process_status_list']['Complete'] = 'Completa';
$app_list_strings['aow_process_status_list']['Running'] = 'Ejecutando';
$app_list_strings['aow_process_status_list']['Pending'] = 'Pendiente';
$app_list_strings['aow_process_status_list']['Failed'] = 'Fallado';
$app_list_strings['aow_condition_operator_list']['And'] = 'Y';
$app_list_strings['aow_condition_operator_list']['OR'] = 'O';
$app_list_strings['aow_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aow_condition_type_list']['Field'] = 'Campo';
$app_list_strings['aow_condition_type_list']['Any_Change'] = 'Cualquier cambio';
$app_list_strings['aow_condition_type_list']['SecurityGroup'] = 'En SecurityGroup';
$app_list_strings['aow_condition_type_list']['Date'] = 'Fecha';
$app_list_strings['aow_condition_type_list']['Multi'] = 'Uno de';
$app_list_strings['aow_action_type_list']['Value'] = 'Valor';
$app_list_strings['aow_action_type_list']['Field'] = 'Campo';
$app_list_strings['aow_action_type_list']['Date'] = 'Fecha';
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
$app_list_strings['aow_date_options']['now'] = 'Ahora';
$app_list_strings['aow_date_options']['today'] = 'Hoy';
$app_list_strings['aow_date_options']['field'] = 'Este Campo';
$app_list_strings['aow_date_operator']['now'] = '';
$app_list_strings['aow_date_operator']['plus'] = '+';
$app_list_strings['aow_date_operator']['minus'] = '-';
$app_list_strings['aow_assign_options']['all'] = 'Todos los usuarios';
$app_list_strings['aow_assign_options']['role'] = 'Todos los usuarios en el rol';
$app_list_strings['aow_assign_options']['security_group'] = 'Todos los usuarios en el Grupo de Seguridad';
$app_list_strings['aow_email_type_list']['Email Address'] = 'Dirección de correo';
$app_list_strings['aow_email_type_list']['Record Email'] = 'Dirección de correo del registro';
$app_list_strings['aow_email_type_list']['Related Field'] = 'Campo relacionado';
$app_list_strings['aow_email_type_list']['Specify User'] = 'Usuario';
$app_list_strings['aow_email_type_list']['Users'] = 'Usuarios';
$app_list_strings['aow_email_to_list']['to'] = 'Para';
$app_list_strings['aow_email_to_list']['cc'] = 'CC';
$app_list_strings['aow_email_to_list']['bcc'] = 'CCO';
$app_list_strings['aow_run_on_list']['All_Records'] = 'Todos los registros';
$app_list_strings['aow_run_on_list']['New_Records'] = 'Nuevos registros';
$app_list_strings['aow_run_on_list']['Modified_Records'] = 'Registros modificados';
$app_list_strings['aow_run_when_list']['Always'] = 'Siempre';
$app_list_strings['aow_run_when_list']['On_Save'] = 'Sólo al guardar';
$app_list_strings['aow_run_when_list']['In_Scheduler'] = 'Sólo en el Planificador';

//gant
$app_list_strings['moduleList']['AM_ProjectTemplates'] = 'Proyectos - Plantillas';
$app_list_strings['moduleList']['AM_TaskTemplates'] = 'Plantillas de tareas de proyecto';
$app_list_strings['relationship_type_list']['FS'] = 'Finalizar para iniciar';
$app_list_strings['relationship_type_list']['SS'] = 'Iniciar para iniciar';
$app_list_strings['duration_unit_dom']['Days'] = 'Días';
$app_list_strings['duration_unit_dom']['Hours'] = 'Horas';
$app_strings['LBL_GANTT_BUTTON_LABEL'] = 'Ver Gantt';
$app_strings['LBL_DETAIL_BUTTON_LABEL'] = 'Ver Detalle';
$app_strings['LBL_CREATE_PROJECT'] = 'Crear Proyecto';

//gmaps
$app_strings['LBL_MAP'] = 'Mapa';

$app_strings['LBL_JJWG_MAPS_LNG'] = 'Longitud';
$app_strings['LBL_JJWG_MAPS_LAT'] = 'Latitud';
$app_strings['LBL_JJWG_MAPS_GEOCODE_STATUS'] = 'Estado de Geocodificación';
$app_strings['LBL_JJWG_MAPS_ADDRESS'] = 'Dirección';

$app_list_strings['moduleList']['jjwg_Maps'] = 'Mapas';
$app_list_strings['moduleList']['jjwg_Markers'] = 'Mapas - marcadores';
$app_list_strings['moduleList']['jjwg_Areas'] = 'Mapas - Áreas';
$app_list_strings['moduleList']['jjwg_Address_Cache'] = 'Mapas - Caché de Direcciones';

$app_list_strings['moduleList']['jjwp_Partners'] = 'Socios JJWP';

$app_list_strings['map_unit_type_list']['mi'] = 'Millas';
$app_list_strings['map_unit_type_list']['km'] = 'Kilómetros';

$app_list_strings['map_module_type_list']['Accounts'] = 'Cuentas';
$app_list_strings['map_module_type_list']['Contacts'] = 'Contactos';
$app_list_strings['map_module_type_list']['Cases'] = 'Casos';
$app_list_strings['map_module_type_list']['Leads'] = 'Clientes Potenciales';
$app_list_strings['map_module_type_list']['Meetings'] = 'Reuniones';
$app_list_strings['map_module_type_list']['Opportunities'] = 'Oportunidades';
$app_list_strings['map_module_type_list']['Project'] = 'Proyectos';
$app_list_strings['map_module_type_list']['Prospects'] = 'Público Objetivo';

$app_list_strings['map_relate_type_list']['Accounts'] = 'Cuenta';
$app_list_strings['map_relate_type_list']['Contacts'] = 'Contacto';
$app_list_strings['map_relate_type_list']['Cases'] = 'Caso';
$app_list_strings['map_relate_type_list']['Leads'] = 'Cliente Potencial';
$app_list_strings['map_relate_type_list']['Meetings'] = 'Reunión';
$app_list_strings['map_relate_type_list']['Opportunities'] = 'Oportunidad';
$app_list_strings['map_relate_type_list']['Project'] = 'Proyecto';
$app_list_strings['map_relate_type_list']['Prospects'] = 'Público Objetivo';

$app_list_strings['marker_image_list']['accident'] = 'Accidente';
$app_list_strings['marker_image_list']['administration'] = 'Administración';
$app_list_strings['marker_image_list']['agriculture'] = 'Agricultura';
$app_list_strings['marker_image_list']['aircraft_small'] = 'Aviación pequeña';
$app_list_strings['marker_image_list']['airplane_tourism'] = 'Avion Turismo';
$app_list_strings['marker_image_list']['airport'] = 'Aeropueerto';
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
$app_list_strings['marker_image_list']['beach'] = 'Playa';
$app_list_strings['marker_image_list']['beautiful'] = 'Belleza';
$app_list_strings['marker_image_list']['bicycle_parking'] = 'Estacionamiento de Bicicletas';
$app_list_strings['marker_image_list']['big_city'] = 'Ciudad Grande';
$app_list_strings['marker_image_list']['bridge'] = 'Puente';
$app_list_strings['marker_image_list']['bridge_modern'] = 'Puente Moderno';
$app_list_strings['marker_image_list']['bus'] = 'Bus';
$app_list_strings['marker_image_list']['cable_car'] = 'Cable carril';
$app_list_strings['marker_image_list']['car'] = 'Automóvil';
$app_list_strings['marker_image_list']['car_rental'] = 'Alquiler de Automóviles';
$app_list_strings['marker_image_list']['carrepair'] = 'Reparación de Automóviles';
$app_list_strings['marker_image_list']['castle'] = 'Castillo';
$app_list_strings['marker_image_list']['cathedral'] = 'Catedral';
$app_list_strings['marker_image_list']['chapel'] = 'Capilla';
$app_list_strings['marker_image_list']['church'] = 'Iglesia';
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
$app_list_strings['marker_image_list']['construction'] = 'Construcción';
$app_list_strings['marker_image_list']['convenience'] = 'Conveniencia';
$app_list_strings['marker_image_list']['court'] = 'Juzgado';
$app_list_strings['marker_image_list']['cruise'] = 'Crucero';
$app_list_strings['marker_image_list']['currency_exchange'] = 'Cambio de Moneda';
$app_list_strings['marker_image_list']['customs'] = 'Aduana';
$app_list_strings['marker_image_list']['cycling'] = 'Ciclismo';
$app_list_strings['marker_image_list']['dam'] = 'Represa';
$app_list_strings['marker_image_list']['dentist'] = 'Dentista';
$app_list_strings['marker_image_list']['deptartment_store'] = 'Tienda por Departamentos';
$app_list_strings['marker_image_list']['disability'] = 'Discapacidad';
$app_list_strings['marker_image_list']['disabled_parking'] = 'Estacionamiento p/Discapacitados';
$app_list_strings['marker_image_list']['doctor'] = 'Doctor';
$app_list_strings['marker_image_list']['dog_leash'] = 'Correa p/Perros';
$app_list_strings['marker_image_list']['down'] = 'Abajo';
$app_list_strings['marker_image_list']['down_left'] = 'Abajo Izquierda';
$app_list_strings['marker_image_list']['down_right'] = 'Abajo Derecha';
$app_list_strings['marker_image_list']['down_then_left'] = 'Abajo luego a la izquierda';
$app_list_strings['marker_image_list']['down_then_right'] = 'Abajo luego a la derecha';
$app_list_strings['marker_image_list']['drugs'] = 'Drogas';
$app_list_strings['marker_image_list']['elevator'] = 'Elevador';
$app_list_strings['marker_image_list']['embassy'] = 'Embajada';
$app_list_strings['marker_image_list']['expert'] = 'Experto';
$app_list_strings['marker_image_list']['factory'] = 'Fábrica';
$app_list_strings['marker_image_list']['falling_rocks'] = 'Zona de Derrumbes';
$app_list_strings['marker_image_list']['fast_food'] = 'Comida Rápida';
$app_list_strings['marker_image_list']['festival'] = 'Festival';
$app_list_strings['marker_image_list']['fjord'] = 'Fiordo';
$app_list_strings['marker_image_list']['forest'] = 'Bosque';
$app_list_strings['marker_image_list']['fountain'] = 'Fuente';
$app_list_strings['marker_image_list']['friday'] = 'Viernes';
$app_list_strings['marker_image_list']['garden'] = 'Jardín';
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
$app_list_strings['marker_image_list']['hotel_1_star'] = 'Hotel 1 Estrella';
$app_list_strings['marker_image_list']['hotel_2_stars'] = 'Hotel 2 Estrellas';
$app_list_strings['marker_image_list']['hotel_3_stars'] = 'Hotel 3 Estrellas';
$app_list_strings['marker_image_list']['hotel_4_stars'] = 'Hotel 4 Estrellas';
$app_list_strings['marker_image_list']['hotel_5_stars'] = 'Hotel 5 Estrellas';
$app_list_strings['marker_image_list']['info'] = 'Información';
$app_list_strings['marker_image_list']['justice'] = 'Juzgado';
$app_list_strings['marker_image_list']['lake'] = 'Lago';
$app_list_strings['marker_image_list']['laundromat'] = 'Lavandería';
$app_list_strings['marker_image_list']['left'] = 'Izquierda';
$app_list_strings['marker_image_list']['left_then_down'] = 'Izquierda Luego Abajo';
$app_list_strings['marker_image_list']['left_then_up'] = 'Izquierda Luego Arriba';
$app_list_strings['marker_image_list']['library'] = 'Biblioteca';
$app_list_strings['marker_image_list']['lighthouse'] = 'Iluminación';
$app_list_strings['marker_image_list']['liquor'] = 'Expendio de Bebidas Alcoholicas';
$app_list_strings['marker_image_list']['lock'] = 'Candado';
$app_list_strings['marker_image_list']['main_road'] = 'Camino Principal';
$app_list_strings['marker_image_list']['massage'] = 'Masajes';
$app_list_strings['marker_image_list']['mobile_phone_tower'] = 'Antena de Telefonía Móvil';
$app_list_strings['marker_image_list']['modern_tower'] = 'Torre Moderna';
$app_list_strings['marker_image_list']['monastery'] = 'Monasterio';
$app_list_strings['marker_image_list']['monday'] = 'Lunes';
$app_list_strings['marker_image_list']['monument'] = 'Monumento';
$app_list_strings['marker_image_list']['mosque'] = 'Mezquita';
$app_list_strings['marker_image_list']['motorcycle'] = 'Motocicleta';
$app_list_strings['marker_image_list']['museum'] = 'Museo';
$app_list_strings['marker_image_list']['music_live'] = 'Música en Vivo';
$app_list_strings['marker_image_list']['oil_pump_jack'] = 'Gato de la bomba de aceite';
$app_list_strings['marker_image_list']['pagoda'] = 'Pagoda';
$app_list_strings['marker_image_list']['palace'] = 'Palacio';
$app_list_strings['marker_image_list']['panoramic'] = 'Vista Panorámica';
$app_list_strings['marker_image_list']['park'] = 'Parque';
$app_list_strings['marker_image_list']['park_and_ride'] = 'Parque y Camiata';
$app_list_strings['marker_image_list']['parking'] = 'Estacionamiento';
$app_list_strings['marker_image_list']['photo'] = 'Foto';
$app_list_strings['marker_image_list']['picnic'] = 'Pícnic';
$app_list_strings['marker_image_list']['places_unvisited'] = 'Lugares no Visitados';
$app_list_strings['marker_image_list']['places_visited'] = 'Lugares Visitados';
$app_list_strings['marker_image_list']['playground'] = 'Plaza';
$app_list_strings['marker_image_list']['police'] = 'Policía';
$app_list_strings['marker_image_list']['port'] = 'Puerto';
$app_list_strings['marker_image_list']['postal'] = 'Postal';
$app_list_strings['marker_image_list']['power_line_pole'] = 'Poste de Línea Eléctrica';
$app_list_strings['marker_image_list']['power_plant'] = 'Planta de Energía';
$app_list_strings['marker_image_list']['power_substation'] = 'Subestación de Energía';
$app_list_strings['marker_image_list']['public_art'] = 'Arte Público';
$app_list_strings['marker_image_list']['rain'] = 'Lluvia';
$app_list_strings['marker_image_list']['real_estate'] = 'Inmobiliaria';
$app_list_strings['marker_image_list']['regroup'] = 'Reagrupamiento';
$app_list_strings['marker_image_list']['resort'] = 'Complejo';
$app_list_strings['marker_image_list']['restaurant'] = 'Restaurante';
$app_list_strings['marker_image_list']['restaurant_african'] = 'Restaurant Africana';
$app_list_strings['marker_image_list']['restaurant_barbecue'] = 'Restaurant Barbacoa';
$app_list_strings['marker_image_list']['restaurant_buffet'] = 'Restaurante de Bufé';
$app_list_strings['marker_image_list']['restaurant_chinese'] = 'Restaurant Chino';
$app_list_strings['marker_image_list']['restaurant_fish'] = 'Restaurant Pescado';
$app_list_strings['marker_image_list']['restaurant_fish_chips'] = 'Restaurant Chips de Pescado';
$app_list_strings['marker_image_list']['restaurant_gourmet'] = 'Restaurante Gourmet';
$app_list_strings['marker_image_list']['restaurant_greek'] = 'Restaurant Griego';
$app_list_strings['marker_image_list']['restaurant_indian'] = 'Restaurant Hindú';
$app_list_strings['marker_image_list']['restaurant_italian'] = 'Restaurant Italiano';
$app_list_strings['marker_image_list']['restaurant_japanese'] = 'Restaurant Japonés';
$app_list_strings['marker_image_list']['restaurant_kebab'] = 'Restaurant Brochette';
$app_list_strings['marker_image_list']['restaurant_korean'] = 'Restaurant Coreano';
$app_list_strings['marker_image_list']['restaurant_mediterranean'] = 'Restaurant Mediterráneo';
$app_list_strings['marker_image_list']['restaurant_mexican'] = 'Restaurant Mexicano';
$app_list_strings['marker_image_list']['restaurant_romantic'] = 'Restaurant Romántico';
$app_list_strings['marker_image_list']['restaurant_thai'] = 'Restaurante Thai';
$app_list_strings['marker_image_list']['restaurant_turkish'] = 'Restaurant Turco';
$app_list_strings['marker_image_list']['right'] = 'Derecha';
$app_list_strings['marker_image_list']['right_then_down'] = 'Derecha Luego Abajo';
$app_list_strings['marker_image_list']['right_then_up'] = 'Derecha Luego Arriba';
$app_list_strings['marker_image_list']['saturday'] = 'Sábado';
$app_list_strings['marker_image_list']['school'] = 'Escuela';
$app_list_strings['marker_image_list']['shopping_mall'] = 'Mall';
$app_list_strings['marker_image_list']['shore'] = 'Apuntalamiento';
$app_list_strings['marker_image_list']['sight'] = 'Vista';
$app_list_strings['marker_image_list']['small_city'] = 'Pequeña Ciudad';
$app_list_strings['marker_image_list']['snow'] = 'Nieve';
$app_list_strings['marker_image_list']['spaceport'] = 'Puerto Espacial';
$app_list_strings['marker_image_list']['speed_100'] = 'Velocidad 100';
$app_list_strings['marker_image_list']['speed_110'] = 'Velocidad 110';
$app_list_strings['marker_image_list']['speed_120'] = 'Velocidad 120';
$app_list_strings['marker_image_list']['speed_130'] = 'Velocidad 130';
$app_list_strings['marker_image_list']['speed_20'] = 'Velocidad 20';
$app_list_strings['marker_image_list']['speed_30'] = 'Velocidad 30';
$app_list_strings['marker_image_list']['speed_40'] = 'Velocidad 40';
$app_list_strings['marker_image_list']['speed_50'] = 'Velocidad 50';
$app_list_strings['marker_image_list']['speed_60'] = 'Velocidad 60';
$app_list_strings['marker_image_list']['speed_70'] = 'Velocidad 70';
$app_list_strings['marker_image_list']['speed_80'] = 'Velocidad 80';
$app_list_strings['marker_image_list']['speed_90'] = 'Velocidad 90';
$app_list_strings['marker_image_list']['speed_hump'] = 'Velocidad Hump';
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
$app_list_strings['marker_image_list']['thursday'] = 'Jueves';
$app_list_strings['marker_image_list']['toilets'] = 'Aseos';
$app_list_strings['marker_image_list']['toll_station'] = 'Peaje';
$app_list_strings['marker_image_list']['tower'] = 'Torre';
$app_list_strings['marker_image_list']['traffic_enforcement_camera'] = 'Control de Velocidad';
$app_list_strings['marker_image_list']['train'] = 'Tren';
$app_list_strings['marker_image_list']['tram'] = 'Tranvía';
$app_list_strings['marker_image_list']['truck'] = 'Camión';
$app_list_strings['marker_image_list']['tuesday'] = 'Martes';
$app_list_strings['marker_image_list']['tunnel'] = 'Tunel';
$app_list_strings['marker_image_list']['turn_left'] = 'Giro a la Izquierda';
$app_list_strings['marker_image_list']['turn_right'] = 'Giro a la Derecha';
$app_list_strings['marker_image_list']['university'] = 'Universidad';
$app_list_strings['marker_image_list']['up'] = 'Arriba';
$app_list_strings['marker_image_list']['up_left'] = 'Arriba Izquierda';
$app_list_strings['marker_image_list']['up_right'] = 'Arriba Derecha';
$app_list_strings['marker_image_list']['up_then_left'] = 'Arriba Luego Izquierda';
$app_list_strings['marker_image_list']['up_then_right'] = 'Arriba Luego Derecha';
$app_list_strings['marker_image_list']['vespa'] = 'Vespa';
$app_list_strings['marker_image_list']['video'] = 'Video';
$app_list_strings['marker_image_list']['villa'] = 'Villa';
$app_list_strings['marker_image_list']['water'] = 'Agua';
$app_list_strings['marker_image_list']['waterfall'] = 'Cascada';
$app_list_strings['marker_image_list']['watermill'] = 'Molino de Agua';
$app_list_strings['marker_image_list']['waterpark'] = 'Parque Acuático';
$app_list_strings['marker_image_list']['watertower'] = 'Torre de Agua';
$app_list_strings['marker_image_list']['wednesday'] = 'Miércoles';
$app_list_strings['marker_image_list']['wifi'] = 'WiFi';
$app_list_strings['marker_image_list']['wind_turbine'] = 'Turbina de Viento';
$app_list_strings['marker_image_list']['windmill'] = 'Molino de Viento';
$app_list_strings['marker_image_list']['winery'] = 'Lagar';
$app_list_strings['marker_image_list']['work_office'] = 'Oficina';
$app_list_strings['marker_image_list']['world_heritage_site'] = 'Patrimonio de la Humanidad';
$app_list_strings['marker_image_list']['zoo'] = 'Zoo';

//Reschedule
$app_list_strings['call_reschedule_dom'][''] = '';
$app_list_strings['call_reschedule_dom']['Out of Office'] = 'Fuera de la Oficina';
$app_list_strings['call_reschedule_dom']['In a Meeting'] = 'En una reunion';

$app_strings['LBL_RESCHEDULE_LABEL'] = 'Replanificaciones';
$app_strings['LBL_RESCHEDULE_TITLE'] = 'Por favor ingrese los datos de la Replanificaci&oacute;n';
$app_strings['LBL_RESCHEDULE_DATE'] = 'Fecha';
$app_strings['LBL_RESCHEDULE_REASON'] = 'Raz&oacute;n:';
$app_strings['LBL_RESCHEDULE_ERROR1'] = 'Por favor seleccione una fecha v&aacute;lida';
$app_strings['LBL_RESCHEDULE_ERROR2'] = 'Por favor seleccione una raz&oacute;n';

$app_strings['LBL_RESCHEDULE_PANEL'] = 'Replanificaciones';
$app_strings['LBL_RESCHEDULE_HISTORY'] = 'Historial de Intentos de Llamada';
$app_strings['LBL_RESCHEDULE_COUNT'] = 'Intentos de Llamada';

//SecurityGroups
$app_list_strings['moduleList']['SecurityGroups'] = 'Grupos de Seguridad';
$app_strings['LBL_SECURITYGROUP'] = 'Grupo de Seguridad';

$app_list_strings['moduleList']['OutboundEmailAccounts'] = 'Cuentas de correo electrónico saliente';

//social
$app_strings['FACEBOOK_USER_C'] = 'Facebook';
$app_strings['TWITTER_USER_C'] = 'Twitter';
$app_strings['LBL_PANEL_SOCIAL_FEED'] = 'Detalles de la actividad Social';

$app_strings['LBL_SUBPANEL_FILTER_LABEL'] = 'Filtro';

$app_strings['LBL_COLLECTION_TYPE'] = 'Tipo';

$app_strings['LBL_ADD_TAB'] = 'Añadir pestaña';
$app_strings['LBL_EDIT_TAB'] = 'Editar pestañas';
$app_strings['LBL_SUITE_DASHBOARD'] = 'Cuadro de Mando SuiteCRM'; //Can be translated in all caps. This string will be used by SuiteP template menu actions
$app_strings['LBL_ENTER_DASHBOARD_NAME'] = 'Introduzca el nombre del Dashboard:';
$app_strings['LBL_NUMBER_OF_COLUMNS'] = 'Número de columnas:';
$app_strings['LBL_DELETE_DASHBOARD1'] = '¿Seguro que desea eliminar';
$app_strings['LBL_DELETE_DASHBOARD2'] = 'tablero?';
$app_strings['LBL_ADD_DASHBOARD_PAGE'] = 'Agregar una página del Dashboard';
$app_strings['LBL_DELETE_DASHBOARD_PAGE'] = 'Eliminar página actual del Dashboard';
$app_strings['LBL_RENAME_DASHBOARD_PAGE'] = 'Cambiar el nombre de página del Dashboard';
$app_strings['LBL_SUITE_DASHBOARD_ACTIONS'] = 'Acciones'; //Can be translated in all caps. This string will be used by SuiteP template menu actions

$app_list_strings['collection_temp_list'] = array(
    'Tasks' => 'Tareas',
    'Meetings' => 'Reuniones',
    'Calls' => 'Llamadas',
    'Notes' => 'Notas',
    'Emails' => 'Correos'
);

$app_list_strings['moduleList']['TemplateEditor'] = 'Editor de Segmento de Plantilla';
$app_strings['LBL_CONFIRM_CANCEL_INLINE_EDITING'] = "Usted ha hecho clic afuera sin guardar. Haga clic en aceptar si desea PERDER sus cambios, o cancelar si desea seguir editando";
$app_strings['LBL_LOADING_ERROR_INLINE_EDITING'] = "Hubo un error al cargar el campo. La sesión puede haber expirado. Inicia sesión nuevamente para solucionar este problema";

//SuiteSpots
$app_list_strings['spots_areas'] = array(
    'getSalesSpotsData' => 'Ventas',
    'getAccountsSpotsData' => 'Cuentas',
    'getLeadsSpotsData' => 'Clientes Potenciales',
    'getServiceSpotsData' => 'Servicio',
    'getMarketingSpotsData' => 'Marketing',
    'getMarketingActivitySpotsData' => 'Actividad de marketing',
    'getActivitiesSpotsData' => 'Actividades',
    'getQuotesSpotsData' => 'Presupuestos'
);

$app_list_strings['moduleList']['Spots'] = 'Análisis dinámico';

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
$app_list_strings['day_list']['Monday'] = 'Lunes';
$app_list_strings['day_list']['Tuesday'] = 'Martes';
$app_list_strings['day_list']['Wednesday'] = 'Miércoles';
$app_list_strings['day_list']['Thursday'] = 'Jueves';
$app_list_strings['day_list']['Friday'] = 'Viernes';
$app_list_strings['day_list']['Saturday'] = 'Sábado';
$app_list_strings['day_list']['Sunday'] = 'Domingo';
$app_list_strings['pdf_page_size_dom']['A4'] = 'A4';
$app_list_strings['pdf_page_size_dom']['Letter'] = 'Carta';
$app_list_strings['pdf_page_size_dom']['Legal'] = 'Legal';
$app_list_strings['pdf_orientation_dom']['Portrait'] = 'Vertical';
$app_list_strings['pdf_orientation_dom']['Landscape'] = 'Horizontal';
$app_list_strings['run_when_dom']['When True'] = 'Evaluar al guardar'; // PR 6143
$app_list_strings['run_when_dom']['Once True'] = 'Perpetuo - (El campo debe ser auditado)';
$app_list_strings['sa_status_list']['Complete'] = 'Completa';
$app_list_strings['sa_status_list']['In_Review'] = 'En Revisión';
$app_list_strings['sa_status_list']['Issue_Resolution'] = 'Resolución de problemas';
$app_list_strings['sa_status_list']['Pending_Apttus_Submission'] = 'Envío de Apttus pendiente';
$app_list_strings['sharedGroupRule']['none'] = 'Sin acceso';
$app_list_strings['sharedGroupRule']['view'] = 'Solo lectura';
$app_list_strings['sharedGroupRule']['view_edit'] = 'Ver y editar';
$app_list_strings['sharedGroupRule']['view_edit_delete'] = 'Ver, editar y borrar';
$app_list_strings['moduleList']['SharedSecurityRulesFields'] = 'Campos de reglas de seguridad compartidos';
$app_list_strings['moduleList']['SharedSecurityRules'] = 'Reglas de seguridad compartidas';
$app_list_strings['moduleList']['SharedSecurityRulesActions'] = 'Acciones de reglas de seguridad compartidas';
$app_list_strings['shared_email_type_list'][''] = '';
$app_list_strings['shared_email_type_list']['Specify User'] = 'Usuario';
$app_list_strings['shared_email_type_list']['Users'] = 'Usuarios';
$app_list_strings['aow_condition_type_list']['Value'] = 'Valor';
$app_list_strings['aow_condition_type_list']['Field'] = 'Campo';
$app_list_strings['aow_condition_type_list']['Any_Change'] = 'Cualquier cambio';
$app_list_strings['aow_condition_type_list']['SecurityGroup'] = 'En SecurityGroup';
$app_list_strings['aow_condition_type_list']['currentUser'] = 'Usuario logueado como';
$app_list_strings['aow_condition_type_list']['Date'] = 'Fecha';
$app_list_strings['aow_condition_type_list']['Multi'] = 'Multiple';


$app_list_strings['moduleList']['SurveyResponses'] = 'Respuestas a la encuesta';
$app_list_strings['moduleList']['Surveys'] = 'Encuestas';
$app_list_strings['moduleList']['SurveyQuestionResponses'] = 'Respuestas de preguntas de encuesta';
$app_list_strings['moduleList']['SurveyQuestions'] = 'Preguntas de la encuesta';
$app_list_strings['moduleList']['SurveyQuestionOptions'] = 'Opciones de preguntas de encuesta';
$app_list_strings['survey_status_list']['Draft'] = 'Borrador';
$app_list_strings['survey_status_list']['Public'] = 'Público';
$app_list_strings['survey_status_list']['Closed'] = 'Cerrado';
$app_list_strings['surveys_question_type']['Text'] = 'Texto';
$app_list_strings['surveys_question_type']['Textbox'] = 'Cuadro de texto';
$app_list_strings['surveys_question_type']['Checkbox'] = 'Casilla de Verificación';
$app_list_strings['surveys_question_type']['Radio'] = 'Radio';
$app_list_strings['surveys_question_type']['Dropdown'] = 'Desplegable';
$app_list_strings['surveys_question_type']['Multiselect'] = 'Selección múltiple';
$app_list_strings['surveys_question_type']['Matrix'] = 'Matriz';
$app_list_strings['surveys_question_type']['DateTime'] = 'Fecha y hora';
$app_list_strings['surveys_question_type']['Date'] = 'Fecha';
$app_list_strings['surveys_question_type']['Scale'] = 'Escala';
$app_list_strings['surveys_question_type']['Rating'] = 'Calificación';
$app_list_strings['surveys_matrix_options'][0] = 'Satisfecho';
$app_list_strings['surveys_matrix_options'][1] = 'Ni satisfecho ni insatisfecho';
$app_list_strings['surveys_matrix_options'][2] = 'Insatisfecho';

$app_strings['LBL_OPT_IN_PENDING_EMAIL_NOT_SENT'] = 'Autorización pendiente. Confirmación no enviada';
$app_strings['LBL_OPT_IN_PENDING_EMAIL_FAILED'] = 'Envío de e-mail de confirmación fallado';
$app_strings['LBL_OPT_IN_PENDING_EMAIL_SENT'] = 'Autorización pendiente. Confirmación ya enviada';
$app_strings['LBL_OPT_IN'] = 'Adherido';
$app_strings['LBL_OPT_IN_CONFIRMED'] = 'Adhesión confirmada';
$app_strings['LBL_OPT_IN_OPT_OUT'] = 'Rehusado';
$app_strings['LBL_OPT_IN_INVALID'] = 'No Válido';

/** @see SugarEmailAddress */
$app_list_strings['email_settings_opt_in_dom'] = array(
    'not-opt-in' => 'Deshabilitado',
    'opt-in' => 'Autorizar',
    'confirmed-opt-in' => 'Adhesión confirmada'
);

$app_list_strings['email_confirmed_opt_in_dom'] = array(
    'not-opt-in' => 'No autorizado',
    'opt-in' => 'Autorizar',
    'confirmed-opt-in' => 'Adhesión confirmada'
);

$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL'] = 'El e-mail de confirmación de autorización ha sido agregado a la cola de mensajes para %s dirección(es). ';
$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_NOT_OPT_IN'] = 'No se puede enviar e-mail a %s correo(s) porque la(s) dirección(es) no está(n) autorizada(s) a recibir mensajes.';
$app_strings['RESPONSE_SEND_CONFIRM_OPT_IN_EMAIL_MISSING_EMAIL_ADDRESS_ID'] = '%s dirección de correo electrónico no tiene un id válido. ';

$app_strings['ERR_TWO_FACTOR_FAILED'] = 'Falló la Autenticación de dos factores';
$app_strings['ERR_TWO_FACTOR_CODE_SENT'] = 'Se ha enviado código de Autenticación de dos factores.';
$app_strings['ERR_TWO_FACTOR_CODE_FAILED'] = 'El envío del código de autenticación en dos factores ha fallado.';
$app_strings['LBL_THANKS_FOR_SUBMITTING'] = '¡Gracias por contarnos sus experiencias!';

$app_strings['ERR_IP_CHANGE'] = 'Hemos finalizado su sesión debido a un cambio significativo en su dirección IP';
$app_strings['ERR_RETURN'] = 'Volver al inicio';


$app_list_strings['oauth2_grant_type_dom'] = array(
    'password' => 'Otorgar Contraseña',
    'client_credentials' => 'Credenciales del cliente',
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
    'Search' => 'Búsqueda (nueva)',
    'UnifiedSearch' => 'Búsqueda global unificada (heredada)'
];


$app_strings['LBL_DEFAULT_API_ERROR_TITLE'] = 'Error en API JSON';
$app_strings['LBL_DEFAULT_API_ERROR_DETAIL'] = 'Error en API JSON.';
$app_strings['LBL_API_EXCEPTION_DETAIL'] = 'Versión de API: 8';
$app_strings['LBL_BAD_REQUEST_EXCEPTION_DETAIL'] = 'Por favor, asegúrese de rellenar todos los campos requeridos';
$app_strings['LBL_EMPTY_BODY_EXCEPTION_DETAIL'] = 'Json API espera que el cuerpo de la solicitud sea JSON';
$app_strings['LBL_INVALID_JSON_API_REQUEST_EXCEPTION_DETAIL'] = 'No se puede validar la solicitud de carga útil Json Api';
$app_strings['LBL_INVALID_JSON_API_RESPONSE_EXCEPTION_DETAIL'] = 'No se puede validar la respuesta de carga útil Json Api';
$app_strings['LBL_MODULE_NOT_FOUND_EXCEPTION_DETAIL'] = 'Json API no puede encontrar recursos';
$app_strings['LBL_NOT_ACCEPTABLE_EXCEPTION_DETAIL'] = 'Json API expects the "Aceptar" header to be application/vnd.api+json';
$app_strings['LBL_UNSUPPORTED_MEDIA_TYPE_EXCEPTION_DETAIL'] = 'Json API expects the "Content-Type" header to be application/vnd.api+json';

$app_strings['MSG_BROWSER_NOTIFICATIONS_ENABLED'] = 'Las notificaciones de escritorio están ahora habilitadas para este navegador web.';
$app_strings['MSG_BROWSER_NOTIFICATIONS_DISABLED'] = 'Las notificaciones de escritorio están desactivadas para este navegador web. Utilice las preferencias de su navegador para habilitarlas otra vez.';
$app_strings['MSG_BROWSER_NOTIFICATIONS_UNSUPPORTED'] = 'Este navegador no es compatible con las notificaciones de escritorio.';

$app_strings['LBL_GOOGLE_SYNC_ERR'] = 'Error SuiteCRM Google Sync';
$app_strings['LBL_THERE_WAS_AN_ERR'] = 'Hubo un error: ';
$app_strings['LBL_CLICK_HERE'] = 'Haga clic aquí';
$app_strings['LBL_TO_CONTINUE'] = ' para continuar.';

$app_strings['IMAP_HANDLER_ERROR'] = 'ERROR: {error}; se usó la clave: "{key}".';
$app_strings['IMAP_HANDLER_SUCCESS'] = 'OK: configuración de prueba cambiada a "{key}"';
$app_strings['IMAP_HANDLER_ERROR_INVALID_REQUEST'] = 'Petición no válida, use el valor "{var}".';
$app_strings['IMAP_HANDLER_ERROR_UNKNOWN_BY_KEY'] = 'Se produjo un error desconocido, la clave "{key}" no fue guardada.';
$app_strings['IMAP_HANDLER_ERROR_NO_TEST_SET'] = 'No existen las configuraciones de prueba.';
$app_strings['IMAP_HANDLER_ERROR_NO_KEY'] = 'Clave no encontrada.';
$app_strings['IMAP_HANDLER_ERROR_KEY_SAVE'] = 'Error al guardar la clave.';
$app_strings['IMAP_HANDLER_ERROR_UNKNOWN'] = 'Error desconocido';
$app_strings['LBL_SEARCH_TITLE']                   = 'Búsqueda';
$app_strings['LBL_SEARCH_TEXT_FIELD_TITLE_ATTR']   = 'Criterios de búsqueda';
$app_strings['LBL_SEARCH_SUBMIT_FIELD_TITLE_ATTR'] = 'Búsqueda';
$app_strings['LBL_SEARCH_SUBMIT_FIELD_VALUE']      = 'Búsqueda';
$app_strings['LBL_SEARCH_QUERY']                   = 'Consulta: ';
$app_strings['LBL_SEARCH_RESULTS_PER_PAGE']        = 'Resultados por página: ';
$app_strings['LBL_SEARCH_ENGINE']                  = 'Buscador: ';
$app_strings['LBL_SEARCH_TOTAL'] = 'Resultado(s) total(es): ';
$app_strings['LBL_SEARCH_PREV'] = 'Anterior';
$app_strings['LBL_SEARCH_NEXT'] = 'Siguiente';
$app_strings['LBL_SEARCH_PAGE'] = 'Página ';
$app_strings['LBL_SEARCH_OF'] = ' de '; // Usage: Page 1 of 5
$app_strings['LBL_USE_ADVANCED_SEARCH'] = 'Utilizar la búsqueda avanzada';
$app_strings['LBL_USE_BASIC_SEARCH'] = 'Utilizar la búsqueda básica';

// PDF Engines
$app_strings['LBL_LEGACY_MPDF_ENGINE'] = 'Motor MPDF obsoleto';
$app_strings['LBL_TCPDF_ENGINE'] = 'Motor TCPDF';

$app_strings['ERR_INVALID_FILE_NAME'] = 'El nombre del fichero no es válido:';
$app_strings['LBL_LOGGER_VALID_FILENAME_CHARACTERS'] = 'Solo puede contener caracteres alfanuméricos, el punto (.), el guion (-) o el guion bajo (_).';
$app_strings['LBL_LOGGER_INVALID_FILENAME'] = 'El nombre del fichero de importación no es válido';

$app_strings['LBL_PASSWORD_SET_NEW_VALUE_TO_RESET'] = 'Contraseña establecida. Introduzca un valor para establecer una nueva contraseña.';
$app_strings['LBL_VALUE_SET_PLACEHOLDER'] = 'Valor establecido. Introduzca un nuevo valor para sobrescribir el valor actual.';
