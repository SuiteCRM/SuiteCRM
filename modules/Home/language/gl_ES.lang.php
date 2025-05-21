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
    'LBL_MODULE_NAME' => 'Inicio',
    'LBL_NEW_FORM_TITLE' => 'Novo Contacto',
    'LBL_FIRST_NAME' => 'Nome:',
    'LBL_LAST_NAME' => 'Apelidos:',
    'LBL_LIST_LAST_NAME' => 'Apelidos',
    'LBL_PHONE' => 'Teléfono:',
    'LBL_EMAIL_ADDRESS' => 'Correo electrónico:',
    'LBL_MY_PIPELINE_FORM_TITLE' => 'O Meu Pipeline',
    'LBL_PIPELINE_FORM_TITLE' => 'Proceso por Etapa de Vendas',
    'LBL_RGraph_PIPELINE_FORM_TITLE' => 'Proceso por Etapa de Vendas',
    'LNK_NEW_CONTACT' => 'Novo Contacto',
    'LNK_NEW_ACCOUNT' => 'Crear unha conta',
    'LNK_NEW_OPPORTUNITY' => 'Nova Oportunidade',
    'LNK_NEW_LEAD' => 'Novo Cliente Potencial',
    'LNK_NEW_CASE' => 'Novo Caso',
    'LNK_NEW_NOTE' => 'Nova Nota ou Arquivo Adxunto',
    'LNK_NEW_CALL' => 'Rexistrar Chamada',
    'LNK_NEW_EMAIL' => 'Arquivar Email',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_NEW_TASK' => 'Nova Tarefa',
    'LNK_NEW_BUG' => 'Informar de Incidencia',
    'LNK_NEW_SEND_EMAIL' => 'Redactar Email',
    'LBL_NO_ACCESS' => 'Non ten acceso a esta área. Contacte co Administrador do seu sitio para obtelo.',
    'LBL_NO_RESULTS_IN_MODULE' => '-- Sen Resultados --',
    'LBL_NO_RESULTS' => '<h2>Non se encontraron resultados. Por favor, realice unha nova busca.</h2><br>',
    'LBL_NO_RESULTS_TIPS' => '<h3>Trucos para a Busca:</h3><ul><li>Asegúrese que seleccionou as categorías adecuadas máis arriba.</li><li>Amplíe os seus criterios de busca.</li><li>Se aínda así non obtén resultados, probe coa opción de busca avanzada.</li></ul>',

    'LBL_ADD_DASHLETS' => 'Agregar SuiteCRM Dashlets',
    'LBL_WEBSITE_TITLE' => 'Sitio Web',
    'LBL_RSS_TITLE' => 'Fonte de Noticias',
    'LBL_CLOSE_DASHLETS' => 'Cerrar',
    'LBL_OPTIONS' => 'Opcións',
    // dashlet search fields
    'LBL_TODAY' => 'Hoxe',
    'LBL_YESTERDAY' => 'Onte',
    'LBL_TOMORROW' => 'Mañana',
    'LBL_NEXT_WEEK' => 'A Próxima Semana',
    'LBL_LAST_7_DAYS' => 'Últimos 7 Días',
    'LBL_NEXT_7_DAYS' => 'Seguintes 7 Días',
    'LBL_LAST_MONTH' => 'Último Mes',
    'LBL_NEXT_MONTH' => 'Seguinte Mes',
    'LBL_LAST_YEAR' => 'Último Ano',
    'LBL_NEXT_YEAR' => 'Próximo Ano',
    'LBL_LAST_30_DAYS' => 'Últimos 30 Días',
    'LBL_NEXT_30_DAYS' => 'Próximos 30 días',
    'LBL_THIS_MONTH' => 'Este Mes',
    'LBL_THIS_YEAR' => 'Este Ano',

    'LBL_MODULES' => 'Módulos',
    'LBL_CHARTS' => 'Gráficos',
    'LBL_TOOLS' => 'Ferramentas',
    'LBL_WEB' => 'Web',
    'LBL_SEARCH_RESULTS' => 'Resultados de Busca',

    // Dashlet Categories
    'dashlet_categories_dom' => array(
        'Module Views' => 'Vistas do Módulo',
        'Portal' => 'Portal',
        'Charts' => 'Gráficos',
        'Tools' => 'Ferramentas',
        'Miscellaneous' => 'Varios'
    ),
    'LBL_ADDING_DASHLET' => 'Agregando SuiteCRM Dashlet...',
    'LBL_ADDED_DASHLET' => 'SuiteCRM Dashlet Agregado',
    'LBL_REMOVE_DASHLET_CONFIRM' => '¿Está seguro de que desexa quitar o SuiteCRM Dashlet?',
    'LBL_REMOVING_DASHLET' => 'Quitando SuiteCRM Dashlet...',
    'LBL_REMOVED_DASHLET' => 'SuiteCRM Dashlet Quitado',
    'LBL_DASHLET_CONFIGURE_GENERAL' => 'Xeral',
    'LBL_DASHLET_CONFIGURE_FILTERS' => 'Filtros',
    'LBL_DASHLET_CONFIGURE_MY_ITEMS_ONLY' => 'Só Os Meus Elementos',
    'LBL_DASHLET_CONFIGURE_TITLE' => 'Título',
    'LBL_DASHLET_CONFIGURE_DISPLAY_ROWS' => 'Mostrar Filas',

    'LBL_DASHLET_DELETE' => 'Eliminar SuiteCRM Dashlet',
    'LBL_DASHLET_REFRESH' => 'Actualizar SuiteCRM Dashlet',
    'LBL_DASHLET_EDIT' => 'Editar SuiteCRM Dashlet',

    // Default out-of-box names for tabs
    'LBL_HOME_PAGE_1_NAME' => 'O Meu CRM',
    'LBL_CLOSE_SITEMAP' => 'Cerrar',

    'LBL_SEARCH' => 'Buscar',
    'LBL_CLEAR' => 'Limpar',

    'LBL_BASIC_CHARTS' => 'Gráficos Básicos',

    'LBL_DASHLET_SEARCH' => 'Buscar SuiteCRM Dashlet',

//ABOUT page
    'LBL_VERSION' => 'Versión',
    'LBL_BUILD' => 'Compilación',

    'LBL_SOURCE_SUGAR' => 'SugarCRM Inc - proveedores de framework CE',

    'LBL_DASHLET_TITLE' => 'Os Meus Sitios',
    'LBL_DASHLET_OPT_TITLE' => 'Título',
    'LBL_DASHLET_INCORRECT_URL' => 'Ubicación de sitio web incorrecto',
    'LBL_DASHLET_OPT_URL' => 'Enderezo de Sitio Web',
    'LBL_DASHLET_OPT_HEIGHT' => 'Altura de Dashlet (en píxeles)',
    'LBL_DASHLET_SUITE_NEWS' => 'Noticias sobre SuiteCRM',
    'LBL_DASHLET_DISCOVER_SUITE' => 'Descubra SuiteCRM',
    'LBL_BASIC_SEARCH' => 'Filtro rápido' /*for 508 compliance fix*/,
    'LBL_ADVANCED_SEARCH' => 'Filtro avanzado' /*for 508 compliance fix*/,
    'LBL_TOUR_HOME' => 'Icono de inicio',
    'LBL_TOUR_HOME_DESCRIPTION' => 'Rápidamente volve ao dashboard de páxina de inicio cun só clic.',
    'LBL_TOUR_MODULES' => 'Módulos',
    'LBL_TOUR_MODULES_DESCRIPTION' => 'Todos os módulos importantes están aquí.',
    'LBL_TOUR_MORE' => 'Máis módulos',
    'LBL_TOUR_MORE_DESCRIPTION' => 'O resto dos seus módulos está aquí.',
    'LBL_TOUR_SEARCH' => 'Busca Full-Text',
    'LBL_TOUR_SEARCH_DESCRIPTION' => 'A busca é agora moito mellor.',
    'LBL_TOUR_NOTIFICATIONS' => 'Notificacións',
    'LBL_TOUR_NOTIFICATIONS_DESCRIPTION' => 'Notificacións da aplicación de SuiteCRM van aquí.',
    'LBL_TOUR_PROFILE' => 'Perfil',
    'LBL_TOUR_PROFILE_DESCRIPTION' => 'Perfil de acceso, axustes e peche de sesión.',
    'LBL_TOUR_QUICKCREATE' => 'Creación rápida',
    'LBL_TOUR_QUICKCREATE_DESCRIPTION' => 'Crear rexistros rápidamente sen perder o seu lugar.',
    'LBL_TOUR_FOOTER' => 'Pé de páxina expandible',
    'LBL_TOUR_FOOTER_DESCRIPTION' => 'Facilmente expande e contrae o pé de páxina.',
    'LBL_TOUR_CUSTOM' => 'Aplicacións personalizadas',
    'LBL_TOUR_CUSTOM_DESCRIPTION' => 'Integracións especiais van aquí.',
    'LBL_TOUR_BRAND' => 'A Súa marca',
    'LBL_TOUR_BRAND_DESCRIPTION' => 'Aquí vai o teu logo. Podes mover o cursor encima para máis información.',
    'LBL_TOUR_WELCOME' => 'Benvido a SuiteCRM',
    'LBL_TOUR_WATCH' => 'Ver novidades en SuiteCRM',
    'LBL_TOUR_FEATURES' => '<ul style=""><li class="icon-ok">Nova e simplificada barra de navegación</li><li class="icon-ok">Novo pé de páxina expandible</li><li class="icon-ok">Busca opotimizada</li><li class="icon-ok">Accións de menú actualizadas</li></ul><p>e moito máis</p>',
    'LBL_TOUR_VISIT' => 'Para máis información por favor visite a nosa aplicación',
    'LBL_TOUR_DONE' => '¡Listo!',
    'LBL_TOUR_REFERENCE_1' => 'Sempre podes consultar a nosa',
    'LBL_TOUR_REFERENCE_2' => 'a través do enlace de "Foro de apoio" na pestana perfil.',
    'LNK_TOUR_DOCUMENTATION' => 'documentación',
    'LBL_TOUR_CALENDAR_URL_1' => '¿Compartir o teu calendario de SuiteCRM con aplicacións de terceiros, como Microsoft Outlook ou Exchange? Se é así, tes un novo enderezo URL. Esta URL nova e máis segura inclúe unha clave persoal que evitará a publicación non autorizada do teu calendario.',
    'LBL_TOUR_CALENDAR_URL_2' => 'Recuperar a nova URL do calendario compartido.',
    'LBL_CONTRIBUTORS' => 'Contribuidores',
    'LBL_ABOUT_SUITE' => 'Acerca de SuiteCRM',
    'LBL_PARTNERS' => 'Socios',
    'LBL_FEATURING' => 'Os módulos de AOS, AOW, AOR, AOP, AOE e Replanificación son de SalesAgility.',

    'LBL_CONTRIBUTOR_SUITECRM' => 'SuiteCRM - CRM de Fontes Abertas para o mundo',
    'LBL_CONTRIBUTOR_SECURITY_SUITE' => 'SecuritySuite por Jason Eggers',
    'LBL_CONTRIBUTOR_JJW_GMAPS' => 'JJWDesign Google Maps por Jeffrey J. Walters',
    'LBL_CONTRIBUTOR_CONSCIOUS' => 'SuiteCRM LOGO provisto por Conscious Solutions',
    'LBL_CONTRIBUTOR_RESPONSETAP' => 'Contribución á versión 7.3 de SuiteCRM por ResponseTap',
    'LBL_CONTRIBUTOR_GMBH' => 'Campos calculados de fluxo de traballo aportados por diligent technology & business consulting GmbH',

    'LBL_LANGUAGE_ABOUT' => 'Sobre as traducións de SuiteCRM',
    'LBL_LANGUAGE_COMMUNITY_ABOUT' => 'Tradución en colaboración pola comunidade de SuiteCRM',
    'LBL_LANGUAGE_COMMUNITY_PACKS' => 'Tradución creada usando Crowdin',

    'LBL_ABOUT_SUITE_2' => 'SuiteCRM é publicado baixo licenza open source - GPL3',
    'LBL_ABOUT_SUITE_4' => 'Todo o código de SuiteCRM desenvolvido e administrado polo proxecto será lanzado como open source - GPL3',
    'LBL_ABOUT_SUITE_5' => 'O soporte sobre SuiteCRM está dispoñible tanto de forma gratuita como paga',

    'LBL_SUITE_PARTNERS' => 'Temos leais partners de SuiteCRM entusiastas do open source. Para ver a nosa lista completa de partners, vexa o noso website.',

    'LBL_SAVE_BUTTON' => 'Gardar',
    'LBL_DELETE_BUTTON' => 'Eliminar',
    'LBL_APPLY_BUTTON' => 'Aplicar',
    'LBL_SEND_INVITES' => 'Gardar e Enviar Invitacións',
    'LBL_CANCEL_BUTTON' => 'Cancelar',
    'LBL_CLOSE_BUTTON' => 'Peche',

    'LBL_CREATE_NEW_RECORD' => 'Crear Actividade',
    'LBL_CREATE_CALL' => 'Rexistrar Chamada',
    'LBL_CREATE_MEETING' => 'Programar Reunión',

    'LBL_GENERAL_TAB' => 'Detalles',
    'LBL_PARTICIPANTS_TAB' => 'Asistentes',
    'LBL_REPEAT_TAB' => 'Repetir',

    'LBL_REPEAT_TYPE' => 'Repetir',
    'LBL_REPEAT_INTERVAL' => 'Intervalo',
    'LBL_REPEAT_END' => 'Fin',
    'LBL_REPEAT_END_AFTER' => 'Despois de',
    'LBL_REPEAT_OCCURRENCES' => 'Ocorrencias',
    'LBL_REPEAT_END_BY' => 'Por',
    'LBL_REPEAT_DOW' => 'En',
    'LBL_REPEAT_UNTIL' => 'Repetir Ata',
    'LBL_REPEAT_COUNT' => 'Número de Ocorrencias',
    'LBL_REPEAT_LIMIT_ERROR' => 'A túa solicitude ía a crear máis  de $limit reunións.',

    //Events
    'LNK_EVENT' => 'Evento',
    'LNK_EVENT_VIEW' => 'Ver Evento',
    'LBL_DATE' => 'Data: ',
    'LBL_DURATION' => 'Duración: ',
    'LBL_NAME' => 'Título: ',
    'LBL_HOUR_ABBREV' => 'hora',
    'LBL_HOURS_ABBREV' => 'horas',
    'LBL_MINSS_ABBREV' => 'minutos',
    'LBL_LOCATION' => 'Lugar:',
    'LBL_STATUS' => 'Estado:',
    'LBL_DESCRIPTION' => 'Descrición:',
    //End Events

    'LBL_ELASTIC_SEARCH_EXCEPTION_SEARCH_INVALID_REQUEST' => 'Produciuse un erro ao realizar a busca. A sintaxe da súa consulta podería non ser válida.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_SEARCH_ENGINE_NOT_FOUND' => 'Non se pode encontrar o motor de busca solicitado. Intente realizar a busca de novo.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_NO_NODES_AVAILABLE' => 'Erro ao conectar ao servidor Elasticsearch.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_SEARCH' => 'Produciuse un erro interno na Busca.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_DEFAULT' => 'Produciuse un erro descoñecido ao realizar a busca.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_END_MESSAGE' => 'Contacte cun administrador sie o problema persiste. Máis información dispoñible nos rexistros.',
    'LBL_ELASTIC_SEARCH_EXCEPTION_MISSING_INDEX' => 'No se pudo encontrar el índice de búsqueda para uno o más módulos. Por favor, asegúrate de que el crontab esté configurado y en funcionamiento, abre la configuración de Elasticsearch y haz clic en "Programar indexación completa" y considera crear una tarea del Scheduler "Indexador de Elasticsearch" si no existe.',

    'LBL_ELASTIC_SEARCH_DEFAULT' => 'No hay resultados que coincidan con tus criterios de búsqueda. Intenta ampliar la búsqueda.',

    'LNK_TASK_VIEW' => 'Ver Tarea',
);
