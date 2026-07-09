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
$mod_strings = array (
  'LBL_ASSIGNED_TO_ID' => 'Asignado a (ID)',
  'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
  'LBL_ASSIGNED_TO' => 'Asignado a',
  'LBL_LIST_ASSIGNED_TO_NAME' => 'Asignado a',
  'LBL_LIST_ASSIGNED_USER' => 'Asignado a',
  'LBL_CREATED' => 'Creado por',
  'LBL_CREATED_USER' => 'Creado por',
  'LBL_CREATED_ID' => 'Creado por (ID)',
  'LBL_MODIFIED' => 'Modificado por',
  'LBL_MODIFIED_NAME' => 'Modificado por',
  'LBL_MODIFIED_USER' => 'Modificado por',
  'LBL_MODIFIED_ID' => 'Modificado por (ID)',
  'LBL_SECURITYGROUPS' => 'Grupos de seguridade',
  'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de seguridade',
  'LBL_ID' => 'ID',
  'LBL_DATE_ENTERED' => 'Data de Creación',
  'LBL_DATE_MODIFIED' => 'Data de Modificación',
  'LBL_DESCRIPTION' => 'Descrición',
  'LBL_DELETED' => 'Eliminado',
  'LBL_NAME' => 'Nome',
  'LBL_LIST_NAME' => 'Nome',
  'LBL_EDIT_BUTTON' => 'Editar',
  'LBL_QUICKEDIT_BUTTON' => '↙ Editar',
  'LBL_REMOVE' => 'Desvincular',
  'LBL_ASCENDING' => 'Ascendente',
  'LBL_DESCENDING' => 'Descendente',

  'LBL_LIST_FORM_TITLE' => 'Lista de Formularios Web Avanzados',
  'LBL_MODULE_NAME' => 'Formularios Web Avanzados',
  'LBL_MODULE_TITLE' => 'Formularios Web Avanzados',
  'LBL_HOMEPAGE_TITLE' => 'Mis Formularios Web Avanzados',
  'LNK_NEW_RECORD' => 'Crear un Formulario Web Avanzado',
  'LNK_LIST' => 'Ver Formularios Web Avanzados',
  'LNK_IMPORT_stic_AWF_Forms' => 'Importar Formularios Web Avanzados',
  'LBL_SEARCH_FORM_TITLE' => 'Buscar Formularios Web Avanzados',
  'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
  'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
  'LBL_NEW_FORM_TITLE' => 'Nuevo Formulario Web Avanzado',

  'LBL_DEFAULT_PANEL' => 'Datos generales',
  'LBL_PANEL_RECORD_DETAILS' => 'Detalles del registro',

  // -- MODULE FIELDS --
  'LBL_STATUS' => 'Estado',
  'LBL_START_DATE' => 'Fecha de inicio',
  'LBL_END_DATE' => 'Fecha de fin',
  'LBL_PUBLIC_URL' => 'URL',
  'LBL_PROCESSING_MODE' => 'Modo de procesamiento',
  'LBL_PROCESSING_MODE_DESC' => 'Define cómo se gestionan las respuestas recibidas. En modo síncrono las respuestas se almacenan y se procesan al recibirse. En modo asíncrono las respuestas se almacenan al recibirse y se procesan posteriormente mediante un proceso en segundo plano, lo que puede ser de interés para optimizar el rendimiento del sistema en casos de gran afluencia.',
  'LBL_CONFIGURATION' => 'Configuración',
  'LBL_ANALYTICS_VIEWS' => 'Visitas totales',
  'LBL_ANALYTICS_BLOCKED' => 'Visitas bloqueadas (no público)',
  'LBL_ANALYTICS_SUBMISSIONS' => 'Respuestas válidas',
  'LBL_ANALYTICS_SPAM' => 'Spam detectado',
  'LBL_ANALYTICS_REFERRERS' => 'Fuentes de tráfico (dominios)',

  // -- WIZARD --
  // Main actions
  'LBL_WIZARD_PREVIOUS' => '<< Anterior',
  'LBL_WIZARD_NEXT' => 'Siguiente >>',
  'LBL_WIZARD_FINISH' => 'Finalizar',
  'LBL_WIZARD_SAVE' => 'Guardar',
  'LBL_WIZARD_SAVED_DRAFT' => 'Formulario guardado',
  'LBL_WIZARD_SHOW_DEBUG_INFO' => 'Mostrar detalles',

  // Edit Warning
  'LBL_WIZARD_FORM_EDIT_WARNING_TITLE' => '⚠️ ¡Precaución al editar!',
  'LBL_WIZARD_FORM_EDIT_WARNING_PUBLIC' => 'Este formulario es público: alguien podría estar rellenándolo en este mismo momento.',
  'LBL_WIZARD_FORM_EDIT_WARNING_RESPONSES' => 'Hay %s respuestas registradas: cambiar la estructura del formulario podría causar inconsistencias.',
  'LBL_WIZARD_FORM_EDIT_WARNING_PROCEED' => 'En caso de querer realizar cambios significativos se recomienda valorar la posibilidad de duplicar el formulario.',

  // Corrupted form warning
  'LBL_WIZARD_FORM_CORRUPTED_WARNING_MESSAGE' => '⚠️ Error crítico: No se ha podido cargar la configuración correctamente. El formulario se mostrará vacío y el guardado automático se ha desactivado para evitar la pérdida de datos.',
  
  // Steps
  'LBL_WIZARD_TITLE_STEP1' => 'Información general',
  'LBL_WIZARD_DESC_STEP1' => 'Definición de las propiedades generales del formulario.',
  'LBL_WIZARD_TITLE_STEP2' => 'Estructura y campos',
  'LBL_WIZARD_DESC_STEP2' => 'Definición del contenido del formulario. Los bloques de datos pueden vincularse a módulos de SinergiaCRM o funcionar de forma independiente. Además, es posible configurar validaciones de datos, detección de duplicados, etc.',
  'LBL_WIZARD_TITLE_STEP2_START' => 'Empiece a construir el formulario',
  'LBL_WIZARD_TITLE_STEP3' => 'Lógica y automatismos',
  'LBL_WIZARD_DESC_STEP3' => 'Definición de las acciones que se ejecutarán cuando se reciba una respuesta: crear o actualizar registros, enviar correos electrónicos, redireccionar a páginas web, etc.',
  'LBL_WIZARD_TITLE_STEP4' => 'Maquetación',
  'LBL_WIZARD_DESC_STEP4' => 'Diseño de la apariencia del formulario. Permite definir el estilo visual, organizar los bloques de datos, personalizar la cabecera y el pie del formulario, etc.',
  'LBL_WIZARD_TITLE_STEP5' => 'Resumen y publicación',
  'LBL_WIZARD_DESC_STEP5' => 'Revisión y activación del formulario. Permite activar la recepción de respuestas, obtener el enlace de acceso público o descargar el código HTML para integrarlo en una página web externa.',

  // General buttons
  'LBL_BUTTON_ADD' => 'Añadir',
  'LBL_BUTTON_EDIT' => 'Editar',
  'LBL_BUTTON_DELETE' => 'Eliminar',
  'LBL_BUTTON_MOVE_UP' => 'Mover arriba',
  'LBL_BUTTON_MOVE_DOWN' => 'Mover abajo',
  'LBL_BUTTON_DUPLICATE' => 'Duplicar',
  'LBL_BUTTON_RELOAD' => 'Recargar',
  'LBL_BUTTON_COPY' => 'Copiar',
  'LBL_BUTTON_OPEN' => 'Abrir',
  'LBL_BUTTON_DOWNLOAD' => 'Descargar',

  // General editors
  'LBL_SELECT_PLACEHOLDER' => 'Seleccione un elemento...',
  'LBL_SELECT_WRITE_TO_SEARCH' => 'Escriba para filtrar la lista...',
  'LBL_SELECT_NO_RESULTS' => 'No existen coincidencias',

  // DataBlocks
  'LBL_DATABLOCK' => 'Bloque de datos',
  'LBL_DATABLOCKS' => 'Bloques de datos',
  'LBL_DATABLOCK_DETACHED' => 'Datos no enlazados',
  'LBL_DATABLOCK_ADD' => 'Añadir un bloque de datos',
  'LBL_DATABLOCK_ADD_TITLE' => 'Configurar un bloque de datos relacionado con un módulo del sistema',
  'LBL_DATABLOCK_ADD_UNLINKED' => 'Añadir un bloque de datos no enlazado',
  'LBL_DATABLOCK_ADD_UNLINKED_TITLE' => 'Configurar un bloque de datos sin relacionarlo con ningún módulo del sistema',
  'LBL_DATABLOCK_NEW' => 'Nuevo bloque de datos',
  'LBL_DATABLOCK_NEW_UNLINKED' => 'Nuevo bloque de datos no enlazado',
  'LBL_DATABLOCK_MODULE' => 'Módulo',
  'LBL_DATABLOCK_NAME' => 'Nombre',
  'LBL_DATABLOCK_INTERNAL_NAME' => 'Nombre interno',

  // DataBlock -> Fields
  'LBL_FIELDS' => 'Campos',
  'LBL_FIELDS_FORM_TAB' => 'Formulario',
  'LBL_FIELDS_FORM_TAB_DESC' => 'Campos que se incluirán en el formulario público.',
  'LBL_FIELDS_FIXED_TAB' => 'Servidor',
  'LBL_FIELDS_FIXED_TAB_DESC' => 'Campos con valor prefijado que se incorporarán a los datos recibidos de los formularios.',
  'LBL_FIELD_FORM' => 'Campo en el formulario',
  'LBL_FIELD_FORM_ADD' => 'Añadir campo',
  'LBL_FIELD_FORM_NEW' => 'Nuevo campo',
  'LBL_FIELD_UNLINKED' => 'Campo no enlazado',
  'LBL_FIELD_UNLINKED_ADD' => 'Añadir campo no enlazado',
  'LBL_FIELD_UNLINKED_NEW' => 'Nuevo campo no enlazado',
  'LBL_FIELD_FIXED' => 'Valor fijo',
  'LBL_FIELD_FIXED_ADD' => 'Añadir campo de servidor',
  'LBL_FIELD_FIXED_NEW' => 'Nuevo campo de servidor',
  'LBL_FIELD_CONVERT_TO_FIELD_FORM' => 'Convertir en campo de formulario',
  'LBL_FIELD_CONVERT_TO_FIELD_FIXED' => 'Convertir en campo de servidor',
  'LBL_FIELD_DEFINITION' => 'Definición',
  'LBL_FIELD_DEFINITION_FORM' => 'Representación en el formulario',
  'LBL_FIELD_DEFINITION_VALIDATIONS' => 'Validación de datos',

  'LBL_FIELD' => 'Campo',
  'LBL_FIELD_NAME' => 'Nombre',
  'LBL_FIELD_INTERNAL_NAME' => 'Nombre interno',
  'LBL_FIELDS_SHOW_ALL' => 'Mostrar todos los campos',
  'LBL_FIELDS_SHOW_ALL_DESC' => 'Incluir en el desplegable todos los campos del módulo aunque no se muestren en las vistas de edición o detalle.',
  'LBL_FIELD_LABEL' => 'Etiqueta',
  'LBL_FIELD_REQUIRED_IN_FORM' => 'Campo obligatorio',
  'LBL_FIELD_IN_FORM' => 'En el formulario',
  'LBL_FIELD_TYPE_IN_FORM' => 'Tipo de entrada',
  'LBL_FIELD_SUBTYPE_IN_FORM' => 'Tipo de editor',
  'LBL_FIELD_PLACEHOLDER' => 'Texto de fondo',
  'LBL_FIELD_DESCRIPTION' => 'Texto de ayuda',
  'LBL_FIELD_ADD_LINK' => 'Añadir enlace',
  'LBL_FIELD_LINK_CREATE' => 'Crear enlace',
  'LBL_FIELD_LINK_TEXT' => 'Texto del enlace',
  'LBL_FIELD_LINK_URL' => 'URL de destino',
  'LBL_FIELD_VALUE_TYPE' => 'Tipo de valor',
  'LBL_FIELD_VALUE_OPTIONS_LIST' => 'Lista asociada',
  'LBL_FIELD_VALUE_OPTIONS_CUSTOMIZE' => 'Personalizar opciones',
  'LBL_FIELD_VALUE_OPTIONS_CUSTOMIZED' => '(Modificado)',
  'LBL_FIELD_VALUE_OPTIONS_SELECT_REGS' => 'Seleccionar registros',
  'LBL_FIELD_VALUE_OPTIONS' => 'Valores posibles',
  'LBL_FIELD_VALUE_OPTION_SHOW_NAME' => 'Mostrar valores internos',
  'LBL_FIELD_VALUE_OPTION_NAME' => 'Valor interno',
  'LBL_FIELD_VALUE_OPTION_ORIGINAL_LABEL' => 'Texto original',
  'LBL_FIELD_VALUE_OPTION_LABEL' => 'Texto',
  'LBL_FIELD_VALUE_OPTION_SHOW' => 'Mostrar',
  'LBL_FIELD_VALUE_OPTION_ACTIONS' => 'Acciones',
  'LBL_FIELD_VALUE' => 'Valor',
  'LBL_FIELD_VALUE_RELATIVE_DATE_OPTION' => 'Fecha relativa',
  'LBL_FIELD_VALUE_RELATIVE_DATE_OPTION_DESC' => 'Permite indicar una fecha relativa a la fecha de las respuestas recibidas.',
  'LBL_FIELD_VALUE_RELATIVE_DATE_CUSTOM' => 'Fecha relativa personalizada',
  'LBL_FIELD_VALUE_RELATIVE_DATE_CUSTOM_DESC' => 'Expresión en inglés compatible con la función strtotime() de PHP. Ejemplos: tomorrow 14:00, next monday, +2 weeks, first day of next month.',
  'LBL_FIELD_ACTIONS' => 'Acciones',
  
  // DataBlock -> Field -> Validations
  'LBL_FIELD_VALIDATION' => 'Validación',
  'LBL_FIELD_VALIDATOR' => 'Acción de validación',
  'LBL_FIELD_VALIDATION_ADD' => 'Añadir validación',
  'LBL_FIELD_VALIDATION_NEW' => 'Nueva validación',
  'LBL_FIELD_VALIDATION_EDIT' => 'Editar validación',
  'LBL_FIELD_VALIDATION_PARAMETERS' => 'Parámetros',
  'LBL_FIELD_VALIDATION_ERROR_MESSAGE' => 'Mensaje de error',
  'LBL_FIELD_VALIDATION_ACTIONS' => 'Acciones',
  'LBL_FIELD_ACTIVE_VALIDATIONS' => 'Validaciones activas',

  // DataBlock -> Duplicate checks
  'LBL_DUPLICATE_CHECK' => 'Detección de duplicados',
  'LBL_DUPLICATE_CHECK_ADD' => 'Añadir detección de duplicados',
  'LBL_DUPLICATE_FIELDS' => 'Campos a verificar',
  'LBL_DUPLICATE_FIELDS_SEL_FIELDS' => 'Seleccionar campo(s)...',
  'LBL_ONDUPLICATE_ACTION' => 'Acción en caso de duplicado',

  // DataBlock -> Relationships
  'LBL_RELATIONSHIP' => 'Relación',
  'LBL_RELATIONSHIPS' => 'Relaciones entre bloques de datos',
  'LBL_RELATIONSHIP_ADD' => 'Añadir relación',
  'LBL_RELATIONSHIP_NEW' => 'Nueva relación entre bloques de datos',  
  'LBL_RELATIONSHIP_NO_MODULE_RELATED' => 'Sin módulo relacionado',
  'LBL_RELATIONSHIP_NEW_DATABLOCK' => 'Nuevo bloque de datos',
  'LBL_RELATIONSHIP_DATABLOCK_ORIG' => 'Bloque de datos origen',
  'LBL_RELATIONSHIP_DATABLOCK_DEST' => 'Bloque de datos destino',

  // Flows
  'LBL_FLOWS' => 'Flujos de acciones',
  'LBL_FLOW' => 'Flujo de acciones',
  'LBL_FLOW_RECEIPT' => 'Respuesta automática',
  'LBL_FLOW_MAIN' => 'Principal',
  'LBL_FLOW_ONERROR' => 'Error',

  // Flow -> Action
  'LBL_ACTION' => 'Acción',
  'LBL_ACTION_ADD' => 'Añadir acción',
  'LBL_ACTION_NEW' => 'Nueva acción',
  'LBL_ACTION_TERMINAL' => 'Acción final',
  'LBL_ACTION_TERMINAL_ADD' => 'Añadir acción final',
  'LBL_ACTION_TERMINAL_NEW' => 'Nueva acción final',
  'LBL_ACTION_SELECT' => 'Seleccionar acción',
  'LBL_ACTION_TO_VIEW_DETAILS' => 'Seleccione una acción para ver sus detalles.',
  'LBL_ACTION_CONTINUE' => 'Continuar',
  'LBL_ACTION_BACK' => 'Atrás',
  'LBL_ACTION_NAME' => 'Nombre',
  'LBL_ACTION_CATEGORY' => 'Categoría',
  'LBL_ACTION_PARAMETERS' => 'Parámetros',
  'LBL_ACTION_ACTIONS' => 'Acciones',
  'LBL_ACTIONS_SHOW_ALL' => 'Mostrar todas las acciones',
  'LBL_ACTIONS_SHOW_ALL_DESC' => 'Muestra todas las acciones definidas en el formulario, incluyendo las acciones creadas de forma automática.',
  'LBL_ACTION_NO_PARAMS' => 'Sin parámetros',
  'LBL_ACTION_PARAM_SELECT_NO_OPTION' => '-- Seleccionar --',
  'LBL_ACTION_PARAM_CRM_RECORD_MODULE' => 'Módulo',
  'LBL_ACTION_PARAM_CRM_RECORD_RECORD' => 'Registro',
  'LBL_ACTION_PARAM_OPTION_SELECTOR_OPTION' => 'Opción',
  'LBL_ACTION_PARAM_MISSING_MESSAGE' => 'Hay parámetros obligatorios sin valor. Revise la configuración de la acción y asigne valores a los parámetros requeridos antes de guardar.',
  'LBL_ACTION_CONDITION_TEXT' => 'Esta acción solo se ejecutará si se cumple la condición.',
  'LBL_ACTION_CONTINUE_ON_ERROR' => 'Continuar en caso de error',
  'LBL_ACTION_CONTINUE_ON_ERROR_DESC' => 'Si se activa, el flujo continuará incluso si la acción falla.',

  // Conditions (Datablock field validations & Actions)
  'LBL_CONDITION' => 'Condición para la ejecución',
  'LBL_CONDITION_SUMMARY' => 'Condición',
  'LBL_APPLY_CONDITION' => 'Condicionar al valor de otro campo',
  'LBL_CONDITION_FIELD_NAME' => 'Campo',
  'LBL_CONDITION_VALUE' => 'Valor',

  // Layout 
  'LBL_LAYOUT_SETTINGS' => 'Configuración',
  'LBL_LAYOUT_FORM_DESIGN' => 'Diseño del formulario',
  'LBL_LAYOUT_PREVIEW' => 'Previsualización',
  'LBL_LAYOUT_HEADER' => 'Cabecera',
  'LBL_LAYOUT_FOOTER' => 'Pie',

  // Layout -> Theme
  'LBL_THEME_GENERAL' => 'General',
  'LBL_THEME_WEB_TITLE_TEXT' => 'Título de la página',
  'LBL_THEME_WEB_TITLE_VALUE' => 'Formulario Web Avanzado',
  'LBL_THEME_SUBMIT_BUTTON_TEXT' => 'Texto del botón de envío',
  'LBL_THEME_SUBMIT_BUTTON_TEXT_VALUE' => 'Enviar',
  'LBL_THEME_MAIN_COLORS' => 'Colores',
  'LBL_THEME_PRIMARY_COLOR' => 'Principal',
  'LBL_THEME_PAGE_BG_COLOR' => 'Fondo de página',
  'LBL_THEME_FORM_BG_COLOR' => 'Fondo del formulario',
  'LBL_THEME_TYPOGRAPGY_TEXT' => 'Formato del texto',
  'LBL_THEME_FONT_FAMILY' => 'Fuente',
  'LBL_THEME_FONT_SIZE' => 'Tamaño',
  'LBL_THEME_TEXT_COLOR' => 'Color',
  'LBL_THEME_FIELDS_LABELS' => 'Campos y etiquetas',
  'LBL_THEME_BORDERS' => 'Bordes y sombras',
  'LBL_THEME_BORDER_RADIUS_CONTAINER' => 'Redondeo (cajas)',
  'LBL_THEME_BORDER_RADIUS_CONTROLS' => 'Redondeo (botones/controles)',
  'LBL_THEME_BORDER_WIDTH' => 'Ancho del borde',
  'LBL_THEME_BORDER_COLOR' => 'Color del borde',
  'LBL_THEME_FLOATING_LABELS' => 'Etiquetas flotantes',
  'LBL_THEME_FLOATING_LABELS_DESC' => 'Si se activa, las etiquetas se colocan dentro de los campos y se moverán al escribir.',
  'LBL_THEME_LABEL_WEIGHT_BOLD' => 'Etiquetas en negrita',
  'LBL_THEME_SHADOW_INTENSITY' => 'Sombreado',
  'LBL_THEME_INPUT_STYLE' => 'Estilo de campos',
  'LBL_THEME_STRUCTURE' => 'Estructura y distribución',
  'LBL_THEME_FORM_WIDTH' => 'Ancho máximo',
  'LBL_THEME_FIELD_SPACING' => 'Espaciado entre campos',
  'LBL_THEME_SUBMIT_FULL_WIDTH' => 'Ancho completo del botón de envío',
  'LBL_THEME_EQUAL_HEIGHT_SECTIONS' => 'Igualar altura de secciones',
  'LBL_THEME_SECTIONS_PER_ROW' => 'Columnas (secciones)',
  'LBL_THEME_FIELDS_PER_ROW' => 'Columnas (campos)',
  'LBL_THEME_PER_ROW_DESC' => 'El número de columnas se adaptará automáticamente al ancho de la pantalla.',
  'LBL_THEME_ADVANCED' => 'Avanzado',
  'LBL_THEME_ADVANCED_NO_ADMIN_DESC' => 'La configuración avanzada (CSS y JS) está restringida a usuarios administradores.',
  'LBL_THEME_CUSTOM_CSS' => 'CSS personalizado',
  'LBL_THEME_CUSTOM_CSS_DESC' => 'Se inyectará en un bloque <style>.',
  'LBL_THEME_CUSTOM_JS' => 'JS personalizado',
  'LBL_THEME_CUSTOM_JS_DESC' => 'Se procesará cuando todo el formulario se haya cargado.',
  
  'LBL_THEME_CLOSED_FORM' => 'Aviso: Formulario cerrado',
  'LBL_THEME_CLOSED_FORM_DESC' => 'Aviso que aparecerá en el formulario cuando ya no acepte respuestas.',
  'LBL_THEME_CLOSED_FORM_TITLE' => 'Título de aviso',
  'LBL_THEME_CLOSED_FORM_TITLE_VALUE' => '⛔ Formulario cerrado',
  'LBL_THEME_CLOSED_FORM_TEXT' => 'Texto de aviso',
  'LBL_THEME_CLOSED_FORM_TEXT_VALUE' => 'Este formulario no acepta respuestas.',

  'LBL_THEME_PROCESSED_FORM' => 'Mensaje: Datos procesados',
  'LBL_THEME_PROCESSED_FORM_DESC' => 'Mensaje que se mostrará por defecto al procesar correctamente una respuesta al formulario. No se mostrará si se define una acción final.',
  'LBL_THEME_PROCESSED_FORM_TITLE' => 'Título del mensaje',
  'LBL_THEME_PROCESSED_FORM_TITLE_VALUE' => 'Procesado',
  'LBL_THEME_PROCESSED_FORM_TEXT' => 'Texto del mensaje',
  'LBL_THEME_PROCESSED_FORM_TEXT_VALUE' => 'Sus datos han sido procesados correctamente.',
  
  'LBL_THEME_RECEIPT_FORM' => 'Mensaje: Datos recibidos',
  'LBL_THEME_RECEIPT_FORM_DESC' => 'Mensaje que se mostrará por defecto al recibir una respuesta y guardarla para procesarla más tarde. Este mensaje no se mostrará si se define una acción final.',
  'LBL_THEME_RECEIPT_FORM_TITLE' => 'Título del mensaje',
  'LBL_THEME_RECEIPT_FORM_TITLE_VALUE' => 'Recibido',
  'LBL_THEME_RECEIPT_FORM_TEXT' => 'Texto del mensaje',
  'LBL_THEME_RECEIPT_FORM_TEXT_VALUE' => 'Sus datos han sido recibidos correctamente y serán procesados lo antes posible.',

  'LBL_THEME_RESET_BUTTON' => 'Configuración por defecto',
  
  // Layout -> Sections
  'LBL_SECTIONS' => 'Secciones',
  'LBL_SECTION_ADD' => 'Añadir sección',
  'LBL_SECTION_NEW' => 'Nueva sección',
  'LBL_SECTION_CONFIG' => 'Configuración',
  'LBL_SECTION_CONTENT' => 'Contenido',
  'LBL_SECTION_TITLE' => 'Título',
  'LBL_SECTION_SUBTITLE' => 'Subtítulo',
  'LBL_SECTION_NO_TITLE' => '< Sin título >',
  'LBL_SECTION_SHOW_TITLE' => 'Mostrar título',
  'LBL_SECTION_CONTAINER' => 'Contenedor visual',
  'LBL_SECTION_IS_COLLAPSIBLE' => 'Colapsable',
  'LBL_SECTION_ISCOLLAPSED' => 'Colapsado inicialmente',
  'LBL_SECTION_MOVE_ELEMENT_NO_OPTION' => 'Mover a...',
  'LBL_SECTION_EMPTY_DESC' => 'Esta sección está vacía. Mueva bloques aquí desde otras secciones.',

  // Form generation
  'LBL_CODE_GENERATING' => 'Generando el código...',
  'LBL_CODE_GENERATING_ERROR' => 'Error generando el código',
  'LBL_CODE_LOADING' => 'Cargando el código...',
  'LBL_CODE_LOADING_ERROR' => 'Error de conexión',

  'LBL_HONEYPOT_LABEL' => 'Mantenga este campo vacío',
  'LBL_REQUIRED_FIELD_MESSAGE' => 'Es necesario informar este campo',
  
  'LBL_PREVIEW_RIBBON' => 'Previsualización',
  'LBL_PREVIEW_LOADING' => 'Cargando...',
  'LBL_PREVIEW_LOAD_ERROR' => 'Error cargando la previsualización',
  'LBL_PREVIEW_DESC' => 'Visualización real generada por el servidor.',
  'LBL_PREVIEW_MODE_ALERT' => 'El formulario está en modo previsualización. No está activado el envío de datos.',
  'LBL_PREVIEW_TOOLBAR' => 'Previsualización',
  'LBL_PREVIEW_ACTIVE_TEXT' => 'Activo',
  'LBL_PREVIEW_INACTIVE_TEXT' => 'Inactivo',
  'LBL_PREVIEW_IN_NEW_TAB' => 'Previsualizar en pestaña nueva',

  'LBL_FORM_PUBLISH_OPTIONS' => 'Opciones de publicación',
  'LBL_FORM_PUBLISH_LINK' => 'Enlace público',
  'LBL_FORM_PUBLISH_LINK_DESC' => 'Copie y comparta este enlace para acceder al formulario.',
  'LBL_FORM_PUBLISH_IFRAME' => 'Incrustar (iframe)',
  'LBL_FORM_PUBLISH_IFRAME_DESC' => 'Copie este código para incrustar el formulario en un sitio web externo manteniéndolo alojado en SinergiaCRM.',
  'LBL_FORM_PUBLISH_HTML' => 'Código HTML',
  'LBL_FORM_PUBLISH_HTML_DESC' => 'Utilice este código para alojar el formulario en un sitio web externo.',

  'LBL_COPY_TO_CLIPBOARD_DONE' => 'Copiado al portapapeles',

  'LBL_RATE_ARIA' => 'Valore con un %s',

  // Errors
  'LBL_ERROR_DATABLOCK_IS_INVALID' => 'El bloque de datos tiene errores',
  'LBL_ERROR_DATABLOCK_NAME' => 'El nombre interno del bloque de datos está vacío',
  'LBL_ERROR_DATABLOCK_TITLE' => 'El bloque de datos debe tener un nombre público',
  'LBL_ERROR_NO_DATABLOCKS' => 'Debe definirse al menos un bloque de datos para continuar',
  'LBL_ERROR_FIELD_IS_INVALID' => 'El campo tiene errores',
  'LBL_ERROR_FIELD_NAME' => 'El nombre interno del campo está vacío',
  'LBL_ERROR_FIELD_LABEL' => 'No existe etiqueta para el campo',
  'LBL_ERROR_FIELD_TYPE' => 'No se ha definido el tipo de campo o de editor en el formulario',
  'LBL_ERROR_FIELD_OPTIONS' => 'Desplegable sin opciones definidas',
  'LBL_ERROR_FIELD_FIXED_EMPTY' => 'Campo fijo sin valor asignado',
  'LBL_OK_FIELD_IS_VALID' => 'El campo es correcto',

  // -- SUBPANELS --
  'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_RESPONSES_TITLE' => 'Respuestas a formularios',

  // -- HOOK ACTIONS --
  // Generic 
  'LBL_CUSTOM_ACTION_DATABLOCK_PARAM_TEXT' => 'Bloque de datos',
  'LBL_CUSTOM_ACTION_DATABLOCK_PARAM_DESC' => 'Seleccione el bloque de datos que será utilizado como parámetro en la acción',

  // SaveRecordAction
  'LBL_SAVE_RECORD_ACTION_TITLE' => 'Guardar registro',
  'LBL_SAVE_RECORD_ACTION_DESC' => 'Guarda o actualiza un registro a partir de los datos del formulario',
  'LBL_SAVE_RECORD_ACTION_DUPLICATE_RULE_MATCHED_TEXT' => 'Coincidencia por campos',

  // RelateRecordsAction
  'LBL_RELATE_RECORDS_ACTION_TITLE' => 'Crear relación',
  'LBL_RELATE_RECORDS_ACTION_DESC' => 'Crea una relación entre dos registros',
  'LBL_RELATE_RECORDS_ACTION_TARGET_OBJECT_TEXT' => 'Destino de la relación',
  'LBL_RELATE_RECORDS_ACTION_TARGET_OBJECT_DESC' => 'El bloque de datos o registro destino de la relación a guardar',
  'LBL_RELATE_RECORDS_ACTION_OPTION_BLOCK_TEXT' => 'Bloque de datos destino',
  'LBL_RELATE_RECORDS_ACTION_OPTION_VALUE_TEXT' => 'ID del registro destino',
  'LBL_RELATE_RECORDS_ACTION_RELATIONSHIP_TEXT' => 'Relación a actualizar',
  'LBL_RELATE_RECORDS_ACTION_RELATIONSHIP_DESC' => 'El nombre interno de la relación que enlaza con el bloque de datos destino',

  // AddToTargetListAction
  'LBL_ADD_TO_TARGET_LIST_ACTION_TITLE' => 'Añadir a Lista de Público Objetivo',
  'LBL_ADD_TO_TARGET_LIST_ACTION_DESC' => 'Añade el registro procesado (persona, interesado, usuario u organización) a una Lista de Público Objetivo existente',
  'LBL_ADD_TO_TARGET_LIST_ACTION_CONTACT_TO_ADD_TEXT' => 'Destinatario',
  'LBL_ADD_TO_TARGET_LIST_ACTION_CONTACT_TO_ADD_DESC' => 'Indica el bloque de datos que contiene el destinatario que se va a añadir a la Lista de Público Objetivo',
  'LBL_ADD_TO_TARGET_LIST_ACTION_TARGET_LIST_RECORD_TEXT' => 'Lista de Público Objetivo (LPO)',
  
  // SendEmailToDataBlockAction
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_TITLE' => 'Enviar correo al remitente del formulario',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_DESC' => 'Envía un correo electrónico al registro procesado (persona, interesado, usuario u organización) contenido en un bloque de datos',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_RECIPIENT_BLOCK_TEXT' => 'Destinatario',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_RECIPIENT_BLOCK_DESC' => 'Indica el bloque de datos que contiene el destinatario del correo electrónico',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_TEMPLATE_TEXT' => 'Plantilla de correo electrónico',

  // SendEmailToAddressAction
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_TITLE' => 'Enviar correo a una dirección',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_DESC' => 'Envía un correo electrónico a una dirección de correo electrónico concreta',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_EMAIL_TEXT' => 'Correo electrónico',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_TEMPLATE_TEXT' => 'Plantilla de correo electrónico',
  
  
  // SendEmailToAssignedAction
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_TITLE' => 'Enviar correo a un usuario asignado',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_DESC' => 'Envia un correo electrónico al usuario asignado del formulario o de un registro',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_SOURCE_TEXT' => 'Origen del usuario asignado',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_SOURCE_DESC' => 'Indica el registro del cual se obtendrá el usuario asignado',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_OWNER_TEXT' => 'Formulario',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RESPONSE_TEXT' => 'Respuesta del formulario',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_DATABLOCK_TEXT' => 'Bloque de datos',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RECORD_TEXT' => 'Registro fijo',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RELATE_TEXT' => 'Campo relacionado',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_TEMPLATE_TEXT' => 'Plantilla de correo electrónico',
  
  // RedirectAction
  'LBL_REDIRECT_ACTION_TITLE' => 'Ir a una página web',
  'LBL_REDIRECT_ACTION_DESC' => 'Redirecciona el navegador del usuario final a una página web concreta',
  'LBL_REDIRECT_ACTION_URL_TEXT' => 'URL de redirección',
  'LBL_REDIRECT_ACTION_URL_DESC' => 'Indica la dirección de la página web a la que redirigir al usuario final. Debe incluir el protocolo (http:// o https://).',
  'LBL_REDIRECT_ACTION_METHOD_TEXT' => 'Método de envío',
  'LBL_REDIRECT_ACTION_METHOD_DESC' => 'Indica, en caso de que sea necesario, cómo se deben enviar los datos a la página de redirección.',
  'LBL_REDIRECT_ACTION_METHOD_GET_TEXT' => 'GET (los datos se añaden a la URL de redirección)',
  'LBL_REDIRECT_ACTION_METHOD_POST_TEXT' => 'POST (los datos se envían mediante un formulario oculto)',
  'LBL_REDIRECT_ACTION_FIELDS_TEXT' => 'Campos a enviar',
  'LBL_REDIRECT_ACTION_FIELDS_DESC' => 'Indica los campos a enviar a la URL de redirección. En caso de no necesitar enviar datos debe dejarse en blanco.',
  'LBL_REDIRECT_ACTION_REDIRECTING' => 'Redireccionando...',
  'LBL_REDIRECT_ACTION_SUBMIT_BUTTON' => 'Pulse aquí para continuar',

  // RedirectSummaryPageAction
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE' => 'Mostrar resumen de datos',
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_DESC' => 'Redirecciona el navegador del usuario final a una página en la que se muestran los datos facilitados',
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE_TEXT' => 'Título de la página',
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE_DEFAULT' => 'Resumen de los datos facilitados',

  // CheckSessionAction
  'LBL_CHECK_SESSION_ACTION_TITLE' => 'Verificar sesión activa y permisos',
  'LBL_CHECK_SESSION_ACTION_DESC' => 'Bloquea el procesamiento del formulario si no hay una sesión de usuario activa o si el usuario no tiene los permisos para crear los registros asociados al formulario',
  'LBL_CHECK_SESSION_ACTION_SESSION_ERROR_MSG_TEXT' => 'Mensaje por sesión no activa',
  'LBL_CHECK_SESSION_ACTION_SESSION_ERROR_MSG_TEXT_DEFAULT' => 'Acceso no autorizado. Por favor, inicie sesión para continuar.',
  'LBL_CHECK_SESSION_ACTION_PERMISSIONS_ERROR_MSG_TEXT' => 'Mensaje por falta de permisos',
  'LBL_CHECK_SESSION_ACTION_PERMISSIONS_ERROR_MSG_TEXT_DEFAULT' => 'Acceso no autorizado. No posee los permisos necesarios para continuar.',
  'LBL_CHECK_SESSION_ACTION_CHECKING' => 'Verificando acceso y permisos...',
  'LBL_CHECK_SESSION_ACTION_DENIED_TITLE' => '🚫 Acceso denegado',
  'LBL_CHECK_SESSION_ACTION_LOGIN' => 'Iniciar sesión',
  'LBL_CHECK_SESSION_ACTION_ACTIVE_SESSION' => 'Sesión activa',


  // -- DEFERRED ACTIONS --
  // PaymentRouterAction
  'LBL_PAYMENT_ROUTER_ACTION_TITLE' => 'Realizar pago en plataforma externa',
  'LBL_PAYMENT_ROUTER_ACTION_DESC' => 'Procesa el pago correspondiente a un bloque de datos en una plataforma externa.',
  'LBL_PAYMENT_ROUTER_ACTION_PAYMENT_COMMITMENT_TEXT' => 'Compromiso de Pago',
  'LBL_PAYMENT_ROUTER_ACTION_PAYMENT_COMMITMENT_DESC' => 'Selecciona el bloque de datos con el Compromiso de Pago para realizar su pago en la plataforma externa.',


  // -- VALIDATOR ACTIONS --
  // RegexValidatorAction
  'LBL_REGEX_VALIDATOR_ACTION_TITLE' => 'Validador Regex',
  'LBL_REGEX_VALIDATOR_ACTION_DESC' => 'Valida un campo según una expresión regular',
  'LBL_REGEX_VALIDATOR_ACTION_PATTERN_TEXT' => 'Expresión regular',
  'LBL_REGEX_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El formato del campo no es válido',

  // EmailValidatorAction
  'LBL_EMAIL_VALIDATOR_ACTION_TITLE' => 'Validador de correo electrónico',
  'LBL_EMAIL_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un correo electrónico válido',
  'LBL_EMAIL_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'La dirección de correo electrónico no es válida',

  // DniValidatorAction
  'LBL_DNI_VALIDATOR_ACTION_TITLE' => 'Validador de DNI/NIF',
  'LBL_DNI_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un DNI/NIF español válido',
  'LBL_DNI_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El DNI/NIF no es válido',

  // CifValidatorAction
  'LBL_CIF_VALIDATOR_ACTION_TITLE' => 'Validador de NIF de persona jurídica',
  'LBL_CIF_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un NIF español de persona jurídica válido',
  'LBL_CIF_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El NIF no es válido',

  // NieValidatorAction
  'LBL_NIE_VALIDATOR_ACTION_TITLE' => 'Validador de NIE',
  'LBL_NIE_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un NIE español válido',
  'LBL_NIE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El NIE no es válido',

  // CatSalutCipValidatorAction
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_TITLE' => 'Validador de CIP (CatSalut)',
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un Código de Identificación Personal (CIP) de CatSalut válido',
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El CIP no es válido',

  // NafValidatorAction
  'LBL_NAF_VALIDATOR_ACTION_TITLE' => 'Validador de NUSS',
  'LBL_NAF_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un número de afiliación a la Seguridad Social (NUSS) válido',
  'LBL_NAF_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El número de afiliación a la Seguridad Social no es válido',

  // NumericValidatorAction
  'LBL_NUMERIC_VALIDATOR_ACTION_TITLE' => 'Validador numérico',
  'LBL_NUMERIC_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un valor numérico y que, opcionalmente, se encuentre dentro de un rango',
  'LBL_NUMERIC_VALIDATOR_ACTION_MIN_TEXT' => 'Valor mínimo (opcional)',
  'LBL_NUMERIC_VALIDATOR_ACTION_MAX_TEXT' => 'Valor máximo (opcional)',
  'LBL_NUMERIC_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El valor debe ser numérico y estar entre los valores permitidos',

  // TextLengthValidatorAction
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_TITLE' => 'Validador de longitud de texto',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un texto con una longitud dentro de un rango',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_MIN_LENGTH_TEXT' => 'Longitud mínima (opcional)',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_MAX_LENGTH_TEXT' => 'Longitud máxima (opcional)',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El texto debe tener una longitud entre los valores permitidos',

  // IbanValidatorAction
  'LBL_IBAN_VALIDATOR_ACTION_TITLE' => 'Validador de IBAN',
  'LBL_IBAN_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un IBAN válido',
  'LBL_IBAN_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El IBAN no es válido',

  // PhoneValidatorAction
  'LBL_PHONE_VALIDATOR_ACTION_TITLE' => 'Validador de teléfono',
  'LBL_PHONE_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un número de teléfono español válido (al menos 9 dígitos numéricos)',
  'LBL_PHONE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El teléfono no es válido',

  // SpanishZipValidatorAction
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_TITLE' => 'Validador de código postal',
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga un código postal español válido (5 dígitos numéricos)',
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El código postal no es válido',

  // TrueValidatorAction
  'LBL_TRUE_VALIDATOR_ACTION_TITLE' => 'Selección obligada',
  'LBL_TRUE_VALIDATOR_ACTION_DESC' => 'Asegura que una casilla esté marcada (por ejemplo, la aceptación de condiciones)',
  'LBL_TRUE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'Es necesario aceptar este campo para continuar',

  // AgeValidatorAction
  'LBL_AGE_VALIDATOR_ACTION_TITLE' => 'Validador de edad',
  'LBL_AGE_VALIDATOR_ACTION_DESC' => 'Calcula la edad a partir de la fecha de nacimiento y verifica que esté entre la mínima y la máxima permitidas',
  'LBL_AGE_VALIDATOR_ACTION_MIN_YEARS_TEXT' => 'Edad mínima (opcional)',
  'LBL_AGE_VALIDATOR_ACTION_MAX_YEARS_TEXT' => 'Edad máxima (opcional)',
  'LBL_AGE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'La edad no está en el rango permitido',

  // UrlValidatorAction
  'LBL_URL_VALIDATOR_ACTION_TITLE' => 'Validador de URL',
  'LBL_URL_VALIDATOR_ACTION_DESC' => 'Valida que un campo contenga una URL válida',
  'LBL_URL_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'La URL no es válida',
);
