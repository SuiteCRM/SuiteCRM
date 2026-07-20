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
  'LBL_ASSIGNED_TO_ID' => 'Assignat a (ID)',
  'LBL_ASSIGNED_TO_NAME' => 'Assignat a',
  'LBL_ASSIGNED_TO' => 'Assignat a',
  'LBL_LIST_ASSIGNED_TO_NAME' => 'Assignat a',
  'LBL_LIST_ASSIGNED_USER' => 'Assignat a',
  'LBL_CREATED' => 'Creat per',
  'LBL_CREATED_USER' => 'Creat per',
  'LBL_CREATED_ID' => 'Creat per (ID)',
  'LBL_MODIFIED' => 'Modificat per',
  'LBL_MODIFIED_NAME' => 'Modificat per',
  'LBL_MODIFIED_USER' => 'Modificat per',
  'LBL_MODIFIED_ID' => 'Modificat per (ID)',
  'LBL_SECURITYGROUPS' => 'Grups de seguretat',
  'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grups de seguretat',
  'LBL_ID' => 'ID',
  'LBL_DATE_ENTERED' => 'Data de Creació',
  'LBL_DATE_MODIFIED' => 'Data de Modificació',
  'LBL_DESCRIPTION' => 'Descripció',
  'LBL_DELETED' => 'Suprimit',
  'LBL_NAME' => 'Nom',
  'LBL_LIST_NAME' => 'Nom',
  'LBL_EDIT_BUTTON' => 'Edita',
  'LBL_QUICKEDIT_BUTTON' => '↙ Edita',
  'LBL_REMOVE' => 'Desvincula',
  'LBL_ASCENDING' => 'Ascendent',
  'LBL_DESCENDING' => 'Descendent',

  'LBL_LIST_FORM_TITLE' => 'Llista de Formularis Web Avançats',
  'LBL_MODULE_NAME' => 'Formularis Web Avançats',
  'LBL_MODULE_TITLE' => 'Formularis Web Avançats',
  'LBL_HOMEPAGE_TITLE' => 'Els meus Formularis Web Avançats',
  'LNK_NEW_RECORD' => 'Crea un Formulari Web Avançat',
  'LNK_LIST' => 'Mostra els Formularis Web Avançats',
  'LNK_IMPORT_stic_AWF_Forms' => 'Importa Formularis Web Avançats',
  'LBL_SEARCH_FORM_TITLE' => 'Cerca Formularis Web Avançats',
  'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
  'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activitats',
  'LBL_NEW_FORM_TITLE' => 'Nou Formulari Web Avançat',

  'LBL_DEFAULT_PANEL' => 'Dades generals',
  'LBL_PANEL_RECORD_DETAILS' => 'Detalls del registre',

  // -- MODULE FIELDS --
  'LBL_STATUS' => 'Estat',
  'LBL_START_DATE' => "Data d'inici",
  'LBL_END_DATE' => 'Data de finalització',
  'LBL_PUBLIC_URL' => 'URL',
  'LBL_PROCESSING_MODE' => 'Mode de processament',
  'LBL_PROCESSING_MODE_DESC' => "Defineix com es gestionen les respostes rebudes. En mode síncron les respostes s'emmagatzemen i es processen en rebre's. En mode asíncron les respostes s'emmagatzemen en rebre's i es processen posteriorment mitjançant un procés en segon pla, la qual cosa pot ser d'interès per optimitzar el rendiment del sistema en casos de gran afluència.",
  'LBL_FORM_TYPE' => 'Tipus de formulari',
  'LBL_FORM_TYPE_DESC' => "Defineix el públic destinatari del formulari. Els formularis web són accessibles per a qualsevol persona mentre que els formularis del CRM només poden ser emplenats per usuaris autenticats.",
  'LBL_CONFIGURATION' => 'Configuració',
  'LBL_ANALYTICS_VIEWS' => 'Visites totals',
  'LBL_ANALYTICS_BLOCKED' => 'Visites bloquejades (no públic)',
  'LBL_ANALYTICS_SUBMISSIONS' => 'Respostes vàlides',
  'LBL_ANALYTICS_SPAM' => 'Spam detectat',
  'LBL_ANALYTICS_REFERRERS' => 'Fonts de tràfic (dominis)',

  // -- WIZARD --
  // Main actions
  'LBL_WIZARD_PREVIOUS' => '<< Anterior',
  'LBL_WIZARD_NEXT' => 'Següent >>',
  'LBL_WIZARD_FINISH' => 'Finalitza',
  'LBL_WIZARD_SAVE' => 'Desa',
  'LBL_WIZARD_SAVED_DRAFT' => 'Formulari desat',
  'LBL_WIZARD_SHOW_DEBUG_INFO' => 'Mostra els detalls',

  // Edit Warning
  'LBL_WIZARD_FORM_EDIT_WARNING_TITLE' => '⚠️ Precaució en editar!',
  'LBL_WIZARD_FORM_EDIT_WARNING_PUBLIC' => 'Aquest formulari és públic: algú podria estar emplenant-lo en aquest mateix moment.',
  'LBL_WIZARD_FORM_EDIT_WARNING_RESPONSES' => "Hi ha %s respostes enregistrades: canviar l'estructura del formulari podria causar inconsistències.",
  'LBL_WIZARD_FORM_EDIT_WARNING_PROCEED' => 'En cas de voler realitzar canvis significatius es recomana valorar la possibilitat de duplicar el formulari.',
  
  // Corrupted form warning
  'LBL_WIZARD_FORM_CORRUPTED_WARNING_MESSAGE' => "⚠️ Error crític: No s'ha pogut carregar la configuració correctament. El formulari es mostrarà buit i s'ha bloquejat el desat automàtic per evitar pèrdues de dades.",

  // Steps
  'LBL_WIZARD_TITLE_STEP1' => 'Informació general',
  'LBL_WIZARD_DESC_STEP1' => 'Definició de les propietats generals del formulari.',
  'LBL_WIZARD_TITLE_STEP2' => 'Estructura i camps',
  'LBL_WIZARD_DESC_STEP2' => 'Definició del contingut del formulari. Els blocs de dades poden vincular-se a mòduls de SinergiaCRM o funcionar de forma independent. A més, és possible configurar validacions de dades, detecció de duplicats, etc.',
  'LBL_WIZARD_TITLE_STEP2_START' => 'Comenceu a construir el formulari',
  'LBL_WIZARD_TITLE_STEP3' => 'Lògica i automatismes',
  'LBL_WIZARD_DESC_STEP3' => "Definició de les accions que s'executaran quan es rebi una resposta: crear o actualitzar registres, enviar correus electrònics, redireccionar a pàgines web, etc.",
  'LBL_WIZARD_TITLE_STEP4' => 'Maquetació',
  'LBL_WIZARD_DESC_STEP4' => "Disseny de l'aparença del formulari. Permet definir l'estil visual, organitzar els blocs de dades, personalitzar la capçalera i el peu del formulari, etc.",
  'LBL_WIZARD_TITLE_STEP5' => 'Resum i publicació',
  'LBL_WIZARD_DESC_STEP5' => "Revisió i activació del formulari. Permet activar la recepció de respostes, obtenir l'enllaç d'accés públic o descarregar el codi HTML per integrar-lo en una pàgina web externa.",

  // General buttons
  'LBL_BUTTON_ADD' => 'Afegeix',
  'LBL_BUTTON_EDIT' => 'Edita',
  'LBL_BUTTON_DELETE' => 'Elimina',
  'LBL_BUTTON_MOVE_UP' => 'Mou amunt',
  'LBL_BUTTON_MOVE_DOWN' => 'Mou avall',
  'LBL_BUTTON_DUPLICATE' => 'Duplica',
  'LBL_BUTTON_RELOAD' => 'Recarrega',
  'LBL_BUTTON_COPY' => 'Copia',
  'LBL_BUTTON_OPEN' => 'Obre',
  'LBL_BUTTON_DOWNLOAD' => 'Descarrega',

  // General editors
  'LBL_SELECT_PLACEHOLDER' => 'Seleccioneu un element...',
  'LBL_SELECT_WRITE_TO_SEARCH' => 'Escriviu per filtrar la llista...',
  'LBL_SELECT_NO_RESULTS' => 'No hi ha coincidències',

  // DataBlocks
  'LBL_DATABLOCK' => 'Bloc de dades',
  'LBL_DATABLOCKS' => 'Blocs de dades',
  'LBL_DATABLOCK_DETACHED' => 'Dades no enllaçades',
  'LBL_DATABLOCK_ADD' => 'Afegeix un bloc de dades',
  'LBL_DATABLOCK_ADD_TITLE' => 'Configureu un bloc de dades relacionat amb un mòdul del sistema',
  'LBL_DATABLOCK_ADD_UNLINKED' => 'Afegeix un bloc de dades no enllaçat',
  'LBL_DATABLOCK_ADD_UNLINKED_TITLE' => 'Configureu un bloc de dades sense relacionar-lo amb cap mòdul del sistema',
  'LBL_DATABLOCK_NEW' => 'Nou bloc de dades',
  'LBL_DATABLOCK_NEW_UNLINKED' => 'Nou bloc de dades no enllaçat',
  'LBL_DATABLOCK_MODULE' => 'Mòdul',
  'LBL_DATABLOCK_NAME' => 'Nom',
  'LBL_DATABLOCK_INTERNAL_NAME' => 'Nom intern',

  // DataBlock -> Fields
  'LBL_FIELDS' => 'Camps',
  'LBL_FIELDS_FORM_TAB' => 'Formulari',
  'LBL_FIELDS_FORM_TAB_DESC' => "Camps que s'inclouran al formulari públic.",
  'LBL_FIELDS_FIXED_TAB' => 'Servidor',
  'LBL_FIELDS_FIXED_TAB_DESC' => "Camps amb valor prefixat que s'incorporaran a les dades rebudes dels formularis.",
  'LBL_FIELD_FORM' => 'Camp al formulari',
  'LBL_FIELD_FORM_ADD' => 'Afegeix un camp',
  'LBL_FIELD_FORM_NEW' => 'Nou camp',
  'LBL_FIELD_UNLINKED' => 'Camp no enllaçat',
  'LBL_FIELD_UNLINKED_ADD' => 'Afegeix un camp no enllaçat',
  'LBL_FIELD_UNLINKED_NEW' => 'Nou camp no enllaçat',
  'LBL_FIELD_FIXED' => 'Valor fix',
  'LBL_FIELD_FIXED_ADD' => 'Afegeix un camp de servidor',
  'LBL_FIELD_FIXED_NEW' => 'Nou camp de servidor',
  'LBL_FIELD_CONVERT_TO_FIELD_FORM' => 'Converteix-lo en camp de formulari',
  'LBL_FIELD_CONVERT_TO_FIELD_FIXED' => 'Converteix-lo en camp de servidor',
  'LBL_FIELD_DEFINITION' => 'Definició',
  'LBL_FIELD_DEFINITION_FORM' => 'Representació al formulari',
  'LBL_FIELD_DEFINITION_VALIDATIONS' => 'Validació de dades',

  'LBL_FIELD' => 'Camp',
  'LBL_FIELD_NAME' => 'Nom',
  'LBL_FIELD_INTERNAL_NAME' => 'Nom intern',
  'LBL_FIELDS_SHOW_ALL' => 'Mostra tots els camps',
  'LBL_FIELDS_SHOW_ALL_DESC' => "Inclou al desplegable tots els camps del mòdul encara que no es mostrin a les vistes d'edició o detall.",
  'LBL_FIELD_LABEL' => 'Etiqueta',
  'LBL_FIELD_REQUIRED_IN_FORM' => 'Camp obligatori',
  'LBL_FIELD_IN_FORM' => 'Al formulari',
  'LBL_FIELD_TYPE_IN_FORM' => "Tipus d'entrada",
  'LBL_FIELD_SUBTYPE_IN_FORM' => "Tipus d'editor",
  'LBL_FIELD_PLACEHOLDER' => 'Text de fons',
  'LBL_FIELD_DESCRIPTION' => "Text d'ajuda",
  'LBL_FIELD_ADD_LINK' => 'Afegeix un enllaç',
  'LBL_FIELD_LINK_CREATE' => 'Crea un enllaç',
  'LBL_FIELD_LINK_TEXT' => "Text de l'enllaç",
  'LBL_FIELD_LINK_URL' => 'URL de destí',
  'LBL_FIELD_VALUE_TYPE' => 'Tipus de valor',
  'LBL_FIELD_VALUE_OPTIONS_LIST' => 'Llista associada',
  'LBL_FIELD_VALUE_OPTIONS_CUSTOMIZE' => 'Personalitza les opcions',
  'LBL_FIELD_VALUE_OPTIONS_CUSTOMIZED' => '(Modificat)',
  'LBL_FIELD_VALUE_OPTIONS_SELECT_REGS' => 'Selecciona els registres',
  'LBL_FIELD_VALUE_OPTIONS' => 'Valors possibles',
  'LBL_FIELD_VALUE_OPTION_SHOW_NAME' => 'Mostra els valors interns',
  'LBL_FIELD_VALUE_OPTION_NAME' => 'Valor intern',
  'LBL_FIELD_VALUE_OPTION_ORIGINAL_LABEL' => 'Text original',
  'LBL_FIELD_VALUE_OPTION_LABEL' => 'Text',
  'LBL_FIELD_VALUE_OPTION_SHOW' => 'Mostrar',
  'LBL_FIELD_VALUE_OPTION_ACTIONS' => 'Accions',
  'LBL_FIELD_VALUE' => 'Valor',
  'LBL_FIELD_VALUE_RELATIVE_DATE_OPTION' => 'Data relativa',
  'LBL_FIELD_VALUE_RELATIVE_DATE_OPTION_DESC' => 'Permet indicar una data relativa a la data de les respostes rebudes.',
  'LBL_FIELD_VALUE_RELATIVE_DATE_CUSTOM' => 'Data relativa personalitzada',
  'LBL_FIELD_VALUE_RELATIVE_DATE_CUSTOM_DESC' => 'Expressió en anglès compatible amb la funció strtotime() de PHP. Exemples: tomorrow 14:00, next monday, +2 weeks, first day of next month.',
  'LBL_FIELD_ACTIONS' => 'Accions',
  
  // DataBlock -> Field -> Validations
  'LBL_FIELD_VALIDATION' => 'Validació',
  'LBL_FIELD_VALIDATOR' => 'Acció de validació',
  'LBL_FIELD_VALIDATION_ADD' => 'Afegeix una validació',
  'LBL_FIELD_VALIDATION_NEW' => 'Nova validació',
  'LBL_FIELD_VALIDATION_EDIT' => 'Edita la validació',
  'LBL_FIELD_VALIDATION_PARAMETERS' => 'Paràmetres',
  'LBL_FIELD_VALIDATION_ERROR_MESSAGE' => "Missatge d'error",
  'LBL_FIELD_VALIDATION_ACTIONS' => 'Accions',
  'LBL_FIELD_ACTIVE_VALIDATIONS' => 'Validacions actives',

  // DataBlock -> Duplicate checks
  'LBL_DUPLICATE_CHECK' => 'Detecció de duplicats',
  'LBL_DUPLICATE_CHECK_ADD' => 'Afegeix una detecció de duplicats',
  'LBL_DUPLICATE_FIELDS' => 'Camps a verificar',
  'LBL_DUPLICATE_FIELDS_SEL_FIELDS' => 'Selecciona el(s) camp(s)...',
  'LBL_ONDUPLICATE_ACTION' => 'Acció en cas de duplicat',

  // DataBlock -> Relationships
  'LBL_RELATIONSHIP' => 'Relació',
  'LBL_RELATIONSHIPS' => 'Relacions entre blocs de dades',
  'LBL_RELATIONSHIP_ADD' => 'Afegeix una relació',
  'LBL_RELATIONSHIP_NEW' => 'Nova relació entre blocs de dades',  
  'LBL_RELATIONSHIP_NO_MODULE_RELATED' => 'Sense mòdul relacionat',
  'LBL_RELATIONSHIP_NEW_DATABLOCK' => 'Nou bloc de dades',
  'LBL_RELATIONSHIP_TYPE' => 'Tipus',
  'LBL_RELATIONSHIP_INVOLVED_BLOCKS' => 'Blocs de dades involucrats',
  'LBL_AWF_FIELD_SUFFIX' => 'camp',

  // Flows
  'LBL_FLOWS' => "Fluxos d'accions",
  'LBL_FLOW' => "Flux d'accions",
  'LBL_FLOW_RECEIPT' => 'Resposta automàtica',
  'LBL_FLOW_MAIN' => 'Principal',
  'LBL_FLOW_ONERROR' => 'Error',
  'LBL_FLOW_DEFERRED_MAIN' => 'Finalitzada',

  // Flow -> Action
  'LBL_ACTION' => 'Acció',
  'LBL_ACTION_ADD' => 'Afegeix una acció',
  'LBL_ACTION_NEW' => 'Nova acció',
  'LBL_ACTION_TERMINAL' => 'Acció final',
  'LBL_ACTION_TERMINAL_ADD' => 'Afegeix una acció final',
  'LBL_ACTION_TERMINAL_NEW' => 'Nova acció final',
  'LBL_ACTION_SELECT' => 'Selecciona una acció',
  'LBL_ACTION_TO_VIEW_DETAILS' => "Seleccioneu una acció per veure'n els detalls.",
  'LBL_ACTION_CONTINUE' => 'Continua',
  'LBL_ACTION_BACK' => 'Enrere',
  'LBL_ACTION_NAME' => 'Nom',
  'LBL_ACTION_CATEGORY' => 'Categoria',
  'LBL_ACTION_PARAMETERS' => 'Paràmetres',
  'LBL_ACTION_ACTIONS' => 'Accions',
  'LBL_ACTION_AUTOMATIC' => 'Automàtica',
  'LBL_ACTION_NO_PARAMS' => 'Sense paràmetres',
  'LBL_ACTION_PARAM_SELECT_NO_OPTION' => '-- Selecciona --',
  'LBL_ACTION_PARAM_CRM_RECORD_MODULE' => 'Mòdul',
  'LBL_ACTION_PARAM_CRM_RECORD_RECORD' => 'Registre',
  'LBL_ACTION_PARAM_OPTION_SELECTOR_OPTION' => 'Opció',
  'LBL_ACTION_PARAM_MISSING_MESSAGE' => "Hi ha paràmetres obligatoris sense valor. Reviseu la configuració de l'acció i assigneu valors als paràmetres requerits abans de desar.",
  'LBL_ACTION_CONDITION_TEXT' => "Aquesta acció només s'executarà si es compleix la condició.",
  'LBL_ACTION_CONTINUE_ON_ERROR' => "Continua en cas d'error",
  'LBL_ACTION_CONTINUE_ON_ERROR_DESC' => "Si s'activa, el flux continuarà encara que l'acció falli.",

  // Conditions (Datablock field validations & Actions)
  'LBL_CONDITION' => "Condició per a l'execució",
  'LBL_CONDITION_SUMMARY' => 'Condició',
  'LBL_APPLY_CONDITION' => "Condicionar al valor d'un altre camp",
  'LBL_CONDITION_FIELD_NAME' => 'Camp',
  'LBL_CONDITION_VALUE' => 'Valor',

  // Layout 
  'LBL_LAYOUT_SETTINGS' => 'Configuració',
  'LBL_LAYOUT_FORM_DESIGN' => 'Disseny del formulari',
  'LBL_LAYOUT_PREVIEW' => 'Previsualització',
  'LBL_LAYOUT_HEADER' => 'Capçalera',
  'LBL_LAYOUT_FOOTER' => 'Peu',

  // Layout -> Theme
  'LBL_THEME_GENERAL' => 'General',
  'LBL_THEME_WEB_TITLE_TEXT' => 'Títol de la pàgina',
  'LBL_THEME_WEB_TITLE_VALUE' => 'Formulari Web Avançat',
  'LBL_THEME_SUBMIT_BUTTON_TEXT' => "Text del botó d'enviament",
  'LBL_THEME_SUBMIT_BUTTON_TEXT_VALUE' => 'Envia',
  'LBL_THEME_MAIN_COLORS' => 'Colors',
  'LBL_THEME_PRIMARY_COLOR' => 'Principal',
  'LBL_THEME_PAGE_BG_COLOR' => 'Fons de pàgina',
  'LBL_THEME_FORM_BG_COLOR' => 'Fons del formulari',
  'LBL_THEME_TYPOGRAPGY_TEXT' => 'Format del text',
  'LBL_THEME_FONT_FAMILY' => 'Font',
  'LBL_THEME_FONT_SIZE' => 'Mida',
  'LBL_THEME_TEXT_COLOR' => 'Color',
  'LBL_THEME_FIELDS_LABELS' => 'Camps i etiquetes',
  'LBL_THEME_BORDERS' => 'Vores i ombres',
  'LBL_THEME_BORDER_RADIUS_CONTAINER' => 'Arrodoniment (caixes)',
  'LBL_THEME_BORDER_RADIUS_CONTROLS' => 'Arrodoniment (botons/controls)',
  'LBL_THEME_BORDER_WIDTH' => 'Amplada de la vora',
  'LBL_THEME_BORDER_COLOR' => 'Color de la vora',
  'LBL_THEME_FLOATING_LABELS' => 'Etiquetes flotants',
  'LBL_THEME_FLOATING_LABELS_DESC' => "Si s'activa, les etiquetes es col·loquen dins dels camps i es mouran en escriure-hi.",
  'LBL_THEME_LABEL_WEIGHT_BOLD' => 'Etiquetes en negreta',
  'LBL_THEME_SHADOW_INTENSITY' => 'Ombrejat',
  'LBL_THEME_INPUT_STYLE' => 'Estil de camps',
  'LBL_THEME_STRUCTURE' => 'Estructura i distribució',
  'LBL_THEME_FORM_WIDTH' => 'Amplada màxima',
  'LBL_THEME_FIELD_SPACING' => 'Espai entre camps',
  'LBL_THEME_SUBMIT_FULL_WIDTH' => "Amplada completa del botó d'enviament",
  'LBL_THEME_EQUAL_HEIGHT_SECTIONS' => "Iguala l'alçada de les seccions",
  'LBL_THEME_SECTIONS_PER_ROW' => 'Columnes (seccions)',
  'LBL_THEME_FIELDS_PER_ROW' => 'Columnes (camps)',
  'LBL_THEME_PER_ROW_DESC' => "El nombre de columnes s'adaptarà automàticament a l'amplada de la pantalla.",
  'LBL_THEME_ADVANCED' => 'Avançat',
  'LBL_THEME_ADVANCED_NO_ADMIN_DESC' => 'La configuració avançada (CSS i JS) està restringida a usuaris administradors.',
  'LBL_THEME_CUSTOM_CSS' => 'CSS personalitzat',
  'LBL_THEME_CUSTOM_CSS_DESC' => "S'injectarà en un bloc <style>.",
  'LBL_THEME_CUSTOM_JS' => 'JS personalitzat',
  'LBL_THEME_CUSTOM_JS_DESC' => "Es processarà quan tot el formulari s'hagi carregat.",
  
  'LBL_THEME_CLOSED_FORM' => 'Avís: Formulari tancat',
  'LBL_THEME_CLOSED_FORM_DESC' => 'Avís que apareixerà al formulari quan ja no accepti respostes.',
  'LBL_THEME_CLOSED_FORM_TITLE' => "Títol de l'avís",
  'LBL_THEME_CLOSED_FORM_TITLE_VALUE' => '⛔ Formulari tancat',
  'LBL_THEME_CLOSED_FORM_TEXT' => "Text de l'avís",
  'LBL_THEME_CLOSED_FORM_TEXT_VALUE' => 'Aquest formulari no accepta respostes.',

  'LBL_THEME_PROCESSED_FORM' => 'Missatge: Dades processades',
  'LBL_THEME_PROCESSED_FORM_DESC' => 'Missatge que es mostrarà per defecte en processar correctament una resposta al formulari. No es mostrarà si es defineix una acció final.',
  'LBL_THEME_PROCESSED_FORM_TITLE' => 'Títol del missatge',
  'LBL_THEME_PROCESSED_FORM_TITLE_VALUE' => 'Processament',
  'LBL_THEME_PROCESSED_FORM_TEXT' => 'Text del missatge',
  'LBL_THEME_PROCESSED_FORM_TEXT_VALUE' => "Les dades s'han processat correctament.",
  
  'LBL_THEME_RECEIPT_FORM' => 'Missatge: Dades rebudes',
  'LBL_THEME_RECEIPT_FORM_DESC' => 'Missatge que es mostrarà per defecte en rebre una resposta i desar-la per processar-la més tard. Aquest missatge no es mostrarà si es defineix una acció final.',
  'LBL_THEME_RECEIPT_FORM_TITLE' => 'Títol del missatge',
  'LBL_THEME_RECEIPT_FORM_TITLE_VALUE' => 'Rebut',
  'LBL_THEME_RECEIPT_FORM_TEXT' => 'Text del missatge',
  'LBL_THEME_RECEIPT_FORM_TEXT_VALUE' => "Les dades s'han rebut correctament i seran processades tan aviat com sigui possible.",

  'LBL_THEME_RESET_BUTTON' => 'Configuració per defecte',
  
  // Layout -> Sections
  'LBL_SECTIONS' => 'Seccions',
  'LBL_SECTION_ADD' => 'Afegeix una secció',
  'LBL_SECTION_NEW' => 'Nova secció',
  'LBL_SECTION_CONFIG' => 'Configuració',
  'LBL_SECTION_CONTENT' => 'Contingut',
  'LBL_SECTION_TITLE' => 'Títol',
  'LBL_SECTION_SUBTITLE' => 'Subtítol',
  'LBL_SECTION_NO_TITLE' => '< Sense títol >',
  'LBL_SECTION_SHOW_TITLE' => 'Mostra el títol',
  'LBL_SECTION_CONTAINER' => 'Contenidor visual',
  'LBL_SECTION_IS_COLLAPSIBLE' => 'Col·lapsable',
  'LBL_SECTION_ISCOLLAPSED' => 'Col·lapsat inicialment',
  'LBL_SECTION_MOVE_ELEMENT_NO_OPTION' => 'Mou a...',
  'LBL_SECTION_EMPTY_DESC' => "Aquesta secció està buida. Mogueu blocs aquí des d'altres seccions.",

  // Form generation
  'LBL_CODE_GENERATING' => 'Generant el codi...',
  'LBL_CODE_GENERATING_ERROR' => 'Error generant el codi',
  'LBL_CODE_LOADING' => 'Carregant el codi...',
  'LBL_CODE_LOADING_ERROR' => 'Error de connexió',

  'LBL_REQUIRED_FIELD_MESSAGE' => 'Cal emplenar aquest camp',
  
  'LBL_PREVIEW_RIBBON' => 'Previsualització',
  'LBL_PREVIEW_LOADING' => 'Carregant...',
  'LBL_PREVIEW_LOAD_ERROR' => 'Error en carregar la previsualització',
  'LBL_PREVIEW_DESC' => 'Visualització real generada pel servidor.',
  'LBL_PREVIEW_MODE_ALERT' => "El formulari està en mode previsualització. L'enviament de dades no està activat.",
  'LBL_PREVIEW_TOOLBAR' => 'Previsualització',
  'LBL_PREVIEW_ACTIVE_TEXT' => 'Actiu',
  'LBL_PREVIEW_INACTIVE_TEXT' => 'Inactiu',
  'LBL_PREVIEW_IN_NEW_TAB' => 'Previsualitza en una pestanya nova',

  'LBL_FORM_PUBLISH_OPTIONS' => 'Opcions de publicació',
  'LBL_FORM_PUBLISH_LINK' => 'Enllaç públic',
  'LBL_FORM_PUBLISH_LINK_DESC' => 'Copieu i compartiu aquest enllaç per accedir al formulari.',
  'LBL_FORM_PUBLISH_IFRAME' => 'Incrusta (iframe)',
  'LBL_FORM_PUBLISH_IFRAME_DESC' => 'Copieu aquest codi per incrustar el formulari en un lloc web extern tot mantenint-lo allotjat a SinergiaCRM.',
  'LBL_FORM_PUBLISH_HTML' => 'Codi HTML',
  'LBL_FORM_PUBLISH_HTML_DESC' => 'Utilitzeu aquest codi per allotjar el formulari en un lloc web extern.',

  'LBL_COPY_TO_CLIPBOARD_DONE' => 'Copiat al porta-retalls',

  'LBL_RATE_ARIA' => 'Valoreu amb un %s',

  // Errors
  'LBL_ERROR_DATABLOCK_IS_INVALID' => 'El bloc de dades té errors',
  'LBL_ERROR_DATABLOCK_NAME' => 'El nom intern del bloc de dades està buit',
  'LBL_ERROR_DATABLOCK_TITLE' => 'El bloc de dades ha de tenir un nom públic',
  'LBL_ERROR_NO_DATABLOCKS' => "S'ha de definir almenys un bloc de dades per continuar",
  'LBL_ERROR_FIELD_IS_INVALID' => 'El camp té errors',
  'LBL_ERROR_FIELD_NAME' => 'El nom intern del camp està buit',
  'LBL_ERROR_FIELD_LABEL' => 'No existeix etiqueta per al camp',
  'LBL_ERROR_FIELD_TYPE' => "No s'ha definit el tipus de camp o d'editor al formulari",
  'LBL_ERROR_FIELD_OPTIONS' => 'Desplegable sense opcions definides',
  'LBL_ERROR_FIELD_FIXED_EMPTY' => 'Camp fix sense valor assignat',
  'LBL_OK_FIELD_IS_VALID' => 'El camp és correcte',

  // -- SUBPANELS --
  'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_RESPONSES_TITLE' => 'Respostes a formularis',

  // -- HOOK ACTIONS --
  // Generic 
  'LBL_CUSTOM_ACTION_DATABLOCK_PARAM_TEXT' => 'Bloc de dades',
  'LBL_CUSTOM_ACTION_DATABLOCK_PARAM_DESC' => "Seleccioneu el bloc de dades que serà utilitzat com a paràmetre a l'acció",

  // SaveRecordAction
  'LBL_SAVE_RECORD_ACTION_TITLE' => 'Desa el registre',
  'LBL_SAVE_RECORD_ACTION_DESC' => 'Desa o actualitza un registre a partir de les dades del formulari',
  'LBL_SAVE_RECORD_ACTION_DUPLICATE_RULE_MATCHED_TEXT' => 'Coincidència per camps',
  'LBL_SAVE_RECORD_ACTION_RELATION_CONFIGS_TEXT' => 'Camps de relació',

  // RelateRecordsAction
  'LBL_RELATE_RECORDS_ACTION_TITLE' => 'Crea la relació',
  'LBL_RELATE_RECORDS_ACTION_DESC' => 'Crea una relació entre dos registres',
  'LBL_RELATE_RECORDS_ACTION_TARGET_OBJECT_TEXT' => 'Destí de la relació',
  'LBL_RELATE_RECORDS_ACTION_TARGET_OBJECT_DESC' => 'El bloc de dades o registre destí de la relació a desar',
  'LBL_RELATE_RECORDS_ACTION_OPTION_BLOCK_TEXT' => 'Bloc de dades destí',
  'LBL_RELATE_RECORDS_ACTION_OPTION_VALUE_TEXT' => 'ID del registre destí',
  'LBL_RELATE_RECORDS_ACTION_RELATIONSHIP_TEXT' => 'Relació a actualitzar',
  'LBL_RELATE_RECORDS_ACTION_RELATIONSHIP_DESC' => 'El nom intern de la relació que enllaça amb el bloc de dades destí',
  'LBL_RELATE_RECORDS_ACTION_RELATION_ID_NAME_TEXT' => 'Camp relacionat',

  // AddToTargetListAction
  'LBL_ADD_TO_TARGET_LIST_ACTION_TITLE' => 'Afegeix a una Llista de Públic Objectiu',
  'LBL_ADD_TO_TARGET_LIST_ACTION_DESC' => 'Afegeix el registre processat (persona, interessat, usuari o organització) a una Llista de Públic Objectiu existent',
  'LBL_ADD_TO_TARGET_LIST_ACTION_CONTACT_TO_ADD_TEXT' => 'Destinatari',
  'LBL_ADD_TO_TARGET_LIST_ACTION_CONTACT_TO_ADD_DESC' => "Indica el bloc de dades que conté el destinatari que s'afegirà a la Llista de Públic Objectiu",
  'LBL_ADD_TO_TARGET_LIST_ACTION_TARGET_LIST_RECORD_TEXT' => 'Llista de Públic Objectiu (LPO)',
  
  // SendEmailToDataBlockAction
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_TITLE' => 'Envia un correu al remitent del formulari',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_DESC' => 'Envia un correu electrònic al registre processat (persona, interessat, usuari o organització) contingut en un bloc de dades',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_RECIPIENT_BLOCK_TEXT' => 'Destinatari',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_RECIPIENT_BLOCK_DESC' => 'Indica el bloc de dades que conté el destinatari del correu electrònic',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_TEMPLATE_TEXT' => 'Plantilla de correu electrònic',

  // SendEmailToAddressAction
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_TITLE' => 'Envia un correu a una adreça',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_DESC' => 'Envia un correu electrònic a una adreça de correu electrònic concreta',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_EMAIL_TEXT' => 'Correu electrònic',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_TEMPLATE_TEXT' => 'Plantilla de correu electrònic',
  
  // SendEmailToAssignedAction
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_TITLE' => 'Envia un correu a un usuari assignat',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_DESC' => "Envia un correu electrònic a l'usuari assignat del formulari o d'un registre",
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_SOURCE_TEXT' => "Origen de l'usuari assignat",
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_SOURCE_DESC' => "Indica el registre del qual s'obtindrà l'usuari assignat",
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_OWNER_TEXT' => 'Formulari',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RESPONSE_TEXT' => 'Resposta del formulari',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_DATABLOCK_TEXT' => 'Bloc de dades',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RECORD_TEXT' => 'Registre fix',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RELATE_TEXT' => 'Camp relacionat',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_TEMPLATE_TEXT' => 'Plantilla de correu electrònic',
  
  // RedirectAction
  'LBL_REDIRECT_ACTION_TITLE' => 'Ves a una pàgina web',
  'LBL_REDIRECT_ACTION_DESC' => "Redirecciona el navegador de l'usuari final a una pàgina web concreta",
  'LBL_REDIRECT_ACTION_URL_TEXT' => 'URL de redirecció',
  'LBL_REDIRECT_ACTION_URL_DESC' => "Indica l'adreça de la pàgina web a la qual redirigir l'usuari final. Ha d'incloure el protocol (http:// o https://).",
  'LBL_REDIRECT_ACTION_METHOD_TEXT' => "Mètode d'enviament",
  'LBL_REDIRECT_ACTION_METHOD_DESC' => "Indica, en cas que sigui necessari, com s'han d'enviar les dades a la pàgina de redirecció.",
  'LBL_REDIRECT_ACTION_METHOD_GET_TEXT' => "GET (les dades s'afegeixen a la URL de redirecció)",
  'LBL_REDIRECT_ACTION_METHOD_POST_TEXT' => "POST (les dades s'envien mitjançant un formulari ocult)",
  'LBL_REDIRECT_ACTION_FIELDS_TEXT' => 'Camps a enviar',
  'LBL_REDIRECT_ACTION_FIELDS_DESC' => "Indica els camps a enviar a la URL de redirecció. Si no cal enviar dades deixeu-lo en blanc.",
  'LBL_REDIRECT_ACTION_REDIRECTING' => 'Redireccionant...',
  'LBL_REDIRECT_ACTION_SUBMIT_BUTTON' => 'Premeu aquí per continuar',

  // RedirectToRecordAction
  'LBL_REDIRECT_TO_RECORD_ACTION_TITLE' => "Ves a un registre",
  'LBL_REDIRECT_TO_RECORD_ACTION_DESC' => "Redirecciona el navegador de l'usuari final a la pàgina d'un registre concret del CRM",
  'LBL_REDIRECT_TO_RECORD_ACTION_TARGET_DATA_BLOCK_TEXT' => 'Bloc de dades del registre',
  'LBL_REDIRECT_TO_RECORD_ACTION_TARGET_DATA_BLOCK_DESC' => "Indica el bloc de dades que conté el registre al qual es redirigirà l'usuari final.",
  'LBL_REDIRECT_TO_RECORD_ACTION_CRM_VIEW_TEXT' => 'Vista del CRM',
  'LBL_REDIRECT_TO_RECORD_ACTION_CRM_VIEW_DETAILVIEW_TEXT' => 'Vista de detall',
  'LBL_REDIRECT_TO_RECORD_ACTION_CRM_VIEW_EDITVIEW_TEXT' => "Vista d'edició",

  // RedirectSummaryPageAction
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE' => 'Mostra el resum de dades',
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_DESC' => "Redirecciona el navegador de l'usuari final a una pàgina on es mostren les dades facilitades",
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE_TEXT' => 'Títol de la pàgina',
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE_DEFAULT' => 'Resum de les dades facilitades',

  // CheckSessionAction
  'LBL_CHECK_SESSION_ACTION_TITLE' => 'Verifica la sessió i els permisos',
  'LBL_CHECK_SESSION_ACTION_DESC' => "Bloqueja el processament del formulari si no hi ha una sessió d'usuari activa o si l'usuari no té els permisos per crear els registres associats al formulari",
  'LBL_CHECK_SESSION_ACTION_PERMISSIONS_ERROR_MSG_TEXT' => 'Missatge per falta de permisos',
  'LBL_CHECK_SESSION_ACTION_PERMISSIONS_ERROR_MSG_TEXT_DEFAULT' => 'Accés no autoritzat. No disposeu dels permisos necessaris per continuar.',
  'LBL_CHECK_SESSION_ACTION_CHECKING' => 'Verificant accés i permisos...',
  'LBL_CHECK_SESSION_ACTION_DENIED_TITLE' => '🚫 Accés denegat',
  'LBL_CHECK_SESSION_ACTION_ACTIVE_SESSION' => 'Sessió activa',

  // -- DEFERRED ACTIONS --
  'LBL_PARAM_EXPIRATION_DAYS' => 'Dies per a la caducitat',
  'LBL_PARAM_EXPIRATION_DAYS_DESC' => "Nombre de dies després del qual caducarà l'acció diferida.",
  'LBL_PARAM_ALREADY_PROCESSED_TITLE' => 'Títol per a enllaç ja utilitzat',
  'LBL_PARAM_ALREADY_PROCESSED_TITLE_DESC' => "Títol de l'avís que es mostrarà quan s'accedeixi a l'enllaç si ja s'ha utilitzat.",
  'LBL_PARAM_ALREADY_PROCESSED_TITLE_DEFAULT' => 'Acció ja realitzada',
  'LBL_PARAM_ALREADY_PROCESSED_TEXT' => "Text per a enllaç ja utilitzat",
  'LBL_PARAM_ALREADY_PROCESSED_TEXT_DESC' => "Text de l'avís que es mostrarà quan s'accedeixi a l'enllaç si ja s'ha utilitzat.",
  'LBL_PARAM_ALREADY_PROCESSED_TEXT_DEFAULT' => "Aquesta acció ja s'ha completat anteriorment de manera correcta i no és necessari repetir-la.",
  'LBL_PARAM_EXPIRED_TITLE' => 'Títol per a enllaç caducat',
  'LBL_PARAM_EXPIRED_TITLE_DESC' => "Títol de l'avís que es mostrarà quan s'accedeixi a l'enllaç caducat.",
  'LBL_PARAM_EXPIRED_TITLE_DEFAULT' => 'Enllaç caducat',
  'LBL_PARAM_EXPIRED_TEXT' => "Text per a enllaç caducat",
  'LBL_PARAM_EXPIRED_TEXT_DESC' => "Text de l'avís que es mostrarà quan s'accedeixi a l'enllaç caducat.",
  'LBL_PARAM_EXPIRED_TEXT_DEFAULT' => 'Aquest enllaç ha caducat per motius de seguretat.',

  // EmailConfirmationAction 
  'LBL_EMAIL_CONFIRMATION_ACTION_TITLE' => 'Confirma el correu electrònic', 
  'LBL_EMAIL_CONFIRMATION_ACTION_DESC' => "Genera un enllaç únic i l'envia per correu electrònic perquè l'usuari pugui confirmar la seva adreça de correu electrònic.", 
  'LBL_EMAIL_CONFIRMATION_ACTION_FLOW_SUCCESS' => 'Correu confirmat',
  'LBL_EMAIL_CONFIRMATION_ACTION_FLOW_ERROR' => 'Error',
  'LBL_EMAIL_CONFIRMATION_ACTION_RECIPIENT_BLOCK_TEXT' => 'Destinatari', 
  'LBL_EMAIL_CONFIRMATION_ACTION_RECIPIENT_BLOCK_DESC' => "Indica el bloc de dades que conté el correu electrònic a verificar i al qual s'enviarà l'enllaç de confirmació.", 
  'LBL_EMAIL_CONFIRMATION_ACTION_TEMPLATE_TEXT' => 'Plantilla de correu electrònic', 
  'LBL_EMAIL_CONFIRMATION_ACTION_TEMPLATE_DESC' => "La plantilla de correu electrònic ha de tenir la variable {::confirmation_url::} al cos del missatge perquè es generi l'enllaç de confirmació.", 

  // PaymentRouterAction
  'LBL_PAYMENT_ROUTER_ACTION_TITLE' => 'Fes un pagament',
  'LBL_PAYMENT_ROUTER_ACTION_DESC' => 'Processa en una plataforma externa el pagament corresponent a un bloc de dades.',
  'LBL_PAYMENT_ROUTER_ACTION_FLOW_SUCCESS' => 'Pagament confirmat',
  'LBL_PAYMENT_ROUTER_ACTION_FLOW_ERROR' => 'Error',
  'LBL_PAYMENT_ROUTER_ACTION_PAYMENT_COMMITMENT_TEXT' => 'Compromís de pagament',
  'LBL_PAYMENT_ROUTER_ACTION_PAYMENT_COMMITMENT_DESC' => "Selecciona el bloc de dades que conté el compromís de pagament del qual s'haurà de fer el pagament en una plataforma externa.",

  // -- VALIDATOR ACTIONS --
  // RegexValidatorAction
  'LBL_REGEX_VALIDATOR_ACTION_TITLE' => 'Validador Regex',
  'LBL_REGEX_VALIDATOR_ACTION_DESC' => 'Valida un camp segons una expressió regular',
  'LBL_REGEX_VALIDATOR_ACTION_PATTERN_TEXT' => 'Expressió regular',
  'LBL_REGEX_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El format del camp no és vàlid',

  // EmailValidatorAction
  'LBL_EMAIL_VALIDATOR_ACTION_TITLE' => 'Validador de correu electrònic',
  'LBL_EMAIL_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui una adreça de correu electrònic vàlida',
  'LBL_EMAIL_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => "L'adreça de correu electrònic no és vàlida",

  // DniValidatorAction
  'LBL_DNI_VALIDATOR_ACTION_TITLE' => 'Validador de DNI/NIF',
  'LBL_DNI_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui un DNI/NIF espanyol vàlid',
  'LBL_DNI_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El DNI/NIF no és vàlid',

  // CifValidatorAction
  'LBL_CIF_VALIDATOR_ACTION_TITLE' => 'Validador de NIF de persona jurídica',
  'LBL_CIF_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui un NIF espanyol de persona jurídica vàlid',
  'LBL_CIF_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El NIF no és vàlid',

  // NieValidatorAction
  'LBL_NIE_VALIDATOR_ACTION_TITLE' => 'Validador de NIE',
  'LBL_NIE_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui un NIE espanyol vàlid',
  'LBL_NIE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El NIE no és vàlid',

  // CatSalutCipValidatorAction
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_TITLE' => 'Validador de CIP (CatSalut)',
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_DESC' => "Valida que un camp contingui un Codi d'Identificació Personal (CIP) de CatSalut vàlid",
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El CIP no és vàlid',

  // NafValidatorAction
  'LBL_NAF_VALIDATOR_ACTION_TITLE' => 'Validador de NUSS',
  'LBL_NAF_VALIDATOR_ACTION_DESC' => "Valida que un camp contingui un número d'afiliació a la Seguretat Social (NUSS) vàlid",
  'LBL_NAF_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => "El número d'afiliació a la Seguretat Social no és vàlid",

  // NumericValidatorAction
  'LBL_NUMERIC_VALIDATOR_ACTION_TITLE' => 'Validador numèric',
  'LBL_NUMERIC_VALIDATOR_ACTION_DESC' => "Valida que un camp contingui un valor numèric i que, opcionalment, es trobi dins d'un rang",
  'LBL_NUMERIC_VALIDATOR_ACTION_MIN_TEXT' => 'Valor mínim (opcional)',
  'LBL_NUMERIC_VALIDATOR_ACTION_MAX_TEXT' => 'Valor màxim (opcional)',
  'LBL_NUMERIC_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El valor ha de ser numèric i estar entre els valors permesos',

  // TextLengthValidatorAction
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_TITLE' => 'Validador de longitud de text',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_DESC' => "Valida que un camp contingui un text amb una longitud dins d'un rang",
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_MIN_LENGTH_TEXT' => 'Longitud mínima (opcional)',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_MAX_LENGTH_TEXT' => 'Longitud màxima (opcional)',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El text ha de tenir una longitud entre els valors permesos',

  // IbanValidatorAction
  'LBL_IBAN_VALIDATOR_ACTION_TITLE' => "Validador d'IBAN",
  'LBL_IBAN_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui un IBAN vàlid',
  'LBL_IBAN_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => "L'IBAN no és vàlid",

  // PhoneValidatorAction
  'LBL_PHONE_VALIDATOR_ACTION_TITLE' => 'Validador de telèfon',
  'LBL_PHONE_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui un número de telèfon espanyol vàlid (almenys 9 dígits numèrics)',
  'LBL_PHONE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El telèfon no és vàlid',

  // SpanishZipValidatorAction
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_TITLE' => 'Validador de codi postal',
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui un codi postal espanyol vàlid (5 dígits numèrics)',
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'El codi postal no és vàlid',

  // TrueValidatorAction
  'LBL_TRUE_VALIDATOR_ACTION_TITLE' => 'Selecció obligada',
  'LBL_TRUE_VALIDATOR_ACTION_DESC' => "Assegura que una casella estigui marcada (per exemple, l'acceptació de condicions)",
  'LBL_TRUE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'Cal acceptar aquest camp per continuar',

  // AgeValidatorAction
  'LBL_AGE_VALIDATOR_ACTION_TITLE' => "Validador d'edat",
  'LBL_AGE_VALIDATOR_ACTION_DESC' => "Calcula l'edat a partir de la data de naixement i verifica que estigui entre la mínima i la màxima permeses",
  'LBL_AGE_VALIDATOR_ACTION_MIN_YEARS_TEXT' => 'Edat mínima (opcional)',
  'LBL_AGE_VALIDATOR_ACTION_MAX_YEARS_TEXT' => 'Edat màxima (opcional)',
  'LBL_AGE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => "L'edat no està en el rang permès",

  // UrlValidatorAction
  'LBL_URL_VALIDATOR_ACTION_TITLE' => "Validador d'URL",
  'LBL_URL_VALIDATOR_ACTION_DESC' => 'Valida que un camp contingui una URL vàlida',
  'LBL_URL_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => "L'URL no és vàlida",
);
