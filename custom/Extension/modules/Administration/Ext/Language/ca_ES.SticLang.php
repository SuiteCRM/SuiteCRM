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

// Secció d'administració de SinergiaCRM
$mod_strings['LBL_SINERGIACRM_TAB_TITLE'] = 'SinergiaCRM';
$mod_strings['LBL_SINERGIACRM_TAB_DESCRIPTION'] = "Opcions d'administració de SinergiaCRM";

// Elements de la secció d'administració (títols i descripcions curtes)
$mod_strings['LBL_STIC_VALIDATION_ACTIONS_LINK_TITLE'] = 'Accions de validació';
$mod_strings['LBL_STIC_VALIDATION_ACTIONS_DESCRIPTION'] = 'Gestiona les accions de validació i permet vincular-les a les tasques programades.';

$mod_strings['LBL_STIC_VALIDATION_RESULTS_LINK_TITLE'] = 'Resultats de validació';
$mod_strings['LBL_STIC_VALIDATION_RESULTS_DESCRIPTION'] = 'Gestiona i revisa els resultats de les accions de validació.';

$mod_strings['LBL_STIC_CUSTOM_VIEWS_LINK_TITLE'] = 'Vistes personalitzades';
$mod_strings['LBL_STIC_CUSTOM_VIEWS_DESCRIPTION'] = 'Personalització condicional de les vistes dels mòduls.';

$mod_strings['LBL_STIC_SETTINGS_LINK_TITLE'] = 'Configuració';
$mod_strings['LBL_STIC_SETTINGS_DESCRIPTION'] = 'Opcions de configuració de SinergiaCRM.';

$mod_strings['LBL_STIC_TEST_DATA_LINK_TITLE'] = 'Dades de prova';
$mod_strings['LBL_STIC_TEST_DATA_DESCRIPTION'] = 'Carrega o elimina dades de prova.';

$mod_strings['LBL_STIC_SINERGIADA_LINK_TITLE'] = 'Sinergia Data Analytics';
$mod_strings['LBL_STIC_SINERGIADA_DESCRIPTION'] = 'Reconstrueix la integració amb Sinergia Data Analytics.';
$mod_strings['LBL_STIC_SINERGIADA_MAX_USERS_ERROR'] = "S'ha excedit el límit d'usuaris no administradors a SinergiaDA. Màxim permès: <b>__max_users__</b>. Valor actual: <b>__enabled_users__</b>. Desactiveu els usuaris que calgui i torneu a provar-ho.";

$mod_strings['LBL_STIC_MAIN_MENU_LINK_TITLE'] = 'Menú principal';
$mod_strings['LBL_STIC_MAIN_MENU_DESCRIPTION'] = "Configuració de l'estructura i el contingut del menú";

// Dades de prova
$mod_strings['LBL_STIC_TEST_DATA_NOTICE'] = "<strong>Important:</strong> Els registres de prova carregats als diferents mòduls no s'han de fer servir per emmagatzemar dades reals, ja que poden ser eliminats en el futur.";
$mod_strings['LBL_STIC_TEST_DATA_INSERT_LINK_TITLE'] = 'Carrega el conjunt de dades de prova';
$mod_strings['LBL_STIC_TEST_DATA_INSERT_DESCRIPTION'] = "Carrega un conjunt de dades de prova per facilitar l'aprenentatge de l'ús de SinergiaCRM. Aquestes dades podran ser eliminades lliurement en qualsevol moment posterior.";
$mod_strings['LBL_STIC_TEST_DATA_INSERT_SUCCESS'] = 'Dades de prova carregades correctament.';
$mod_strings['LBL_STIC_TEST_DATA_INSERT_ERROR'] = 'Hi ha hagut errors al carregar les dades de prova. Reviseu el <a target="_blank" href="index.php?action=LogView&module=Configurator&doaction=all&filter=action_insertSticData">log</a>.';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_LINK_TITLE'] = 'Elimina el conjunt de dades de prova';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_DESCRIPTION'] = 'Elimina el conjunt de dades de prova prèviament carregat.';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_SUCCESS'] = 'Dades de prova eliminades correctament.';
$mod_strings['LBL_STIC_TEST_DATA_REMOVE_ERROR'] = "Hi ha hagut errors a l'eliminar les dades de prova. " & 'Reviseu el <a target="_blank" href="index.php?action=LogView&module=Configurator&doaction=all&filter=action_removeSticData">log</a>.';

// SinergiaDA
$mod_strings['LBL_STIC_RUN_SDA_ACTIONS_LINK_TITLE'] = 'Reconstrueix ara';
$mod_strings['LBL_STIC_RUN_SDA_ACTIONS_DESCRIPTION'] = "Reconstrueix i repara les vistes i els elements necessaris per a la integració amb Sinergia Data Analytics. Si n'hi ha, afegeix els nous camps.";
$mod_strings['LBL_STIC_GO_TO_SDA_LINK_TITLE'] = 'Ves a Sinergia Data Analytics';
$mod_strings['LBL_STIC_RUN_SDA_SUCCESS_MSG'] = 'La reconstrucció de Sinergia Data Analytics ha acabat correctament.';
$mod_strings['LBL_STIC_RUN_SDA_ERROR_MSG'] = "Durant la reconstrucció de Sinergia Data Analytics s'han trobat els següents errors. Contacteu amb el suport tècnic de SinergiaTIC si ho considereu necessari.";

// Menú principal avançat
$mod_strings['LBL_STIC_MENU_CONFIGURE_TITLE'] = 'Configuració del menú principal';
$mod_strings['LBL_STIC_MENU_ENABLED_NOT_INCLUDED'] = 'Mòduls habilitats no inclosos al menú';
$mod_strings['LBL_STIC_MENU_ENABLED_INCLUDED'] = 'Configuració del menú';
$mod_strings['LBL_STIC_MENU_SAVE'] = 'Desa i aplica';
$mod_strings['LBL_STIC_MENU_RESTORE'] = 'Restaura';
$mod_strings['LBL_STIC_MENU_RESTORE_CONFIRM'] = 'Restaurar el menú predeterminat de SinergiaCRM?';
$mod_strings['LBL_STIC_MENU_INFO'] = 'El menú principal conté dos tipus d\'elements: d\'una banda, dreceres als diferents mòduls de SinergiaCRM i, de l\'altra, nodes de suport que es poden fer servir per agrupar mòduls, enllaçar amb altres llocs web, etc. Aquests últims s\'identifiquen mitjançant una marca de color a la part inferior dreta. Per incloure un mòdul al menú principal cal que estigui <a href="index.php?module=Administration&action=ConfigureTabs" target="_blank">habilitat</a>. Si ja ho està, podeu arrossegar-lo des de la zona de mòduls no inclosos (dreta) cap al node del menú on vulgueu que aparegui (esquerra). Per reorganitzar el menú, arrossegueu qualsevol element a la posició desitjada. Amb el botó dret del ratolí podeu mostrar el menú contextual associat a cada node, que us permetrà crear-ne de nous (que en el cas dels de suport poden apuntar a qualsevol URL), duplicar-los, canviar-ne el nom (només en el cas dels nodes de suport) o eliminar-los. La modificació de noms de mòduls s\'ha de fer a <a href="index.php?action=wizard&module=Studio&wizard=StudioWizard&option=RenameTabs">Reanomena pestanyes</a>.';
$mod_strings['LBL_STIC_MENU_ICONS'] = 'Mostra les icones dels mòduls';
$mod_strings['LBL_STIC_MENU_ALL'] = "Mostra l'opció TOT";
$mod_strings['LBL_STIC_MENU_COMMAND_CREATE'] = 'Crea';
$mod_strings['LBL_STIC_MENU_COMMAND_CREATE_DEFAULT'] = 'Nou node';
$mod_strings['LBL_STIC_MENU_COMMAND_RENAME'] = 'Canvia el nom';
$mod_strings['LBL_STIC_MENU_COMMAND_EDITURL'] = 'Edita la URL';
$mod_strings['LBL_STIC_MENU_COMMAND_EDITURL_PROMPT'] = 'Introduïu la URL';
$mod_strings['LBL_STIC_MENU_COMMAND_EDITURL_PROMPT_VALIDATE'] = 'Introduïu una URL vàlida';
$mod_strings['LBL_STIC_MENU_COMMAND_REMOVE'] = 'Elimina';
$mod_strings['LBL_STIC_MENU_COMMAND_DUPLICATE'] = 'Duplica';
$mod_strings['LBL_STIC_MENU_COMMAND_NEW_MAIN_NODE'] = 'Nou node principal';
$mod_strings['LBL_STIC_MENU_COMMAND_EXPAND'] = "Expandeix l'arbre";
$mod_strings['LBL_STIC_MENU_COMMAND_COLLAPSE'] = "Contrau l'arbre";

// Cadenes de SuiteCRM modificades
$mod_strings['LBL_CONFIGURE_GROUP_TABS'] = 'Agrupació de subpanells';
$mod_strings['LBL_CONFIGURE_GROUP_TABS_DESC'] = "Configuració de l'agrupació dels subpanells a les vistes de detall";

// Altres cadenes
$mod_strings['LBL_TRACKERS_TITLE'] = 'Monitoratge';
$mod_strings['LBL_TRACKERS_DESCRIPTION'] = "Registre de les sessions d'usuari i de les accions sobre els registres.";
$mod_strings['LBL_ADMIN_ACTIONS'] = "Accions d'Administració";
$mod_strings['ERR_SYS_GEN_PWD_TPL_NOT_SELECTED'] = "Indiqueu la plantilla de correu que es farà servir quan el sistema generi la contrasenya d'un nou usuari.";
