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
$mod_strings['LBL_CONNECTOR_DEFAULT_CONFIGURED_OPTIONS'] = 'Opcions configurades per defecte';
$mod_strings['LBL_CONNECTOR_HELP'] = "Per facilitar la creació del registre de proveïdor OAuth extern, en seleccionar un connector es configuren diverses opcions per defecte i, en conseqüència, no cal indicar-les explícitament. Les opcions configurades per defecte es mostren a l'àrea inferior. Cal tenir en compte que el connector genèric no configura cap opció per defecte. Més informació a l'apartat <a href='https://wikisuite.sinergiacrm.org/index.php?title=Configuraci%C3%B3n_de_correo#Proveedores_OAuth_externos' target='_blank'>Proveïdors Oauth Externs</a> del wiki de SinergiaCRM.";
$mod_strings['LBL_CLIENT_ID_HELP'] = "En aquest camp cal indicar-hi l'ID de client generat a l'aplicació creada al proveïdor de correu extern.";    
$mod_strings['LBL_CLIENT_SECRET_HELP'] = "En aquest camp cal indicar-hi el valor del secret generat a l'aplicació creada al proveïdor de correu extern. És important no confondre el valor del secret amb l'ID del secret.";
$mod_strings['LBL_SCOPE_HELP'] = "En aquest camp s'hi poden indicar els permisos atorgats a l'aplicació creada al proveïdor de correu extern.";
$mod_strings['LBL_AUTHORIZE_URL_HELP'] = "En aquest camp cal indicar-hi la URL on es connectarà SinergiaCRM per realitzar el procés d'autenticació i autorització.";
$mod_strings['LBL_AUTHORIZE_URL_OPTIONS_HELP'] = "En aquest camp s'hi poden indicar paràmetres que seran afegits a la URL d'autorització i que serveixen per personalitzar l'experiència d'usuari durant el procés d'autenticació i autorització amb el proveïdor de correu. Per exemple: 'access_type=offline' sol·licita un token de refresc per a l'accés sense connexió, 'prompt=consent' força el proveïdor a mostrar la pantalla de permisos encara que ja s'hagin concedit, etc.";
$mod_strings['LBL_URL_ACCESS_TOKEN_HELP'] = "En aquest camp cal indicar-hi la URL a la qual SinergiaCRM enviarà el codi d'autorització obtingut amb la URL d'autorització per obtenir els tokens d'accés i de refresc.";
$mod_strings['LBL_REDIRECT_URI_HELP'] = "En aquest camp es mostra la URL de redirecció que cal indicar al camp URI de redirecció a l'aplicació creada al proveïdor de correu extern. És la URL de SinergiaCRM a la qual el proveïdor extern redirigirà l'usuari després de fer el procés d'autenticació i autorització.";
$mod_strings['LBL_EXTRA_PROVIDER_PARAMS_HELP'] = "En aquest camp s'hi poden indicar paràmetres addicionals (URL o dades) específics del proveïdor per realitzar determinades accions. Per exemple, 'urlResourceOwnerDetails=><url_informació_usuari>' defineix l'URL on SinergiaCRM pot connectar-se per obtenir informació de l'usuari un cop estigui autenticat.";
