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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

include_once __DIR__."/ActionParameterOption.php";
include_once __DIR__."/ActionParameterDefinition.php";
include_once __DIR__."/ActionSelectorOptionDefinition.php";
include_once __DIR__."/ActionDefinition.php";

include_once __DIR__."/ActionParameterDefinitionDTO.php";
include_once __DIR__."/ActionSelectorOptionDefinitionDTO.php";
include_once __DIR__."/ActionDefinitionDTO.php";

include_once __DIR__."/IServerAction.php";
include_once __DIR__."/ServerActionDefinition.php";
include_once __DIR__."/ServerDataBlockActionDefinition.php";
include_once __DIR__."/ServerBeanActionDefinition.php";
include_once __DIR__."/ITerminalAction.php";
include_once __DIR__."/IFrontendAction.php";
include_once __DIR__."/IWebhookDecodable.php";

include_once __DIR__."/UI/UIActionDefinition.php";

include_once __DIR__."/Validator/ValidatorActionDefinition.php";

include_once __DIR__."/DataProvider/DataProviderActionDefinition.php";

include_once __DIR__."/Hook/HookActionDefinition.php";
include_once __DIR__."/Hook/HookDataBlockActionDefinition.php";
include_once __DIR__."/Hook/HookBeanActionDefinition.php";

include_once __DIR__."/Deferred/IDeferredAction.php";
include_once __DIR__."/Deferred/DeferredActionHelperTrait.php";
include_once __DIR__."/Deferred/DeferredActionDefinition.php";
include_once __DIR__."/Deferred/DeferredDataBlockActionDefinition.php";
include_once __DIR__."/Deferred/DeferredBeanActionDefinition.php";

include_once __DIR__."/Group/GroupActionDefinition.php";
