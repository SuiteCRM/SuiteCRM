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
include_once __DIR__."/stic_AWFUtils.php";

include_once __DIR__."/AntiSpamResult.php";
include_once __DIR__."/AntiSpamService.php";

include_once __DIR__."/BeanReference.php";
include_once __DIR__."/BeanModified.php";
include_once __DIR__."/DataBlockResolved.php";
include_once __DIR__."/DataBlockFieldResolved.php";
include_once __DIR__."/OptionSelectorResolved.php";
include_once __DIR__."/FieldModification.php";
include_once __DIR__."/ActionResult.php";
include_once __DIR__."/DeferredContextData.php";
include_once __DIR__."/ExecutionContext.php";

include_once __DIR__."/RequiredParameterException.php";
include_once __DIR__."/ParameterResolverService.php";

include_once __DIR__."/ServerActionFactory.php";
include_once __DIR__."/ServerActionFlowExecutor.php";
include_once __DIR__."/ActionDiscoveryService.php";

include_once __DIR__."/formdefs/includes.php";
include_once __DIR__."/actiondefs/includes.php";