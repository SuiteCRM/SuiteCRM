<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

    $extensions = array(
        "actionviewmap" =>   array("section" => "action_view_map","extdir" => "ActionViewMap",  "file" => 'action_view_map.ext.php'),
        "actionfilemap" =>   array("section" => "action_file_map","extdir" => "ActionFileMap",  "file" => 'action_file_map.ext.php'),
        "actionremap" =>     array("section" => "action_remap",   "extdir" => "ActionReMap",    "file" => 'action_remap.ext.php'),
        "administration" =>  array("section" => "administration", "extdir" => "Administration", "file" => 'administration.ext.php', "module" => "Administration"),
        "entrypoints" =>     array("section" => "entrypoints",	  "extdir" => "EntryPointRegistry",	"file" => 'entry_point_registry.ext.php', "module" => "application"),
        "exts"         =>    array("section" => "extensions",	  "extdir" => "Extensions",		"file" => 'extensions.ext.php', "module" => "application"),
        "file_access" =>     array("section" => "file_access",    "extdir" => "FileAccessControlMap", "file" => 'file_access_control_map.ext.php'),
        "languages" =>       array("section" => "language",	      "extdir" => "Language",    	"file" => '' /* custom rebuild */),
        "layoutdefs" =>      array("section" => "layoutdefs", 	  "extdir" => "Layoutdefs",     "file" => 'layoutdefs.ext.php'),
        "links" =>           array("section" => "linkdefs",       "extdir" => "GlobalLinks",    "file" => 'links.ext.php', "module" => "application"),
        "logichooks" =>      array("section" => "hookdefs", 	  "extdir" => "LogicHooks",     "file" => 'logichooks.ext.php'),
        "menus" =>           array("section" => "menu",    	      "extdir" => "Menus",          "file" => "menu.ext.php"),
        "modules" =>         array("section" => "beans", 	      "extdir" => "Include", 	    "file" => 'modules.ext.php', "module" => "application"),
        "schedulers" =>      array("section" => "scheduledefs",	  "extdir" => "ScheduledTasks", "file" => 'scheduledtasks.ext.php', "module" => "Schedulers"),
        "userpage" =>        array("section" => "user_page",      "extdir" => "UserPage",       "file" => 'userpage.ext.php', "module" => "Users"),
        "utils" =>           array("section" => "utils",          "extdir" => "Utils",          "file" => 'custom_utils.ext.php', "module" => "application"),
        "vardefs" =>         array("section" => "vardefs",	      "extdir" => "Vardefs",    	"file" => 'vardefs.ext.php'),
        "jsgroupings" =>     array("section" => "jsgroups",	      "extdir" => "JSGroupings",    "file" => 'jsgroups.ext.php'),
        "aow" =>     		 array("section" => "aow_actions",	  "extdir" => "Actions",		"file" => 'actions.ext.php', "module" => "AOW_Actions"),
        "pdf" =>     		 array("section" => "pdfs",	          "extdir" => "PDF",		    "file" => 'pdfs.ext.php', "module" => "application"),
        "externaloauthproviders" =>  array("section" => "external_oauth_providers", "extdir" => "ExternalOAuthProviders", "file" => 'externaloauthproviders.ext.php', "module" => "application"),
);
if (file_exists("custom/application/Ext/Extensions/extensions.ext.php")) {
    include("custom/application/Ext/Extensions/extensions.ext.php");
}
