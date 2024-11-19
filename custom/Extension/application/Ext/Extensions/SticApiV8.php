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
$extensions["api_routes"] =  array(
    "section" => "api_routes",
    "extdir" => "Api/V8/Config",
    "file" => 'routes.php',
    "module" => "");

$extensions["api_controllers"] =  array(
    "section" => "api_controllers",
    "extdir" => "Api/V8/ParentControllers",
    "file" => '../controllers.php',
    "module" => "");

$extensions["api_services"] =  array(
    "section" => "api_services",
    "extdir" => "Api/V8/ParentServices",
    "file" => '../services.php',
    "module" => "");

$extensions["api_params"] =  array(
    "section" => "api_params",
    "extdir" => "Api/V8/ParentParams",
    "file" => '../params.php',
    "module" => "");