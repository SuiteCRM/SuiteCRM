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


if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class SugarWidgetSubPanelDynamicParentNameLink extends SugarWidgetField
{
    public function displayList(&$layout_def)
    {
        $module = '';
        if (!empty($layout_def['target_module_key']) &&
            !empty($layout_def['fields'][strtoupper($layout_def['target_module_key'])])) {
            $module = $layout_def['fields'][strtoupper($layout_def['target_module_key'])];
        }
        if (empty($module)) {
            $module = !empty($layout_def['target_module']) ? $layout_def['target_module'] : $layout_def['module'];
        }

        $record = '';
        if (!empty($layout_def['target_record_key']) &&
            !empty($layout_def['fields'][strtoupper($layout_def['target_record_key'])])) {
            $record = $layout_def['fields'][strtoupper($layout_def['target_record_key'])];
        }

        if (empty($module) || empty($record)) {
            return '';
        }

        $bean = BeanFactory::newBean($module);
        if (!$bean) {
            return "";
        }

        $bean->disable_row_level_security = true;

        $query = "SELECT * FROM {$bean->table_name} WHERE id = '" . $GLOBALS['db']->quote($record) . "'";
        $result = $GLOBALS['db']->query($query);
        $row = $GLOBALS['db']->fetchByAssoc($result);

        if (empty($row)) {
            return "";
        }

        $bean->fetched_row = $row;
        $bean->populateFromRow($row);

        if (method_exists($bean, '_create_proper_name_field')) {
            $bean->_create_proper_name_field();
        }

        $value = '';
        if ($module === 'Documents') {
            $value = !empty($bean->document_name) ? $bean->document_name : '';
        } else {
            $value = !empty($bean->name) ? $bean->name : '';
        }


        $url = "index.php?module={$module}&action=DetailView&record={$record}";
        return "<a href=\"{$url}\">{$value}</a>";
    }
}
