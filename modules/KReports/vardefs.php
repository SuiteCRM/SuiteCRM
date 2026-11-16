<?php
/**
 * This file is part of KReporter. KReporter is an enhancement developed
 * by Christian Knoll. All rights are (c) 2012 by Christian Knoll
 *
 * This file has been modified by SinergiaTIC in SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * You can contact Christian Knoll at info@kreporter.org
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$dictionary['KReport'] = array('table' => 'kreports',
    'fields' => array(
        'report_module' => array(
            'name' => 'report_module',
            'type' => 'enum',
            'options' => 'moduleList',
            'len' => '45',
            'vname' => 'LBL_MODULE',
            'massupdate' => false,
            'inline_edit' => 0,
        ),
        'report_status' => array(
            'name' => 'report_status',
            'type' => 'enum',
            'options' => 'kreportstatus',
            'len' => '1',
            'vname' => 'LBL_REPORT_STATUS',
            'massupdate' => 1,
        ),
        'report_segmentation' => array(
            'name' => 'report_segmentation',
            'type' => 'enum',
            'options' => 'stic_kreports_segmentations_list',
            'len' => 100,
            'vname' => 'LBL_REPORT_SEGMENTATION',
            'massupdate' => 1,
        ),
        'union_modules' => array(
            'name' => 'union_modules',
            'type' => 'text',
        ),
        'reportoptions' => array(
            'name' => 'reportoptions',
            'type' => 'text',
            'vname' => 'LBL_REPORTOPTIONS'
        ),
        'listtype' => array(
            'name' => 'listtype',
            'type' => 'varchar',
            'len' => '10',
            'vname' => 'LBL_LISTTYPE',
            'massupdate' => false,
        ),
        'listtypeproperties' => array(
            'name' => 'listtypeproperties',
            'type' => 'text',
        ),
        'selectionlimit' => array(
            'name' => 'selectionlimit',
            'type' => 'varchar',
            'len' => '25',
            'vname' => 'LBL_SELECTIONLIMIT',
            'massupdate' => false,
        ),
        'presentation_params' => array(
            'name' => 'presentation_params',
            'type' => 'text',
            'vname' => 'LBL_PRESENTATION_PARAMS',
        ),   
        'visualization_params' => array(
            'name' => 'visualization_params',
            'type' => 'text',
            'vname' => 'LBL_VISUALIZATION_PARAMS',
        ),
        'integration_params' => array(
            'name' => 'integration_params',
            'type' => 'text',
            'vname' => 'LBL_INTEGRATION_PARAMS',
        ),        
        'wheregroups' => array(
            'name' => 'wheregroups',
            'type' => 'text',
            'vname' => 'LBL_WHEREGROUPS',
            'default' => '[]',
        ),
        'whereconditions' => array(
            'name' => 'whereconditions',
            'type' => 'mediumtext',
            'vname' => 'LBL_WHERECONDITION',
            'default' => '[]',
        ),
        'listfields' => array(
            'name' => 'listfields',
            'type' => 'mediumtext',
            'vname' => 'LBL_LISTFIELDS'
        ),
        'unionlistfields' => array(
            'name' => 'unionlistfields',
            'type' => 'mediumtext',
            'vname' => 'LBL_UNIONLISTFIELDS'
        ),
        'advancedoptions' => array(
            'name' => 'advancedoptions',
            'type' => 'text',
            'vname' => 'LBL_ADVANCEDOPTIONS'
        )
    ),
    'indices' => array(
        array('name' => 'idx_reminder_name', 'type' => 'index', 'fields' => array('name')),
    ),
    'optimistic_locking' => true,
);
if ($GLOBALS['sugar_flavor'] != 'CE')
    VardefManager::createVardef('KReports', 'KReport', array('default', 'assignable', 'team_security'));
else if (isset($GLOBALS['sugar_config']['securitysuite_version']))
    VardefManager::createVardef('KReports', 'KReport', array('default', 'assignable', 'security_groups'));
else
    VardefManager::createVardef('KReports', 'KReport', array('default', 'assignable'));
