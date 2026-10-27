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
$module_name = 'stic_Assessments_Scores';
$listViewDefs[$module_name] = array(
    'NAME' => array(
        'width' => '30%',
        'label' => 'LBL_NAME',
        'default' => true,
        'link' => true,
    ),
    'SCORE_VALUE' => array(
        'type' => 'decimal',
        'width' => '10%',
        'label' => 'LBL_SCORE_VALUE',
        'default' => true,
    ),
    'CLASSIFICATION_LABEL' => array(
        'width' => '15%',
        'label' => 'LBL_CLASSIFICATION_LABEL',
        'default' => true,
    ),
    'STIC_ASSESSMENTS_STIC_ASSESSMENTS_SCORES_NAME' => array(
        'type' => 'relate',
        'link' => true,
        'label' => 'LBL_STIC_ASSESSMENTS_STIC_ASSESSMENTS_SCORES_FROM_STIC_ASSESSMENTS_TITLE',
        'id' => 'STIC_ASSESSMENTS_STIC_ASSESSMENTS_SCORESSTIC_ASSESSMENTS_IDA',
        'width' => '15%',
        'default' => true,
    ),
    'FORM_ID' => array(
        'width' => '10%',
        'label' => 'LBL_FORM_ID',
        'default' => false,
    ),
    'ASSIGNED_USER_NAME' => array(
        'width' => '10%',
        'label' => 'LBL_ASSIGNED_TO_NAME',
        'module' => 'Employees',
        'id' => 'ASSIGNED_USER_ID',
        'default' => true,
    ),
    'DATE_ENTERED' => array(
        'type' => 'datetime',
        'width' => '10%',
        'label' => 'LBL_DATE_ENTERED',
        'default' => true,
    ),
);
