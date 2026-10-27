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

$dictionary['stic_Assessments_Scores'] = array(
    'table' => 'stic_assessments_scores',
    'audited' => true,
    'inline_edit' => true,
    'duplicate_merge' => true,
    'fields' => array(
        'form_id' => array(
            'required' => false,
            'name' => 'form_id',
            'vname' => 'LBL_FORM_ID',
            'type' => 'id',
            'len' => 36,
            'reportable' => false,
            'studio' => 'visible',
        ),
        'score_value' => array(
            'required' => false,
            'name' => 'score_value',
            'vname' => 'LBL_SCORE_VALUE',
            'type' => 'decimal',
            'len' => '10,2',
            'massupdate' => true,
            'inline_edit' => true,
            'reportable' => true,
            'studio' => 'visible',
            'enable_range_search' => true,
        ),
        'classification_label' => array(
            'required' => false,
            'name' => 'classification_label',
            'vname' => 'LBL_CLASSIFICATION_LABEL',
            'type' => 'varchar',
            'len' => '255',
            'massupdate' => true,
            'inline_edit' => true,
            'reportable' => true,
            'studio' => 'visible',
        ),
        'submission_data' => array(
            'required' => false,
            'name' => 'submission_data',
            'vname' => 'LBL_SUBMISSION_DATA',
            'type' => 'text',
            'rows' => 4,
            'cols' => 60,
            'studio' => 'visible',
        ),
        'stic_assessments_stic_assessments_scores' => array(
            'name' => 'stic_assessments_stic_assessments_scores',
            'type' => 'link',
            'relationship' => 'stic_assessments_stic_assessments_scores',
            'source' => 'non-db',
            'module' => 'stic_Assessments',
            'bean_name' => 'stic_Assessments',
            'vname' => 'LBL_STIC_ASSESSMENTS_STIC_ASSESSMENTS_SCORES_FROM_STIC_ASSESSMENTS_TITLE',
            'id_name' => 'stic_assessments_stic_assessments_scoresstic_assessments_ida',
        ),
        'stic_assessments_stic_assessments_scores_name' => array(
            'name' => 'stic_assessments_stic_assessments_scores_name',
            'type' => 'relate',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_ASSESSMENTS_STIC_ASSESSMENTS_SCORES_FROM_STIC_ASSESSMENTS_TITLE',
            'save' => true,
            'id_name' => 'stic_assessments_stic_assessments_scoresstic_assessments_ida',
            'link' => 'stic_assessments_stic_assessments_scores',
            'table' => 'stic_assessments',
            'module' => 'stic_Assessments',
            'rname' => 'name',
        ),
        'stic_assessments_stic_assessments_scoresstic_assessments_ida' => array(
            'name' => 'stic_assessments_stic_assessments_scoresstic_assessments_ida',
            'type' => 'id',
            'vname' => 'LBL_STIC_ASSESSMENT_ID',
            'required' => false,
            'reportable' => false,
            'len' => 36,
        ),
    ),
    'relationships' => array(
        'stic_assessments_stic_assessments_scores' => array(
            'lhs_module' => 'stic_Assessments',
            'lhs_table' => 'stic_assessments',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Assessments_Scores',
            'rhs_table' => 'stic_assessments_scores',
            'rhs_key' => 'stic_assessments_stic_assessments_scoresstic_assessments_ida',
            'relationship_type' => 'one-to-many',
        ),
    ),
    'indices' => array(
        array(
            'name' => 'idx_stic_assessments_scores_assessment',
            'type' => 'index',
            'fields' => array('stic_assessments_stic_assessments_scoresstic_assessments_ida'),
        ),
        array(
            'name' => 'idx_stic_assessments_scores_form',
            'type' => 'index',
            'fields' => array('form_id'),
        ),
    ),
    'optimistic_locking' => true,
    'unified_search' => true,
    'unified_search_default_enabled' => true,
);

if (!class_exists('VardefManager')) {
    require_once 'include/SugarObjects/VardefManager.php';
}
VardefManager::createVardef('stic_Assessments_Scores', 'stic_Assessments_Scores', array('basic', 'assignable', 'security_groups'));

// Set special values for SuiteCRM base fields
$dictionary['stic_Assessments_Scores']['fields']['name']['required'] = '0'; // Name is not required in this module
$dictionary['stic_Assessments_Scores']['fields']['name']['importable'] = true; // Name is importable but not required in this module
$dictionary['stic_Assessments_Scores']['fields']['description']['rows'] = '2'; // Make textarea fields shorter
