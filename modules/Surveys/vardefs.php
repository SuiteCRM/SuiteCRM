<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

$dictionary['Surveys'] = array(
    'table'              => 'surveys',
    'audited'            => true,
    'inline_edit'        => true,
    'duplicate_merge'    => true,
    'fields'             => array(
        'status'                   => array(
            'required'                  => false,
            'name'                      => 'status',
            'vname'                     => 'LBL_STATUS',
            'type'                      => 'enum',
            'massupdate'                => 0,
            'default'                   => 'LBL_DRAFT',
            'no_default'                => false,
            'comments'                  => '',
            'help'                      => '',
            'importable'                => 'true',
            'duplicate_merge'           => 'disabled',
            'duplicate_merge_dom_value' => '0',
            'audited'                   => false,
            'inline_edit'               => true,
            'reportable'                => true,
            'unified_search'            => false,
            'merge_filter'              => 'disabled',
            'len'                       => 100,
            'size'                      => '20',
            'options'                   => 'survey_status_list',
            'studio'                    => 'visible',
            'dependency'                => false,
        ),
        'survey_questions_display' => array(
            'name'                      => 'survey_questions_display',
            'vname'                     => 'LBL_SURVEY_QUESTIONS_DISPLAY',
            'type'                      => 'function',
            'source'                    => 'non-db',
            'massupdate'                => 0,
            'importable'                => 'false',
            'duplicate_merge'           => 'disabled',
            'duplicate_merge_dom_value' => 0,
            'audited'                   => false,
            'reportable'                => false,
            'inline_edit'               => false,
            'function'                  => array(
                'name'    => 'survey_questions_display',
                'returns' => 'html',
                'include' => 'modules/Surveys/Lines/Lines.php'
            ),
        ),
        'survey_url_display'       => array(
            'name'                      => 'survey_url_display',
            'vname'                     => 'LBL_SURVEY_URL_DISPLAY',
            'type'                      => 'function',
            'source'                    => 'non-db',
            'massupdate'                => 0,
            'importable'                => 'false',
            'duplicate_merge'           => 'disabled',
            'duplicate_merge_dom_value' => 0,
            'audited'                   => false,
            'reportable'                => false,
            'inline_edit'               => false,
            'function'                  => array(
                'name'    => 'survey_url_display',
                'returns' => 'html',
                'include' => 'modules/Surveys/Utils/utils.php'
            ),
        ),

        'submit_text'              => array(
            'required'   => false,
            'name'       => 'submit_text',
            'vname'      => 'LBL_SUBMIT_TEXT',
            'type'       => 'varchar',
            'massupdate' => 0,
            'default'    => 'Submit',
        ),
        'satisfied_text'           => array(
            'required'   => false,
            'name'       => 'satisfied_text',
            'vname'      => 'LBL_SATISFIED_TEXT',
            'type'       => 'varchar',
            'massupdate' => 0,
            'default'    => 'Satisfied',
        ),
        'neither_text'             => array(
            'required'   => false,
            'name'       => 'neither_text',
            'vname'      => 'LBL_NEITHER_TEXT',
            'type'       => 'varchar',
            'massupdate' => 0,
            'default'    => 'Neither Satisfied nor Dissatisfied',
        ),
        'dissatisfied_text'        => array(
            'required'   => false,
            'name'       => 'dissatisfied_text',
            'vname'      => 'LBL_DISSATISFIED_TEXT',
            'type'       => 'varchar',
            'massupdate' => 0,
            'default'    => 'Dissatisfied',
        ),
        "surveys_surveyquestions"  => array(
            'name'         => 'surveys_surveyquestions',
            'type'         => 'link',
            'relationship' => 'surveys_surveyquestions',
            'source'       => 'non-db',
            'module'       => 'SurveyQuestions',
            'bean_name'    => 'SurveyQuestions',
            'side'         => 'right',
            'vname'        => 'LBL_SURVEYS_SURVEYQUESTIONS_FROM_SURVEYQUESTIONS_TITLE',
        ),
        "surveys_surveyresponses"  => array(
            'name'         => 'surveys_surveyresponses',
            'type'         => 'link',
            'relationship' => 'surveys_surveyresponses',
            'source'       => 'non-db',
            'module'       => 'SurveyResponses',
            'bean_name'    => 'SurveyResponses',
            'side'         => 'right',
            'vname'        => 'LBL_SURVEYS_SURVEYRESPONSES_FROM_SURVEYRESPONSES_TITLE',
        ),
    ),
    'relationships'      => array(
        'surveys_surveyquestions' => array(
            'rhs_module'        => 'SurveyQuestions',
            'rhs_table'         => 'surveyquestions',
            'rhs_key'           => 'survey_id',
            'lhs_module'        => 'Surveys',
            'lhs_table'         => 'surveys',
            'lhs_key'           => 'id',
            'relationship_type' => 'one-to-many',
        ),
        'surveys_surveyresponses' => array(
            'rhs_module'        => 'SurveyResponses',
            'rhs_table'         => 'surveyresponses',
            'rhs_key'           => 'survey_id',
            'lhs_module'        => 'Surveys',
            'lhs_table'         => 'surveys',
            'lhs_key'           => 'id',
            'relationship_type' => 'one-to-many',
        ),
        'surveys_campaigns' => array(
            'rhs_module'        => 'Campaigns',
            'rhs_table'         => 'campaigns',
            'rhs_key'           => 'survey_id',
            'lhs_module'        => 'Surveys',
            'lhs_table'         => 'surveys',
            'lhs_key'           => 'id',
            'relationship_type' => 'one-to-many',
        ),
    ),
    'optimistic_locking' => true,
    'unified_search'     => true,
);
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('Surveys', 'Surveys', array('basic', 'assignable', 'security_groups'));
