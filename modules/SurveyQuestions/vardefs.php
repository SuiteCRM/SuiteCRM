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

$dictionary['SurveyQuestions'] = array(
    'table'              => 'surveyquestions',
    'audited'            => true,
    'inline_edit'        => true,
    'duplicate_merge'    => true,
    'fields'             => array(
        'sort_order'                              => array(
            'required'                  => false,
            'name'                      => 'sort_order',
            'vname'                     => 'LBL_SORT_ORDER',
            'type'                      => 'int',
            'massupdate'                => 0,
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
            'len'                       => '255',
            'size'                      => '20',
            'enable_range_search'       => false,
            'disable_num_format'        => '',
            'min'                       => false,
            'max'                       => false,
        ),
        'type'                                    => array(
            'required'                  => false,
            'name'                      => 'type',
            'vname'                     => 'LBL_TYPE',
            'type'                      => 'enum',
            'massupdate'                => 0,
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
            'options'                   => 'surveys_question_type',
            'studio'                    => 'visible',
            'dependency'                => false,
        ),
        'happiness_question'                      => array(
            'required' => false,
            'name'     => 'happiness_question',
            'vname'    => 'LBL_HAPPINESS_QUESTION',
            'type'     => 'bool',
        ),
        "surveyquestions_surveyquestionoptions"   => array(
            'name'         => 'surveyquestions_surveyquestionoptions',
            'type'         => 'link',
            'relationship' => 'surveyquestions_surveyquestionoptions',
            'source'       => 'non-db',
            'module'       => 'SurveyQuestionOptions',
            'bean_name'    => 'SurveyQuestionOptions',
            'side'         => 'right',
            'vname'        => 'LBL_SURVEYQUESTIONS_SURVEYQUESTIONOPTIONS_FROM_SURVEYQUESTIONOPTIONS_TITLE',
        ),
        "surveyquestions_surveyquestionresponses" => array(
            'name'         => 'surveyquestions_surveyquestionresponses',
            'type'         => 'link',
            'relationship' => 'surveyquestions_surveyquestionresponses',
            'source'       => 'non-db',
            'module'       => 'SurveyQuestionResponses',
            'bean_name'    => 'SurveyQuestionResponses',
            'side'         => 'right',
            'vname'        => 'LBL_SURVEYQUESTIONS_SURVEYQUESTIONRESPONSES_FROM_SURVEYQUESTIONRESPONSES_TITLE',
        ),
        "survey"                                  => array(
            'name'         => 'survey',
            'type'         => 'link',
            'relationship' => 'surveys_surveyquestions',
            'source'       => 'non-db',
            'module'       => 'Surveys',
            'bean_name'    => 'Surveys',
            'vname'        => 'LBL_SURVEYS_SURVEYQUESTIONS_FROM_SURVEYS_TITLE',
            'id_name'      => 'survey_id',
            'link_type'    => 'one',
            'side'         => 'left',
        ),
        "survey_name"                             => array(
            'name'    => 'survey_name',
            'type'    => 'relate',
            'source'  => 'non-db',
            'vname'   => 'LBL_SURVEYS_SURVEYQUESTIONS_FROM_SURVEYS_TITLE',
            'save'    => true,
            'id_name' => 'survey_id',
            'link'    => 'survey',
            'table'   => 'surveys',
            'module'  => 'Surveys',
            'rname'   => 'name',
        ),
        "survey_id"                               => array(
            'name'       => 'survey_id',
            'type'       => 'id',
            'reportable' => false,
            'vname'      => 'LBL_SURVEYS_SURVEYQUESTIONS_FROM_SURVEYQUESTIONS_TITLE',
        ),
    ),
    'relationships'      => array(
        'surveyquestions_surveyquestionoptions' => array(
            'rhs_module'        => 'SurveyQuestionOptions',
            'rhs_table'         => 'surveyquestionoptions',
            'rhs_key'           => 'survey_question_id',
            'lhs_module'        => 'SurveyQuestions',
            'lhs_table'         => 'surveyquestions',
            'lhs_key'           => 'id',
            'relationship_type' => 'one-to-many',
        ),
       "surveyquestions_surveyquestionresponses" => array(
            'rhs_module'        => 'SurveyQuestionResponses',
            'rhs_table'         => 'surveyquestionresponses',
            'rhs_key'           => 'surveyquestion_id',
            'lhs_module'        => 'SurveyQuestions',
            'lhs_table'         => 'surveyquestions',
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
VardefManager::createVardef('SurveyQuestions', 'SurveyQuestions', array('basic', 'assignable', 'security_groups'));
