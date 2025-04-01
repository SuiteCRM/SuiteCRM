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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Surveys extends Basic
{

    /** @var bool $new_schema */
    public $new_schema = true;

    /** @var string $module_dir */
    public $module_dir = 'Surveys';

    /** @var string $object_name */
    public $object_name = 'Surveys';

    /** @var string $table_name */
    public $table_name = 'surveys';

    /** @var bool $importable */
    public $importable = false;

    /**
     * To ensure that modules created and deployed under CE will continue to function under team security
     * if the instance is upgraded to PRO
     * @var bool $disable_row_level_security
     */
    public $disable_row_level_security = true;

    /** @var string $id */
    public $id;

    /** @var  string $name */
    public $name;

    /** @var  string $date_entered */
    public $date_entered;

    /** @var  string $date_modified */
    public $date_modified;

    /** @var  string $modified_user_id */
    public $modified_user_id;

    /** @var  string $modified_by_name */
    public $modified_by_name;

    /** @var  string $created_by */
    public $created_by;

    /** @var  string $created_by_name */
    public $created_by_name;

    /** @var  string $description */
    public $description;

    /** @var  int|bool $deleted */
    public $deleted;

    /** @var  string $created_by_link */
    public $created_by_link;

    /** @var  string $modified_user_link */
    public $modified_user_link;

    /** @var  string $assigned_user_id */
    public $assigned_user_id;

    /** @var  string $assigned_user_name */
    public $assigned_user_name;

    /** @var  string $assigned_user_link */
    public $assigned_user_link;

    /** @var  string $SecurityGroups
     * */
    public $SecurityGroups;

    /** @var  string $status */
    public $status;

    /**
     * Surveys constructor.
     */
    public function __construct()
    {
        parent::__construct();
    }

    /**
     * @param string $interface
     * @return bool
     */
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    /**
     * @param bool $check_notify
     * @return string
     */
    public function save($check_notify = false)
    {
        $res = parent::save($check_notify);
        if (empty($_REQUEST['survey_questions_supplied'])) {
            return $res;
        }

        foreach ($_REQUEST['survey_questions_names'] as $key => $val) {
            if (!empty($_REQUEST['survey_questions_ids'][$key])) {
                $question = BeanFactory::getBean('SurveyQuestions', $_REQUEST['survey_questions_ids'][$key]);
            } else {
                $question = BeanFactory::newBean('SurveyQuestions');
            }
            $question->name = $val;
            $question->type = $_REQUEST['survey_questions_types'][$key];
            $question->sort_order = $_REQUEST['survey_questions_sortorder'][$key];
            $question->survey_id = $this->id;
            $question->deleted = $_REQUEST['survey_questions_deleted'][$key];
            $question->save();
            if (!empty($_REQUEST['survey_questions_options'][$key])) {
                $this->saveOptions(
                    $_REQUEST['survey_questions_options'][$key],
                    $_REQUEST['survey_questions_options_id'][$key],
                    $_REQUEST['survey_questions_options_deleted'][$key],
                    $question->id
                );
            }
        }

        return $res;
    }

    /**
     * @param array $options
     * @param array $ids
     * @param array $deleted
     * @param string $questionId
     */
    private function saveOptions(array $options, array $ids, array $deleted, $questionId)
    {
        foreach ($options as $key => $option) {
            if (!empty($ids[$key])) {
                $optionBean = BeanFactory::getBean('SurveyQuestionOptions', $ids[$key]);
            } else {
                $optionBean = BeanFactory::newBean('SurveyQuestionOptions');
            }
            if ($deleted[$key]) {
                $optionBean->deleted = 1;
            }
            $optionBean->name = $option;
            $optionBean->survey_question_id = $questionId;
            $optionBean->sort_order = $key;
            $optionBean->save();
        }
    }

    /**
     * @return array
     */
    public function getMatrixOptions()
    {
        return array(
            0 => !empty($this->satisfied_text) ? $this->satisfied_text : 'Satisfied',
            1 => !empty($this->neither_text) ? $this->neither_text : 'Neither Satisfied nor Dissatisfied',
            2 => !empty($this->dissatisfied_text) ? $this->dissatisfied_text : 'Dissatisfied',
        );
    }

    /**
     * @return string
     */
    public function getSubmitText()
    {
        if (!empty($this->submit_text)) {
            return $this->submit_text;
        }

        return "Submit";
    }
}
