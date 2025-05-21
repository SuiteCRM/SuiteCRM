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

class SurveysLogicHooks
{

    public function after_save(&$bean, $event, $arguments)
    {

        // If mass duplicate
        if (!empty($_REQUEST['mass_duplicate'])) {
            global $db;
            include_once 'SticInclude/Utils.php';

            // *****************************************
            // 1) Duplicate related SurveyQuestions records
            $surveyQuestionsQuery = "SELECT
                                    id
                                FROM
                                    surveyquestions
                                WHERE
                                    deleted = 0
                                AND survey_id='{$bean->fromId}'";

            $surveyQuestionsResults = $db->query($surveyQuestionsQuery);

            while ($rowQuestions = $db->fetchByAssoc($surveyQuestionsResults)) {

                // Change the parent_id field to point to the new bean id
                $surveyQuestionsChanges = ['survey_id' => $bean->id];
                SticUtils::duplicateBeanRecord('SurveyQuestions', $rowQuestions['id'], $surveyQuestionsChanges);

                // *****************************************
                // 2) For each SurveyQuestion, duplicate related SurveQuestionOptions records
                $surveyQuestionOptionsQuery = "SELECT
                                    id
                                FROM
                                    surveyquestionoptions
                                WHERE
                                    deleted = 0
                                    AND survey_question_id = '{$rowQuestions['id']}'
                                    ";

                $surveyQuestionOptionsResults = $db->query($surveyQuestionOptionsQuery);

                while ($surveyQuestionOptionsRow = $db->fetchByAssoc($surveyQuestionOptionsResults)) {
                    // Change the survey_question_id field to point to the new bean id
                    $surveyQuestionOptionsChanges = ['survey_question_id' => $rowQuestions['id']];
                    SticUtils::duplicateBeanRecord('SurveyQuestionOptions', $surveyQuestionOptionsRow['id'], $surveyQuestionOptionsChanges);
                }
            }
        }
    }
}
