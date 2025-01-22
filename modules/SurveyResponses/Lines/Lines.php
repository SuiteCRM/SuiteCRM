<?php

function question_responses_display(SurveyResponses $focus, $field, $value, $view)
{
    if ($view == 'EditView') {
        return '';
    }
    global $app_list_strings;
    $smarty = new Sugar_Smarty();
    $questionResponseBeans =
        $focus->get_linked_beans('surveyresponses_surveyquestionresponses', 'SurveyQuestionResponses');
    $questionResponseMap = array();
    foreach ($questionResponseBeans as $questionResponseBean) {
        if (empty($questionResponseMap[$questionResponseBean->surveyquestion_id])) {
            $questionResponseMap[$questionResponseBean->surveyquestion_id] = array();
        }
        $questionResponseMap[$questionResponseBean->surveyquestion_id][] = $questionResponseBean;
    }
    $questionResponses = array();
    foreach ($questionResponseMap as $questionId => $questionResponseArr) {
        $data = array();
        $question = BeanFactory::getBean('SurveyQuestions', $questionId);
        $data['sort_order'] = $question->sort_order;
        $data['questionName'] = $question->name;
        $data['answer'] = convertQuestionResponseForDisplay($questionResponseArr, $question->type);
        $questionResponses[] = $data;
    }
    usort(
        $questionResponses,
        function ($a, $b) {
            return $a['sort_order'] - $b['sort_order'];
        }
    );
    $smarty->assign('questionResponses', $questionResponses);
    $smarty->assign('APP_LIST', $app_list_strings);
    $html = $smarty->fetch(get_custom_file_if_exists('modules/SurveyResponses/tpls/detailquestionresponses.tpl'));

    return $html;
}

function convertQuestionResponseForDisplay($responseArr, $type)
{
    global $timedate, $app_list_strings;
    if (empty($responseArr)) {
        return '';
    }
    switch ($type) {
        case "Checkbox":
            return $responseArr[0]->answer_bool ? '<img width=20 src="modules/Surveys/imgs/checked.png"/>' : '';
        case "Radio":
        case "Dropdown":
        case "Multiselect":
            $bits = array();
            foreach ($responseArr as $response) {
                $options =
                    $response->get_linked_beans(
                        'surveyquestionoptions_surveyquestionresponses',
                        'SurveyQuestionOptions'
                    );
                foreach ($options as $option) {
                    $bits[] = $option->name;
                }
            }

            return implode(',', $bits);
        case "Matrix":
            $str = '<dl>';
            $strArr = array();
            foreach ($responseArr as $response) {
                $options =
                    $response->get_linked_beans(
                        'surveyquestionoptions_surveyquestionresponses',
                        'SurveyQuestionOptions'
                    );
                $tmpStr = '';
                foreach ($options as $option) {
                    $tmpStr .= '<dt>' . $option->name . '</dt>';
                    $sortOrder = $option->sort_order;
                }
                if (!empty($app_list_strings['surveys_matrix_options'][$response->answer])) {
                    $tmpStr .= '<dd>' . $app_list_strings['surveys_matrix_options'][$response->answer] . '</dd>';
                } else {
                    $tmpStr .= '<dd>' . $response->answer . '</dd>';
                }
                $strArr[] = array('str' => $tmpStr, 'sort_order' => $sortOrder);
            }
            usort(
                $strArr,
                function ($a, $b) {
                    return $a['sort_order'] - $b['sort_order'];
                }
            );
            foreach ($strArr as $bits) {
                $str .= $bits['str'];
            }
            $str .= '</dl>';

            return $str;
        case "DateTime":
            return $responseArr[0]->answer_datetime;
        case "Date":
            $date = $timedate->fromUser($responseArr[0]->answer_datetime);
            if (!$date) {
                return $responseArr[0]->answer_datetime;
            } else {
                $date = $timedate->tzGMT($date);

                return $timedate->asUserDate($date);
            }
            // no break
        case "Rating":
            $answer = $responseArr[0]->answer ?? '';
            if (is_int($answer)) {
                return str_repeat('<img width=20 src="modules/Surveys/imgs/star.png"/>', $answer);
            }
            return '';
        case "Scale":
            return $responseArr[0]->answer . '/10';
        case "Textbox":
        case "Text":
        default:
            return $responseArr[0]->answer;
    }
}
