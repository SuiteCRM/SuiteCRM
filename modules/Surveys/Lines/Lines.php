<?php

function survey_questions_display(Surveys $focus, $field, $value, $view)
{
    $hasResponses = !empty($focus->id) && $focus->get_linked_beans('surveys_surveyresponses');
    if ($view == 'EditView' && !$hasResponses) {
        return survey_questions_display_edit($focus, $field, $value, $view);
    }

    return survey_questions_display_detail($focus, $field, $value, $view);
}

function survey_questions_display_detail(Surveys $focus, $field, $value, $view)
{
    global $app_list_strings, $mod_strings;
    $smarty = new Sugar_Smarty();
    $questionBeans = $focus->get_linked_beans('surveys_surveyquestions', 'SurveyQuestions');
    $questions = array();
    foreach ($questionBeans as $questionBean) {
        $questions[] = $questionBean->toArray();
    }
    usort(
        $questions,
        function ($a, $b) {
            return $a['sort_order'] - $b['sort_order'];
        }
    );
    $smarty->assign('questions', $questions);
    $smarty->assign('message', '');
    $isDuplicate = $_REQUEST['isDuplicate'] ?? 'false';
    if ($view == 'EditView' && isFalse($isDuplicate)) {
        $smarty->assign('message', $mod_strings['LBL_CANT_EDIT_RESPONDED']);
    }
    $smarty->assign('APP_LIST', $app_list_strings);
    $html = $smarty->fetch(get_custom_file_if_exists('modules/Surveys/tpls/detailsurveyquestions.tpl'));

    return $html;
}

function survey_questions_display_edit(Surveys $focus, $field, $value, $view)
{
    global $mod_strings, $app_list_strings;
    $smarty = new Sugar_Smarty();
    if (empty($focus->id)) {
        $questionBeans = array();
    } else {
        $questionBeans = $focus->get_linked_beans('surveys_surveyquestions', 'SurveyQuestions', 'sort_order');
    }
    $questions = array();
    foreach ($questionBeans as $questionBean) {
        $question = array();
        $question['id'] = $questionBean->id;
        $question['name'] = $questionBean->name;
        $question['type'] = $questionBean->type;
        $question['sort_order'] = $questionBean->sort_order;
        $question['options'] = array();
        foreach ($questionBean->get_linked_beans(
            'surveyquestions_surveyquestionoptions',
            'SurveyQuestionOptions',
            'sort_order'
        ) as $option) {
            $optionArr = array();
            $optionArr['id'] = $option->id;
            $optionArr['name'] = $option->name;
            $question['options'][] = $optionArr;
        }
        $questions[] = $question;
    }

    $smarty->assign('MOD', $mod_strings);
    $smarty->assign('questions', $questions);
    $questionBean = BeanFactory::getBean('SurveyQuestions');
    $options = $questionBean->field_defs['type']['options'];
    $typeSelect = get_select_options_with_id($app_list_strings[$options], '');
    $typeSelect = str_replace("\n", '', (string) $typeSelect);
    $smarty->assign('question_type_options', $typeSelect);
    $html = $smarty->fetch(get_custom_file_if_exists('modules/Surveys/tpls/editsurveyquestions.tpl'));

    return $html;
}
