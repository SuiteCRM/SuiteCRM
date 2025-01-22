<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
require_once 'include/DetailView/DetailView2.php';

#[\AllowDynamicProperties]
class SurveysViewReports extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }

    private function getSurveyStats()
    {
        $db = DBManagerFactory::getInstance();
        $quotedId = $db->quote($this->bean->id);
        $sentQuery = <<<EOF
SELECT COUNT(campaign_log.target_id) AS sent, COUNT(DISTINCT campaign_log.target_id) AS distinct_sent 
FROM campaigns
INNER JOIN campaign_log ON (campaigns.id = campaign_log.campaign_id 
                            AND campaign_log.deleted = 0 
                            AND activity_type = 'targeted'
                            AND campaign_log.target_type = 'Contacts')
WHERE campaigns.survey_id = '$quotedId' AND campaigns.deleted = 0
EOF;

        $surveysSent = $db->fetchOne($sentQuery);

        return $surveysSent;
    }

    public function display()
    {
        global $mod_strings, $timedate;
        parent::display();
        $this->ss->assign('mod', $mod_strings);
        $this->ss->assign('survey', $this->bean->toArray());
        $responses =
            $this->bean->get_linked_beans('surveys_surveyresponses', 'SurveyResponses', 'surveyresponses.date_created');
        $this->ss->assign('responsesCount', is_countable($responses) ? count($responses) : 0);

        $sentCount = [];
        $distinctCount = [];

        $surveysSent = $this->getSurveyStats();
        if ($surveysSent) {
            $sentCount = $surveysSent['sent'];
            $distinctCount = $surveysSent['distinct_sent'];
        }

        $data = $this->generateSkeletonData($this->bean);

        foreach ($responses as $response) {
            foreach ($response->get_linked_beans('surveyresponses_surveyquestionresponses') as $questionResponse) {
                $questionId = $questionResponse->surveyquestion_id;
                $dataType = $data[$questionId]['type'] ?? '';
                switch ($dataType) {
                    case "Checkbox":
                        $answerBool = !empty($questionResponse->answer_bool) ? 1 : 0;
                        $data[$questionId]['responses'][$answerBool]['count']++;
                        $data[$questionId]['chartData'][$answerBool]++;
                        break;
                    case "DateTime":
                    case "Date":
                        $dateStr = $questionResponse->answer_datetime;
                        if ($data[$questionId]['type'] == 'Date') {
                            $date = $timedate->fromUser($questionResponse->answer_datetime);
                            if ($date) {
                                $date = $timedate->tzGMT($date);
                                $dateStr = $timedate->asUserDate($date);
                            }
                        }
                        $data[$questionId]['responses'][] = array(
                            'answer'  => $dateStr,
                            'contact' => !empty($response->contact_id) ? array(
                                'id'   => $response->contact_id,
                                'name' => $response->contact_name
                            ) : false,
                            'time'    => $questionResponse->date_entered,
                        );

                        break;
                    case "Matrix":
                        $options =
                            $questionResponse->get_linked_beans(
                                'surveyquestionoptions_surveyquestionresponses',
                                'SurveyQuestionOptions'
                            );
                        foreach ($options as $option) {
                            $data[$questionId]['responses'][$option->id]['options'][$questionResponse->answer]['count']++;
                            $data[$questionId]['responses'][$option->id]['chartData'][$questionResponse->answer]++;
                        }
                        break;
                    case "Multiselect":
                    case "Radio":
                    case "Dropdown":
                        $options =
                            $questionResponse->get_linked_beans(
                                'surveyquestionoptions_surveyquestionresponses',
                                'SurveyQuestionOptions'
                            );
                        foreach ($options as $option) {
                            $data[$questionId]['responses'][$option->id]['count']++;
                            $data[$questionId]['chartData'][$option->id]++;
                        }
                        break;
                    case "Rating":
                    case "Scale":
                        $data[$questionId]['chartData'][$questionResponse->answer]++;
                        $data[$questionId]['responses'][$questionResponse->answer]['count']++;
                        break;
                    case "Textbox":
                    case "Text":
                    default:
                        $data[$questionId]['responses'][] = array(
                            'answer'  => $questionResponse->answer,
                            'contact' => !empty($response->contact_id) ? array(
                                'id'   => $response->contact_id,
                                'name' => $response->contact_name
                            ) : false,
                            'time'    => $questionResponse->date_entered,
                        );
                        break;
                }
            }
        }

        $this->ss->assign('data', $data);
        $this->ss->assign('surveysSent', $sentCount);
        $this->ss->assign('surveysSentDistinct', $distinctCount);
        $html = $this->ss->fetch('modules/Surveys/tpls/reports.tpl');
        echo $html;
    }

    private function getCheckboxQuestionSkeleton($arr)
    {
        global $mod_strings;
        $arr['chartData'] = array(0 => 0, 1 => 0);
        $arr['chartLabels'] = array($mod_strings['LBL_UNCHECKED'], $mod_strings['LBL_CHECKED']);
        $arr['responses'][0] = array(
            'count' => 0,
            'label' => $mod_strings['LBL_UNCHECKED']
        );
        $arr['responses'][1] = array(
            'count' => 0,
            'label' => $mod_strings['LBL_CHECKED']
        );

        return $arr;
    }

    private function getMatrixQuestionSkeleton($arr, $options)
    {
        global $app_list_strings;
        foreach ($options as $option) {
            $arr['responses'][$option->id] = array(
                'options' => array(),
                'label'   => $option->name,
                'order'   => $option->sort_order
            );
            foreach ($app_list_strings['surveys_matrix_options'] as $key => $val) {
                $arr['responses'][$option->id]['options'][$key] = array('count' => 0, 'label' => $val);
                $arr['responses'][$option->id]['chartLabels'][$key] = $val . "  ";
                $arr['responses'][$option->id]['chartData'][$key] = 0;
            }
        }

        return $arr;
    }

    private function getChoiceQuestionSkeleton($arr, $options)
    {
        foreach ($options as $option) {
            $arr['chartLabels'][$option->id] = html_entity_decode((string) $option->name, ENT_QUOTES | ENT_HTML5);
            $arr['chartData'][$option->id] = 0;
            $arr['responses'][$option->id] = array(
                'count' => 0,
                'label' => $option->name,
                'order' => $option->sort_order
            );
        }

        return $arr;
    }

    private function getRatingQuestionSkeleton($arr)
    {
        global $mod_strings;

        for ($x = 1; $x <= 5; $x++) {
            $arr['chartLabels'][$x] = $x . $mod_strings['LBL_STARS'];
            $arr['chartData'][$x] = 0;
            $arr['responses'][$x] = array(
                'count' => 0,
                'label' => $x . $mod_strings['LBL_STARS'],
                'order' => $x
            );
        }

        return $arr;
    }

    private function getScaleQuestionSkeleton($arr)
    {
        for ($x = 1; $x <= 10; $x++) {
            $arr['chartLabels'][$x] = $x;
            $arr['chartData'][$x] = 0;
            $arr['responses'][$x] = array(
                'count' => 0,
                'label' => $x,
                'order' => $x
            );
        }

        return $arr;
    }

    private function generateSkeletonData(Surveys $survey)
    {
        $data = array();
        $questions = $survey->get_linked_beans('surveys_surveyquestions', 'SurveyQuestions', 'sort_order');
        foreach ($questions as $question) {
            $data[$question->id] = array(
                'id'        => $question->id,
                'name'      => $question->name,
                'type'      => $question->type,
                'order'     => $question->sort_order,
                'responses' => array(),
            );
            switch ($question->type) {
                case "Checkbox":
                    $data[$question->id] = $this->getCheckboxQuestionSkeleton($data[$question->id]);
                    break;
                case "Matrix":
                    $options =
                        $question->get_linked_beans(
                            'surveyquestions_surveyquestionoptions',
                            'SurveyQuestionOptions',
                            'sort_order'
                        );
                    $data[$question->id] = $this->getMatrixQuestionSkeleton($data[$question->id], $options);
                    break;
                case "Multiselect":
                case "Radio":
                case "Dropdown":
                    $options =
                        $question->get_linked_beans(
                            'surveyquestions_surveyquestionoptions',
                            'SurveyQuestionOptions',
                            'sort_order'
                        );
                    $data[$question->id] = $this->getChoiceQuestionSkeleton($data[$question->id], $options);
                    break;
                case "Rating":
                    $data[$question->id] = $this->getRatingQuestionSkeleton($data[$question->id]);
                    break;
                case "Scale":
                    $data[$question->id] = $this->getScaleQuestionSkeleton($data[$question->id]);
                    break;
                case "Textbox":
                case "Text":
                default:
                    //No action needed;
                    break;
            }
        }

        return $data;
    }
}
