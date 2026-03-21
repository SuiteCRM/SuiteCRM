<?php
use SuiteCRM\Utility\SuiteValidator;

//Grab the survey
if (empty($_REQUEST['id'])) {
    header('HTTP/1.0 404 Not Found');
    exit();
}

$isValidator = new SuiteValidator();
$surveyId = '';

if (!empty($_REQUEST['id']) && $isValidator->isValidId($_REQUEST['id'])) {
    $surveyId = $_REQUEST['id'];
} else {
    LoggerManager::getLogger()->warn('Invalid survey ID.');
}

$survey = BeanFactory::getBean('Surveys', $surveyId);

if (empty($survey->id)) {
    header('HTTP/1.0 404 Not Found');
    exit();
}
if ($survey->status === 'Closed') {
    displayClosedPage($survey);
    exit();
}
if ($survey->status !== 'Public') {
    header('HTTP/1.0 404 Not Found');
    exit();
}

$contactId = '';

if (!empty($_REQUEST['contact']) && $isValidator->isValidId($_REQUEST['contact'])) {
    $contactId = $_REQUEST['contact'];
} else {
    LoggerManager::getLogger()->warn('Invalid contact ID in survey.');
}

$trackerId = '';

if (!empty($_REQUEST['tracker']) && $isValidator->isValidId($_REQUEST['tracker'])) {
    $trackerId = $_REQUEST['tracker'];
} else {
    LoggerManager::getLogger()->warn('Invalid tracker ID in survey.');
}

$themeObject = SugarThemeRegistry::current();
$companyLogoURL = $themeObject->getImageURL('company_logo.png');

require_once 'modules/Campaigns/utils.php';
if ($trackerId) {
    $surveyLinkTracker = getSurveyLinkTracker($trackerId);
    log_campaign_activity($trackerId, 'link', true, $surveyLinkTracker);
}

function getSurveyLinkTracker($trackerId)
{
    $db = DBManagerFactory::getInstance();
    $trackerId = $db->quote($trackerId);
    $sql = <<<EOF
SELECT id 
FROM campaign_trkrs 
WHERE campaign_id IN (
            SELECT campaign_id 
            FROM campaign_log WHERE target_tracker_key = "$trackerId"
            ) 
AND tracker_name = "SurveyLinkTracker"
EOF;

    $row = $db->fetchOne($sql);
    if ($row) {
        return $row['id'];
    }

    return false;
}

?>
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">

        <title><?= $survey->name ?></title>

        <link href="themes/SuiteP/css/bootstrap.min.css" rel="stylesheet">
        <link href="modules/Surveys/javascript/rating/rating.min.css" rel="stylesheet">
        <link href="modules/Surveys/javascript/datetimepicker/jquery.dateandtime.css" rel="stylesheet">
        <link href="include/javascript/jquery/themes/base/jquery.ui.all.css" rel="stylesheet">
        <link href="modules/Surveys/javascript/survey.css" rel="stylesheet">
    </head>
    <body>
    <div class="container">
        <div class="text-center">
                <img src="<?php echo $companyLogoURL ?>"/>
        </div>
        <div class="pad">
        <div class="row well">
            <div class="col-md-offset-2 col-md-8">
                <h1><?= $survey->name ?></h1>
                <?= displaySurvey($survey, $contactId, $trackerId); ?>
            </div>
        </div>
    </div>
    <script src="include/javascript/jquery/jquery-min.js"></script><script src="include/javascript/jquery/jquery-ui-min.js"></script>
    <script src="modules/Surveys/javascript/datetimepicker/jquery.dateandtime.js"></script>
    <script src="modules/Surveys/javascript/rating/rating.min.js"></script>
    <script>

      $(function () {
        $(".datetimefield").dateAndTime();
        
        new StarRating('.starRating', {
          tooltip : false,
        });
      });
    </script>
    </body>
    </html>


<?php
function displaySurvey($survey, $contactId, $trackerId)
{
    ?>
    <form method="post" onsubmit="disableSubmitButton(this)">
        <input type="hidden" name="entryPoint" value="surveySubmit">
        <input type="hidden" name="id" value="<?= $survey->id ?>">
        <input type="hidden" name="contact" value="<?= $contactId ?>">
        <input type="hidden" name="tracker" value="<?= $trackerId ?>">
        <?php
        $questions = $survey->get_linked_beans('surveys_surveyquestions', 'SurveyQuestions');
    usort(
            $questions,
            function ($a, $b) {
                return $a->sort_order - $b->sort_order;
            }
        );
    foreach ($questions as $question) {
        displayQuestion($survey, $question);
    } ?>
        <button class="btn btn-primary" type="submit"><?php echo $survey->getSubmitText(); ?></button>
    </form>
    <script>
        function disableSubmitButton(form) {
            form.querySelector('button[type="submit"]').disabled = true; 
        }
    </script>    
    <?php
}

function displayQuestion($survey, $question)
{
    ?>
    <div class="panel panel-default">
        <div class="panel-heading">
            <h3 class="panel-title"><label for="question<?= $question->id ?>"><?= $question->name; ?></label></h3>
        </div>
        <div class="panel-body">
            <div class="form-group" type="<?= strtolower($question->type) ?>">
                <?php
                $options = array();
    foreach ($question->get_linked_beans(
                    'surveyquestions_surveyquestionoptions',
                    'SurveyQuestionOptions',
                    'sort_order'
                ) as $option) {
        $optionArr = array();
        $optionArr['id'] = $option->id;
        $optionArr['name'] = $option->name;
        $options[] = $optionArr;
    }
    switch ($question->type) {

                    case "Textbox":
                        echo "<textarea class=\"form-control\" id='question" .
                            $question->id .
                            "' name='question[" .
                            $question->id .
                            "]'></textarea>";
                        break;
                    case "Checkbox":
                        echo "<div class='checkbox'><label>";
                        echo "<input id='question" .
                            $question->id .
                            "' name='question[" .
                            $question->id .
                            "]' type='checkbox'/>";
                        echo "</label></div>";
                        break;
                    case "Radio":
                        displayRadioField($question, $options);
                        break;
                    case "Dropdown":
                        echo "<select class=\"form-control\" id='question".$question->id."' name='question[".$question->id."]'>";
                        echo "<option value=''></option>";
                        foreach ($options as $option) {
                            echo "<option value='".$option['id']."'>".$option['name']."</option>";
                        }
                        echo "</select>";
                        break;
                    case "Multiselect":
                        displayMultiselectField($question, $options);
                        break;
                    case "Matrix":
                        displayMatrixField($survey, $question, $options);
                        break;
                    case "Date":
                        displayDateField($question);
                        break;
                    case "DateTime":
                        displayDateTimeField($question);
                        break;
                    case "Rating":
                        displayRatingField($question);
                        break;
                    case "Scale":
                        displayScaleField($question);
                        break;
                    case "Text":
                    default:
                        displayTextField($question);
                        break;
                } ?>
            </div>
        </div>
    </div>
    <?php
}

function displayTextField($question)
{
    echo "<input class=\"form-control\" id='question" .
        $question->id .
        "' name='question[" .
        $question->id .
        "]' type='text'/>";
}

function displayRadioField($question, $options)
{
    echo "<div id='question" . $question->id . "' name='question[" . $question->id . "]' >";

    foreach ($options as $optionId => $option) {
        $id = 'question' . $question->id . '_' . $optionId;
        $name = 'question[' . $question->id . ']';
        displayRadioButton($option, $name, $id);
    }

    echo "</div>";
}

function displayRadioButton($option, $name, $id)
{
    echo "<div class='radio_button'>";
    echo "<input id='$id' name='" . $name . "' value='" . $option['id'] . "' type='radio'/>";
    echo "<label class='btn' for='$id' id='$id'>" . $option['name'] . "</label>";
    echo "</div>";
}

function displayMultiselectField($question, $options)
{
    echo "<div id='question".$question->id."' name='question[".$question->id."]' multiple=\"true\">";
    foreach ($options as $optionId => $checkbox) {
        $id = "question".$question->id.'_'.$optionId;
        echo "<div class='multi-checkbox'>";
        echo "<input 
                id ='$id'
                name =\"question[{$question->id}][]\"
                type =\"checkbox\"
                value = \"{$checkbox['id']}\"
             />";
        echo "<label class='btn' for='$id' id='$id'>".$checkbox['name']."</label>";
        echo "</div>";
    }
    echo "</div>";
}

function displayScaleField($question)
{
    echo "<table class='scale' id='question".$question->id."' name='question[".$question->id."]'>";
    $scaleMax = 10;
    
    echo "<tr class='multiple'>";
    echo "<td class=''>";
    echo "<div class='ScaleGridContainer'>";
    for ($x = 1; $x <= $scaleMax; $x++) {
        
        $name = "question[".$question->id."]";
        $id = 'question'.$question->id.'_'.$x;

        echo "<div class='pad'>";
        echo "<div class='radio_button'>";
        echo "<input id='$id' name='".$name."' value='".$x."' type='radio'/>";
        echo "<label class='btn' for='$id' id='$id'>".$x."</label>";
        echo "</div>";
        echo "</div>";
    }
    echo "</div>";
    echo "</td>";
    echo "</tr>";
    
    echo "</table>";
}

function displayRatingField($question)
{
    $ratingMax = 5;
    echo "<select class='starRating' id='question" . $question->id . "' name='question[" . $question->id . "]'>";
    echo "<option value=''>Select a rating</option>";
    for ($x = 1; $x <= $ratingMax; $x++) {
        echo "<option value='" . $x . "'>" . $x . "</option>";
    }
    echo "</select>";
}

function displayMatrixField($survey, $question, $options)
{
    $matrixOptions = $survey->getMatrixOptions();
    echo "<table class='table table-striped' id='question" . $question->id . "' name='question[" . $question->id . "]'>";
    echo "<tr>";
    echo "<th></th>";
    foreach ($matrixOptions as $matrixOption) {
        echo "<th>";
        echo trim(preg_replace('/\s+/', '<br />', $matrixOption));
        echo "</th>";
    }
    echo "</tr>";
    foreach ($options as $option) {
        echo "<tr class='radio_line'>";
        echo "<th class='matrix-option__label'>";
        echo $option['name'];
        echo "</th>";
        foreach ($matrixOptions as $x => $matrixOption) {
            $id = 'question' . $question->id . $option['id'] . '_' . $x;
            $name = 'question[' . $question->id . '][' . $option['id'] . ']';

            echo "<td>";
            echo "<div class='radio'><label class='radio_btn'>";
            echo "<input id='$id' name='" . $name . "' value='" . $x . "' type='radio'/>";
            echo "</label>";
            echo "</div>";
            echo "</td>";
        }
        echo "</tr>";
    }
    echo "</table>";
}

function displayDateTimeField($question)
{
    echo "<input class=\"form-control datetimefield\" id='question" . $question->id . "' name='question[" . $question->id . "]' type='text' />";
}

function displayDateField($question)
{
    echo "<input class=\"form-control datefield \" id='question" . $question->id . "' name='question[" . $question->id . "]' type='date' />";
}

function displayClosedPage($survey)
{
    $ss = new Sugar_Smarty();

    $header = <<<EOF
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>$survey->name</title>
        <link href="themes/SuiteP/css/bootstrap.min.css" rel="stylesheet">
</head>
<body>
EOF;

    $footer = <<<EOF
<script src="include/javascript/jquery/jquery-min.js"></script>
</body>
</html>
EOF;

    $ss->assign('MESSAGE', translate('LBL_SURVEY_CLOSE_RESPONSE', $survey->module_name));
    $ss->assign('HEADER', $header);
    $ss->assign('SURVEY', $survey);
    $ss->assign('LOGO', SugarThemeRegistry::current()->getImageURL('company_logo.png') );
    $ss->assign('FOOTER', $footer);

    echo $ss->fetch('modules/Surveys/tpls/closeSurvey.tpl');
}
