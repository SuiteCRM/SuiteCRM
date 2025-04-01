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
        <link href="modules/Surveys/javascript/datetimepicker/jquery-ui-timepicker-addon.css" rel="stylesheet">
        <link href="include/javascript/jquery/themes/base/jquery.ui.all.css" rel="stylesheet">
    </head>
    <body>
    <div class="container">
        <div class="row">
            <div class="col-md-offset-3 col-md-6">
                <img src="<?php echo $companyLogoURL ?>"/>
            </div>
        </div>
        <div class="row well">
            <div class="col-md-offset-2 col-md-8">
                <h1><?= $survey->name ?></h1>
                <?= displaySurvey($survey, $contactId, $trackerId); ?>
            </div>
        </div>
    </div>
    <script src="include/javascript/jquery/jquery-min.js"></script>
    <script src="include/javascript/jquery/jquery-ui-min.js"></script>
    <script src="modules/Surveys/javascript/datetimepicker/jquery-ui-timepicker-addon.js"></script>
    <script src="modules/Surveys/javascript/rating/rating.min.js"></script>
    <script>

      $(function () {
        $(".datefield").datepicker({
          dateFormat: "yy-mm-dd"
        });
        $('.ui-datepicker-trigger').click(function (ev) {
          var target = $(ev.target);

          var datepicker = target.closest('.input-group').find('.datefield');
          console.log(datepicker);
          datepicker.datepicker('show');
        });
        $(".datetimefield").datetimepicker({
          dateFormat: "yy-mm-dd"
        });
        $('.ui-datetimepicker-trigger').click(function (ev) {
          var target = $(ev.target);

          var datetimepicker = target.closest('.input-group').find('.datetimefield');
          console.log(datetimepicker);
          datetimepicker.datetimepicker('show');
        });
        $('.starRating').rating();
      });
    </script>
    </body>
    </html>


<?php
function displaySurvey($survey, $contactId, $trackerId)
{
    ?>
    <form method="post">
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
            <div class="form-group">
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
                        foreach ($options as $option) {
                            echo "<div class='radio'>";
                            echo "<label>";
                            echo "<input  id='question" .
                                $question->id .
                                "' name='question[" .
                                $question->id .
                                "]' value='" .
                                $option['id'] .
                                "' type='radio'/>";
                            echo $option['name'];
                            echo "</label>";
                            echo "</div>";
                        }
                        break;
                    case "Dropdown":
                    case "Multiselect":
                        $multi = $question->type == 'Multiselect' ? ' multiple="true" ' : '';
                        $name =
                            $question->type == 'Multiselect' ? "question[" . $question->id . "][]" :
                                "question[" . $question->id . "]";
                        echo "<select class=\"form-control\" id='question" . $question->id . "' name='$name' $multi>";
                        foreach ($options as $option) {
                            echo "<option value='" . $option['id'] . "'>" . $option['name'] . "</option>";
                        }
                        echo "</select>";
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

function displayScaleField($question)
{
    echo "<table class='table'><tr>";
    $scaleMax = 10;
    for ($x = 1; $x <= $scaleMax; $x++) {
        echo "<th>" . $x . "</th>";
    }
    echo "</tr><tr>";
    for ($x = 1; $x <= $scaleMax; $x++) {
        echo "<td><input id='question" .
            $question->id .
            "' name='question[" .
            $question->id .
            "]' value='" .
            $x .
            "' type='radio'/></td>";
    }
    echo "</tr></table>";
}

function displayRatingField($question)
{
    $ratingMax = 5;
    echo "<div class='starRating'>";
    for ($x = 1; $x <= $ratingMax; $x++) {
        echo "<input class='rating' id='question" .
            $question->id .
            "' name='question[" .
            $question->id .
            "]' value='" .
            $x .
            "' type='radio'/>";
    }
    echo "</div>";
}

function displayMatrixField($survey, $question, $options)
{
    $matrixOptions = $survey->getMatrixOptions();
    echo "<table width='75%'>";
    echo "<tr>";
    echo "<th style='width:25%'></th>";
    foreach ($matrixOptions as $matrixOption) {
        echo "<th style='width:25%'>";
        echo $matrixOption;
        echo "</th>";
    }

    foreach ($options as $option) {
        echo "<tr>";
        echo "<td style='width:25%'>";
        echo $option['name'];
        echo "</td>";
        foreach ($matrixOptions as $x => $matrixOption) {
            echo "<td style='width:25%'><input  id='question" .
                $question->id .
                "' name='question[" .
                $question->id .
                "][" .
                $option['id'] .
                "]' 
value='" .
                $x .
                "' type='radio'/></td>";
        }
        echo "</tr>";
    }
    echo "</table>";
}

function displayDateTimeField($question)
{
    echo "<div class=\"input-group\">";
    echo "<input class=\"form-control datetimefield\" id='question" .
        $question->id .
        "' name='question[" .
        $question->id .
        "]' type='text'/>";
    echo "<div class=\"input-group-addon ui-datetimepicker-trigger\"><span class=\"suitepicon suitepicon-module-calendar\"></span></div></div>";
}

function displayDateField($question)
{
    echo "<div class=\"input-group\">";
    echo "<input class=\"form-control datefield\" id='question" .
        $question->id .
        "' name='question[" .
        $question->id .
        "]' type='text'/>";
    echo "<div class=\"input-group-addon ui-datepicker-trigger\"><span class=\"suitepicon suitepicon-module-calendar\"></span></div></div>";
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
