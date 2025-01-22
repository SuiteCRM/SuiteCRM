<h1>{$survey.name}</h1>
<table>
    <tr>
        <th>{$MOD.LBL_SURVEY_RESPONSES}</th>
        <td class="survey-report-summary">{$responsesCount}</td>
    </tr>
    <tr>
        <th>{$MOD.LBL_SURVEYS_SENT}</th>
        <td class="survey-report-summary">{$surveysSent}</td>
    </tr>
    <tr>
        <th>{$MOD.LBL_SURVEY_DISTINCT}</th>
        <td class="survey-report-summary">{$surveysSentDistinct}</td>
    </tr>
</table>
<style>
    {literal}
    .survey-report-summary
    {
        padding-left: 2em;
    }
    {/literal}
</style>


{foreach from=$data key=i item=question}
    <hr>
    <div class="panel panel-default">
        <div class="panel-heading">
            <a class="" role="button" data-toggle="collapse" href="#top-panel-{$i}" aria-expanded="false">
                <div class="col-xs-10 col-sm-11 col-md-11">
                    <h4>{$question.name}</h4>
                </div>
            </a>
        </div>
        <div class="panel-body panel-collapse collapse in" id="top-panel-{$i}" style="padding: 50px;">
            <div class="tab-content">
                {if $question.type == 'Checkbox'}
                    {include file='modules/Surveys/tpls/Reports/checkbox.tpl'}
                {elseif $question.type == 'Dropdown' || $question.type == 'Multiselect' || $question.type == 'Radio' || $question.type == 'Rating' || $question.type == 'Scale'}
                    {include file='modules/Surveys/tpls/Reports/option.tpl'}
                {elseif $question.type == 'Matrix'}
                    {include file='modules/Surveys/tpls/Reports/matrix.tpl'}
                {else}
                    {include file='modules/Surveys/tpls/Reports/other.tpl'}
                {/if}
            </div>
        </div>
    </div>
{/foreach}
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.common.core.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.common.dynamic.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.common.key.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.common.effects.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.common.tooltips.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.common.context.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.common.annotate.js'></script>

<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.funnel.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.drawing.rect.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.drawing.text.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.pie.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.bar.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.line.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.radar.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.hbar.js'></script>
<script type='text/javascript' src='include/SuiteGraphs/rgraph/libraries/RGraph.rose.js'></script>
<script type='text/javascript' src='modules/Surveys/javascript/Survey.js'></script>
<script>
    {literal}
    $(document).ready(function () {
        Survey.showHide($('.showHideResponses'));
    });
    {/literal}
</script>
