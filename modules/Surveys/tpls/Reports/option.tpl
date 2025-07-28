<table style="width: 25%" class="table table-bordered">
    <tr>
        <th>{$mod.LBL_RESPONSE_ANSWER}</th>
        <th>{$mod.LBL_RESPONSE_COUNT}</th>
    </tr>
    {foreach from=$question.responses item=response}
        <tr>
            <td>{$response.label}</td>
            <td>{$response.count}</td>
        </tr>
    {/foreach}
</table>
<canvas width="750" height="400" id='{$question.id}Chart' class=''></canvas>
<script>
    {literal}
    $(document).ready(function () {
      var chartData = {/literal}{$question.chartData|@array_values|@json_encode}{literal};
      var chartLabels = {/literal}{$question.chartLabels|@array_values|@json_encode}{literal};
      var max = Math.max.apply(null, chartData);
      new RGraph.Bar({
        id: '{/literal}{$question.id}{literal}Chart',
        data: [chartData],
        options: {
          title: '{/literal}{$question.name}{literal}',
          textSize: 10,
          textAngle: 40,
          titleSize: 16,
          titleFont: 'Lato',
          titleY: 10,
          colors: ['#f08377', '#534d64', '#778591', '#bfcad3', '#d8f5ee'],
          gutterBottom: 110,
          gutterTop: 60,
          gutterLeft: 50,
          tooltips: function (ind) {
            return chartLabels[ind] + " - " + chartData[ind];
          },
          tooltipsEvent: 'onmousemove',
          labels: chartLabels,
            {/literal}
            {* STIC Custom 20250711 JBL - Avoid error in max with invalid args *}
            {* https://github.com/SinergiaTIC/SinergiaCRM/pull/722 *}
            {* {if $question.chartData|@array_values|@max < 10} *}
            {if $question.chartData && is_array($question.chartData) && $question.chartData|@count > 0 && $question.chartData|@array_values|@max < 10}
            {* END STIC Custom *}
        numyticks: max,
          ylabelsCount: max,
          ymax: max,
            {/if}
            {literal}
        }
      }).draw();
    });
    {/literal}
</script>
