{foreach from=$question.responses key=responseId item=response}
    <h3>{$response.label}</h3>
    <table style="width: 25%" class="table table-bordered">
        <tr>
            <th>{$mod.LBL_RESPONSE_ANSWER}</th>
            <th>{$mod.LBL_RESPONSE_COUNT}</th>
        </tr>
        {foreach from=$response.options item=option}
            <tr>
                <td>{$option.label}</td>
                <td>{$option.count}</td>
            </tr>
        {/foreach}
    </table>
    <canvas width="750" height="400" id='{$responseId}Chart' class=''></canvas>
    <script>
        {literal}
        $(document).ready(function () {
          var chartData = {/literal}{$response.chartData|@array_values|@json_encode}{literal};
          var chartLabels = {/literal}{$response.chartLabels|@array_values|@json_encode}{literal};
          var max = Math.max.apply(null, chartData);
          new RGraph.Bar({
            id: '{/literal}{$responseId}{literal}Chart',
            data: chartData,
            options: {
              title: '{/literal}{$question.name} - {$response.label}{literal}',
              textSize: 10,
              titleSize: 16,
              titleFont: 'Lato',
              titleY: 10,
              textAngle: 30,
              colorsSequential: true,
              colors: ['#f08377', '#534d64', '#778591', '#bfcad3', '#d8f5ee'],
              gutterTop: 60,
              gutterBottom: 110,
              gutterLeft: 50,
              tooltips: function (ind) {
                return chartLabels[ind] + " - " + chartData[ind];
              },
              tooltipsEvent: 'onmousemove',
              labels: chartLabels,
                {/literal}
                {if $response.chartData|@array_values|@max < 10}
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
{/foreach}
