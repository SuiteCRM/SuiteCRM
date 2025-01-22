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
      var chartData = {/literal}{$question.chartData|@json_encode}{literal};
      var chartLabels = {/literal}{$question.chartLabels|@json_encode}{literal};
      new RGraph.Pie({
        id: '{/literal}{$question.id}{literal}Chart',
        data: chartData,
        options: {
          title: '{/literal}{$question.name}{literal}',
          radius: 140,
          gutterTop: 60,
          textSize: 10,
          titleSize: 16,
          titleFont: 'Lato',
          colors: ['#f08377', '#534d64', '#778591', '#bfcad3', '#d8f5ee'],
          tooltips: function (ind) {
            return chartLabels[ind] + " - " + chartData[ind];
          },
          tooltipsEvent: 'onmousemove',
          labels: chartLabels,
        }
      }).draw();
    });
    {/literal}
</script>
