{if !isset({{sugarvar key='value' string=true}})}
    {assign var="value" value={{sugarvar key='default_value' string=true}} }
{else}
    {assign var="value" value={{sugarvar key='value' string=true}} }
{/if}
<span id="{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_cron_value">
    {$value}
</span>
<script>
    var id = '#{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_cron_value';
    var el = $(id);
    el.text(getCRONHumanReadable(el.text()));

    {literal}
    function getCRONHumanReadable(schedule){
        schedule = schedule.trim();
        {/literal}var weekdays = {{$weekday_vals}};{literal}
        var bits = schedule.split(' ');
        var mins = bits[0];
        var hours = bits[1];
        var day = bits[2];
        var month = bits[3];
        var weekday = bits[4];

        if(month !== '*'){
            return schedule;
        }
        if(mins === '*') {
            return schedule;
        }
        if(hours === '*') {
            return schedule;
        }
        if(mins.length < 2){
            mins = '0'+mins;
        }
        if(hours.length < 2){
            hours = '0'+hours;
        }
        if(weekday !== '*' && day !== '*'){
            return schedule;
        }else if(weekday !== '*'){
            var days = weekday.split(',');
            var dayLabels = [];
            for(var x = 0; x < days.length; x++){
                dayLabels[x] = weekdays[days[x]];
            }
            {/literal}
            return '{{$APP.LBL_CRON_WEEKLY}} {{$APP.LBL_CRON_ON_THE_WEEKDAY}} '+dayLabels.join(', ')+' {{$APP.LBL_CRON_AT}} '+hours+':'+mins;
            {literal}
        }else if(day !== '*'){
            {/literal}
            return '{{$APP.LBL_CRON_MONTHLY}} {{$APP.LBL_CRON_ON_THE_MONTHDAY}} '+day+' {{$APP.LBL_CRON_AT}} '+hours+':'+mins;
            {literal}
        }
        {/literal}
        return '{{$APP.LBL_CRON_DAILY}} {{$APP.LBL_CRON_AT}} '+hours+':'+mins
        {literal}

    }
    {/literal}
</script>