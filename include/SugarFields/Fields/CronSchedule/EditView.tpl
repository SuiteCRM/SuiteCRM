<script type="text/javascript" src='{sugar_getjspath file="include/SugarFields/Fields/CronSchedule/SugarFieldCronSchedule.js"}'></script>
{if !isset({{sugarvar key='value' string=true}})}
    {assign var="value" value={{sugarvar key='default_value' string=true}} }
{else}
    {assign var="value" value={{sugarvar key='value' string=true}} }
{/if}
<input type='hidden'
       name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}'
       id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}'
       value='{$value}'>
<label>
    {{$APP.LBL_CRON_RAW}}
<input type="checkbox"
       name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw'
       id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw'
        >
</label>
<span style='display:none;' id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_basic'>
<select
        name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_type'
        id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_type'>
    {{$types}}
</select>

<span id="{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_monthly_options" style="display: none">
    {{$APP.LBL_CRON_ON_THE_MONTHDAY}}
    <select
            multiple="multiple"
            name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_days'
            id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_days'>
        {{$days}}
    </select>

</span>
<span id="{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_weekly_options" style="display: none">
    {{$APP.LBL_CRON_ON_THE_WEEKDAY}}
    <select
            multiple="multiple"
            name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_weekdays'
            id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_weekdays'>
        {{$weekdays}}
    </select>

</span>
{{$APP.LBL_CRON_AT}}
<select
        type="text"
        name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_time_hours'
        id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_time_hours'
        value="23:00"
        >{{$hours}}</select>:
    <select
            type="text"
            name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_time_minutes'
            id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_time_minutes'
            value="23:00"
            >
        {{$minutes}}
        </select>
</span>
<span style='display:none;' id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_advanced'>
    <label>{{$APP.LBL_CRON_MIN}}
    <input type="text"
           size="2"
           name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_minutes'
           id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_minutes'
            >
    </label>
    <label>{{$APP.LBL_CRON_HOUR}}
    <input type="text"
           size="2"
           name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_hours'
           id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_hours'
            >
    </label>
    <label>{{$APP.LBL_CRON_DAY}}
    <input type="text"
           size="2"
           name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_day'
           id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_day'
            >
    </label>
    <label>{{$APP.LBL_CRON_MONTH}}
    <input type="text"
           size="2"
           name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_month'
           id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_month'
            >
    </label>
    <label>{{$APP.LBL_CRON_DOW}}
    <input type="text"
           size="2"
           placeholder=""
           name='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_weekday'
           id='{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}_raw_weekday'
            >
    </label>

    </span>

<script>
    {literal}
    $(document).ready(function(){
        {/literal}
        var id = '{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}';
        {literal}

        $('#'+id+'_type').on('change',function(){
           updateCRONType(id);
        });
        $('#'+id+'_raw').on('change',function(){
            updateCRONDisplay(id);
        });
        $('#'+id+'_basic').on('change',function(){
            updateCRONValue(id);
        });
        var rawChange = function(){
            updateCRONValue(id);
        }
        $('#'+id+'_raw_minutes').change(rawChange);
        $('#'+id+'_raw_hours').change(rawChange);
        $('#'+id+'_raw_day').change(rawChange);
        $('#'+id+'_raw_month').change(rawChange);
        $('#'+id+'_raw_weekday').change(rawChange);
        updateCRONDisplay(id);
        updateCRONType(id);
        updateCRONFields(id);
    });
    {/literal}
</script>