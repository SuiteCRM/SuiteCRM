function updateCRONType(id){
    var weekly = $('#'+id+'_weekly_options');
    var monthly = $('#'+id+'_monthly_options');
    switch($('#'+id+'_type').val()){
        case 'monthly':
            weekly.hide();
            monthly.show();
            break;
        case 'weekly':
            monthly.hide();
            weekly.show();
            break;
        case 'daily':
        default:
            weekly.hide();
            monthly.hide();
    }
}
function updateCRONDisplay(id){
    var adv = $('#'+id+'_advanced');
    var basic = $('#'+id+'_basic');
    if($('#'+id+'_raw').is(':checked')){
        basic.hide();
        adv.show();
    }else{
        adv.hide();
        basic.show();
    }
}
function updateCRONFields(id){
    var schedule = $('#'+id).val();
    var bits = schedule.split(' ');
    var mins = bits[0];
    var hours = bits[1];
    var day = bits[2];
    var month = bits[3];
    var weekday = bits[4];
    $('#'+id+'_raw_minutes').val(mins);
    $('#'+id+'_raw_hours').val(hours);
    $('#'+id+'_raw_day').val(day);
    $('#'+id+'_raw_month').val(month);
    $('#'+id+'_raw_weekday').val(weekday);
    var canShowBasic = true;
    if(mins === '*') {
        canShowBasic = false;
    }else{
        $('#'+id+'_time_minutes').val(mins);
    }
    if(hours === '*') {
        canShowBasic = false;
    }else{
        $('#'+id+'_time_hours').val(hours);
    }
    if(weekday !== '*' && day !== '*'){
        canShowBasic = false;
    } else if (mins !== '*' && hours !== '*' && day === '*' && month === '*' && weekday === '*') {
        $('#'+id+'_type').val('daily');
        $('#'+id+'_type').change();
    }else if(weekday !== '*'){
        $('#'+id+'_weekdays').val(weekday.split(','));
        $('#'+id+'_type').val('weekly');
        $('#'+id+'_type').change();
    }else if(day !== '*'){
        $('#'+id+'_type').val('monthly');
        $('#'+id+'_type').change();
        $('#'+id+'_days').val(day.split(','));
    }
    if(month !== '*'){
        canShowBasic = false;
    }
    if(!mins && !hours && !day && !month && !weekday){
        canShowBasic = true;//New field
    }
    if(!canShowBasic){
        $('#'+id+'_raw').attr('checked',true);
        $('#'+id+'_raw').change();
    }
}

function updateCRONValue(id){
    //If advanced mode, do nothing
    if($('#'+id+'_raw').is(':checked')){
        var minutes = $('#'+id+'_raw_minutes').val();
        var hours = $('#'+id+'_raw_hours').val();
        var days = $('#'+id+'_raw_day').val();
        var months = $('#'+id+'_raw_month').val();
        var weekdays = $('#'+id+'_raw_weekday').val();
        $('#'+id).val(minutes + ' ' + hours + ' ' + days + ' '+months+' '+weekdays);
        return;
    }
    var days = '',weekdays = '',hours = '', minutes = '';
    switch($('#'+id+'_type').val()) {
        case 'monthly':
            days = $('#'+id+'_days').val();
            break;
        case 'weekly':
            weekdays = $('#'+id+'_weekdays').val();
            break;
        case 'daily':
        default:
            break;
    }
    hours = $('#'+id+'_time_hours').val();
    minutes = $('#'+id+'_time_minutes').val();
    if(minutes === ''){
        minutes = '*';
    }
    if(hours === ''){
        hours = '*';
    }
    if(!days){
        days = '*';
    }
    if(!weekdays){
        weekdays = '*';
    }
    $('#'+id+'_raw_minutes').val(minutes);
    $('#'+id+'_raw_hours').val(hours);
    $('#'+id+'_raw_day').val(days);
    $('#'+id+'_raw_month').val('*');
    $('#'+id+'_raw_weekday').val(weekdays);
    $('#'+id).val(minutes + ' ' + hours + ' ' + days + ' * '+weekdays);

}