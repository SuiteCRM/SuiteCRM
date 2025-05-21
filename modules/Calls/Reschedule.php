<?php
require_once('modules/Calls_Reschedule/Calls_Reschedule.php');
require_once('modules/Calls/Call.php');


$call = new call();
$timedate =new TimeDate();


$id = $_POST['call_id'];
$date = $_POST['date'];
$reason = $_POST['reason'];
$hour = $_POST['date_start_hours'];
$minutes = $_POST['date_start_minutes'];
$ampm = $_POST['date_start_meridiem'] ?? '';

$time_format = $timedate->get_user_time_format(); //get the logged in users time settings

//Combine date and time dependant on users settings
$time_separator = ":";
if (preg_match('/\d+([^\d])\d+([^\d]*)/s', (string) $time_format, $match)) {
    $time_separator = $match[1];
}

if (!empty($hour) && !empty($minutes)) {
    $time_start = $hour. $time_separator .$minutes;
}

if (isset($ampm) && !empty($ampm)) {
    $time_start = $timedate->merge_time_meridiem($time_start, $timedate->get_time_format(), $ampm);
}

if (isset($time_start) && strlen((string) $date) == 10) {
    $date_start = $date.' ' .$time_start;
}


$call->retrieve($id);
$call->date_start = $date_start;//set new the start date
$call->save();//save the new start date
//get the duration of the call
$hours = $call->duration_hours;
$mins = $call->duration_minutes;

//get the new start date directly from the database to avoid sugar changing the format to users setting
$query = 'SELECT date_start FROM calls WHERE id="'.$id.'"';
$result = $call->db->getOne($query);
//add on the duration of call and save the end date/time
$Date = strtotime($result);
$newDate = strtotime('+'.$hours.' hours', $Date);
$newDate = strtotime('+'.$mins.' minutes', $newDate);
$newDate = date("Y-m-d H:i:s", $newDate);
$call->date_end = $newDate;
//save call and call attempt history
$reschedule = BeanFactory::newBean('Calls_Reschedule');

$reschedule->reason = $reason;
$reschedule->call_id = $id;

$call->save();
$reschedule->save();//save call attempt history line
