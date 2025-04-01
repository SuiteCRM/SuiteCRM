<?php

#[\AllowDynamicProperties]
class reschedule_count
{

    //Counts the number of call attempts made, for displaying in calls list view
    public function count($focus, $event, $args)
    {
        require_once('modules/Calls/reschedule_history.php');

        reschedule_count($focus, '', '', 'ListView');
    }
}
