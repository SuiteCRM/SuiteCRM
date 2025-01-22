<?php

/**
 * This smarty function will generate a carbon diff for humans.
 *
 * A diff for humans is something like: "31 seconds ago" or "54 days ago".
 *
 * The only parameter to pass is 'datetime', to be given in UTC.
 *
 * @param $params array
 * @return string
 * @see Carbon::diffForHumans()
 */
function smarty_function_diff_for_humans(array $params)
{
    global $timedate;

    return \Carbon\Carbon::createFromTimeString($timedate->to_db($params['datetime']))->diffForHumans();
}
