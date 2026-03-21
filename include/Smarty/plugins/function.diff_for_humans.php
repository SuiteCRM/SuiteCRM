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

    $seconds = \Carbon\Carbon::createFromTimeString($timedate->to_db($params['datetime']))->diffInSeconds();

    $interval = new \DateInterval('PT' . round($seconds) . 'S');
    $reference = new \DateTimeImmutable();
    $endTime = $reference->add($interval);
    $diff = $reference->diff($endTime);

    $parts = array_filter([
        $diff->y ? $diff->y . 'y' : null,
        $diff->m ? $diff->m . 'mo' : null,
        $diff->d ? $diff->d . 'd' : null,
        $diff->h ? $diff->h . 'h' : null,
        $diff->i ? $diff->i . 'm' : null,
        $diff->s ? $diff->s . 's' : null,
    ]);

    if (empty($parts)) {
        $parts[] = '0s';
    }

    return implode(' ', array_slice($parts, 0, 3));
}
