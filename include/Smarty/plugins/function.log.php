<?php

use SuiteCRM\ErrorMessage;

function smarty_function_log($params, &$smarty) {
    $from = $smarty->source->name ?? '';
    $message = "log call at: $from - " . $params['msg'];
    $level = isset($params['level']) ? $params['level'] : ErrorMessage::DEFAULT_LOG_LEVEL;
    ErrorMessage::log($message, $level);
}

