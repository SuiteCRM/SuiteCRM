<?php

$job_strings[] = 'witInsuranceEmailLeadParser';

function witInsuranceEmailLeadParser()
{
    require_once 'custom/modules/Leads/WITInsuranceEmailLeadParser.php';
    $parser = new WITInsuranceEmailLeadParser();
    return $parser->run();
}
