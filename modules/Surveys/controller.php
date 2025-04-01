<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class SurveysController extends SugarController
{
    public function action_Reports()
    {
        $this->view = 'Reports';
    }
}
