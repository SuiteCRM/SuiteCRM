<?php

require_once('modules/Opportunities/OpportunitiesListViewSmarty.php');

#[\AllowDynamicProperties]
class OpportunitiesViewList extends ViewList
{
    public function __construct()
    {
        parent::__construct();
    }




    public function preDisplay()
    {
        $this->lv = new OpportunitiesListViewSmarty();
    }
}
