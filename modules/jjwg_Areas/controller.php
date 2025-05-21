<?php

// modules/jjwg_Areas/controller.php

include_once('include/utils.php');

#[\AllowDynamicProperties]
class jjwg_AreasController extends SugarController
{
    public function action_area_edit_map()
    {
        $this->view = 'area_edit_map';
        $jjwg_Areas = get_module_info('jjwg_Areas');

        // Get the map object
        if (is_guid($_REQUEST['id'])) {
            $jjwg_Areas->retrieve($_REQUEST['id']);
        }
        $GLOBALS['polygon'] = $jjwg_Areas->define_polygon();
        $GLOBALS['loc'] = $jjwg_Areas->define_area_loc();
    }

    public function action_area_detail_map()
    {
        $this->view = 'area_detail_map';
        $jjwg_Areas = get_module_info('jjwg_Areas');

        // Get the map object
        if (is_guid($_REQUEST['id'])) {
            $jjwg_Areas->retrieve($_REQUEST['id']);
        }
        $GLOBALS['polygon'] = $jjwg_Areas->define_polygon();
        $GLOBALS['loc'] = $jjwg_Areas->define_area_loc();
    }
}
