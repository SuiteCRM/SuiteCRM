<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


require_once("include/templates/Template.php");

class TemplateDragDropChooser extends Template
{
    public $args;
    public function __construct()
    {
    }




    /*
     * This function creates the html and uses the args parameter to call the class file
     * ideally, you would want to call the displayScriptTags() function,
     * followed by the displayDefinitionScript();
     * and lastly call the display function
     */
    public function display()
    {

  /*   valid entries for expected arguments array are as follow:
   *   args['left_header'] = value of left table header
   *   args['mid_header'] = value of middle table header
   *   args['right_header'] = value of right table header
   *   args['left_data'] = array to use in left data.
   *   args['mid_data'] = array to use in middle data.
   *   args['right_data'] =array to use in right data.
   *   args['title'] =  title (if any) to be used.
   *   args['classname'] =  name to be used as class. This helps when defining multiple templates on one screen.
   *   args['left_div_name'] = Name to be used for left div (should be unique)
   *   args['mid_div_name'] = Name to be used for middle div (should be unique)
   *   args['right_div_name'] = Name to be used for right div (should be unique)
   *   args['gridcount'] = Number of grids to show.  Acceptable Values are 'one','two' and 'three'
   *                       The string is converted to numeric values, so you could also set these directly
   *                       The values are Zero Based, so to display one column, set to '0'
   *                       To display two columns set to '1', to display three columns set to '2'.
   * $this->args['return_array']= if this is set to true, then 'display()' function returns html and javascript
   *                              in array format.  This will allow granular control of html so that you can
   *                              separate the tables and customize the grid
   */


        //convert values for gridcount in case they are in string form
        if ($this->args['gridcount'] == 'one') {
            $this->args['gridcount'] = 0;
        } elseif ($this->args['gridcount'] == 'two') {
            $this->args['gridcount'] = 1;
        } elseif (
            $this->args['gridcount'] == 'three') {
            $this->args['gridcount'] = 2;
        }

        if (!isset($this->args['classname']) || empty($this->args['classname'])) {
            $this->args['classname'] = 'DragDropGrid';
        }
        $json = getJSONobj();
        //use Json to encode the arrays of data, for passing to javascript.
        //we will always display at least one column, so set left column
        $this->args['left_data'][] = array(' ', ' ');
        $data0_enc = $json->encode($this->args['left_data']);
        $left_div_name = $this->args['left_div_name'];

        //if count is set to 1, then we are displaying two columns, set the 2 column variables
        if ($this->args['gridcount']==1) {
            $this->args['right_data'][] = array(' ', ' ');
            $data1_enc = $json->encode($this->args['right_data']);
            $right_div_name = $this->args['right_div_name'];
        }

        //if count is set to 2, then we are displaying three columns, set the 3 column variables
        if ($this->args['gridcount']==2) {
            $this->args['mid_data'][] = array(' ', ' ');
            $data1_enc = $json->encode($this->args['mid_data']);
            $mid_div_name = $this->args['mid_div_name'];
            $this->args['right_data'][] = array(' ', ' ');
            $data2_enc = $json->encode($this->args['right_data']);
            $right_div_name = $this->args['right_div_name'];
        }
        $html_str_arr = array();
        //create the table, with the divs that will get populated.  Populate both the string and array version
        $html_str  =   "<div id='" . $this->args['classname'] . "'><table  align='left'  width='180px' border='1' cellspacing='0' cellpadding='0'>";
        $html_str_arr['begin'] = $html_str;
        $html_str .=   "<tr><td width='180px' class='tabDetailViewDF'><div id='$left_div_name' class='ygrid-mso' style='width:180px;height:270px;overflow:hidden;'> </div></td>";
        $html_str_arr['left'] = "<tr><td width='180px' class='tabDetailViewDF'><div id='$left_div_name' class='ygrid-mso' style='width:180px;height:270px;overflow:hidden;'> </div></td>";
        //set the middle column only if we are displaying 3 columns
        if ($this->args['gridcount']==2) {
            $html_str .=   "<td width='180px' class='tabDetailViewDF'><div id='$mid_div_name' class='ygrid-mso' style='width:180px;height:270px;overflow:hidden;'> </div></td>";
            $html_str_arr['middle'] = "<td width='180px' class='tabDetailViewDF'><div id='$mid_div_name' class='ygrid-mso' style='width:180px;height:270px;overflow:hidden;'> </div></td>";
        }
        //set the right column if we are not in 1 column only mode
        if ($this->args['gridcount']>0) {
            $html_str .=   "<td width='180px' class='tabDetailViewDF'><div id='$right_div_name' class='ygrid-mso' style='width:180px;height:270px;overflow:hidden;'> </div></td>";
            $html_str_arr['right'] = "<td width='180px' class='tabDetailViewDF'><div id='$right_div_name' class='ygrid-mso' style='width:180px;height:270px;overflow:hidden;'> </div></td>";
        }
        $html_str .=   "</tr></table></div>";
        $html_str_arr['end'] = "</tr></table></div>";

        //create the needed javascript to set the values and invoke listener
        $j_str = "<script> ";
        $j_str .= $this->args['classname'] . ".rows0 = {$data0_enc};\n";
        $j_str .= $this->args['classname'] . ".hdr0 = '{$this->args['left_header']}';\n";
        if ($this->args['gridcount']==1) {
            $j_str .= $this->args['classname'] . ".rows1 = {$data1_enc};\n";
            $j_str .= $this->args['classname'] . ".hdr1 = '{$this->args['right_header']}';\n";
        }
        if ($this->args['gridcount']==2) {
            $j_str .= $this->args['classname'] . ".rows1 = {$data1_enc}; \n";
            $j_str .= $this->args['classname'] . ".rows2 = {$data2_enc}; \n";
            $j_str .= $this->args['classname'] . ".hdr1 = '{$this->args['mid_header']}'; \n";
            $j_str .= $this->args['classname'] . ".hdr2 = '{$this->args['right_header']}'; \n";
        }
        $divs_str = "'".$left_div_name ."'";
        if ($this->args['gridcount']==2) {
            $divs_str .= ", '".$mid_div_name."'";
        }
        if ($this->args['gridcount']>0) {
            $divs_str .= ", '".$right_div_name."'";
        }

        $j_str .= $this->args['classname'] . ".divs = [$divs_str]; ";

        $j_str .= "YAHOO.util.Event.on(window, 'load', " . $this->args['classname'] . ".init); ";
        $j_str .= "</script> ";
        //return display string
        $str = $j_str . '  ' . $html_str;
        $html_str_arr['script'] = $j_str;

        if (isset($this->args['return_array']) && $this->args['return_array']) {
            return $html_str_arr;
        } else {
            return $str;
        }
    }

    /*
     * This script is the javascript class definition for the template drag drop object.  This
     * makes use of the args['classname'] parameter to name the class and to prefix variables with.  This is done
     * dynamically so that multiple template dragdrop objects can be defined on the same page if needed
     * without having the variables mix up as you drag rows around.
     */
    public function displayDefinitionScript()
    {
        //create some defaults in case arguments are missing

        //convert values for gridcount in case they are in string form
        if (!isset($this->args['gridcount']) || empty($this->args['gridcount']) || $this->args['gridcount'] == 'one') {
            $this->args['gridcount'] = 0;
        } elseif ($this->args['gridcount'] == 'two') {
            $this->args['gridcount'] = 1;
        } elseif (
            $this->args['gridcount'] == 'three') {
            $this->args['gridcount'] = 2;
        }



        //default class name
        if (!isset($this->args['classname']) || empty($this->args['classname'])) {
            $this->args['classname'] = 'DragDropGrid';
        }
        //default columns to one if the value is set to anything other than the expected 0,1 or 2
        if (($this->args['gridcount'] != 0) && ($this->args['gridcount'] != 1) && ($this->args['gridcount'] != 2)) {
            $this->args['gridcount'] = 0;
        }

        //default div names
        if (!isset($this->args['left_div_name']) || empty($this->args['left_div_name'])) {
            $this->args['left_div_name'] = 'left';
        }
        if (!isset($this->args['mid_div_name']) || empty($this->args['mid_div_name'])) {
            $this->args['mid_div_name'] = 'mid';
        }
        if (!isset($this->args['right_div_name']) || empty($this->args['right_div_name'])) {
            $this->args['right_div_name'] = 'right';
        }



        //create javascript that defines the javascript class for this instance
        //start by defining the variables that the grids will be referenced by
        $j_str =   "
        <script>
           var container = new YAHOO.util.Element('grid_Div').addClass('yui-skin-sam');
           var " . $this->args['classname'] . "_grid2, " . $this->args['classname'] . "_grid1, " . $this->args['classname'] . "_grid0;
           var " . $this->args['classname'] . "_sugar_grid2, " . $this->args['classname'] . "_sugar_grid1, " . $this->args['classname'] . "_sugar_grid0;

           /*
            * This invokes the grid objects
            */

            " . $this->args['classname'] . " =  {" ;
        $j_str .=   "
            rows0 : [],
            rows1 : [],
            rows2 : [],
            hdr0 : [],
            hdr1 : [],
            hdr2 : [],
            divs : [],
            formatter : function(elCell, oRecord, oColumn, oData) {
                if (oRecord.getData()[0] == ' ' ) {
                    return;
                }
                elCell.innerHTML = '<span id=\"' + oRecord.getData()[1] + '_row\">' + oRecord.getData()[0] + '</span>';
            },
            // bug50219 - added sorting for field labels
            customSort: function( a , b , desc , field )  {
                var comp = YAHOO.util.Sort.compare;
                var aString = a._oData[0].replace(/<[^>]*>/g, '');
                var bString = b._oData[0].replace(/<[^>]*>/g, '');
                return comp(aString, bString, desc);
            },
            init : function(){
            ";
        $count = 0;
        while ($count<$this->args['gridcount']+1) {
            // bug50219 Set up default strings for sortable grids
            $sortParams = "sortable: false";
            // set strings to sortable if it is SUGAR_GRID_grid0
            if ($count == 0) {
                $sortParams = "sortable: true, sortOptions:{sortFunction: {$this->args['classname']}.customSort} ";
            }

            $j_str .= "
                {$this->args['classname']}_grid{$count}source = new YAHOO.util.LocalDataSource({$this->args['classname']}.rows$count);
                {$this->args['classname']}_grid{$count}cols = [
                    {key:'label', {$sortParams} , label:{$this->args['classname']}.hdr$count, formatter: {$this->args['classname']}.formatter}
                ];
                {$this->args['classname']}_grid$count = new YAHOO.SUGAR.DragDropTable(
                    {$this->args['classname']}.divs[$count], {$this->args['classname']}_grid{$count}cols, {$this->args['classname']}_grid{$count}source,
                    {
                        trackMouseOver: false,
                        height: '250px'
                    }
                );";

            $j_str .= "
                {$this->args['classname']}_sugar_grid$count = {$this->args['classname']}_grid$count;";

            $count = $count+1;
        }
        $j_str.="
            }
        };

        </script>
        ";
        //all done, return final script
        return $j_str;
    }


    /*
     * this function returns the src style sheet and script tags that need to be included
     * for the template chooser to work
     */

    public function displayScriptTags()
    {
        global $sugar_version, $sugar_config;
        $j_str =   "
        <link rel='stylesheet' type='text/css' href='include/javascript/yui/build/datatable/assets/skins/sam/datatable.css'/>
        <script type='text/javascript' src='cache/include/javascript/sugar_grp_yui_widgets.js'></script>";

        return $j_str;
    }
}
