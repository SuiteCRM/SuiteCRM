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


/**
 * DetailView - display single record
 * @api
 */
#[\AllowDynamicProperties]
class DetailView extends ListView
{
    public $list_row_count = null;
    public $return_to_list_only=false;
    public $offset_key_mismatch=false;
    public $no_record_found=false;

    public function __construct()
    {
        parent::__construct();

        global $theme, $app_strings, $currentModule;
        $this->local_theme = $theme;
        $this->local_app_strings =$app_strings;
    }




    /**
     * @param string $html_varName
     * @param SugarBean $seed
     * @param int $offset
     * @return SugarBean
     */
    public function processSugarBean($html_varName, $seed, $offset)
    {
        global $row_count, $sugar_config;

        global $next_offset;
        global $previous_offset;
        global $list_view_row_count;
        global $current_offset;
        if (!empty($sugar_config['disable_vcr'])) {
            $seed->retrieve($_REQUEST['record']);
            return $seed;
        }
        $isfirstview = 0;

        $nav_history_set=false;
        $nav_history_array=array();
        $nav_offset='';
        $nav_ids_visited=array();
        $nav_stamp='';

        //from list				 					offset is there but $bNavHistorySet is false.
        //from next,previous,start and end buttons	offset and $bNavHistorySet is true.
        //from tracker 								offset is not there but $bNavHistorySet may or may not exist.
        if (isset($_REQUEST['offset']) && !empty($_REQUEST['offset'])) {
            //get offset values.
            $offset = $_REQUEST['offset'];
            if ($offset < 0) {
                $offset = 0;
            }
            //if the stamp has changed, ignore the offset and navigate to the record.
            //use case, search, navigate to detail, copy URL, search again, paste URL.
            if (!$this->isRequestFromListView($html_varName)) {
                $result = $seed->retrieve($_REQUEST['record']);
                return $result;
            }

            if ($nav_history_set) {
                if (isset($nav_ids_visited[$offset])) {
                    unset($nav_ids_visited[$offset]);
                }
            }
        } else {
            if ($nav_history_set) {
                //try to locate the ID in the nav_history array.

                $key = array_search($_REQUEST['record'], $nav_ids_visited, true);
                if ($key === false) {
                    //do not show the VCR buttons.

                    $result = $seed->retrieve($_REQUEST['record']);
                    return $result;
                }
                $offset=$key;
                $_REQUEST['offset'] = $offset;
                $_GET['offset'] = $offset;
                $_POST['offset'] = $offset;

                $_REQUEST['stamp'] = $nav_stamp;
                $_GET['stamp'] = $nav_stamp;
                $_POST['stamp'] = $nav_stamp;
                if (isset($nav_ids_visited[$offset])) {
                    unset($nav_ids_visited[$offset]);
                }
            } else {
                if (!empty($seed->id)) {
                    return $seed;
                }

                $result = $seed->retrieve($_REQUEST['record']);
                return $result;
            }
        }

        //Check if this is the first time we have viewed this record
        $var = $this->getLocalSessionVariable($html_varName, "IS_FIRST_VIEW");
        if (!isset($var) || !$var) {
            $isFirstView = true;
        } else {
            $isFirstView = false;
        }
        //indicate that this is not the first time anymore
        $this->setLocalSessionVariable($html_varName, "IS_FIRST_VIEW", false);

        // All 3 databases require this because the limit query does a > db_offset comparison.
        $db_offset=$offset-1;

        $this->populateQueryWhere($isFirstView, $html_varName);

        $accessWhere = $seed->buildAccessWhere('view');
        if (!empty($accessWhere)) {
            $this->query_where .= empty($this->query_where) ? $accessWhere : ' AND ' . $accessWhere;
        }

        $order = $this->getLocalSessionVariable($seed->module_dir.'2_'.$html_varName, "ORDER_BY");
        $orderBy = '';
        if (!empty($order['orderBy'])) {
            $orderBy = $order['orderBy'];
        }
        if (!empty($orderBy) && !empty($order['direction'])) {
            $orderBy .= ' ' . $order['direction'];
        }

        $this->query_orderby =  $seed->process_order_by($orderBy, null);
        $current_offset = $_REQUEST['offset'] -1;
        $response = $seed->process_detail_query(SugarVCR::retrieve($seed->module_dir), 0, -1, -1, '', $current_offset);
        //$response = $seed->get_detail(, $this->query_where, $db_offset);
        $object = $response['bean'];
        $row_count = $response['row_count'];
        $next_offset = $response['next_offset'];
        $previous_offset = $response['previous_offset'];
        $list_view_row_count = $row_count;
        $this->setListViewRowCount($row_count);

        //if the retrieved id is not same as the request ID then hide the VCR buttons.
        if (empty($object->id)) {
            $this->no_record_found=true;
        }
        if (empty($_REQUEST['InDetailNav']) && strcmp($_REQUEST['record'], $object->id)!=0) {
            $this->offset_key_mismatch=true;
        }
        if ($this->no_record_found || $this->offset_key_mismatch) {
            if ($nav_history_set) {
                $this->return_to_list_only=true;
            }
            $result = $seed->retrieve($_REQUEST['record']);
            return $result;
        }

        //update the request with correct value for the record attribute.
        //need only when using the VCR buttons. This is a workaround need to fix the values
        //set in the VCR links.
        $_REQUEST['record'] = $object->id;
        $_GET['record'] = $object->id;
        $_POST['record'] = $object->id;

        //set nav_history.
        if (empty($nav_stamp)) {
            $nav_stamp=$_GET['stamp'];
        }
        if (empty($nav_offset)) {
            $nav_offset=$offset;
        }

        return $object;
    }

    public function populateQueryWhere($isfirstview, $html_varName)
    {
        if ($isfirstview) {
            $this->query_where = $this->getVariableFromSession($_REQUEST['module'], 'QUERY_WHERE');

            //this is a fail safe, in case the old ListView is still in use
            if (empty($this->query_where)) {
                $this->query_where = $this->getLocalSessionVariable($html_varName, "QUERY_WHERE");
            }
            //SETTING QUERY FOR LATER USE
            $this->setSessionVariable("QUERY_DETAIL", "where", $this->query_where);
        } else {
            $this->query_where = $this->getSessionVariable("QUERY_DETAIL", "where");
        }
    }

    public function processListNavigation($xtpl, $html_varName, $current_offset, $display_audit_link = false, $next_offset = null, $previous_offset = null, $row_count = null, $sugarbean = null, $subpanel_def = null, $col_count = 20)
    {
        global $export_module, $sugar_config, $current_user;
        //intialize audit_link
        $audit_link = '';

        $row_count = $this->getListViewRowCount();

        if ($display_audit_link && (!isset($sugar_config['disc_client']) || $sugar_config['disc_client'] == false)) {
            //Audit link
            $popup_request_data = array(
                'call_back_function' => 'set_return',
                'form_name' => 'EditView',
                'field_to_name_array' => array(),
            );
            $json = getJSONobj();
            $encoded_popup_request_data = $json->encode($popup_request_data);
            $audit_link = "<a href='javascript:void(0)' onclick='open_popup(\"Audit\", \"600\", \"400\", \"&record=".$_REQUEST['record']."&module_name=".$_REQUEST['module']."\", true, false, $encoded_popup_request_data);'>".$this->local_app_strings['LNK_VIEW_CHANGE_LOG']."</a>";
        }

        $html_text = "";
        $pre_html_text = "";
        $pre_html_text .= "<tr class='pagination'>\n";
        $pre_html_text .= "<td COLSPAN=\"20\">\n";
        $pre_html_text .= "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\"><tr><td style=\"text-align: left\" >&nbsp;".$audit_link."</td>\n";



        if ($this->return_to_list_only == true) {
            if ($current_offset != 0 && $this->isRequestFromListView($html_varName)) {
                if ($current_offset < 0) {
                    $current_offset = 1;
                } else {
                    if ($current_offset > $row_count) {
                        $current_offset = $row_count;
                    }
                }

                $this->set_base_URL($html_varName);
                $list_URL = $this->base_URL.'&action=index&module='.$_REQUEST['module'];
                $current_page = floor($current_offset / $this->records_per_page) * $this->records_per_page;

                $list_URL .= '&'.$this->getSessionVariableName($html_varName, "offset").'='.$current_page;
                //$list_link = "<a href=\"$list_URL\" >".$this->local_app_strings['LNK_LIST_RETURN']."&nbsp;</a>";
                $list_link = "<button type='button' class='button' title='{$GLOBALS['app_strings']['LNK_LIST_RETURN']}' onClick='location.href=\"$list_URL\";'>".$this->local_app_strings['LNK_LIST_RETURN']."</button>";

                $html_text .= "<td nowrap align='right' scope='row'>".$list_link;

                if ($row_count != 0) {
                    $resume_URL  = $this->base_URL.$current_offset."&InDetailNav=1";
                    //$resume_link = "<a href=\"$resume_URL\" >".$this->local_app_strings['LNK_RESUME']."&nbsp;</a>";
                    $resume_link = "<button type='button' class='button' title='$this->local_app_strings['LNK_RESUME']' onClick='location.href=\"$resume_URL\";'>".$this->local_app_strings['LNK_RESUME']."</button>";

                    $html_text .= "&nbsp;&nbsp;".$resume_link;
                }
                $html_text .= "</td>";
            }
        } else {
            if ($current_offset != 0 && $this->isRequestFromListView($html_varName)) {
                if ($current_offset < 0) {
                    $current_offset = 1;
                } else {
                    if ($current_offset > $row_count) {
                        $current_offset = $row_count;
                    }
                }

                $next_offset = $current_offset + 1;
                $previous_offset = $current_offset - 1;

                $this->set_base_URL($html_varName);

                $start_URL = $this->base_URL."1"."&InDetailNav=1";
                $current_URL = $this->base_URL.$current_offset."&InDetailNav=1";
                $previous_URL  = $this->base_URL.$previous_offset."&InDetailNav=1";
                $next_URL  = $this->base_URL.$next_offset."&InDetailNav=1";
                $end_URL  = $this->base_URL.$row_count."&InDetailNav=1";

                $current_page = floor($current_offset / $this->records_per_page) * $this->records_per_page;

                if (1 == $current_offset) {
                    //$start_link = SugarThemeRegistry::current()->getImage("start_off","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_START'])."&nbsp;".$this->local_app_strings['LNK_LIST_START'];
                    //$previous_link = SugarThemeRegistry::current()->getImage("previous_off","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_PREVIOUS'])."&nbsp;".$this->local_app_strings['LNK_LIST_PREVIOUS']."";
                    $start_link = "<button type='button' title='{$this->local_app_strings['LNK_LIST_START']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("start_off", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_START'])."</button>";
                    $previous_link = "<button type='button' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("previous_off", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_PREVIOUS'])."</button>";
                } else {
                    //$start_link = "<a href=\"$start_URL\">".SugarThemeRegistry::current()->getImage("start","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_START'])."</a>&nbsp;<a href=\"$start_URL\">".$this->local_app_strings['LNK_LIST_START']."</a>";
                    $start_link = "<button type='button' class='button' title='{$this->local_app_strings['LNK_LIST_START']}' onClick='location.href=\"$start_URL\";'>".SugarThemeRegistry::current()->getImage("start", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_START'])."</button>";

                    if (0 != $current_offset) {
                        //$previous_link = "<a href=\"$previous_URL\">".SugarThemeRegistry::current()->getImage("previous","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_PREVIOUS'])."</a>&nbsp;<a href=\"$previous_URL\" >".$this->local_app_strings['LNK_LIST_PREVIOUS']."</a>";
                        $previous_link = "<button type='button' class='button' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' onClick='location.href=\"$previous_URL\";'>".SugarThemeRegistry::current()->getImage("previous", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_PREVIOUS'])."</button>";
                    } else {
                        //$previous_link = SugarThemeRegistry::current()->getImage("previous_off","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_PREVIOUS'])."&nbsp;".$this->local_app_strings['LNK_LIST_PREVIOUS'];
                        $previous_link = "<button type='button' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("previous_off", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_PREVIOUS'])."</button>";
                    }
                }


                if ($row_count <= $current_offset) {
                    //$end_link = $this->local_app_strings['LNK_LIST_END']."&nbsp;".SugarThemeRegistry::current()->getImage("end_off","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_END']);
                    //$next_link = $this->local_app_strings['LNK_LIST_NEXT']."&nbsp;".SugarThemeRegistry::current()->getImage("next_off","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_NEXT']);
                    $end_link = "<button type='button' title='{$this->local_app_strings['LNK_LIST_END']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("end_off", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_END'])."</button>";
                    $next_link = "<button type='button' title='{$this->local_app_strings['LNK_LIST_NEXT']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("next_off", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_NEXT'])."</button>";
                } else {
                    //$end_link = "<a href=\"$end_URL\">".$this->local_app_strings['LNK_LIST_END']."</a>&nbsp;<a href=\"$end_URL\">".SugarThemeRegistry::current()->getImage("end","border='0' align='absmiddle'",null,null,'.gif',$this->local_app_strings['LNK_LIST_END'])."</a>";
                    //$next_link = "<a href=\"$next_URL\">".$this->local_app_strings['LNK_LIST_NEXT']."</a>&nbsp;<a href=\"$next_URL\">".SugarThemeRegistry::current()->getImage("next","border='0' align='absmiddle'",,null,null,'.gif',$this->local_app_strings['LNK_LIST_NEXT'])."</a>";
                    $end_link = "<button type='button' class='button' title='{$this->local_app_strings['LNK_LIST_END']}' onClick='location.href=\"$end_URL\";'>".SugarThemeRegistry::current()->getImage("end", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_END'])."</button>";
                    $next_link = "<button type='button' class='button' title='{$this->local_app_strings['LNK_LIST_NEXT']}' onClick='location.href=\"$next_URL\";'>".SugarThemeRegistry::current()->getImage("next", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_NEXT'])."</button>";
                }

                $html_text .= "<td nowrap align='right' >".$start_link."&nbsp;&nbsp;".$previous_link."&nbsp;&nbsp;(".$current_offset." ".$this->local_app_strings['LBL_LIST_OF']." ".$row_count.")&nbsp;&nbsp;".$next_link."&nbsp;&nbsp;".$end_link."</td>";
            }
        }
        $post_html_text = "</tr></table>\n";
        $post_html_text .= "</td>\n";
        $post_html_text .= "</tr>\n";
        $showVCRControl = true;
        if (isset($sugar_config['disable_vcr'])) {
            $showVCRControl = !$sugar_config['disable_vcr'];
        }
        if ($showVCRControl && $html_text != "") {
            $xtpl->assign("PAGINATION", $pre_html_text.$html_text.$post_html_text);
        }
    }


    public function set_base_URL($html_varName)
    {
        if (!isset($this->base_URL)) {
            $this->base_URL = $_SERVER['PHP_SELF'];
            if (empty($this->base_URL)) {
                $this->base_URL = 'index.php';
            }

            /*fixes an issue with
            deletes when doing a search*/
            foreach ($_GET as $name=>$value) {
                if (!empty($value)) {
                    if ($name != $this->getSessionVariableName($html_varName, "ORDER_BY") && $name != "offset" && substr_count($name, "ORDER_BY")==0 && $name!="isfirstview") {
                        if (is_array($value)) {
                            foreach ($value as $valuename=>$valuevalue) {
                                $this->base_URL	.= "&{$name}[]=".$valuevalue;
                            }
                        } else {
                            if (substr_count((string) $this->base_URL, '?') > 0) {
                                $this->base_URL	.= "&$name=$value";
                            } else {
                                $this->base_URL	.= "?$name=$value";
                            }
                        }
                    }
                }
            }

            if ($_SERVER['REQUEST_METHOD'] == 'POST') {
                $this->base_URL .= '?';
                if (isset($_REQUEST['action'])) {
                    $this->base_URL .= '&action='.$_REQUEST['action'];
                }
                if (isset($_REQUEST['record'])) {
                    $this->base_URL .= '&record='.$_REQUEST['record'];
                }
                if (isset($_REQUEST['module'])) {
                    $this->base_URL .= '&module='.$_REQUEST['module'];
                }
            }
            $this->base_URL .= "&offset=";
        }
    }
    public function setListViewRowCount($count)
    {
        $this->list_row_count = $count;
    }

    public function getListViewRowCount()
    {
        return $this->list_row_count;
    }

    /* This method will return in all of these cases: When selecting any of the VCR buttons (start,prev,next or last)
     * and navigating from list to detail for the first time.
     * if false in this case: the user changes the list query (which generates a new stamp) and pastes a URL
     * from a previously navigated item.
     */
    public function isRequestFromListView($html_varName)
    {
        $varList = $this->getLocalSessionVariable($html_varName, "FROM_LIST_VIEW");
        if (isset($_GET['stamp']) && isset($varList) && $varList == $_GET['stamp']) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Return a variable from the session. uses the new ListView session data. Hence the '2'
     *
     * @param unknown_type $name - the name of the variable to set in the session
     * @param unknown_type $value - the value of the variable to set
     */
    public function getVariableFromSession($name, $value)
    {
        if (isset($_SESSION[$name."2_".$value])) {
            return $_SESSION[$name."2_".$value];
        } else {
            return "";
        }
    }
}
