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


    require_once('modules/Calendar/Calendar.php');

    #[\AllowDynamicProperties]
    class vCal extends SugarBean
    {

        // Stored fields
        public $id;
        public $date_modified;
        public $user_id;
        public $content;
        public $deleted;
        public $type;
        public $source;
        public $module_dir = "vCals";
        public $table_name = "vcals";

        public $object_name = "vCal";
        public $tracker_visibility = false;

        public $new_schema = true;

        public $field_defs = array();

        // This is used to retrieve related fields from form posts.
        public $additional_column_fields = array();

        public const UTC_FORMAT = 'Ymd\THi00\Z';
        public const EOL = "\r\n";
        public const TAB = "\t";
        public const CHARSPERLINE = 75;

        public function __construct()
        {
            parent::__construct();
            $this->disable_row_level_security = true;
        }

        public function get_summary_text()
        {
            return "";
        }


        public function fill_in_additional_list_fields()
        {
        }

        public function fill_in_additional_detail_fields()
        {
        }

        public function get_list_view_data()
        {
        }

        // combines all freebusy vcals and returns just the FREEBUSY lines as a string
        public function get_freebusy_lines_cache(&$user_bean)
        {
            $ical_array = array();
            // First, get the list of IDs.
            $query = "SELECT id from vcals where user_id='{$user_bean->id}' AND type='vfb' AND deleted=0";
            $vCal = BeanFactory::newBean('vCals');
            $vcal_arr = $this->build_related_list($query, $vCal);

            foreach ($vcal_arr as $focus) {
                if (empty($focus->content)) {
                    return '';
                }

                $ical_arr = self::create_ical_array_from_string($focus->content);
                $ical_array = array_merge($ical_array, $ical_arr);
            }

            return self::create_ical_string_from_array($ical_array);
        }

        // query and create the FREEBUSY lines for SugarCRM Meetings and Calls and
        // return the string
        public function create_sugar_freebusy($user_bean, $start_date_time, $end_date_time)
        {
            $ical_array = array();
            global $DO_USER_TIME_OFFSET, $timedate;

            $DO_USER_TIME_OFFSET = true;
            if (empty($GLOBALS['current_user']) || empty($GLOBALS['current_user']->id)) {
                $GLOBALS['current_user'] = $user_bean;
            }
            // get activities.. queries Meetings and Calls
            $activityList = array(
                "Meetings" => array("showCompleted" => true, "start" => "date_start", "end" => "date_end"), "Calls" => array("showCompleted" => true, "start" => "date_start", "end" => "date_end"), "Tasks" => array("showCompleted" => true, "start" => "date_start", "end" => "date_due")
            );

            $acts_arr = CalendarActivity::get_activities($activityList, $user_bean->id, false, $start_date_time, $end_date_time, 'freebusy');
            // loop thru each activity, get start/end time in UTC, and return FREEBUSY strings
            foreach ($acts_arr as $act) {
                if (empty($act->start_time)) {
                    $act->start_time = $timedate->fromUser($act->sugar_bean->date_start, $user_bean);
                }

                if (empty($act->end_time)) {
                    $act->end_time = $timedate->fromUser($act->sugar_bean->date_finish, $user_bean);
                }

                $ID = $act->sugar_bean->id;
                $startTimeUTC = $act->start_time->format(self::UTC_FORMAT);
                $endTimeUTC = $act->end_time->format(self::UTC_FORMAT);
                $ical_array[] = array("FREEBUSY", $startTimeUTC . "/" . $endTimeUTC);
                $ical_array[] = array("X-FREEBUSY-ID", $ID);
                $ical_array[] = array("X-FREEBUSY-TYPE", get_class($act->sugar_bean));
                //$ical_array[] = array(array("X-FREEBUSYID", $ID), array("FREEBUSY", $startTimeUTC ."/". $endTimeUTC));
            }

            return self::create_ical_string_from_array($ical_array);
            //        return $ical_array;
        }

        // return a freebusy vcal string
        public function get_vcal_freebusy($user_focus, $cached = true)
        {
            global $locale, $timedate;
            $ical_array = array();
            $ical_array[] = array("BEGIN", "VCALENDAR");
            $ical_array[] = array("VERSION", "2.0");
            $ical_array[] = array("PRODID", "-//SuiteCRM//SuiteCRM Calendar//EN");
            $ical_array[] = array("BEGIN", "VFREEBUSY");

            $name = $locale->getLocaleFormattedName($user_focus->first_name, $user_focus->last_name);
            $email = $user_focus->email1;

            // get current date for the user
            $now_date_time = $timedate->getNow(true);

            // get start date ( 1 day ago )
            $start_date_time = $now_date_time->get("yesterday");

            // get date 2 months from start date
            global $sugar_config;
            $timeOffset = 2;
            if (isset($sugar_config['vcal_time']) && $sugar_config['vcal_time'] != 0 && $sugar_config['vcal_time'] < 13) {
                $timeOffset = $sugar_config['vcal_time'];
            }
            $end_date_time = $start_date_time->get("+$timeOffset months");

            // get UTC time format
            $utc_start_time = $start_date_time->asDb();
            $utc_end_time = $end_date_time->asDb();
            $utc_now_time = $now_date_time->asDb();

            $ical_array[] = array("ORGANIZER;CN=$name", "VFREEBUSY");
            $ical_array[] = array("DTSTART", $utc_start_time);
            $ical_array[] = array("DTEND", $utc_end_time);

            $str = self::create_ical_string_from_array($ical_array);

            // now insert the freebusy lines
            // retrieve cached freebusy lines from vcals
            if ($timeOffset != 0) {
                //               $freebusy = $this->create_sugar_freebusy($user_focus,$start_date_time,$end_date_time);
                //               $str .= self::create_ical_string_from_array($freebusy[0]);
                //               $str .= self::create_ical_string_from_array($freebusy[1]);
                // TODO: add back once the previous works!!!!
                //               if ($cached == true)
                //               {
                //                 $str .= $this->get_freebusy_lines_cache($user_focus);
                //               }
                //               // generate freebusy from Meetings and Calls
                //               else
                //               {
                //$freebusy = self::create_ical_string_from_array($user_focus,$start_date_time,$end_date_time);

                $str .= $this->create_sugar_freebusy($user_focus, $start_date_time, $end_date_time);
                //               }
            }

            // UID:20030724T213406Z-10358-1000-1-12@phoenix
            $str .= self::fold_ical_lines("DTSTAMP", $utc_now_time) . self::EOL;
            $str .= "END:VFREEBUSY" . self::EOL;
            $str .= "END:VCALENDAR" . self::EOL;

            return $str;
        }

        // static function:
        // cache vcals
        public static function cache_sugar_vcal(&$user_focus)
        {
            self::cache_sugar_vcal_freebusy($user_focus);
        }

        // static function:
        // caches vcal for Activities in Sugar database
        public static function cache_sugar_vcal_freebusy(&$user_focus)
        {
            $focus = BeanFactory::newBean('vCals');
            // set freebusy members and save
            $arr = array('user_id' => $user_focus->id, 'type' => 'vfb', 'source' => 'sugar');
            $focus->retrieve_by_string_fields($arr);


            $focus->content = $focus->get_vcal_freebusy($user_focus, false);
            $focus->type = 'vfb';
            $focus->date_modified = null;
            $focus->source = 'sugar';
            $focus->user_id = $user_focus->id;
            $focus->save();
        }

        /*
         * Lines of text SHOULD NOT be longer than 75 octets, excluding the line break.
         * Long content lines SHOULD be split into a multiple line representations using a line "folding" technique
         */
        public static function fold_ical_lines($key, $value)
        {
            $iCalValue = $key . ":" . $value;

            if (strlen($iCalValue) <= self::CHARSPERLINE) {
                return $iCalValue;
            }

            $firstchars = substr($iCalValue, 0, self::CHARSPERLINE);
            $remainingchars = substr($iCalValue, self::CHARSPERLINE);
            $end = self::EOL . self::TAB;

            $remainingchars = substr(
                chunk_split($end . $remainingchars, self::CHARSPERLINE + strlen(self::EOL), $end),
                0,
                -strlen($end) // exclude last EOL and TAB chars
            );

            return $firstchars . $remainingchars;
        }

        /**
         * this function takes an iCal string and converts it to iCal array while following RFC rules
         */
        public static function create_ical_array_from_string($ical_string)
        {
            $ical_string = preg_replace("/\r\n\s+/", "", (string) $ical_string);
            $lines = preg_split("/\r?\n/", $ical_string);
            $ical_array = array();

            foreach ($lines as $line) {
                $line = self::unescape_ical_chars($line);
                $line = explode(":", $line, 2);
                if (count($line) != 2) {
                    continue;
                }
                $ical_array[] = array($line[0], $line[1]);
            }

            return $ical_array;
        }

        /**
         * this function takes an iCal array and converts it to iCal string while following RFC rules
         */
        public static function create_ical_string_from_array($ical_array, $no_folding = false)
        {
            $str = "";
            foreach ($ical_array as $ical) {
                if ($no_folding) {
                    $str .= $ical[0] . ":" . self::escape_ical_chars($ical[1]) . self::EOL;
                } else {
                    $str .= self::fold_ical_lines($ical[0], self::escape_ical_chars($ical[1])) . self::EOL;
                }
            }

            return $str;
        }

        /**
         * escape iCal chars as per RFC 5545: http://tools.ietf.org/html/rfc5545#section-3.3.11
         *
         * @param string $string string to escape chars
         *
         * @return escaped string
         */
        public static function escape_ical_chars($string)
        {
            $string = str_replace(array("\\", "\r", "\n", ";", ","), array("\\\\", "\\r", "\\n", "\\;", "\\,"), $string);

            return $string;
        }

        /**
         * unescape iCal chars as per RFC 5545: http://tools.ietf.org/html/rfc5545#section-3.3.11
         *
         * @param string $string string to escape chars
         *
         * @return unescaped string
         */
        public static function unescape_ical_chars($string)
        {
            $string = str_replace(array("\\r", "\\n", "\\;", "\\,", "\\\\"), array("\r", "\n", ";", ",", "\\"), $string);

            return $string;
        }

        /**
         * get ics file content for meeting invite email
         */
        public static function get_ical_event(SugarBean $bean, User $user)
        {
            global $timedate;
            $ical_array = array();

            $ical_array[] = array("BEGIN", "VCALENDAR");
            $ical_array[] = array("VERSION", "2.0");
            $ical_array[] = array("PRODID", "-//SuiteCRM//SuiteCRM Calendar//EN");
            $ical_array[] = array("BEGIN", "VEVENT");
            $ical_array[] = array("UID", $bean->id);
            $ical_array[] = array("ORGANIZER;CN=" . $user->name, "mailto:" . $user->email1);
            $ical_array[] = array("DTSTART", $timedate->fromDb($bean->date_start)->format(self::UTC_FORMAT));
            $ical_array[] = array("DTEND", $timedate->fromDb($bean->date_end)->format(self::UTC_FORMAT));

            $ical_array[] = array(
                "DTSTAMP",
                $GLOBALS['timedate']->getNow(false)->format(self::UTC_FORMAT)
            );
            $ical_array[] = array("SUMMARY", $bean->name);
            $ical_array[] = array("LOCATION", $bean->location);

            $descPrepend = empty($bean->join_url) ? "" : $bean->join_url . self::EOL . self::EOL;
            $ical_array[] = array("DESCRIPTION", $descPrepend . $bean->description);

            $ical_array[] = array("END", "VEVENT");
            $ical_array[] = array("END", "VCALENDAR");

            return self::create_ical_string_from_array($ical_array);
        }
    }
