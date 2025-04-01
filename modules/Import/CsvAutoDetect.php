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
 * Description: Class to detect csv file settings (delimiter, enclosure, etc)
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

/* sample usage

    $auto = new CsvAutoDetect('/tmp/_books.csv', 10);

    $delimiter = $enclosure = $heading = false;

    $ret = $auto->getCsvSettings($delimiter, $enclosure);
    if ($ret) {
        echo "found delimiter = ".$delimiter."<br>";
        echo "found enclosure = ".$enclosure."<br>";
    } else {
        echo "couldn't find settings<br>";
    }

    $ret = $auto->hasHeader($heading);
    if ($ret) {
        $header = $heading?'true':'false';
        echo "found heading = ".$header."<br>";
    } else {
        echo "couldn't determine header info<br>";
    }

    $date_format = $auto->getDateFormat();
    if ($date_format) {
        echo "found date format=".$date_format."<br>";
    } else {
        echo "couldn't find date format<br>";
    }

    $time_format = $auto->getTimeFormat();
    if ($time_format) {
        echo "found time format=".$time_format."<br>";
    } else {
        echo "couldn't find time format<br>";
    }

*/

require_once('include/parsecsv.lib.php');

#[\AllowDynamicProperties]
class CsvAutoDetect
{
    protected $_parser = null;

    protected $_csv_file = null;

    protected $_max_depth = 15;

    protected $_parsed = false;

    protected static $_date_formats = array(
        'm/d/Y' => "/^(0?[1-9]|1[012])\/(0?[1-9]|[12][0-9]|3[01])\/\d\d\d\d/", // 12/23/2010 or 3/23/2010
        'd/m/Y' => "/^(0?[1-9]|[12][0-9]|3[01])\/(0?[1-9]|1[012])\/\d\d\d\d/", // 23/12/2010 or 23/3/2010
        'Y/m/d' => "/^\d\d\d\d\/(0?[1-9]|1[012])\/(0?[1-9]|[12][0-9]|3[01])/", // 2010/12/23 or 2010/3/23
        'm-d-Y' => "/^(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])-\d\d\d\d/", // 12-23-2010 or 3-23-2010
        'd-m-Y' => "/^(0?[1-9]|[12][0-9]|3[01])-(0?[1-9]|1[012])-\d\d\d\d/", // 23-12-2010 or 23-3-2010
        'Y-m-d' => "/^\d\d\d\d-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])/", // 2010-12-23 or 2010-3-23
        'm.d.Y' => "/^(0?[1-9]|1[012])\.(0?[1-9]|[12][0-9]|3[01])\.\d\d\d\d/", // 12.23.2010 or 3.23.2010
        'd.m.Y' => "/^(0?[1-9]|[12][0-9]|3[01])\.(0?[1-9]|1[012])\.\d\d\d\d/", // 23.12.2010 or 23.3.2010
        'Y.m.d' => "/^\d\d\d\d\.(0?[1-9]|1[012])\.(0?[1-9]|[12][0-9]|3[01])/", // 2010.12.23 or 2010.3.23
    );

    protected static $_time_formats =  array(
        'h:ia' => "/(^| )(0?[0-9]|1[0-2]):(0?[0-9]|[1-5][0-9])(:0?[0-9]|[1-5][0-9])?[am|pm]/", // 11:00pm or 11:00:00pm or 9:3pm
        'h:iA' => "/(^| )(0?[0-9]|1[0-2]):(0?[0-9]|[1-5][0-9])(:0?[0-9]|[1-5][0-9])?[AM|PM]/", // 11:00PM or 11:00:00PM or 9:3PM
        'h:i a' => "/(^| )(0?[0-9]|1[0-2]):(0?[0-9]|[1-5][0-9])(:0?[0-9]|[1-5][0-9])? [am|pm]/", // 11:00 pm or 11:00:00 pm or 9:3 pm
        'h:i A' => "/(^| )(0?[0-9]|1[0-2]):(0?[0-9]|[1-5][0-9])(:0?[0-9]|[1-5][0-9])? [AM|PM]/", // 11:00 PM or 11:00:00 PM or 9:3 PM
        'H:i' => "/(^| )(0?[0-9]|1[0-9]|2[0-4]):(0?[0-9]|[1-5][0-9])(:0?[0-9]|[1-5][0-9])?/", // 23:00 or 23:00:00 or 9:3
        'h.ia' => "/(^| )(0?[0-9]|1[0-2])\.(0?[0-9]|[1-5][0-9])(\.0?[0-9]|[1-5][0-9])?[am|pm]/", // 11.00pm or 11.00.00pm or 9.3pm
        'h.iA' => "/(^| )(0?[0-9]|1[0-2])\.(0?[0-9]|[1-5][0-9])(\.0?[0-9]|[1-5][0-9])?[AM|PM]/", // 11.00PM or 11.00.00PM or 9.3PM
        'h.i a' => "/(^| )(0?[0-9]|1[0-2])\.(0?[0-9]|[1-5][0-9])(\.0?[0-9]|[1-5][0-9])? [am|pm]/", // 11.00 pm or 11.00.00 pm or 9.3 pm
        'h.i A' => "/(^| )(0?[0-9]|1[0-2])\.(0?[0-9]|[1-5][0-9])(\.0?[0-9]|[1-5][0-9])? [AM|PM]/", // 11.00 PM or 11.00.00 PM or 9.3 PM
        'H.i' => "/(^| )(0?[0-9]|1[0-9]|2[0-4])\.(0?[0-9]|[1-5][0-9])(\.0?[0-9]|[1-5][0-9])?/", // 23.00 or 23.00.00 or 9.3
    );


    /**
     * Constructor
     *
     * @param string $csv_filename
     * @param int $max_depth
     */
    public function __construct($csv_filename, $max_depth = 2)
    {
        $this->_csv_file = $csv_filename;

        $this->_parser = new parseCSV();

        $this->_parser->auto_depth = $max_depth;

        $this->_max_depth = $max_depth;
    }



    /**
     * To get the possible csv settings (delimiter, enclosure).
     * This function causes CSV to be parsed.
     * So call this function before calling others.
     *
     * @param string $delimiter
     * @param string $enclosure
     * @return bool true if settings are found, false otherwise
     */
    public function getCsvSettings(&$delimiter, &$enclosure)
    {
        // try parsing the file to find possible delimiter and enclosure
        $this->_parser->heading = false;

        $found_setting = false;

        $singleQuoteParsedOK = $doubleQuoteParsedOK = false;
        $beginEndWithSingle = $beginEndWithDouble = false;

        // check double quotes first
        $depth = 1;
        $enclosure = "\"";
        $delimiter1 = $this->_parser->auto($this->_csv_file, true, null, null, $enclosure);
        if (strlen((string) $delimiter1) == 1) { // this means parsing ok
            $doubleQuoteParsedOK = true;
            // sometimes it parses ok with either single quote or double quote as enclosure
            // so we need to make sure the data do not begin and end with the other enclosure
            foreach ($this->_parser->data as &$row) {
                foreach ($row as &$data) {
                    $len = strlen((string) $data);
                    // check if it begins and ends with single quotes
                    // if it does, then it double quotes may not be the enclosure
                    if ($len>=2 && $data[0] == "'" && $data[$len-1] == "'") {
                        $beginEndWithSingle = true;
                        break;
                    }
                }
                if ($beginEndWithSingle) {
                    break;
                }
                $depth++;
                if ($depth > $this->_max_depth) {
                    break;
                }
            }
            if (!$beginEndWithSingle) {
                $delimiter = $delimiter1;
                $found_setting = true;
            }
        }

        // check single quotes
        if (!$found_setting) {
            $depth = 1;
            $enclosure = "'";
            $delimiter2 = $this->_parser->auto($this->_csv_file, true, null, null, $enclosure);
            if (strlen((string) $delimiter2) == 1) { // this means parsing ok
                $singleQuoteParsedOK = true;
                foreach ($this->_parser->data as &$row) {
                    foreach ($row as &$data) {
                        $len = strlen((string) $data);
                        // check if it begins and ends with double quotes
                        // if it does, then it single quotes may not be the enclosure
                        if ($len>=2 && $data[0] == "\"" && $data[$len-1] == "\"") {
                            $beginEndWithDouble = true;
                            break;
                        }
                    }
                    if ($beginEndWithDouble) {
                        break;
                    }
                    $depth++;
                    if ($depth > $this->_max_depth) {
                        break;
                    }
                }
                if (!$beginEndWithDouble) {
                    $delimiter = $delimiter2;
                    $found_setting = true;
                }
            }
        }

        if (!$found_setting) {
            // we don't seem to have a perfect enclosure candidate
            // let's pick one of the possible candidates
            if ($doubleQuoteParsedOK) {
                // if double quotes parsed ok, let's take that
                $delimiter = $delimiter1;
                $enclosure = "\"";
                $found_setting = true;
            } else {
                if ($singleQuoteParsedOK) {
                    // otherwise, if single quote parsed ok, let's use it
                    $delimiter = $delimiter2;
                    $enclosure = "'";
                    $found_setting = true;
                }
            }
        }

        if (!$found_setting) {
            return false;
        }

        $this->_parsed = true;

        return true;
    }

    /**
     * To check CSV heading
     *
     * @param bool $heading true of it has header, false if not
     * @return bool true if header is found, false if error
     */
    public function hasHeader(&$heading, $module, $encoding = null)
    {
        if (!$this->_parsed) {
            return false;
        }

        $total_count = is_countable($this->_parser->data[0]) ? count($this->_parser->data[0]) : 0;
        if ($total_count == 0) {
            return false;
        }

        if (!isset($GLOBALS['beanList'][$module])) {
            return false;
        }

        $bean = new $GLOBALS['beanList'][$module]();

        $match_count = 0;

        $mod_strings = return_module_language($GLOBALS['current_language'], $module);

        global $locale;
        // process only the first row
        foreach ($this->_parser->data[0] as $val) {
            if (!empty($encoding)) {
                // Convert all values to UTF-8
                $val = $locale->translateCharset($val, $encoding);
            }
            
            // bug51433 - everything relies on $val having a value so if it's empty,
            // we can skip this iteration and not get warnings
            if (!empty($val)) {
                foreach ($bean->field_defs as $field_name=>$defs) {

                    // check if the CSV item matches field name
                    if (!strcasecmp($val, $field_name)) {
                        $match_count++;
                        break;
                    }
                    // check if the CSV item is part of the label or vice versa
                    else {
                        if (isset($defs['vname']) && isset($mod_strings[$defs['vname']])) {
                            if (stripos(trim($mod_strings[$defs['vname']], ':'), (string) $val) !== false || stripos((string) $val, trim($mod_strings[$defs['vname']], ':')) !== false) {
                                $match_count++;
                                break;
                            }
                        } else {
                            if (isset($defs['vname']) && isset($GLOBALS['app_strings'][$defs['vname']])) {
                                if (stripos(trim($GLOBALS['app_strings'][$defs['vname']], ':'), (string) $val) !== false || stripos((string) $val, trim($GLOBALS['app_strings'][$defs['vname']], ':')) !== false) {
                                    $match_count++;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        // if more than 50% matched, consider it a header
        if ($match_count/$total_count >= 0.5) {
            $heading = true;
        } else {
            $heading = false;
        }

        return true;
    }


    /**
     * To get the possible format (for date or time)
     *
     * @param array $formats
     * @return mixed possible format if found, false otherwise
     */
    protected function getFormat(&$formats)
    {
        if (!$this->_parsed) {
            return false;
        }

        $depth = 1;

        foreach ($this->_parser->data as $row) {
            foreach ($row as $val) {
                foreach ($formats as $format=>$regex) {
                    $ret = preg_match($regex, (string) $val);
                    if ($ret) {
                        return $format;
                    }
                }
            }

            // give up if reaching max depth
            $depth++;
            if ($depth > $this->_max_depth) {
                break;
            }
        }

        return false;
    }


    /**
     * To get the possible date format used in the csv file
     *
     * @return mixed possible date format if found, false otherwise
     */
    public function getDateFormat()
    {
        $format = $this->getFormat(self::$_date_formats);

        return $format;
    }


    /**
     * To get the possible time format used in the csv file
     *
     * @return mixed possible time format if found, false otherwise
     */
    public function getTimeFormat()
    {
        $format = $this->getFormat(self::$_time_formats);

        return $format;
    }
}
