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
 * DetailViewMetaParser.php
 * This is a utility file that attempts to provide support for parsing pre 5.0 SugarCRM
 * DetailView.html files and produce a best guess detailviewdefs.php file equivalent.
 *
 * @author Collin Lee
 */
require_once('include/SugarFields/Parsers/MetaParser.php');

/**
 * DetailViewMetaParser.php
 * This class is responsible for handling the parsing of DetailView.html files from
 * SugarCRM versions prior to 5.x.  It will make a best guess of translating the
 * HTML file to a metadata format.
 *
 * @author Collin Lee
 */
class DetailViewMetaParser extends MetaParser
{
    public function __construct()
    {
        $this->mView = 'DetailView';
    }


    /**
     * parse
     *
     * @param $filePath The file path of the HTML file to parse
     * @param $vardefs The module's vardefs
     * @param $moduleDir The module's directory
     * @param $merge boolean value indicating whether or not to merge the parsed contents
     * @param $masterCopy The file path of the mater copy of the metadata file to merge against
     * @return String format of metadata contents
     **/
    public function parse($filePath, $vardefs = array(), $moduleDir = '', $merge=false, $masterCopy=null)
    {

// Grab file contents
        $contents = file_get_contents($filePath);

        // Remove \n,\r characters to allow for better text parsing
        $contents = $this->trimHTML($contents);


        // Notes DetailView.html file is messed up
        if ($moduleDir == 'Notes') {
            $contents = str_replace('{PAGINATION}<tr><td>', '{PAGINATION}', $contents);
            $contents = str_replace('</td></tr></table><script>', '</table><script>', $contents);
            $contents = str_replace("<tr><div id='portal_flag_row' name='portal_flag_row' style='display:none'>", "<div id='portal_flag_row' name='portal_flag_row' style='display:none'>", $contents);
        }

        $contents = $this->fixDuplicateTrTags($contents);
        $contents = $this->fixRowsWithMissingTr($contents);

        // Get all the tables
        $tables = $this->getElementsByType("table", $contents);

        // Skip the first one
        $tables = array_slice($tables, 1);

        $panels = array();
        $tableCount = 0;
        $metarow = array();
        foreach ($tables as $table) {
            $table = $this->fixTablesWithMissingTr($table);
            $tablerows = $this->getElementsByType("tr", $table);

            foreach ($tablerows as $trow) {
                $metacolumns = array();
                $columns = $this->getElementsByType("td", $trow);
                $columns = array_reverse($columns, true);
                foreach ($columns as $tcols) {
                    $sugarAttrValue = $this->getTagAttribute("sugar", $tcols, "'slot[0-9]+b$'");
                    if (empty($sugarAttrValue)) {
                        continue;
                    }

                    $def = '';
                    $field = $this->getElementValue("span", $tcols);
                    //If it's a space, simply add a blank string
                    if ($field == '&nbsp;') {
                        $metacolumns[] = "";
                    } else {
                        if (!empty($field)) {
                            preg_match_all('/[\{]([^\}].*?)[\}]/s', $field, $matches, PREG_SET_ORDER);
                            if (!empty($matches)) {
                                if (count($matches) > 1) {
                                    $def = array();

                                    $def['name'] = preg_match('/_c$/i', $matches[0][1]) ? $matches[0][1] : strtolower($matches[0][1]);
                                    foreach ($matches as $m) {
                                        if (isset($vardefs[strtolower($m[1])])) {
                                            $def['name'] = strtolower($m[1]);
                                        }
                                    }

                                    $field = preg_replace('/<\{tag\.[a-z_]*?\}/i', '<a', $field);
                                    $field = preg_replace('/<\/\{tag\.[a-z_]*?\}>/i', '</a>', $field);

                                    foreach ($matches as $tag[1]) {
                                        if (preg_match("/^(mod[\.]|app[\.]).*?/i", $tag[1][1])) {
                                            $field = str_replace($tag[1][1], '$'.$tag[1][1], $field);
                                        } else {
                                            $theField = preg_match('/_c$/i', $tag[1][1]) ? $tag[1][1] : strtolower($tag[1][1]);
                                            if (!empty($vardefs[$theField])) {
                                                $field = str_replace($tag[1][1], '$fields.'. $theField.'.value', $field);
                                            } else {
                                                $phpName = $this->findAssignedVariableName($tag[1][1], $filePath);
                                                $field = str_replace($tag[1][1], '$fields.'. $theField.'.value', $field);
                                            } //if-else
                                        }
                                    }

                                    $def['customCode'] = $field;
                                    $def['description'] = 'This field was auto generated';
                                } else {
                                    $def = strtolower($matches[0][1]);
                                }
                            } //if
                            $metacolumns[] = $def;
                        }
                    } //if
                } //foreach($tablecolumns as $tcols)

       $metarow[] = array_reverse($metacolumns);
            } //foreach($tablerows as $trow)


            $id = $tableCount == 0 ? 'default' : $tableCount;
            $tableCount++;
            $panels[$id] = $metarow;
        } //foreach($tables as $table)

        $this->mCustomPanels = $panels;
        $panels = $this->applyPreRules($moduleDir, $panels);

        $templateMeta = array();
        if ($merge && !empty($masterCopy) && file_exists($masterCopy)) {
            $panels = $this->mergePanels($panels, $vardefs, $moduleDir, $masterCopy);
            $templateMeta = $this->mergeTemplateMeta($templateMeta, $moduleDir, $masterCopy);
        }

        $panels = $this->applyRules($moduleDir, $panels);
        return $this->createFileContents($moduleDir, $panels, $templateMeta);
    }
}
