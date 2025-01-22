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



class SugarDependentDropdown
{
    /*
     * Holds processed metadata, ready for JSON
     */
    public $metadata;

    /*
     * Flag to suppress errors and/or log more heavily
     */
    public $debugMode = false;

    /*
     * Default metadata array that will be merged with any passed fields to
     * ensure uniformity
     */
    public $defaults = array(
        'name'		=> '',
        'id'		=> '',
        'type'		=> 'none',	// form element, valid "select", "input", "checkbox", "none"
        'label_pos'	=> 'left',		// valid: 'left', 'right', 'top', 'bottom', 'none' (none)
        'hidden'	=> array(),		// metadata to create hidden fields with values you choose
    );

    /*
     * Fields that must exist in an element (single dropdown/field) metadata
     * array.
     */
    public $elementRequired = array(
        'name',
        'id',
        //'values',
        //'onchange',
        //'force_render',
    );

    /**
     * Fields that will be merged down into individual elements and handlers
     */
    public $alwaysMerge = array(
        'force_render',
    );

    /*
     * Valid 'types' for a dependent dropdown
     */
    public $validTypes = array(
        "select", 	// select dropdown
        "input", 	// text input field
        "checkbox",	// checkbox (radio buttons will not be supported)
        "none", 	// blank
        "multiple"	// custom functionality
    );

    /**
     * Sole constructor
     * @param string $metadata Path to metadata file to consume
     */
    public function __construct($metadata='')
    {
        if (!empty($metadata)) {
            $this->init($metadata);
        }
    }

    /**
     * Prepares an instance of SDD for use with a given set
     * @param string $metadata Path to metadata file to consume
     */
    public function init($metadata)
    {
        if (is_string($metadata)) {
            if ($this->debugMode) {
                $this->debugOutput("Got metadata file [ {$metadata} ]");
            }
            if (file_exists($metadata)) {
                $sugarDependentDropdown = array();
                /*
                 * The metadata file should be prepped in an associative array.
                 * The array should be named "$sugarDependentDropdown".
                 *
                 * Examine:
                 * "include/SugarDependentDropdown/metadata/dependentDropdown.
                 * php" for a commented example.
                 */
                include($metadata); // provides $sugarDependentDropdown

                foreach ($sugarDependentDropdown as $key => $type) {
                    if (is_array($type)) {
                        foreach ($type as $k => $v) {
                            if ($k == 'elements') {
                                ksort($v); // order elements
                                foreach ($v as $index => $element) {
                                    $v[$index] = $this->initElement($element, $type['always_merge']);
                                }
                            } // end Work with elements block
                            $type[$k] = $v;
                        }

                        if (!$this->verifyMetadata($type)) {
                            if ($this->debugMode) {
                                $this->debugOutput("SugarRouting: metadata initialization failed.  Please check your metadata source.");
                            }
                        }
                    }
                    $sugarDependentDropdown[$key] = $type;
                } // end foreach of metadata

                /* we made it through all checks so assign this to the output attribute */
                $this->metadata = $sugarDependentDropdown;
            } // end file_exists();
            else {
                if ($this->debugMode) {
                    $this->debugOutput("SugarRouting could not find file [ {$metadata} ]");
                }
            }
        } // end is_string();
    } // end init()


    ///////////////////////////////////////////////////////////////////////////
    ////	PRIVATE UTILS
    /**
     * Verifies that an element is valid and has all the required info.
     */
    public function isValidElement($element)
    {
        if (is_array($element)) {
            foreach ($this->elementRequired as $k => $req) {
                if (!array_key_exists($req, $element) && !isset($element['handlers'])) {
                    if ($this->debugMode) {
                        $this->debugOutput("Element is missing required field: [ $req ]");
                        $this->debugOutput($element);
                    }
                    return false;
                }
            }
            return true;
        }

        if ($this->debugMode) {
            $this->debugOutput("isValidElement is returning false.  Passed the following as an argument:");
            $this->debugOutput($element);
        }
        return false;
    }

    /**
     * Initializes an element for processing
     * @param array $element Element metadata
     * @return array
     */
    public function initElement($element, $alwaysMerge)
    {
        if ($this->isValidElement($element)) {
            $mergedElement = sugarArrayMerge($this->defaults, $element);

            foreach ($alwaysMerge as $key => $val) {
                if (!isset($mergedElement[$key])) {
                    $mergedElement[$key] = $val;
                }
            }

            if ($this->debugMode) {
                foreach ($this->elementRequired as $k => $v) {
                    if (!array_key_exists($v, $mergedElement) && !isset($mergedElement['handlers'])) {
                        $this->debugOutput("Element is missing required field after initialization: [ $v ].");
                    }
                }
            }

            // iterate through "handlers - mini-elements"
            if (isset($mergedElement['handlers'])) {
                if (is_array($mergedElement['handlers']) && !empty($mergedElement['handlers'])) {
                    foreach ($mergedElement['handlers'] as $miniKey => $miniElement) {
                        // apply parent element's properties to mini-element
                        foreach ($mergedElement as $key => $el) {
                            if ($key != 'handlers' && (!isset($miniElement[$key]))) {// || empty($miniElement[$key])
                                $miniElement[$key] = $mergedElement[$key];
                            }
                        }

                        $miniElement = $this->initElement($miniElement, $alwaysMerge);
                        $mergedElement['handlers'][$miniKey] = $miniElement;
                    }
                } else {
                    if ($this->debugMode) {
                        $this->debugOutput("SugarRouting: element contains 'handlers' but is not an array:");
                        $this->debugOutput($mergedElement);
                    }
                }
            }

            return $mergedElement;
        } else {
            if ($this->debugMode) {
                $this->debugOutput("SugarRouting is trying to initialize a non-element:");
                $this->debugOutput($element);
            }
        }
    }

    /**
     * Verifies that required metadata is present for all dependencies. Called after all metadata defaults are merged
     * and the final array is created.
     * @param array $metadata
     * @return bool
     */
    public function verifyMetadata($metadata)
    {
        if (isset($metadata['elements']) && !empty($metadata['elements']) && is_array($metadata['elements'])) {
            $elements = $metadata['elements'];

            foreach ($elements as $indexName => $element) {
                /* confirm each element has a valid type */
                if (!isset($element['type']) && in_array($element['type'], $this->validTypes)) {
                    if ($this->debugMode) {
                        $this->debugOutput("SugarRouting: valid 'type' not found:");
                        $this->debugOutput($element);
                    }
                    return false;
                }

                /************************************************
                 * Check based on "type"
                 */
                switch ($element['type']) {
                    case "select":
                        if (isset($element['values'])) {
                            $index = substr($indexName, 7, strlen($indexName));


                            /* if we have an array to iterate through - this is not the case with lazy-loaded values */
                            if (is_array($element['values']) && !empty($element['values'])) {
                                $index++; // string to int conversion, i know, sucks
                                $nextElementKey = "element".$index;
                                $nextElement = $elements[$nextElementKey];

                                foreach ($element['values'] as $key => $value) {
                                    if (!array_key_exists($key, $nextElement['handlers'])) {
                                        if ($this->debugMode) {
                                            $this->debugOutput("SugarRouting: next-order element is missing a handler for value: [ {$key} ]");
                                            $this->debugOutput($elements);
                                            $this->debugOutput($nextElement);
                                        }
                                        return false;
                                    }
                                }
                            }
                        } else {
                            if ($this->debugMode) {
                                $this->debugOutput("SugarRouting: 'select' element found, no 'values' defined.");
                                $this->debugOutput($element);
                            }
                            return false;
                        }
                    break; // end "select" check
                }

                /*
                 * Handler "handlers" mini-element metadata definition verification
                 */
                if (isset($element['handlers']) && !empty($element['handlers'])) {
                    // fake metadata container
                    $fakeMetadata = array('elements' => null);
                    $fakeMetadata['elements'] = $element['handlers'];
                    $result = $this->verifyMetadata($fakeMetadata);

                    if (!$result) {
                        if ($this->debugMode) {
                            $this->debugOutput("SugarRouting: metadata verification for 'handlers' failed: ");
                            $this->debugOutput($fakeMetadata);
                        }
                        return false;
                    }
                }
            }

            if ($this->debugMode) {
                $this->debugOutput((count($metadata) > 1) ? "SugarRouting: all checks passed, valid metadata confirmed" : "SugarRouting: 'handlers' checks passed, valid metadata confirmed.");
            }
            return true;
        } else {
            if ($this->debugMode) {
                $this->debugOutput("SugarRouting: Your metadata does not contain a valid 'elements' array:");
                $this->debugOutput($metadata);
            }
        }
        return false;
    }

    /**
     * Prints debug messages to the screen
     * @param mixed
     */
    public function debugOutput($v)
    {
        echo "\n<pre>\n";
        print_r($v);
        echo "\n</pre>\n";
    }
    ////	END PRIVATE UTILS
    ///////////////////////////////////////////////////////////////////////////
} // end Class def
