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



if (file_exists('custom/include/Sugarpdf/sugarpdf_config.php')) {
    require_once('custom/include/Sugarpdf/sugarpdf_config.php');
} else {
    require_once('include/Sugarpdf/sugarpdf_config.php');
}

require_once('include/tcpdf/tcpdf.php');
require_once('include/Sugarpdf/SugarpdfHelper.php');

class Sugarpdf extends TCPDF
{
    /**
     * Stretch options constants
     */
    const STRETCH_NONE = 0;
    const STRETCH_SCALE = 1;
    const STRETCH_SCALE_FORCED = 2;
    const STRETCH_SPACING = 3;
    const STRETCH_SPACING_FORCED = 4;

    /**
     * This array is meant to hold an objects/data that we would like to pass between
     * the controller and the view.  The bean will automatically be set for us, but this
     * is meant to hold anything else.
     */
    public $sugarpdf_object_map = array();
    /**
     * The name of the current module.
     */
    public $module = '';
    /**
     * The name of the current action.
     */
    public $action = '';
    /**
     */
    public $bean = null;
    /**
    * Any errors that occurred this can either be set by the view or the controller or the model
    */
    public $errors = array();
    /**
     * Use to set the filename of the output pdf file.
     */
    public $fileName = PDF_FILENAME;
    /**
     * Use for the ACL access.
     */
    public $aclAction = PDF_ACL_ACCESS;
    /**
     * Constructor which will peform the setup.
     */


    public function __construct($bean = null, $sugarpdf_object_map = array(), $orientation=PDF_PAGE_ORIENTATION, $unit=PDF_UNIT, $format=PDF_PAGE_FORMAT, $unicode=true, $encoding='UTF-8', $diskcache=false)
    {
        global $locale;
        //  $encoding = $locale->getExportCharset();
        if (empty($encoding)) {
            $encoding = "UTF-8";
        }
        parent::__construct($orientation, $unit, $format, $unicode, $encoding, $diskcache);
        $this->module = $GLOBALS['module'];
        $this->bean = $bean;
        $this->sugarpdf_object_map = $sugarpdf_object_map;
        if (!empty($_REQUEST["sugarpdf"])) {
            $this->action = $_REQUEST["sugarpdf"];
        }
    }

    /**
     * This method will be called from the controller and is not meant to be overridden.
     */
    public function process()
    {
        $this->preDisplay();
        $this->display();
    }

    /**
     * This method will display the errors on the page.
     */
    public function displayErrors()
    {
        foreach ($this->errors as $error) {
            echo '<span class="error">' . $error . '</span><br>';
        }
    }

    /**
     * [OVERRIDE] - This method is meant to overidden in a subclass. The purpose of this method is
     * to allow a view to do some preprocessing before the display method is called. This becomes
     * useful when you have a view defined at the application level and then within a module
     * have a sub-view that extends from this application level view.  The application level
     * view can do the setup in preDisplay() that is common to itself and any subviews
     * and then the subview can just override display(). If it so desires, can also override
     * preDisplay().
     */
    public function preDisplay()
    {
        // set document information
        $this->SetCreator(PDF_CREATOR);
        $this->SetAuthor(PDF_AUTHOR);
        $this->SetTitle(PDF_TITLE);
        $this->SetSubject(PDF_SUBJECT);
        $this->SetKeywords(PDF_KEYWORDS);

        // set other properties
        $compression=false;
        if (PDF_COMPRESSION == "on") {
            $compression=true;
        }
        $this->SetCompression($compression);
        $protection=array();
        if (PDF_PROTECTION != "") {
            $protection=explode(",", PDF_PROTECTION);
        }

        $this->SetProtection($protection, blowfishDecode(blowfishGetKey('sugarpdf_pdf_user_password'), PDF_USER_PASSWORD), blowfishDecode(blowfishGetKey('sugarpdf_pdf_owner_password'), PDF_OWNER_PASSWORD));
        $this->setCellHeightRatio(K_CELL_HEIGHT_RATIO);
        $this->setJPEGQuality((int)PDF_JPEG_QUALITY);
        $this->setPDFVersion(PDF_PDF_VERSION);

        // set default header data
        $this->setHeaderData(PDF_HEADER_LOGO, PDF_HEADER_LOGO_WIDTH, PDF_HEADER_TITLE, PDF_HEADER_STRING);

        // set header and footer fonts
        $this->setHeaderFont(array(PDF_FONT_NAME_MAIN, '', PDF_FONT_SIZE_MAIN));
        $this->setFooterFont(array(PDF_FONT_NAME_DATA, '', PDF_FONT_SIZE_DATA));

        //set margins
        $this->SetMargins(PDF_MARGIN_LEFT, PDF_MARGIN_TOP, PDF_MARGIN_RIGHT);
        $this->setHeaderMargin(PDF_MARGIN_HEADER);
        $this->setFooterMargin(PDF_MARGIN_FOOTER);

        //set auto page breaks
        $this->SetAutoPageBreak(true, PDF_MARGIN_BOTTOM);

        //set image scale factor
        $this->setImageScale(PDF_IMAGE_SCALE_RATIO);

        //set some language-dependent strings
        //$this->setLanguageArray($l);

        // ---------------------------------------------------------
    }

    /**
     * [OVERRIDE] - This method is meant to overidden in a subclass.
     */
    public function display()
    {
        $this->AddPage();
        $this->SetFont(PDF_FONT_NAME_MAIN, 'B', 16);
        $this->MultiCell(0, 0, 'Tcpdf class for this module and action has not been implemented.', 0, 'C');
        $this->Info();
    }

    /**
     * [OVERRIDE]
     * This method override the regular Header() method to enable the custom image directory in addition to the OOB image directory.
     * This method is used to render the page header.
     * It is automatically called by AddPage().
     * @access public
    * @see include/tcpdf/TCPDF#Header()
     */
    public function Header()
    {
        $ormargins = $this->getOriginalMargins();
        $headerfont = $this->getHeaderFont();
        $headerdata = $this->getHeaderData();

        if (($headerdata['logo']) and ($headerdata['logo'] != K_BLANK_IMAGE)) {

            // START SUGARPDF
            $logo = K_PATH_CUSTOM_IMAGES.$headerdata['logo'];
            $imsize = @getimagesize($logo);
            if ($imsize === false) {
                // encode spaces on filename
                $logo = str_replace(' ', '%20', $logo);
                $imsize = @getimagesize($logo);
                if ($imsize === false) {
                    $logo = K_PATH_IMAGES.$headerdata['logo'];
                }
            }
            // END SUGARPDF

            $this->Image($logo, $this->GetX(), $this->getHeaderMargin(), $headerdata['logo_width']);
            $imgy = $this->getImageRBY();
        } else {
            $imgy = $this->GetY();
        }
        $cell_height = round(($this->getCellHeightRatio() * $headerfont[2]) / $this->getScaleFactor(), 2);
        // set starting margin for text data cell
        if ($this->getRTL()) {
            $header_x = $ormargins['right'] + ($headerdata['logo_width'] * 1.1);
        } else {
            $header_x = $ormargins['left'] + ($headerdata['logo_width'] * 1.1);
        }
        $this->SetTextColor(0, 0, 0);
        // header title
        $this->SetFont($headerfont[0], 'B', $headerfont[2] + 1);
        $this->SetX($header_x);
        $this->Cell(0, $cell_height, $headerdata['title'], 0, 1, '', 0, '', 0);
        // header string
        $this->SetFont($headerfont[0], $headerfont[1], $headerfont[2]);
        $this->SetX($header_x);
        $this->MultiCell(0, $cell_height, $headerdata['string'], 0, '', 0, 1, '', '', true, 0, false);
        // print an ending header line
        $this->SetLineStyle(array('width' => 0.85 / $this->getScaleFactor(), 'cap' => 'butt', 'join' => 'miter', 'dash' => 0, 'color' => array(0, 0, 0)));
        $this->SetY((2.835 / $this->getScaleFactor()) + max($imgy, $this->GetY()));
        if ($this->getRTL()) {
            $this->SetX($ormargins['right']);
        } else {
            $this->SetX($ormargins['left']);
        }
        $this->Cell(0, 0, '', 'T', 0, 'C');
    }

    /**
    * [OVERRIDE] SetFont method in TCPDF Library
    * This method override the regular SetFont() method to enable the custom font directory in addition to the OOB font directory.
    *
    * @param string $family Family font. It can be either a name defined by AddFont() or one of the standard Type1 families (case insensitive):<ul><li>times (Times-Roman)</li><li>timesb (Times-Bold)</li><li>timesi (Times-Italic)</li><li>timesbi (Times-BoldItalic)</li><li>helvetica (Helvetica)</li><li>helveticab (Helvetica-Bold)</li><li>helveticai (Helvetica-Oblique)</li><li>helveticabi (Helvetica-BoldOblique)</li><li>courier (Courier)</li><li>courierb (Courier-Bold)</li><li>courieri (Courier-Oblique)</li><li>courierbi (Courier-BoldOblique)</li><li>symbol (Symbol)</li><li>zapfdingbats (ZapfDingbats)</li></ul> It is also possible to pass an empty string. In that case, the current family is retained.
    * @param string $style Font style. Possible values are (case insensitive):<ul><li>empty string: regular</li><li>B: bold</li><li>I: italic</li><li>U: underline</li><li>D: line trough</li></ul> or any combination. The default value is regular. Bold and italic styles do not apply to Symbol and ZapfDingbats basic fonts or other fonts when not defined.
    * @param float $size Font size in points. The default value is the current size. If no size has been specified since the beginning of the document, the value taken is 12
    * @param string $fontfile The font definition file. By default, the name is built from the family and style, in lower case with no spaces.
    * @access public
    * @see include/tcpdf/TCPDF#SetFont()
    */
    public function SetFont($family, $style = '', $size = null, $fontfile = '', $subset = 'default', $out = true)
    {
        if (empty($fontfile) && defined('K_PATH_CUSTOM_FONTS')) {
            // This will force addFont to search the custom directory for font before the OOB directory
            $fontfile = K_PATH_CUSTOM_FONTS . "phantomFile.phantom";
        }
        parent::SetFont($family, $style, $size, $fontfile, $subset, $out);
    }

    public function Info()
    {
        $this->SetFont(PDF_FONT_NAME_MAIN, '', 12);
        $this->MultiCell(0, 0, '---', 0, 'L');
        $this->MultiCell(0, 0, 'Class: '.get_class($this), 0, 'L');
        $this->MultiCell(0, 0, 'Extends: '.get_parent_class($this), 0, 'L');
        $this->MultiCell(0, 0, '---', 0, 'L');
        $this->MultiCell(0, 0, 'Module: '.$this->module, 0, 'L');
        $this->MultiCell(0, 0, 'Tcpdf Action: '.$this->action, 0, 'L');
        $this->MultiCell(0, 0, 'Bean ID: '.$this->bean->getFieldValue('id'), 0, 'L');
        $this->SetFont(PDF_FONT_NAME_MAIN, '', 12);
        $this->MultiCell(0, 0, '---', 0, 'L');
    }

    /**
     * [OVERRIDE] Cell method in tcpdf library.
     * Handle charset conversion and HTML entity decode.
     * This method override the regular Cell() method to apply the prepare_string() function to
     * the string to print in the PDF.
     * The cell method is used by all the methods which print text (Write, MultiCell).
     * @see include/tcpdf/TCPDF#Cell()
     */
    public function Cell($w, $h = 0, $txt = '', $border = 0, $ln = 0, $align = '', $fill = false, $link = '', $stretch = 0, $ignore_min_height = false, $calign = 'T', $valign = 'M')
    {
        parent::Cell($w, $h, prepare_string($txt), $border, $ln, $align, $fill, $link, $stretch, $ignore_min_height, $calign, $valign);
    }

    /**
     * This Ln1() method will always print a line break of one line height.
     * The regular Ln() method print a line break which has the height of the last printed cell.
     */
    public function Ln1()
    {
        parent::Ln($this->FontSize * $this->cell_height_ratio + 2 * $this->cMargin, false);
    }


    /**
     * This method allow printing a table using the MultiCell method with a formatted options array in parameter
     * Options :
     * header options override the regular options for the header's cells - $options['header']
     * cell options override the regular options for the specific cell - Array[line number (0 to x)][cell header]['options']
     * @param $item Array[line number (0 to x)][cell header] = Cell content OR
     *              Array[line number (0 to x)][cell header]['value'] = Cell content AND
     *              Array[line number (0 to x)][cell header]['options'] = Array[cell properties] = values
     * @param $options Array which can contain : width (array 'column name'=>'width value + % OR nothing'), isheader (bool), header (array), fill (string: HTML color), ishtml (bool) default: false, border (0: no border (defaul), 1: frame or all of the following characters: L ,T ,R ,B), align (L: left align, C: center, R: right align, J: justification), stretch (array 'column name'=>stretch type)
     * @see MultiCell()
     */
    public function writeCellTable($item, $options=null)
    {
        // Save initial font values
        $fontFamily = $this->getFontFamily();
        $fontSize = $this->getFontSizePt();
        $fontStyle = $this->getFontStyle();
        $this->SetTextColor(0, 0, 0);

        $options = $this->initOptionsForWriteCellTable($options, $item);

        // HEADER
        if (!isset($options['isheader']) || $options['isheader'] == true) {
            $headerOptions = $options;
            if (!empty($options['header']) && is_array($options['header'])) {
                $headerOptions = $this->initOptionsForWriteCellTable($options['header'], $item);
            }
            foreach ($item[0] as $k => $v) {
                $header[$k]=$k;
            }
            $h = $this->getLineHeightFromArray($header, $options["width"]);
            foreach ($header as $v) {
                $this->MultiCell($options["width"][$v], $h, $v, $headerOptions['border'], $headerOptions['align'], $headerOptions['fillstate'], 0, '', '', true, $options['stretch'][$v], $headerOptions['ishtml']);
            }
            $this->SetFillColorArray($this->convertHTMLColorToDec($options['fill']));
            $this->Ln();
        }

        // MAIN
        // default font
        $this->SetFont($fontFamily, $fontStyle, $fontSize);
        $this->SetTextColor(0, 0, 0);
        $even=true;
        $firstrow = true;
        // LINES
        foreach ($item as $k=>$line) {
            $even=!$even;
            $h = $this->getLineHeightFromArray($line, $options["width"]);
            // in the case when cell height is greater than page height
            // need to adjust the current page number
            // so the following output will not overlap the previous output
            if ($this->getNumPages() != $this->getPage()) {
                if (!empty($this->currentY)) {
                    $this->y = $this->currentY;
                    $this->currentY = 0;
                }
                $this->setPage($this->getNumPages());
            }
            $firstcell = true;
            //CELLS
            foreach ($line as $kk=>$cell) {
                $cellOptions = $options;
                $value = $cell;

                if (is_array($cell)) {
                    $value = $cell['value'];
                    if (!empty($cell['options']) && is_array($cell['options'])) {
                        $cellOptions = $this->initOptionsForWriteCellTable($cell['options'], $item);
                    }
                }

                //Bug45077-replacing single quote entities
                $value=str_replace("&#039;", "'", $value);
                //Bug45077-replacing double quote entities
                $value=str_replace("&quot;", '"', $value);

                if ($even && !empty($options['evencolor'])) {
                    $this->SetFillColorArray($this->convertHTMLColorToDec($options['evencolor']));
                    $cellOptions['fillstate']=1;
                } else {
                    if (!$even && !empty($options['oddcolor'])) {
                        $this->SetFillColorArray($this->convertHTMLColorToDec($options['oddcolor']));
                        $cellOptions['fillstate']=1;
                    }
                }

                if ($firstrow) {
                    $this->MultiCell($options["width"][$kk], $h, $value, $cellOptions['border'], $cellOptions['align'], $cellOptions['fillstate'], 0, '', '', true, $options['stretch'][$kk], $cellOptions['ishtml'], true, 0, false);
                } else {
                    if ($firstcell) {
                        // add page only once (for the first cell)
                        $this->MultiCell($options["width"][$kk], $h, $value, $cellOptions['border'], $cellOptions['align'], $cellOptions['fillstate'], 0, '', '', true, 0, $cellOptions['ishtml'], true, 0, true);
                        $firstcell = false;
                    } else {
                        $this->MultiCell($options["width"][$kk], $h, $value, $cellOptions['border'], $cellOptions['align'], $cellOptions['fillstate'], 0, '', '', true, 0, $cellOptions['ishtml'], true, 0, false);
                    }
                }

                $this->SetFillColorArray($this->convertHTMLColorToDec($options['fill']));
            }
            $this->Ln();
            $firstrow = false;
        }
        $this->SetFont($fontFamily, $fontStyle, $fontSize);
        $this->SetTextColor(0, 0, 0);
    }

    /**
     * This method allow printing a table using the writeHTML method with a formatted array in parameter
     * This method can also return the table as HTML code
     * @param $item Array[line number (0 to x)][cell header] = Cell content OR
     *              Array[line number (0 to x)][cell header]['value'] = Cell content AND
     *              Array[line number (0 to x)][cell header]['options'] = Array[cell properties] = values
     * @param $returnHtml (bool) Return the table as HTML code instead of printing the HTML table
     * @param $options Array which can contain : table (array of "HTML proprty"=>"value"),td (array of "HTML proprty"=>"value"), tr (array of "HTML proprty"=>"value"), isheader(bool), header (array of "HTML proprty"=>"value"), width (array 'column name'=>'width value + unit OR nothing')
     * @return the HTML code if $returnHtml set to true
     */
    public function writeHTMLTable($item, $returnHtml=false, $options=null)
    {
        //TODO ISSUE - width in % for the td have to be multiply by the number of column.
        //     ex: for a width of 20% in a table of 6 columns the width will have to be 120% (20*6).
        $html="";
        $line="";
        if (!empty($options)) {
            foreach ($options as $k=>$v) {
                $tmp[strtolower($k)]=$v;
            }
            $options=$tmp;
        } else {
            $options=array();
        }
        if (!isset($options["isheader"]) || $options["isheader"] == true) {
            if (!empty($options["header"])) {
                foreach ($options["header"] as $k=>$v) {
                    $tmp[strtolower($k)]=$v;
                }
                $options["header"]=$tmp;
            } else {
                $options["header"]=array("tr"=>array("bgcolor"=>"#DCDCDC"),"td"=>array());
            }

            foreach ($item[0] as $k => $v) {
                if (!empty($options["width"])) {
                    $options["header"]["td"]["width"]=$options["width"][$k];
                }
                $line.=$this->wrap("td", $k, $options["header"]);
            }
            $html.=$this->wrap("tr", $line, $options["header"]);
        }
        $even = true;
        foreach ($item as $k=>$v) {
            $even = !$even;
            $line="";

            if ($even) {
                if (isset($options['evencolor'])) {
                    $options["tr"]["bgcolor"] = $options['evencolor'];
                }
            } else {
                if (isset($options['oddcolor'])) {
                    $options["tr"]["bgcolor"] = $options['oddcolor'];
                }
            }
            foreach ($v as $kk => $vv) {
                if (!empty($options["width"]) && isset($options["width"][$kk])) {
                    $options["td"]["width"]=$options["width"][$kk];
                }
                $line.=$this->wrap("td", $vv, $options);
            }
            $html.=$this->wrap("tr", $line, $options);
        }
        $html=$this->wrap("table", $html, $options);
        if ($returnHtml) {
            return $html;
        } else {
            $this->writeHTML($html);
        }
    }

    /**
     * return the HTML code of the value wrap with the tag $tag. This method handle options (general and specific)
     * @param $tag
     * @param $value
     * @param $options
     * @return the HTML wrapped code
     */
    private function wrap($tag, $value, $options)
    {
        if (empty($options[$tag])) {
            $options[$tag] = array();
        }
        if (is_array($value)) {
            if (isset($value["options"])) {
                // The options of a specific entity overwrite the general options
                $options[$tag] = $value["options"];
            }
            if (isset($value["value"])) {
                $value = $value["value"];
            } else {
                $value = "";
            }
        }
        return wrapTag($tag, $value, $options[$tag]);
    }

    /**
     * Return the heigth of a line depending of the width, the font and the content
     * @param $line Array containing the data of all the cells of the line
     * @param $width Array containing the width of all the cells of the line
     * @return The heigth of the line
     */
    private function getLineHeightFromArray($line, $width)
    {
        $h=0;
        foreach ($line as $kk=>$cell) {
            $cellValue = $cell;
            if (is_array($cellValue)) {
                $tmp = $cellValue['value'];
                $cellValue = $tmp;
            }
            if ($h<$this->getNumLines($cellValue, $width[$kk])) {
                $h=$this->getNumLines($cellValue, $width[$kk]);
            }
        }
        return $h * $this->FontSize * $this->cell_height_ratio + 2 * $this->cMargin;
    }

    /**
     * Private method for writeCellTable which format and initialize the options array.
     * @param $options array
     * @param $item array
     * @return $options array
     */
    private function initOptionsForWriteCellTable($options, $item)
    {
        if (!empty($options)) {
            foreach ($options as $k=>$v) {
                $tmp[strtolower($k)]=$v;
            }
            $options=$tmp;
        } else {
            $options=array();
        }
        // set to default if empty
        if (empty($options["width"]) || !is_array($options["width"])) {
            $colNum = count($item[0]);
            $defaultWidth = $this->getRemainingWidth()/$colNum;
            foreach ($item[0] as $k => $v) {
                $options["width"][$k]=$defaultWidth;
            }
        } else {
            foreach ($options["width"] as $k => $v) {
                $options["width"][$k] = $this->getHTMLUnitToUnits($v, $this->getRemainingWidth());
            }
        }

        if (empty($options["border"])) {
            $options["border"]=0;
        }

        if (empty($options["align"])) {
            $options["align"]="L";
        }

        if (empty($options['ishtml'])) {
            $options['ishtml'] = false;
        }
        if (empty($options['border'])) {
            $options['border'] = 0;
        }
        foreach ($item[0] as $k => $v) {
            if (empty($options['stretch'][$k])) {
                $options['stretch'][$k] = self::STRETCH_NONE;
            }
        }

        if (!empty($options['fill'])) {
            $this->SetFillColorArray($this->convertHTMLColorToDec($options['fill']));
            $options['fillstate']=1;
        } else {
            $options['fill']="#FFFFFF";//white
            $options['fillstate']=0;
        }

        if (!empty($options['fontfamily'])) {
            $fontFamily = $options['fontfamily'];
        } else {
            $fontFamily = $this->getFontFamily();
        }
        if (!empty($options['fontsize'])) {
            $fontSize = $options['fontsize'];
        } else {
            $fontSize = $this->getFontSizePt();
        }
        if (!empty($options['fontstyle'])) {
            $fontStyle = $options['fontstyle'];
        } else {
            $fontStyle = $this->getFontStyle();
        }
        if (!empty($options['textcolor'])) {
            $this->SetTextColorArray($this->convertHTMLColorToDec($options['textcolor']));
        } else {
            $this->SetTextColor(0, 0, 0);//black
        }

        $this->SetFont($fontFamily, $fontStyle, $fontSize);

        return $options;
    }

    /**
    * This is method is fix for a better handling of the count. This method now handle the line break
    * between words.
    * This method returns the estimated number of lines required to print the text.
    * @param string $txt text to print
    * @param float $w width of cell. If 0, they extend up to the right margin of the page.
    * @return int Return the estimated number of lines.
    * @access public
    * @since 4.5.011
    * @OVERRIDE
    */
    public function getNumLines($txt, $w = 0, $reseth = false, $autopadding = true, $cellpadding = '', $border = 0)
    {
        $lines = 0;
        if (empty($w) or ($w <= 0)) {
            if ($this->rtl) {
                $w = $this->x - $this->lMargin;
            } else {
                $w = $this->w - $this->rMargin - $this->x;
            }
        }
        // max column width
        $wmax = $w - (2 * $this->cMargin);
        // remove carriage returns
        $txt = str_replace("\r", '', $txt);
        // divide text in blocks
        $txtblocks = explode("\n", $txt);
        // for each block;
        foreach ($txtblocks as $block) {
            // estimate the number of lines
            if (empty($block)) {
                $lines++;
            // If the block is in more than one line
            } else {
                if (ceil($this->GetStringWidth($block) / $wmax)>1) {
                    //divide into words
                    $words = explode(" ", $block);
                    //TODO explode with space is not the best things to do...
                    $wordBlock = "";
                    $first=true;
                    $lastNum = 0;
                    $run = false;

                    for ($i=0; $i<count($words); $i++) {
                        if ($first) {
                            $wordBlock = $words[$i];
                        } else {
                            $wordBlock .= " ".$words[$i];
                        }
                        if (ceil($this->GetStringWidth($wordBlock) / $wmax)>1) {
                            if ($first) {
                                $lastNum = ceil($this->GetStringWidth($wordBlock) / $wmax);
                                $run = true;
                                $first = false;
                            } else {
                                if ($run && $lastNum == ceil($this->GetStringWidth($wordBlock) / $wmax)) {
                                    // save the number of line if it is the last loop
                                    if ($i+1 == count($words)) {
                                        $lines += ceil($this->GetStringWidth($wordBlock) / $wmax);
                                    }
                                    continue;
                                } else {
                                    $first = true;
                                    $lines += ceil($this->GetStringWidth(substr($wordBlock, 0, (strlen($wordBlock) - strlen(" ".$words[$i])))) / $wmax);
                                    $i--;
                                    $lastNum = 0;
                                    $run = false;
                                }
                            }
                        } else {
                            $first = false;
                        }
                        // save the number of line if it is the last loop
                        if ($i+1 == count($words)) {
                            $lines += ceil($this->GetStringWidth($wordBlock) / $wmax);
                        }
                    }
                } else {
                    $lines++;
                }
            }
        }
        return $lines;
    }

    /**
     * Disable zlib output compression if we are downloading the PDF.
     *
     * @see TCPDF::Output()
     */
    public function Output($name='doc.pdf', $dest='I')
    {
        if ($dest == 'I' || $dest == 'D') {
            ini_set('zlib.output_compression', 'Off');
        }

        return parent::Output($name, $dest);
    }
}
